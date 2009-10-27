{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

  import Data.Vector
  import Data.Maybe
  import qualified Data.List as L
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Control.Monad
  import Control.Monad.ST
  --import qualified Control.Monad.ST.Lazy as Lazy (ST)
  --import GHC.ST		( ST(..), runST )
  import Statistics.RandomVariate
  import Statistics.Distribution
  import Statistics.Distribution.Exponential
  import Data.Array.Vector
  import System.Environment
  import Text.Printf

  -- some basic aliases and helper data types
  type Point = Vector3
  type Direction = Vector3
  showVector3 v = "<"++ show (v3x v) ++", "++ show (v3y v) ++", "++ show (v3z v) ++">"
  type Path = [Point]
  type MLTState = Path
  data Ray = Ray {
      origin::Point,
      direction::Direction
    } deriving (Eq)
  instance Show Ray where
    show (Ray o d) = "Ray starting at "++ (showVector3 o) ++" going in Direction "++ (showVector3 d) 

  fromTwoPoints :: Point -> Point -> Ray
  fromTwoPoints v w = Ray v (normalize (w-v))

  data Plane = Plane {
      planeNormal :: Direction,
      planeOffset :: Double {-- how much the plane is shifted in normal direction --}
    } deriving (Eq,Show)
  type Interval = (Double,Double)

  -- some utility functions
  normsq v = v `vdot` v
  normalize v = (1/vmag v) *<> v
  
  -- applies f only at the kth element of the list and let's the rest untouched
  mapAt :: Int -> (a->a) -> [a] -> [a]
  mapAt _ _ [] = []
  mapAt k f (x:xs)
    | k==0      = f x : xs
    | otherwise = x : mapAt (k-1) f xs

  -- stuff lives in Containers,
  data Container = forall s. (Confineable s, Show s) => Container s

  instance Show Container where
    show (Container c) = show c
  
  -- Containers should be able to do the following: 
  class Confineable a where
    contains      :: a -> Point -> Bool
    intersectedBy :: a -> Ray -> [Interval]
    randomPointIn :: a -> Gen s -> ST s Point

  instance Confineable Container where
    contains      (Container c) = contains c
    {-# INLINE contains #-}

    intersectedBy (Container c) = intersectedBy c
    {-# INLINE intersectedBy #-}

    randomPointIn (Container c) = randomPointIn c
    {-# INLINE randomPointIn #-}
  

  -- Sphere
  data Sphere = Sphere {
      center :: Point,
      radius :: Double
    } deriving (Eq)
  instance Show Sphere where
    show (Sphere c r) = "Sphere at " ++ (showVector3 c) ++ " with Radius " ++ (show r)

  -- Sphere implements the Confineable type class functions
  instance Confineable Sphere where
    contains (Sphere c r) p = normsq (p - c) <= r*r
    {-# INLINE contains #-}

    intersectedBy (Sphere center radius) (Ray origin direction) = 
      let offset = origin - center
          oo = offset `vdot` offset
          od = offset `vdot` direction
          dd = direction `vdot` direction
          discriminant = od*od - dd*(oo - radius*radius)
      in if discriminant <= 0 
          then []
          else let ddinv = 1 / dd
                   alpha0 = ddinv * (-od)
                   alphadelta = ddinv * sqrt discriminant
                   alphas = (alpha0 - alphadelta,
                             alpha0 + alphadelta)
               in [alphas]
  
    randomPointIn (Sphere center radius) g = do
      unitpoint <- randomPointInUnitSphere g
      return $ center + radius *<> unitpoint
    {-# INLINE randomPointIn #-}
          
  -- PhaseFunction determines the Direction-dependent scattering probability
  data PhaseFunction = Isotropic
                     | Anisotropic (Direction->Direction->Double)
                     
  scatterPDF :: PhaseFunction -> Direction -> Direction -> Double
  scatterPDF         Isotropic   _    _ = 1/(4*pi)
  scatterPDF (Anisotropic phi) win wout = phi win wout
  {-# INLINE scatterPDF #-}
  
  randomDirectionFrom :: PhaseFunction -> Direction -> Gen s -> ST s Direction
  randomDirectionFrom       Isotropic   _ g = randomIsotropicDirection g
  randomDirectionFrom (Anisotropic _) win g = undefined
  {-# INLINE randomDirectionFrom #-}
  
  isIsotropic Isotropic = True
  isIsotropic         _ = False
  {-# INLINE isIsotropic #-}
  
  instance Show PhaseFunction where
    show       Isotropic = "Isotropic"
    show (Anisotropic _) = "Anisotropic"

  weighPhaseFunctions :: [(Double,PhaseFunction)] -> PhaseFunction
  weighPhaseFunctions phiswithweights' = let
      phiswithweights = filter ((>0).fst) phiswithweights'
      (isophiswithweights,aniphiswithweights) = L.partition (isIsotropic.snd) phiswithweights
      aniweightsum = sum $ map fst aniphiswithweights
    in if aniweightsum==0
         then Isotropic
         else let isoweightsum = sum $ map fst isophiswithweights
                  normweight = 1 / (isoweightsum + aniweightsum)
              in Anisotropic (\win wout -> normweight*(
                      isoweightsum/(4*pi) +
                      aniweightsum * sum (map ((\phi -> scatterPDF phi win wout).snd) aniphiswithweights)
                    )
                  )
  
  -- a Texture contains the spatial dependency of 'a'
  data Texture a = Homogenous a
                 | Inhomogenous (Point->a)
  
  instance (Show a) => Show (Texture a) where
    show   (Homogenous x) = show x
    show (Inhomogenous _) = "Inhomogenous"

  isHomogenous (Homogenous _) = True
  isHomogenous _ = False
  {-# INLINE isHomogenous #-}

  addTexturedDoubles :: Texture Double -> Texture Double -> Texture Double
  addTexturedDoubles    (Homogenous x)    (Homogenous y) = Homogenous (x+y)
  addTexturedDoubles    (Homogenous x) (Inhomogenous  f) = Inhomogenous (\p -> x + f p )
  addTexturedDoubles (Inhomogenous  f)    (Homogenous x) = Inhomogenous (\p -> x + f p )
  addTexturedDoubles (Inhomogenous f1) (Inhomogenous f2) = Inhomogenous (\p -> f1 p + f2 p)
  {-# INLINE addTexturedDoubles #-}

  addTexturedPhaseFunctions :: [(Texture Double, Texture PhaseFunction)] -> Texture PhaseFunction
  addTexturedPhaseFunctions sigmaphitexturepairs = 
    let (sigmatexts,phitexts) = unzip sigmaphitexturepairs
        (homsigmas,inhomsigmas) = L.partition isHomogenous sigmatexts
        (  homphis,  inhomphis) = L.partition isHomogenous phitexts
        nohomsigmas   = null homsigmas
        noinhomsigmas = null inhomsigmas
        nohomphis     = null homphis
        noinhomphis   = null inhomphis
    in if noinhomsigmas && noinhomphis
        then let sigmas = map (`evaluatedAt` undefined) homsigmas
                 phis   = map (`evaluatedAt` undefined) homphis
             in Homogenous $ weighPhaseFunctions $ zip sigmas phis
        else let sigmasat p = map (`evaluatedAt` p) sigmatexts
                 phisat   p = map (`evaluatedAt` p) phitexts
             in Inhomogenous (\p -> weighPhaseFunctions $ zip (sigmasat p) (phisat p))
    
          
  -- evaluates a texture of type a at a specific point
  evaluatedAt :: Texture a -> Point -> a
  evaluatedAt   (Homogenous x) _ = x
  evaluatedAt (Inhomogenous f) p = f p
  {-# INLINE evaluatedAt #-}

  -- Material contains local absorption and scattering properties
  data Material = Material (Texture Double) (Texture Double) (Texture PhaseFunction)
  materialTexturedAbsorption    (Material kappatex        _      _) = kappatex
  materialTexturedScattering    (Material        _ sigmatex      _) = sigmatex
  materialTexturedExtinction    (Material kappatex sigmatex      _) = addTexturedDoubles kappatex sigmatex
  materialTexturedPhaseFunction (Material        _        _ phitex) = phitex

  instance Show Material where
    show (Material kappatex sigmatex phitex) =  "kappa="++(show kappatex)++
                                               ", sigma="++(show sigmatex)++
                                               ", phi="++(show phitex)

  boilDownMaterials :: [Material] -> Material
  boilDownMaterials materials = 
    let kappatextures = map materialTexturedAbsorption materials
        summedkappa = foldl addTexturedDoubles (Homogenous 0) kappatextures
        sigmatextures = map materialTexturedScattering materials
        summedsigma = foldl addTexturedDoubles (Homogenous 0) sigmatextures
        phitextures = map materialTexturedPhaseFunction materials
        summedphi = addTexturedPhaseFunctions $ zip sigmatextures phitextures
    in Material summedkappa summedsigma summedphi
    

  summedMaterialAt :: [Entity] -> Point -> Material
  summedMaterialAt entities point = boiledMaterial
    where
      relevantEntities = filter ((`contains` point).entityContainer) entities
      boiledMaterial = boilDownMaterials.concat $ map entityMaterials relevantEntities
  {-# INLINE summedMaterialAt #-}

  propertyAt :: (Material->Texture a) -> [Entity] -> Point -> a
  propertyAt getpropfrom entities point = getpropfrom summat `evaluatedAt` point
    where summat = summedMaterialAt entities point
  {-# INLINE propertyAt #-}
    
  absorptionAt :: [Entity] -> Point -> Double
  absorptionAt = propertyAt materialTexturedAbsorption
  {-# INLINE absorptionAt #-}
    
  scatteringAt :: [Entity] -> Point -> Double
  scatteringAt = propertyAt materialTexturedScattering
  {-# INLINE scatteringAt #-}
  
  extinctionAt :: [Entity] -> Point -> Double
  extinctionAt = propertyAt materialTexturedExtinction
  {-# INLINE extinctionAt #-}

  phaseFunctionAt :: [Entity] -> Point -> PhaseFunction
  phaseFunctionAt = propertyAt materialTexturedPhaseFunction
  {-# INLINE phaseFunctionAt #-}

  -- an Entity is a container filled with light-influencing material
  data Entity = Entity {
      entityContainer::Container,
      entityMaterials::[Material]
    }
    
  instance Show Entity where
    show (Entity container materials) = "Entity contained by a " ++ (show container) ++
                                        " filled with " ++ (show materials)
  

  -- a Lightsource is a container filled with emitting material
  data Lightsource = Lightsource {
      lightsourceContainer::Container,
      brightness::Texture Double
    }

  -- a Scene contains all lightsources and entities
  data Scene = Scene {
      sceneLightsources::[Lightsource],
      sceneEntities::[Entity]
    }
    
  -- the Bool is used to represent if the IntervalLimiter is the begin of an interval
  data IntervalLimiter a = IntervalLimiter {
      intervalLimiterKey::a,
      isIntervalLimiterBegin::Bool,
      intervalLimiterPosition::Double
    }
  instance (Show a) => Show (IntervalLimiter a) where
    show (IntervalLimiter key isbegin pos) =
      (if isbegin then "Begin(" else "End(") ++ (show key) ++ "@" ++ (show pos) ++ ")"
  -- we use the equality just in terms of position (for Ord)
  instance Eq (IntervalLimiter a) where
    (==) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1==pos2
    {-# INLINE (==) #-}
  -- we order IntervalLimiters by position
  instance Ord (IntervalLimiter a) where
    (<=) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1<=pos2
    {-# INLINE (<=) #-}
    
  
  fromInterval :: Interval -> a -> [IntervalLimiter a]
  fromInterval (start,end) key = [IntervalLimiter key  True start,
                                  IntervalLimiter key False   end]
  {-# INLINE fromInterval #-}
                                  
  -- transforms a list of tagged intervals possibly overlapping and with coinciding
  -- endpoints into a list of disjoint intervals with taglist
  cutOverlaps :: (Ord a) => [IntervalLimiter a] -> [(Interval,S.Set a)]
  cutOverlaps limiters = cutOverlaps' S.empty sortedLimiters
                         where sortedLimiters = L.sort limiters
  
  cutOverlaps' :: (Ord a) => (S.Set a) -> [IntervalLimiter a] -> [(Interval, S.Set a)]
  cutOverlaps' active         [] = []
  cutOverlaps' active (l1:l2:ls)
    | l1==l2    = rest
    | otherwise = ((intervalLimiterPosition l1,
                    intervalLimiterPosition l2), newactive):rest
    where getNewActive (IntervalLimiter key isbegin _)
            | isbegin   = S.insert key active
            | otherwise = S.delete key active
          --getNewActive (IntervalLimiter key isbegin _) =
          --  (if isbegin then S.insert else S.delete) key active
          newactive = getNewActive l1
          rest = cutOverlaps' newactive (l2:ls)
  cutOverlaps' active ((IntervalLimiter _ isbegin _):[]) =
    if isbegin then error "last intervallimiter shouldn't be a begin" else []  
  
  {--
  cutoverlapstestcase = concat.map (uncurry fromInterval) $
    zip [(1,5),(1,7),(2,6),(2,5),(3,4),(1,5)] [1,2,3,4,5,6]
  testCutOverlaps = cutOverlaps cutoverlapstestcase == [
      ((1.0,2.0),S.fromList [1,2,6])      ,((2.0,3.0),S.fromList [1,2,3,4,6]),
      ((3.0,4.0),S.fromList [1,2,3,4,5,6]),((4.0,5.0),S.fromList [1,2,3,4,6]),
      ((5.0,6.0),S.fromList [2,3])        ,((6.0,7.0),S.fromList [2])
    ]
    --}


  disjunctIntervalsWithCondensedMaterials :: [Entity] -> Ray -> [(Interval,Material)]
  disjunctIntervalsWithCondensedMaterials entities ray = let
      intervals = [entityContainer entity `intersectedBy` ray | entity<-entities]
      entityindices = [(0::Int)..(length entities -1)]
      nestedindexedintervals = zip intervals entityindices
      indexedintervals = concat [map (\x -> (x, snd ii)) (fst ii) | ii<-nestedindexedintervals]
      taggeddisjointintervals = cutOverlaps.concat $ map (uncurry fromInterval) indexedintervals
      intervalmaterialindices = map (S.toList . snd) taggeddisjointintervals
      entitymateriallists = map entityMaterials entities
      intervalmaterials = [concatMap (entitymateriallists!!) materialindices | materialindices<-intervalmaterialindices]
      condensedintervalmaterials = map boilDownMaterials intervalmaterials
      disjointintervals = fst $ unzip taggeddisjointintervals
      refinedintervalswithmaterials = zip disjointintervals condensedintervalmaterials
    in refinedintervalswithmaterials
    

  -- sort out intervals that are before the ray starts or further away than maxDist
  -- and clip intervals that span across these bounds
  clipAndFilterIntervalsWithMaterial :: Double -> [(Interval,Material)] -> [(Interval,Material)]
  clipAndFilterIntervalsWithMaterial maxDist intervalswithmaterials = let
      maxDist' = max 0 maxDist
      outsideOfInterest = uncurry (\x y -> x>=maxDist' || y<=0) . fst
      filterIntervals = filter (not.outsideOfInterest)
      clipInterval (x,y) = (max 0 x, min maxDist' y)
      clipIntervals = map (uncurry (\x y -> (clipInterval x, y)))
    in filterIntervals . clipIntervals $ intervalswithmaterials
  
  
  data ProbeResult = MaxDepthAtDistance Double | MaxDistAtDepth Double deriving (Show)
  getProbeResultDepth (MaxDistAtDepth depth) = depth
  {-# INLINE getProbeResultDepth #-}
  
  consumeIntervals :: Ray -> Double -> Double -> [(Interval,Material)] -> ProbeResult
  consumeIntervals ray maxDepth accumulatedDepth [] = MaxDistAtDepth accumulatedDepth
  consumeIntervals ray maxDepth accumulatedDepth (((a,b), m):rest) = let
      remainingDepth = maxDepth - accumulatedDepth
      intervalLength = b - a
      extinction = materialTexturedExtinction m `evaluatedAt` undefined -- only works for Homogenous Materials
      intervalDepth = extinction * intervalLength
    in if remainingDepth > intervalDepth
         then consumeIntervals ray maxDepth (accumulatedDepth + intervalDepth) rest
         else let neededDist = remainingDepth / extinction
              in MaxDepthAtDistance (a+neededDist)
  
  -- casts a Ray through a list of entities until either a maximum optical depth
  -- or a maximum distance is reached
  probeEntitiesWithRay :: [Entity] -> Ray -> Double -> Double -> ProbeResult
  probeEntitiesWithRay entities ray maxDist maxDepth = let
      refinedintervalswithtextures = disjunctIntervalsWithCondensedMaterials entities ray
      clippedintervals = clipAndFilterIntervalsWithMaterial maxDist refinedintervalswithtextures
    in consumeIntervals ray maxDepth 0 clippedintervals
            
            
  opticalDepthBetween entities v w = let
      infinity = 1/(0::Double)
      distance = vmag $ w - v
      ray = Ray v ((1/distance)*<>(w-v))
    in getProbeResultDepth $ probeEntitiesWithRay entities ray distance infinity
            
            
  --
  -- random-dependent stuff starts here!
  --
  
  startSeed = runST $ create >>= save

  -- generates a uniformly distributed random Int in the interval (a,b)
  randomIntInRange :: (Int,Int) -> Gen s -> ST s Int
  randomIntInRange (a,b) g =
    if a==b
      then return a
      else do
        u1 <- uniform g
        let rnddbl = u1::Double
            ad = fromIntegral a
            bd = fromIntegral b
        return $! a + truncate ((bd-ad+1)*rnddbl)

  randomListIndex :: [a] -> Gen s -> ST s Int
  randomListIndex [] g = error "cannot choose an elementindex in an empty list"
  randomListIndex l g = randomIntInRange (0,length l - 1) g

  -- chooses uniformly random a list-element
  randomChoice :: [a] -> Gen s -> ST s a
  randomChoice [] g = error "cannot choose an element of an empty list"
  randomChoice choices g = do
    rndindex <- randomListIndex choices g
    return $ choices!!rndindex
    
  randomWeightedChoice :: [(Double,a)] -> Gen s -> ST s a
  randomWeightedChoice weightedchoices g = do
    let (weights,_) = unzip weightedchoices
        totalweight = sum weights
        step ((     _,choice):[])  _ = choice
        step ((weight,choice):wcs) remainingweight
          | remainingweight > weight = step wcs (remainingweight-weight)
          | otherwise = choice
    u1 <- uniform g
    let randomweight = totalweight*(u1::Double)
    return $ step weightedchoices randomweight

  randomWeightedChoices weightedchoices n g =
    replicateM n $ randomWeightedChoice weightedchoices g

  -- generates a d-distributed sample
  distributionSample d g = do
    u1 <- uniform g
    return $ quantile d (u1::Double)

  -- generates a random direction vector with length one
  randomIsotropicDirection :: Gen s -> ST s Direction
  randomIsotropicDirection g = do
    u1 <- uniform g
    u2 <- uniform g
    let z = 2*(u1::Double) - 1
        phi = 2*pi*(u2::Double)
        rho = sqrt (1 - z*z)
    return $ Vector3 (rho * cos phi) (rho * sin phi) z

  randomExponential3D :: Double -> Gen s -> ST s Point
  randomExponential3D lambda g = do
    rdir <- randomIsotropicDirection g
    rexp <- distributionSample (fromLambda (1/lambda)) g
    return $ rexp *<> rdir
    
  randomPointInUnitSphere :: Gen s -> ST s Point
  randomPointInUnitSphere g = do
    u1 <- uniform g
    u2 <- uniform g
    u3 <- uniform g
    let x = 2*(u1::Double)-1
        y = 2*(u2::Double)-1
        z = 2*(u3::Double)-1
        v = Vector3 x y z
    if normsq v <= 1
      then return v
      else randomPointInUnitSphere g
  
  randomPointInEntities entities g = do
    entity <- randomChoice entities g
    let container = entityContainer entity
    randomPointIn container g
  
  randomPathOfLength scene n g = do
    let entities = sceneEntities scene
    replicateM n $ randomPointInEntities entities g
  
  randomIsotropicDirections n g =
    replicateM n $ randomIsotropicDirection g

  runRandomActions seed = runST $ do
    g <- restore seed
    replicateM 10 $ randomIsotropicDirection g

--
-- path sampling
--

  


--
-- metropolis stuff
--

  metropolisStep :: Scene -> [(Double,Mutation)] -> MLTState -> Gen s -> ST s MLTState
  metropolisStep scene mutations oldstate g = do
    mutation <- randomWeightedChoice mutations g
    newstate <- mutateWith mutation scene oldstate g
    let accprob = acceptanceProbabilityOf mutation scene oldstate newstate
    if accprob==0
      then return oldstate
      else do rnd <- uniform g
              if rnd<=accprob
                then return newstate
                else return oldstate

  nMLTSteps :: Scene -> [(Double,Mutation)] -> Int -> MLTState -> Gen s -> ST s [MLTState]
  nMLTSteps scene mutations n initialstate g
    | n==1      = return [initialstate]
    | otherwise = do
        newpath <- metropolisStep scene mutations initialstate g
        futureresults <- nMLTSteps scene mutations (n-1) newpath g
        return (initialstate:futureresults)

  mltActionStep :: Scene -> [(Double,Mutation)] -> (MLTState -> a) -> Int -> (Seed,MLTState) -> ((Seed,MLTState),[a])
  mltActionStep scene mutations extractor n (seed,initialpath) = runST $ do
    g <- restore seed
    states <- nMLTSteps scene
                        mutations
                        n
                        initialpath
                        g
    g' <- save g
    return ((g',last states), map extractor states)
  
  mltAction :: Scene -> [(Double,Mutation)] -> (MLTState -> a) -> Int -> Int -> Int -> [a]
  mltAction scene mutations extractor seedint n chunksize =
    let ipl = 1 -- initial path length
        (seed,initialpath) = runST $ do
                               g <- initialize $ singletonU $ fromIntegral seedint
                               ip <- randomPathOfLength scene ipl g
                               s <- save g
                               return (s,ip)
        chunks = n `div` chunksize
        step k oldstate
          | k==0      = []
          | otherwise = chunk ++ step (k-1) newstate
                        where (newstate,chunk) = mltActionStep scene mutations extractor chunksize oldstate

    in step chunks (seed,initialpath)
    

  -- the standard (possibly wasteful) way to compute the acceptance probability
  -- f x should return the probability density for state x
  -- t x y should return the transition probability from state x to state y
  defautAcceptanceProbability :: (a -> Double) -> (a -> a -> Double) -> a -> a -> Double
  defautAcceptanceProbability f t oldstate newstate =
    (/) ((f newstate) * (t newstate oldstate))
        ((f oldstate) * (t oldstate newstate))
        
  {--
    -- Mutations should adher to this type class:
    -- gives the acceptance probability for the transition (with parameters of type a)
    -- to a new state
    acceptanceProbabilityOf :: a -> Path -> Path -> Double
    -- mutates 
    mutateWith :: a -> Path -> Gen s -> ST s Path
  --}
  class Mutating a where
    mutateWith              :: a -> Scene -> MLTState -> Gen s -> ST s MLTState
    acceptanceProbabilityOf :: a -> Scene -> MLTState -> MLTState -> Double
    
  -- define algebraic datatype which can hold all Mutations which adher to the 'Mutating' type class,
  data Mutation = forall s. (Mutating s) => Mutation s

  instance Mutating Mutation where
    mutateWith              (Mutation m) = mutateWith m
    {-# INLINE mutateWith #-}
    acceptanceProbabilityOf (Mutation m) = acceptanceProbabilityOf m
    {-# INLINE acceptanceProbabilityOf #-}
  
  -- implemented Mutations
  data ExponentialNodeTranslation = ExponentialNodeTranslation Double
  data ExponentialImageNodeTranslation = ExponentialImageNodeTranslation Double
  data IncDecPathLength = IncDecPathLength Double

  instance Mutating ExponentialNodeTranslation where
    mutateWith (ExponentialNodeTranslation l) scene oldstate g = do
      rndindex <- randomListIndex oldstate g
      rndtranslation <- randomExponential3D l g
      return $ mapAt rndindex (+rndtranslation) oldstate
    acceptanceProbabilityOf (ExponentialNodeTranslation l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) (\_ _ -> 1) oldstate newstate
  
  instance Mutating ExponentialImageNodeTranslation where
    mutateWith (ExponentialImageNodeTranslation l) scene oldstate g = do
      rndtranslation <- randomExponential3D l g
      return $ mapAt (length oldstate - 1) (+rndtranslation) oldstate
    acceptanceProbabilityOf (ExponentialImageNodeTranslation l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) (\_ _ -> 1) oldstate newstate
  
  instance Mutating IncDecPathLength where
    mutateWith (IncDecPathLength l) scene oldstate g = do
      u <- uniform g
      let coinflip = u::Bool
          oldnodecount = length oldstate
      if coinflip
        then do -- add node
          rndtranslation <- randomExponential3D l g
          addindex <- randomIntInRange (0,oldnodecount) g
          let (prelist,postlist) = splitAt addindex oldstate
              anchor
                | addindex==0            = if null postlist then Vector3 0 0 0 else head postlist
                | addindex==oldnodecount = if null  prelist then Vector3 0 0 0 else last  prelist
                | otherwise              = let prenode = last prelist
                                               postnode = head postlist
                                           in 0.5*<>(prenode + postnode)
              newnode = anchor + rndtranslation
          return $ prelist ++ [newnode] ++ postlist
        else if oldnodecount > 1
          then do -- delete node
            delindex <- randomListIndex oldstate g
            let (prelist,postlist) = splitAt delindex oldstate
            return $ prelist ++ (tail postlist)
          else
            return oldstate
    acceptanceProbabilityOf (IncDecPathLength l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) (incDecPathLengthTransitionProbability l) oldstate newstate


  incDecPathLengthTransitionProbability :: Double -> MLTState -> MLTState -> Double
  incDecPathLengthTransitionProbability lambda oldpath newpath =
    let oldpathlength = length oldpath
        newpathlength = length newpath
    in 0.5 * if newpathlength > oldpathlength
      then -- added node
        let newnodeindex = fromMaybe oldpathlength $ L.findIndex (uncurry (/=)) $ zip oldpath newpath
            newnode = newpath!!newnodeindex
            anchor
              | newnodeindex==0             = if null oldpath then Vector3 0 0 0 else head oldpath
              | newnodeindex==oldpathlength = if null oldpath then Vector3 0 0 0 else last oldpath
              | otherwise                   = 0.5*<>(sum . take 2 $ drop (newnodeindex-1) oldpath)
            distance = vmag (newnode - anchor)
            exp3dpdf r = (exp (-r/lambda)) / (4*pi*lambda*r*r)
        in (exp3dpdf distance) / fromIntegral (length newpath)
      else -- deleted node
        1 / max 1 (fromIntegral oldpathlength)


  --
  -- path stuff
  --
  
  addLightSourceNode :: Path -> Path
  addLightSourceNode = ((Vector3 0 0 0):)
  addSensorNode :: Path -> Path
  addSensorNode path = path ++ [((Vector3 1 1 0)*(last path) + (Vector3 0 0 1))]
  finalizePath :: Path -> Path
  finalizePath = addSensorNode . addLightSourceNode
  
  measurementContribution (Scene lightsources entities) path = if null path
    then 0
    else let scattercrosssections = map (entities `scatteringAt`) path
         in if any (==0) scattercrosssections
              then 0
              else let scatterfactor = product $ map (*(1/(4*pi))) scattercrosssections
                       completepath = finalizePath path
                       opticaldepth = sum $ edgeMap (opticalDepthBetween entities) completepath
                       edges = edgeMap (-) completepath
                       geometricfactors = product $ map normsq (init edges)
                   in scatterfactor * exp (-opticaldepth) / geometricfactors
                       
  
  edgeMap :: (a->a->b) -> [a] -> [b]
  edgeMap f     (a:[]) = []
  edgeMap f (a:b:rest) = f a b : edgeMap f (b:rest)
    
  -- test-stuff

  testEntities = let cont1 = Container (Sphere (Vector3 0.3 0 0) 0.5)
                     cont2 = Container $ Sphere (Vector3 (-0.5) 0 0) 0.3
                     cont3 = Container $ Sphere (Vector3 0.2 0.1 0.15) 0.1
                     cont4 = Container $ Sphere (Vector3 (-0.35) (-0.7) 0.0) 0.25
                     mat1 = Material (Homogenous  3.0) (Homogenous  4.0) (Homogenous Isotropic)
                     mat2 = Material (Homogenous  0.0) (Homogenous  7.0) (Homogenous Isotropic)
                     mat3 = Material (Homogenous 40.0) (Homogenous  0.0) (Homogenous Isotropic)
                     mat4 = Material (Homogenous  0.0) (Homogenous 40.0) (Homogenous Isotropic)
                     ent1 = Entity cont1 [mat1]
                     ent2 = Entity cont2 [mat2]
                     ent3 = Entity cont3 [mat3]
                     ent4 = Entity cont4 [mat4]
                 in [ent1,ent2,ent3,ent4]

  testRay = Ray (Vector3 0 0 0) $ normalize (Vector3 0 0 1)
  testScene = Scene [] testEntities
  
  testpath1 = [Vector3 0.000124201 (-0.0123588) 0.00415517]
  testpath2 = [Vector3 0.000124201 (-0.0123588) 0.00415517, Vector3 (-0.160068) (-0.499073) (-0.511448)]
  testacceptanceratio = [measurementContribution (standardScene 1) testpath1,
                         measurementContribution (standardScene 1) testpath2,
                         incDecPathLengthTransitionProbability 0.1 testpath1 testpath2,
                         incDecPathLengthTransitionProbability 0.1 testpath2 testpath1
                         ]
  --testacceptanceratio == [14.474861791724885,7.953182840852547,5.968310365946075e-2,0.25]
  standardScene sigma = let
      container = Container $ Sphere (Vector3 0 0 0) 1
      material = Material (Homogenous 0) (Homogenous sigma) (Homogenous Isotropic)
      entities = [Entity container [material]]
    in Scene [] entities
  
  
  showSample (x,y) = printf "{%f,%f}" x y
  showSamplesForMathematica :: [(Double,Double)] -> String
  showSamplesForMathematica samples = "testsamples={" ++ concat (L.intersperse ",\n" $ map showSample samples) ++ "};"
  
  showListForMathematica showelement list = "{" ++ concat (L.intersperse "," $ map showelement list) ++ "}\n"
  showGrid2DForMathematica = showListForMathematica (showListForMathematica show)
  
  binSamplesInGrid :: [(Double,Double)] -> Int -> [[Int]]
  binSamplesInGrid samples n = let
      pairMap f (x,y) = (f x, f y)

      gridIndices :: Int -> [[(Int,Int)]]
      gridIndices n = [[(i,j) | i<-indices1d] | j <- reverse indices1d]
        where indices1d = [0..n-1]

      toUnitSquare :: [(Double,Double)] -> [(Double,Double)]
      toUnitSquare samples = map (pairMap (\x -> 0.5*(x+1))) samples

      coordsToGridIndex :: Int -> (Double,Double) -> (Int,Int)
      coordsToGridIndex n point = pairMap (\x -> truncate $ (fromIntegral n)*x) point

      incBin :: M.Map (Int,Int) Int -> (Int,Int) -> M.Map (Int,Int) Int
      incBin oldmap index = M.insertWith' (+) index 1 oldmap

      getBinCount :: M.Map (Int,Int) Int -> (Int,Int) -> Int
      getBinCount fullbins index = M.findWithDefault 0 index fullbins
      
      emptybins = M.empty
      sampleindices = map (coordsToGridIndex n) (toUnitSquare samples)
      fullbins = L.foldl' incBin emptybins sampleindices
    in map (map $ getBinCount fullbins) (gridIndices n)
  
  binSamplesRadially :: [(Double,Double)] -> Int -> [Int]
  binSamplesRadially samples n = let
      gridIndices :: Int -> [Int]
      gridIndices n = [0..n-1]

      toCenterDistance :: [(Double,Double)] -> [Double]
      toCenterDistance samples = map (\(x,y) -> sqrt (x*x+y*y)) samples

      centerDistanceToBinIndex :: Int -> Double -> Int
      centerDistanceToBinIndex n distance = truncate $ (fromIntegral n)*distance

      incBin :: M.Map Int Int -> Int -> M.Map Int Int
      incBin oldmap index = M.insertWith' (+) index 1 oldmap

      getBinCount :: M.Map Int Int -> Int -> Int
      getBinCount fullbins index = M.findWithDefault 0 index fullbins

      emptybins = M.empty
      sampleindices = map (centerDistanceToBinIndex n) (toCenterDistance samples)
      fullbins = L.foldl' incBin emptybins sampleindices
    in map (getBinCount fullbins) (gridIndices n)
  
  putGridBinnedPhotonCounts gridsize samples = do
    let photonbincounts = binSamplesInGrid samples gridsize
    putStrLn $ "binnedphotons=" ++ init (showGrid2DForMathematica photonbincounts) ++ ";\n"
    
  putRadiallyBinnedPhotonCounts gridsize samples = do
    let photonbincounts = binSamplesRadially samples gridsize
    putStrLn $ "radialphotoncounts=" ++ init (showListForMathematica show photonbincounts) ++ ";\n"
  
  putPhotonList = putStrLn.showSamplesForMathematica
    
  main = do
    [gridsize,n] <- map read `fmap` getArgs
    let mutations = [(3,Mutation $ ExponentialNodeTranslation 0.1),
                     (4,Mutation $ ExponentialImageNodeTranslation 0.3),
                     (3,Mutation $ IncDecPathLength 0.1)]
        extractor = (\v -> (v3x v, v3y v)) . last
        --extractor = (subtract 1).length.finalizePath
        chunksize = min 2500 n
        sigma = 10
        scene = testScene --standardScene sigma
        samples = mltAction scene mutations extractor 19912 n chunksize
    --putRadiallyBinnedPhotonCounts gridsize samples
    putGridBinnedPhotonCounts gridsize samples
    --putPhotonList samples
    --putStrLn.show $ samples

    
  --main = putStrLn "Hi there!"
    
  -- ghc -O2 -fexcess-precision -funfolding-use-threshold=48 --make HMLT.hs -fforce-recomp
  -- ghc -O2 -fexcess-precision -funfolding-use-threshold=48 --make HMLT.hs -prof -auto-all -caf-all -fforce-recomp
  --}