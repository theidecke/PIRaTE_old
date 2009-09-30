{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

  import Data.Vector
  import Data.Maybe
  import qualified Data.List as List
  import qualified Data.Set as Set
  import Control.Monad
  import Control.Monad.ST
  import Statistics.RandomVariate
  import Statistics.Distribution
  import Statistics.Distribution.Exponential
  import Data.Array.Vector.UArr
  import System.Environment
  import Text.Printf

  -- some basic aliases and helper data types
  type Point = Vector3
  type Direction = Vector3
  showVector3 v = "<"++(show (v3x v))++", "++(show (v3y v))++", "++(show (v3z v))++">"
  type Path = [Point]
  type MLTState = Path
  data Ray = Ray {
      origin::Point,
      direction::Direction
    } deriving (Eq)
  instance Show Ray where
    show (Ray o d) = "Ray starting at "++(showVector3 o)++" going in Direction "++(showVector3 d)

  fromTwoPoints :: Point -> Point -> Ray
  fromTwoPoints v w = Ray v (normalize (w-v))

  type Interval = (Double,Double)

  -- some utility functions
  normsq v = v `vdot` v
  normalize v = (1/(vmag v)) *<> v
  
  -- applies f only at the kth element of the list and let's the rest untouched
  mapAt :: Int -> (a->a) -> [a] -> [a]
  mapAt _ _ [] = []
  mapAt k f (x:xs)
    | k==0      = (f x):xs
    | otherwise = x:(mapAt (k-1) f xs)

  -- stuff lives in Containers,
  data Container = SphereContainer Sphere
  instance Show Container where
    show (SphereContainer s) = show s
  instance Confineable Container where
    contains      (SphereContainer s) = contains s
    intersectedBy (SphereContainer s) = intersectedBy s
  
  -- Containers should be able to do the following:
  class Confineable a where
    contains      :: a -> Point -> Bool
    intersectedBy :: a -> Ray -> [Interval]
    
    
  -- Sphere
  data Sphere = Sphere {
      center :: Point,
      radius :: Double
    } deriving (Eq)
  instance Show Sphere where
    show (Sphere c r) = "Sphere at " ++ (showVector3 c) ++ " with Radius " ++ (show r)

  -- Sphere implements the Confineable type class
  instance Confineable Sphere where
    contains (Sphere c r) p = normsq (p - c) <= r*r

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
                   alphaDelta = ddinv * (sqrt discriminant)
                   alphas = (alpha0 - alphaDelta,
                             alpha0 + alphaDelta)
               in [alphas]
                   
          
  
  -- Material contains local absorption and scattering properties
  data Material = Material {
      materialAbsorption::Double,
      materialScattering::Double
    } deriving (Show)
  materialExtinction (Material kappa sigma) = kappa + sigma
  addMaterials (Material kappa1 sigma1) (Material kappa2 sigma2) = Material (kappa1+kappa2) (sigma1+sigma2)
  
  -- a Texture contains the spatial dependency of 'a'
  data Texture a = Homogenous a | Inhomogenous (Point->a)
  isHomogenous (Homogenous _) = True
  isHomogenous _ = False
  addHomogenousMaterials (Homogenous m1) (Homogenous m2) = Homogenous (addMaterials m1 m2)
  addInhomogenousMaterials (Inhomogenous f1) (Inhomogenous f2) = Inhomogenous (\p -> addMaterials (f1 p) (f2 p))
  
  instance (Show a) => Show (Texture a) where
    show (Homogenous a) = "Homogenous " ++ (show a)
    show (Inhomogenous _) = "Inhomogenous "

  -- an Entity is a container filled with light-influencing material
  data Entity = Entity {
      entityContainer::Container,
      entityTextures::[Texture Material]
    }
  instance Show Entity where
    show (Entity container textures) = "Entity contained by a " ++ (show container) ++
                                       " filled with " ++ (show textures)
  

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
  -- we order IntervalLimiters by position
  instance Ord (IntervalLimiter a) where
    (<=) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1<=pos2
    
  
  fromInterval :: Interval -> a -> [IntervalLimiter a]
  fromInterval (start,end) key = [IntervalLimiter key  True start,
                                  IntervalLimiter key False   end]
                                  
  -- transforms a list of tagged intervals possibly overlapping and with coinciding
  -- endpoints into a list of disjoint intervals with taglist
  cutOverlaps :: (Ord a) => [IntervalLimiter a] -> [(Interval,Set.Set a)]
  cutOverlaps limiters = cutOverlaps' Set.empty sortedLimiters
                         where sortedLimiters = List.sort limiters
  
  cutOverlaps' :: (Ord a) => (Set.Set a) -> [IntervalLimiter a] -> [(Interval, Set.Set a)]
  cutOverlaps' active         [] = []
  cutOverlaps' active (l1:l2:ls)
    | l1==l2    = rest
    | otherwise = ((intervalLimiterPosition l1,
                    intervalLimiterPosition l2), newactive):rest
    where getNewActive (IntervalLimiter key isbegin _)
            | isbegin   = Set.insert key active
            | otherwise = Set.delete key active
          --getNewActive (IntervalLimiter key isbegin _) =
          --  (if isbegin then Set.insert else Set.delete) key active
          newactive = getNewActive l1
          rest = cutOverlaps' newactive (l2:ls)
  cutOverlaps' active ((IntervalLimiter _ isbegin _):[]) =
    if isbegin then error "last intervallimiter shouldn't be a begin" else []  
  
  cutoverlapstestcase = concat.map (uncurry fromInterval) $
    zip [(1,5),(1,7),(2,6),(2,5),(3,4),(1,5)] [1,2,3,4,5,6]
  testCutOverlaps = cutOverlaps cutoverlapstestcase == [
      ((1.0,2.0),Set.fromList [1,2,6])      ,((2.0,3.0),Set.fromList [1,2,3,4,6]),
      ((3.0,4.0),Set.fromList [1,2,3,4,5,6]),((4.0,5.0),Set.fromList [1,2,3,4,6]),
      ((5.0,6.0),Set.fromList [2,3])        ,((6.0,7.0),Set.fromList [2])
    ]
  
  -- combines possibly differently textured materials into one
  boilDownMaterials :: [Texture Material] -> Texture Material
  boilDownMaterials textures =
    let (homtexts,inhomtexts) = List.partition isHomogenous textures
        nohomtexts   = null homtexts
        noinhomtexts = null inhomtexts
    in if noinhomtexts
        then if nohomtexts
               then Homogenous (Material 0 0)
               else foldl1 addHomogenousMaterials homtexts
        else if nohomtexts
               then foldl1 addInhomogenousMaterials inhomtexts
               else let (Homogenous hommat) = foldl1 addHomogenousMaterials homtexts
                        combinedinhomtexts  = foldl1 addInhomogenousMaterials inhomtexts
                    in addInhomogenousMaterials (Inhomogenous (const hommat)) combinedinhomtexts

  disjunctIntervalsWithCondensedTextures :: [Entity] -> Ray -> [(Interval,Texture Material)]
  disjunctIntervalsWithCondensedTextures entities ray = let
      intervals = [entityContainer entity `intersectedBy` ray | entity<-entities]
      entityindices = [(0::Int)..(length entities -1)]
      nestedindexedintervals = zip intervals entityindices
      indexedintervals = concat [map (\x -> (x, snd ii)) (fst ii) | ii<-nestedindexedintervals]
      taggeddisjointintervals = cutOverlaps.concat $ map (uncurry fromInterval) indexedintervals
      disjointintervals = fst $ unzip taggeddisjointintervals
      entitytexturelists = map entityTextures entities
      intervaltextureindices = map ((Set.toList).snd) taggeddisjointintervals
      intervaltextures = [concat $ map (entitytexturelists!!) textureindices | textureindices<-intervaltextureindices]
      condensedintervaltextures = map boilDownMaterials intervaltextures
      refinedintervalswithtextures = zip disjointintervals condensedintervaltextures
    in refinedintervalswithtextures
  
  -- sort out intervals that are before the ray starts or further away than maxDist
  -- and clip intervals that span across these bounds
  clipAndFilterTexturedIntervals :: Double -> [(Interval,Texture Material)] -> [(Interval,Texture Material)]
  clipAndFilterTexturedIntervals maxDist texturedintervals = let
      maxDist' = max 0 maxDist
      outsideOfInterest = (uncurry (\x y -> x>=maxDist' || y<=0)).fst
      filterIntervals = filter (not.outsideOfInterest)
      clipInterval (x,y) = (max 0 x, min maxDist' y)
      clipIntervals = map (uncurry (\x y -> (clipInterval x, y)))
    in filterIntervals . clipIntervals $ texturedintervals

  data ProbeResult = MaxDepthAtDistance Double | MaxDistAtDepth Double deriving (Show)
  getProbeResultDepth (MaxDistAtDepth depth) = depth
  
  consumeIntervals :: Ray -> Double -> Double -> [(Interval,Texture Material)] -> ProbeResult
  consumeIntervals ray maxDepth accumulatedDepth [] = MaxDistAtDepth accumulatedDepth
  consumeIntervals ray maxDepth accumulatedDepth (((a,b),  Homogenous  m):rest) = let
      remainingDepth = maxDepth - accumulatedDepth
      intervalLength = b - a
      extinction = materialExtinction m
      intervalDepth = extinction * intervalLength
    in if remainingDepth > intervalDepth
         then consumeIntervals ray maxDepth (accumulatedDepth + intervalDepth) rest
         else let neededDist = remainingDepth / extinction
              in MaxDepthAtDistance (a+neededDist)

  consumeIntervals ray maxDepth accumulatedDepth (((a,b),Inhomogenous mf):rest) = undefined

    
  -- casts a Ray through a list of entities until either a maximum optical depth
  -- or a maximum distance is reached
  probeEntitiesWithRay :: [Entity] -> Ray -> Double -> Double -> ProbeResult
  probeEntitiesWithRay entities ray maxDist maxDepth = let
      refinedintervalswithtextures = disjunctIntervalsWithCondensedTextures entities ray
      clippedintervals = clipAndFilterTexturedIntervals maxDist refinedintervalswithtextures
    in consumeIntervals ray maxDepth 0 clippedintervals
  
  
  --
  -- random-dependent stuff starts here!
  --
  
  startSeed = runST $ create >>= save

  -- generates a uniformly distributed random Int in the interval (a,b)
  randomIntInRange :: (Int,Int) -> Gen s -> ST s Int
  randomIntInRange (a,b) g = do
    if a==b
      then return a
      else do
        u1 <- uniform g
        let rnddbl = u1::Double
            ad = fromIntegral a
            bd = fromIntegral b
        return $ a + (truncate $ (bd-ad+1)*rnddbl)

  randomListIndex :: [a] -> Gen s -> ST s Int
  randomListIndex [] g = error "cannot choose an elementindex in an empty list"
  randomListIndex l g = do
    rndindex <- randomIntInRange (0,length l - 1) g
    return rndindex

  -- chooses uniformly random a list-element
  randomChoice :: [a] -> Gen s -> ST s a
  randomChoice [] g = error "cannot choose an element of an empty list"
  randomChoice choices g = do
    rndindex <- randomListIndex choices g
    return $ choices!!rndindex
    

  -- generates a d-distributed sample
  distributionSample d g = do
    u1 <- uniform g
    return $ quantile d (u1::Double)

  -- generates a random direction vector with length one
  randomDirection :: Gen s -> ST s Direction
  randomDirection g = do
    u1 <- uniform g
    u2 <- uniform g
    let z = 2*(u1::Double) - 1
        phi = 2*pi*(u2::Double)
        rho = sqrt (1 - z*z)
    return $ Vector3 (rho * (cos phi)) (rho * (sin phi)) z

  randomExponential3D :: Double -> Gen s -> ST s Point
  randomExponential3D lambda g = do
    rdir <- randomDirection g
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
    
  randomPathOfLength n g = do
    replicateM n $ randomPointInUnitSphere g
  
  randomDirections n g = do
    replicateM n $ randomDirection g

  runRandomActions seed = runST $ do
    g <- restore seed
    replicateM 10 $ randomDirection g


--
-- metropolis stuff
--

  metropolisStep :: Scene -> [Mutation] -> MLTState -> Gen s -> ST s MLTState
  metropolisStep scene mutations oldstate g = do
    mutation <- randomChoice mutations g
    newstate <- mutateWith mutation scene oldstate g
    let accprob = acceptanceProbabilityOf mutation scene oldstate newstate
    if accprob==0
      then return oldstate
      else do rnd <- uniform g
              if rnd<=accprob
                then return newstate
                else return oldstate

  nMLTStepsAndExtract :: Int -> (MLTState -> a) -> Scene -> [Mutation] -> MLTState -> Gen s -> ST s [a]
  nMLTStepsAndExtract n extractor scene mutations initialstate g = do
    if n<=0
      then return []
      else do
        newpath <- metropolisStep scene mutations initialstate g
        futureresults <- nMLTStepsAndExtract (n-1) extractor scene mutations newpath g
        return $ ((extractor initialstate):futureresults)

  -- the standard (possibly wasteful) way to compute the acceptance probability
  -- f x should return the probability density for state x
  -- t x y should return the transition probability from state x to state y
  defautAcceptanceProbability :: (a -> Double) -> (a -> a -> Double) -> a -> a -> Double
  defautAcceptanceProbability f t oldstate newstate =
    (/) ((f newstate)*(t newstate oldstate))
        ((f oldstate)*(t oldstate newstate))
        
  {--
    -- Mutations should adher to this interface:
    -- gives the acceptance probability for the transition (with parameters of type a)
    -- to a new state
    acceptanceProbabilityOf :: a -> Path -> Path -> Double
    -- mutates 
    mutateWith :: a -> Path -> Gen s -> ST s Path
  --}
  
  data Mutation = ExponentialNodeTranslation Double |
                  ExponentialImageNodeTranslation Double |
                  PathLengthMutation Double


  mutateWith :: Mutation -> Scene -> MLTState -> Gen s -> ST s MLTState
  mutateWith (ExponentialNodeTranslation l) scene oldstate g = do
    rndindex <- randomListIndex oldstate g
    rndtranslation <- randomExponential3D l g
    return $ mapAt rndindex (+rndtranslation) oldstate
  mutateWith (ExponentialImageNodeTranslation l) scene oldstate g = do
    rndtranslation <- randomExponential3D l g
    return $ mapAt (length oldstate - 1) (+rndtranslation) oldstate
  mutateWith (PathLengthMutation l) scene oldstate g = do
    u <- uniform g
    let coinflip = u::Bool
        oldnodecount = length oldstate
    if coinflip
      then do -- add node
        addindex <- randomIntInRange (0,oldnodecount) g
        let (prelist,postlist) = splitAt addindex oldstate
        if addindex>0 && addindex<oldnodecount
          then do -- interior node
            rndtranslation <- randomExponential3D l g
            let prenode = last prelist
                postnode = head postlist
                avgnode = 0.5*<>(prenode + postnode)
                newnode = avgnode + rndtranslation
            return $ prelist ++ [newnode] ++ postlist
          else do -- end node
            newnode <- randomPointInUnitSphere g
            return $ prelist ++ [newnode] ++ postlist
      else if oldnodecount > 1
        then do -- delete node
          delindex <- randomListIndex oldstate g
          let (prelist,postlist) = splitAt delindex oldstate
          return $ prelist ++ (tail postlist)
        else do
          return oldstate


  randomtestaction n pl = do
    g <- create
    rndpath <- randomPathOfLength pl g
    nMLTStepsAndExtract n
                        ((\v -> (v3x v, v3y v)).last.finalizePath)
                        testScene
                        [ExponentialNodeTranslation 0.12,
                         ExponentialNodeTranslation 0.08,
                         ExponentialImageNodeTranslation 0.05,
                         ExponentialImageNodeTranslation 0.02,
                         PathLengthMutation 0.1]
                        rndpath
                        g


  acceptanceProbabilityOf :: Mutation -> Scene -> MLTState -> MLTState -> Double
  acceptanceProbabilityOf (ExponentialNodeTranslation l) scene oldpath newpath =
    defautAcceptanceProbability (measurementContribution scene) (\_ _ -> 1) oldpath newpath
  acceptanceProbabilityOf (ExponentialImageNodeTranslation l) scene oldpath newpath =
    defautAcceptanceProbability (measurementContribution scene) (\_ _ -> 1) oldpath newpath
  acceptanceProbabilityOf (PathLengthMutation l) scene oldpath newpath =
    defautAcceptanceProbability (measurementContribution scene) (pathLengthMutationTransitionProbability l) oldpath newpath

  pathLengthMutationTransitionProbability :: Double -> MLTState -> MLTState -> Double
  pathLengthMutationTransitionProbability lambda oldpath newpath =
    let oldpathlength = length oldpath
        newpathlength = length newpath
    in 0.5 * if newpathlength > oldpathlength
      then -- added node
        let newnodeindex = fromMaybe oldpathlength $ List.findIndex (uncurry (/=)) $ zip oldpath newpath
            plfactor = 1 / (fromIntegral $ length newpath)
        in plfactor * if newnodeindex==0 || newnodeindex==oldpathlength
                        then 0.75 / pi
                        else let exp3d = \r -> (exp (-r/lambda))/(4*pi*lambda*r*r)
                                 newnode = newpath!!newnodeindex
                                 neighbouravg = 0.5*<>(sum.(take 2) $ drop (newnodeindex-1) oldpath)
                                 disttoavg = vmag (newnode - neighbouravg)
                             in exp3d disttoavg
      else -- deleted node
        1 / (max 1 (fromIntegral oldpathlength))


  --
  -- path stuff
  --
  
  addLightSourceNode :: Path -> Path
  addLightSourceNode path = (Vector3 0 0 0):path
  addSensorNode :: Path -> Path
  addSensorNode path = path ++ [((Vector3 1 1 0)*(last path) - (Vector3 0 0 1))]
  finalizePath :: Path -> Path
  finalizePath = addSensorNode . addLightSourceNode
  
  measurementContribution scene path = if null path
    then 0
    else let outsidenodeq = any (\p -> normsq p > 1) path
         in if outsidenodeq
              then 0
              else let completepath = finalizePath path
                       edges = edgeFunction (\v w -> w - v) completepath
                       sigma = 1.0
                       geometricfactors = product $ map ((4*pi/sigma*).normsq) (init edges)
                       opticaldist = sum $ edgeFunction raySphereIntersectionLength completepath
                       opticaldepth = sigma * opticaldist
                   in (exp (-opticaldepth))/geometricfactors
                       
  
  edgeFunction :: (a->a->b) -> [a] -> [b]
  edgeFunction f     (a:[]) = []
  edgeFunction f (a:b:rest) = (f a b):(edgeFunction f (b:rest))
  
  raySphereIntersectionLength v w = let
      distance = vmag $ w - v
      ray = fromTwoPoints v w
      container = SphereContainer $ Sphere (Vector3 0 0 0) 1
      material = Material 0 1
      texture = Homogenous material
      entities = [Entity container [texture]]
      depth = probeEntitiesWithRay entities ray distance (1/(0::Double))
    in getProbeResultDepth depth
    
  -- test-stuff
  testEntities = let sph1 = Sphere (Vector3 0 0 1) 2
                     sph2 = Sphere (Vector3 0 0 4) 2
                     cont1 = SphereContainer sph1
                     cont2 = SphereContainer sph2
                     mat1 = Material 5.0 0.0
                     mat2 = Material 0.0 7.0
                     tex1 = Homogenous mat1
                     tex2 = Homogenous mat2
                     ent1 = Entity cont1 [tex1]
                     ent2 = Entity cont2 [tex2]
                 in [ent1,ent2]
  testRay = Ray (Vector3 0 0 0) $ normalize (Vector3 0 0 1)
  testScene = Scene [] testEntities
  testpath1 = [Vector3 0.000124201 (-0.0123588) 0.00415517]
  testpath2 = [Vector3 0.000124201 (-0.0123588) 0.00415517, Vector3 (-0.160068) (-0.499073) (-0.511448)]
  testacceptanceratio = [measurementContribution testScene testpath1,
                         measurementContribution testScene testpath2,
                         pathLengthMutationTransitionProbability 0.1 testpath1 testpath2,
                         pathLengthMutationTransitionProbability 0.1 testpath2 testpath1
                         ]
  infinity = 1/(0::Double)
  
  showSample (x,y) = printf "{%f,%f}" x y
  showSamplesForMathematica :: [(Double,Double)] -> String
  showSamplesForMathematica samples = "testsamples={" ++ (concat $ List.intersperse ",\n" $ map showSample samples) ++ "};"

    
  main = do
    [n] <- getArgs
    putStrLn.showSamplesForMathematica $ runST $ randomtestaction ((read n)::Int) 1