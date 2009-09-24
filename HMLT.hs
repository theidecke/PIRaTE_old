module Main where

  import Data.Vector
  import qualified Data.List as List
  import qualified Data.Set as Set
  

  -- some basic aliases and helper data types
  type Point = Vector3
  type Direction = Vector3
  showVector3 v = "<"++(show (v3x v))++", "++(show (v3y v))++", "++(show (v3z v))++">"
  type Path = [Point]
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

  testEntities = let sph1 = Sphere (Vector3 0 0 1) 2
                     sph2 = Sphere (Vector3 1 1 4) 3
                     cont1 = SphereContainer sph1
                     cont2 = SphereContainer sph2
                     mat1 = Material 2.0 1.0
                     mat2 = Material 3.0 4.0
                     tex1 = Homogenous mat1
                     tex2 = Homogenous mat2
                     ent1 = Entity cont1 [tex1]
                     ent2 = Entity cont2 [tex2]
                 in [ent1,ent2]
  testRay = Ray (Vector3 1 1 0) $ normalize (Vector3 0 0 1)

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
  

  
  main = putStrLn "Hello World!"