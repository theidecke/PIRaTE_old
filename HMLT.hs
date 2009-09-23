module Main where

  import Data.Vector
  import qualified Data.List as List
  import qualified Data.Set as Set
  

  -- some basic aliases and helper data types
  type Point = Vector3
  type Direction = Vector3
  type Path = [Point]
  data Ray = Ray {
      origin::Point,
      direction::Direction
    } deriving (Show,Eq)
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
    show (Sphere c r) = "Sphere at "++pos++" with Radius "++(show r)
        where pos = "<"++(show (v3x c))++", "
                       ++(show (v3y c))++", "
                       ++(show (v3z c))++">"
    

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
      absorption::Double,
      scattering::Double
    } deriving (Show)
  
  -- a Texture contains the spatial dependency of 'a'
  data Texture a = Homogenous a | Inhomogenous (Point->a)
  instance (Show a) => Show (Texture a) where
    show (Homogenous a) = "Homogenous " ++ (show a) ++ ")"
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

  testEntities = let sph1 = Sphere (Vector3 0 0 3) 2
                     sph2 = Sphere (Vector3 1 1 4) 3
                     cont1 = SphereContainer sph1
                     cont2 = SphereContainer sph2
                     mat1 = Material 2.0 3.0
                     mat2 = Material 0.0 1.0
                     tex1 = Homogenous mat1
                     tex2 = Homogenous mat2
                     ent1 = Entity cont1 [tex1]
                     ent2 = Entity cont2 [tex1,tex2]
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
    
  -- [entityContainer entity `intersectedBy` testRay | entity<-testEntities]
  
  -- casts a Ray through a list of entities until either a maximum optical depth
  -- or a maximum distance is reached
  data ProbeResult = MaxDepthAtDistance Double | MaxDistAtDepth Double deriving (Show)
  probeEntitiesWithRay :: [Entity] -> Double -> Double -> Ray -> ProbeResult
  probeEntitiesWithRay entities maxDist maxDepth ray = undefined


  
  
  main = putStrLn "Hello World!"