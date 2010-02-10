module PIRaTE.SpatialTypes where
  import Data.Vector
  import PIRaTE.UtilityFunctions (normalize)
  
  -- some basic aliases and helper data types
  type Point = Vector3
  showVector3 v = "<"++ show (v3x v) ++", "++ show (v3y v) ++", "++ show (v3z v) ++">"

  newtype Direction = Direction Vector3
  unDirection (Direction d) = d
  appliedToDirection f (Direction d) = Direction (f d)
  fromEdge e = Direction (normalize e)
  
  instance Show Direction where
    show (Direction d) = showVector3 d
  instance Eq Direction where
    (==) (Direction d1) (Direction d2) = (==) d1 d2
  
  arcsec = 2*pi/1296000
  arcmin = 60*arcsec
  degree = 60*arcmin
  
  type Path = [Point]
  pathLength path = length path - 1
  pathNodeCount path = length path
  showPath = show . map quantizeNode where
  quantizeNode (Vector3 x y z) = (round (1000*x),round (1000*y),round (1000*z))
  type MLTState = Path
  fromPath :: Path -> MLTState
  fromPath = id
  mltStatePath :: MLTState -> Path
  mltStatePath = id
  mltStatePathLength :: MLTState -> Int
  mltStatePathLength = pathLength . mltStatePath
  mltStateSubstitutePath :: MLTState -> Path -> MLTState
  mltStateSubstitutePath oldstate newpath = newpath

  showMLTState = concatMap showVector3 . mltStatePath

  data Ray = Ray {
      rayOrigin::Point,
      rayDirection::Direction
    } deriving (Eq)
    
  followFor :: Ray -> Double -> Point
  followFor (Ray origin (Direction direction)) distance = origin + distance |* direction
  
  instance Show Ray where
    show (Ray o d) = "Ray starting at "++ (showVector3 o) ++" going in Direction "++ (show d) 

  fromTwoPoints :: Point -> Point -> Ray
  fromTwoPoints v w = Ray v (Direction $ normalize (w-v))
  
  type Interval = (Double,Double)
