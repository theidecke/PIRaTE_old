module PIRaTE.SpatialTypes where
  import Data.Vector
  import PIRaTE.UtilityFunctions (normalize)
  
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
  
  type Interval = (Double,Double)
