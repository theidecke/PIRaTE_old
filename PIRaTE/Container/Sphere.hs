{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PIRaTE.Container.Sphere where
  import Data.Vector ((|*),vdot)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq)
  import PIRaTE.Confineable
  import PIRaTE.Sampleable
  import PIRaTE.RandomSample (randomPointInUnitSphere)
  
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
    
    -- assuming direction is normalized
    intersectedBy (Sphere center radius) (Ray origin (Direction direction)) = 
      let offset = origin - center
          oo = offset `vdot` offset
          od = offset `vdot` direction
          discriminant = od*od - oo + radius*radius
      in if discriminant <= 0 
          then []
          else let alpha0 = -od
                   alphadelta = sqrt discriminant
                   alphas = (alpha0 - alphadelta,
                             alpha0 + alphadelta)
               in [alphas]

  instance Sampleable Sphere Point where
    sampleProbabilityOf s@(Sphere center radius) p
      | s `contains` p = 3 / (4*pi*(radius^3))
      | otherwise      = 0
    {-# INLINE sampleProbabilityOf #-}
    randomSampleFrom     (Sphere center radius) g =  do
      unitpoint <- randomPointInUnitSphere g
      return $ center + radius |* unitpoint
    {-# INLINE randomSampleFrom #-}
    