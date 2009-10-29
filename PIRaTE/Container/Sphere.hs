{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PIRaTE.Container.Sphere where
  import Data.Vector ((*<>),vdot)
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

  instance Sampleable Sphere Point where
    probabilityDensityOf s@(Sphere center radius) p
      | not (s `contains` p) = 0
      | otherwise            = 3 / (4*pi*(radius^3))
    {-# INLINE probabilityDensityOf #-}
    randomSampleFrom     (Sphere center radius) g =  do
      unitpoint <- randomPointInUnitSphere g
      return $ center + radius *<> unitpoint
    {-# INLINE randomSampleFrom #-}
    