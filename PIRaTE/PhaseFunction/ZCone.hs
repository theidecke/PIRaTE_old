{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module PIRaTE.PhaseFunction.ZCone where
  import Data.Vector (Vector3(..),v3z)
  import PIRaTE.SpatialTypes
  import PIRaTE.Sampleable
  import Control.Monad.ST (ST)
  import Statistics.RandomVariate (Gen,uniform)
  
  -- PhaseFunction which scatters all incoming light into a cone around the z axis
  data ZCone = ZCone Double
  
  fromApexAngle :: Double -> ZCone
  fromApexAngle twotheta = ZCone capheight
      where capheight = 1 - cos theta
            theta = 0.5 * twotheta
  
  fromCapHeight :: Double -> ZCone
  fromCapHeight capheight = ZCone capheight

  instance Show ZCone where
    show (ZCone h) = "ZCone with capheight " ++ (show h)

  instance Sampleable (ZCone,Ray) Direction where
    sampleProbabilityOf (ZCone h,_) (Direction wout)
      | v3z wout > 1 - h = 0.5*h
      | otherwise        = 0
    {-# INLINE sampleProbabilityOf #-}
    randomSampleFrom     (ZCone h,_) g = randomDirectionInZCone h g
    {-# INLINE randomSampleFrom #-}
    
  -- generates a normalized Direction-Vector that is pointing towards a point inside a cone pointing in the positive z-axis with solid angle 2*pi*capheight, capheight = 1 - cos theta
  randomDirectionInZCone :: Double -> Gen s -> ST s Direction
  randomDirectionInZCone capheight g = do
      u1 <- uniform g
      u2 <- uniform g
      let oneminusz = capheight*(u1::Double)  --capheight = 1 - costheta
          phi = 2*pi*(u2::Double)
          rho = sqrt (oneminusz*(2-oneminusz))--sqrt (1 - z*z)
          z = 1 - oneminusz
      return . Direction $ Vector3 (rho * cos phi) (rho * sin phi) z
