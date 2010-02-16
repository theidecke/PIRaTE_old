{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PIRaTE.Container.Box where
  import Data.Vector (Vector3(..))
  import System.Random.MWC (uniform)
  import PIRaTE.SpatialTypes
  import PIRaTE.Confineable
  import PIRaTE.Sampleable
  
  data Box = Box {
      corner1 :: Point,
      corner2 :: Point
    } deriving (Eq)

  fromCorners (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Box (Vector3 (min x1 x2) (min y1 y2) (min z1 z2))
        (Vector3 (max x1 x2) (max y1 y2) (max z1 z2))
  {-# INLINE fromCorners #-}

  instance Show Box where
    show (Box c1 c2) = "Box spanned between " ++ (showVector3 c1) ++
                       " and " ++ (showVector3 c1)

  instance Confineable Box where
    contains (Box (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) (Vector3 x y z) =
      not $ or [x<x1, x>x2, y<y1, y>y2, z<z1, z>z2]
    {-# INLINE contains #-}
    
    intersectedBy (Box (Vector3 x1 y1 z1) (Vector3 x2 y2 z2))
                  (Ray (Vector3 ox oy oz) (Direction (Vector3 dx dy dz)))
      | tmin > tmax = []
      | otherwise   = [(tmin,tmax)]
      where tmin = maximum [txmin,tymin,tzmin]
            tmax = minimum [txmax,tymax,tzmax]
            (txmin,txmax) | dx >= 0   = ((x1-ox)*dxinv, (x2-ox)*dxinv)
                          | otherwise = ((x2-ox)*dxinv, (x1-ox)*dxinv)
            (tymin,tymax) | dy >= 0   = ((y1-oy)*dyinv, (y2-oy)*dyinv)
                          | otherwise = ((y2-oy)*dyinv, (y1-oy)*dyinv)
            (tzmin,tzmax) | dz >= 0   = ((z1-oz)*dzinv, (z2-oz)*dzinv)
                          | otherwise = ((z2-oz)*dzinv, (z1-oz)*dzinv)
            dxinv = 1 / dx
            dyinv = 1 / dy
            dzinv = 1 / dz


  instance Sampleable Box Point where
    sampleProbabilityOf box p
      | box `contains` p = 1 / (boxVolume box)
      | otherwise      = 0
    {-# INLINE sampleProbabilityOf #-}
    randomSampleFrom (Box (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) g = do
      u1 <- uniform g
      u2 <- uniform g
      u3 <- uniform g
      let x = x1 + (u1::Double)*(x2-x1)
          y = y1 + (u2::Double)*(y2-y1)
          z = z1 + (u3::Double)*(z2-z1)
      return $ Vector3 x y z
    {-# INLINE randomSampleFrom #-}

  boxVolume (Box (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) =
    (x2-x1)*(y2-y1)*(z2-z1)
  {-# INLINE boxVolume #-}