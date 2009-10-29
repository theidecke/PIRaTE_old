--{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE FlexibleContexts #-}

module PIRaTE.Confineable where
  import PIRaTE.SpatialTypes
  
  -- Containers should be able to do the following: 
  class Confineable c where
    contains      :: c -> Point -> Bool
    intersectedBy :: c -> Ray -> [Interval]

