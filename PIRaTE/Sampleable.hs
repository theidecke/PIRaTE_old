{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Sampleable where
  import Control.Monad.ST (ST)
  import Statistics.RandomVariate (Gen)

  -- objects of type a from which we can draw samples of type b
  class Sampleable a b where
    randomSampleFrom    :: a -> Gen s -> ST s b
    sampleProbabilityOf :: a -> b -> Double
