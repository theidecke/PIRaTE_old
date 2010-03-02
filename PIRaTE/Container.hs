{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Container where
  import Data.ACVector ((|*),vdot)
  import Control.Monad.ST (ST)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq)
  import PIRaTE.Confineable
  import PIRaTE.Sampleable
  
  -- stuff lives in Containers,
  data Container = forall c. (Confineable c, Sampleable c Point, Show c) => Container c

  instance Show Container where
    show (Container c) = show c
  
  instance Confineable Container where
    contains      (Container c) = contains c
    {-# INLINE contains #-}
    intersectedBy (Container c) = intersectedBy c
    {-# INLINE intersectedBy #-}

  instance Sampleable Container Point where
    --sampleProbabilityOf :: (Sampleable a) => a -> Point -> Double
    sampleProbabilityOf (Container c) point = sampleProbabilityOf c point
    {-# INLINE sampleProbabilityOf #-}
    --randomSampleFrom :: (Sampleable a) => a -> Gen s -> ST s Point
    randomSampleFrom     (Container c)     g = randomSampleFrom c g
    {-# INLINE randomSampleFrom #-}
    