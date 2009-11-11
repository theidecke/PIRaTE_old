{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.PhaseFunction where
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  import PIRaTE.Sampleable
  import PIRaTE.RandomSample (randomWeightedChoice)
  
  -- PhaseFunction determines the Direction-dependent scattering probability
  data PhaseFunction = forall pf. (Sampleable (pf,Ray) Direction, Show pf) => PhaseFunction pf
                     
  instance Show PhaseFunction where
    show (PhaseFunction pf) = show pf

  instance Sampleable (PhaseFunction,Ray) Direction where
    probabilityDensityOf (PhaseFunction pf,inray) wout = probabilityDensityOf (pf,inray) wout
    {-# INLINE probabilityDensityOf #-}
    randomSampleFrom     (PhaseFunction pf,inray)    g = randomSampleFrom (pf,inray) g
    {-# INLINE randomSampleFrom #-}


  newtype IndexedPhaseFunction = IndexedPhaseFunction {ipfPairForm :: (Int,PhaseFunction)}
  
  instance Show IndexedPhaseFunction where
    show (IndexedPhaseFunction (index,_)) = "Phasefunction " ++ show index
  instance Eq IndexedPhaseFunction where
    (==) (IndexedPhaseFunction (index1,_)) (IndexedPhaseFunction (index2,_)) = index1==index2
    {-# INLINE (==) #-}
  instance Ord IndexedPhaseFunction where
    (<=) (IndexedPhaseFunction (index1,_)) (IndexedPhaseFunction (index2,_)) = index1 <= index2
    {-# INLINE (<=) #-}
    
    
  type WeightedPhaseFunction = WS.WeighedSet IndexedPhaseFunction
  
  instance Sampleable (WeightedPhaseFunction,Ray) Direction where
    probabilityDensityOf (wpf,inray) wout = let
        step ipf w (tp,tw) = (tp',tw')
          where tp' = tp + (probabilityDensityOf (pf,inray) wout)
                tw' = tw + w
                pf  = snd . ipfPairForm $ ipf
        (totalprob,totalweight) = WS.foldWithKey step (0,0) wpf
      in if totalweight>0
        then totalprob / totalweight
        else 0
    randomSampleFrom     (wpf,inray) g = do
      indexedphasefunction <- randomWeightedChoice (WS.toWeightList wpf) g
      let pf = snd . ipfPairForm $ indexedphasefunction
      randomSampleFrom (pf,inray) g
    {-# INLINE randomSampleFrom #-}
    
