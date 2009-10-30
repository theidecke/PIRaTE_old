module PIRaTE.PhaseFunction where
  import Data.Monoid
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  
  -- PhaseFunction determines the Direction-dependent scattering probability
  data PhaseFunction = Isotropic
                     | Anisotropic (Direction->Direction->Double)
                     
  scatterPDF :: PhaseFunction -> Direction -> Direction -> Double
  scatterPDF         Isotropic   _    _ = 1/(4*pi)
  scatterPDF (Anisotropic phi) win wout = phi win wout
  {-# INLINE scatterPDF #-}
  
  isIsotropic Isotropic = True
  isIsotropic         _ = False
  {-# INLINE isIsotropic #-}
  
  instance Show PhaseFunction where
    show       Isotropic = "Isotropic"
    show (Anisotropic _) = "Anisotropic"

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
