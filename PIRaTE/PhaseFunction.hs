module PIRaTE.PhaseFunction where
  import Data.Monoid
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

  newtype WeightedPhaseFunction = WeightedPhaseFunction (Double,PhaseFunction)

  fromPhaseFunction :: PhaseFunction -> WeightedPhaseFunction
  fromPhaseFunction pf = WeightedPhaseFunction (1,pf)
  
  toPhaseFunction :: WeightedPhaseFunction -> PhaseFunction
  toPhaseFunction (WeightedPhaseFunction (_,Isotropic)) = Isotropic
  toPhaseFunction (WeightedPhaseFunction (w,Anisotropic phi)) = Anisotropic (\win wout -> (phi win wout)/w)
  
  addWeightedPhaseFunctions wpf1@(WeightedPhaseFunction (w1,pf1))
                            wpf2@(WeightedPhaseFunction (w2,pf2))
    | w1==0 = wpf2
    | w2==0 = wpf1
    | isIsotropic pf1 && isIsotropic pf2 = WeightedPhaseFunction (w1+w2,Isotropic)
    | otherwise = WeightedPhaseFunction (weightsum, newpf)
                    where weightsum = w1+w2
                          pd1 = scatterPDF pf1
                          pd2 = scatterPDF pf2
                          newpf = Anisotropic (\win wout -> (pd1 win wout) + (pd2 win wout) )

  instance Monoid WeightedPhaseFunction where
    mempty = WeightedPhaseFunction (0,Isotropic)
    mappend = addWeightedPhaseFunctions
  