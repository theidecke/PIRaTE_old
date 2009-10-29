module PIRaTE.Material (
    Material,
    toHomogenousMaterial,
    materialTexturedAbsorption,
    materialTexturedScattering,
    materialTexturedExtinction,
    materialTexturedPhaseFunction,
    boilDownMaterials
  ) where
  import Data.Monoid
  import PIRaTE.SpatialTypes
  import PIRaTE.Texture
  import PIRaTE.PhaseFunction
  
  -- Material contains local absorption and scattering properties
  data Material = Material (Texture Double) (Texture Double) (Texture PhaseFunction)

  toHomogenousMaterial :: Double -> Double -> PhaseFunction -> Material
  toHomogenousMaterial kappa sigma pf = Material (Homogenous kappa) (Homogenous sigma) (Homogenous  pf)
  -- $ fromPhaseFunction

  materialTexturedAbsorption    (Material kappatex        _      _) = kappatex
  materialTexturedScattering    (Material        _ sigmatex      _) = sigmatex
  materialTexturedExtinction    (Material kappatex sigmatex      _) = addTexturedDoubles kappatex sigmatex
  materialTexturedPhaseFunction (Material        _        _ phitex) = phitex

  instance Show Material where
    show (Material kappatex sigmatex phitex) =  "kappa="++(show kappatex)++
                                               ", sigma="++(show sigmatex)++
                                               ", phi="++(show phitex)
                                               
  instance Monoid Material where
    mempty = Material mempty mempty (Homogenous Isotropic)
    {-# INLINE mempty #-}
    mappend m1 m2 = undefined --boilDownMaterials [m1,m2] -- if mappend is used in a fold it would get the weights wrong, Material should use WeightedPhaseFunction to solve this
    mconcat ms = boilDownMaterials ms
    {-# INLINE mconcat #-}
                                               
  boilDownMaterials :: [Material] -> Material
  boilDownMaterials materials = 
    let kappatextures = map materialTexturedAbsorption materials
        summedkappa = mconcat kappatextures
        sigmatextures = map materialTexturedScattering materials
        summedsigma = mconcat sigmatextures
        phitextures = map materialTexturedPhaseFunction materials
        summedphi = addTexturedPhaseFunctions $ zip sigmatextures phitextures
    in Material summedkappa summedsigma summedphi
    
  