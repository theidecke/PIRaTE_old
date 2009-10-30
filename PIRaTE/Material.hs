module PIRaTE.Material (
    Material,
    toHomogenousMaterial,
    materialTexturedAbsorption,
    materialTexturedScattering,
    materialTexturedExtinction,
    materialTexturedPhaseFunction,
  ) where
  import Data.Monoid
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  import PIRaTE.Texture
  import PIRaTE.PhaseFunction
  
  -- Material contains local absorption and scattering properties
  data Material = Material (Texture Double) (Texture Double) (Texture WeightedPhaseFunction)

  toHomogenousMaterial :: Double -> Double -> (Int,PhaseFunction) -> Material
  toHomogenousMaterial kappa sigma (index,pf) = Material kappatex sigmatex pftex
    where kappatex = Homogenous kappa
          sigmatex = Homogenous sigma
          pftex = Homogenous $ WS.singleton ipf
          ipf = IndexedPhaseFunction (index,pf)

  materialTexturedAbsorption    (Material kappatex        _      _) = kappatex
  materialTexturedScattering    (Material        _ sigmatex      _) = sigmatex
  materialTexturedExtinction    (Material kappatex sigmatex      _) = kappatex `mappend` sigmatex
  materialTexturedPhaseFunction (Material        _        _ phitex) = phitex

  instance Show Material where
    show (Material kappatex sigmatex phitex) =  "kappa="++(show kappatex)++
                                               ", sigma="++(show sigmatex)++
                                               ", phi="++(show phitex)

  instance Monoid Double where
    mempty = 0
    mappend = (+)

  instance Monoid Material where
    mempty = Material mempty mempty mempty
    {-# INLINE mempty #-}
    mappend = addTexturedMaterials
    {-# INLINE mappend #-}

  addTexturedMaterials (Material k1 s1 wpf1) (Material k2 s2 wpf2) = let
      k'   = k1 `mappend` k2
      s'   = s1 `mappend` s2
      wpf' = wpf1 `mappend` wpf2
    in Material k' s' wpf'
  {-# INLINE addTexturedMaterials #-}
