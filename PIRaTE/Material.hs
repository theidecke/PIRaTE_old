module PIRaTE.Material (
    Material,
    toHomogenousInteractingMaterial,
    toHomogenousEmittingMaterial,
    toHomogenousSensingMaterial,
    materialAbsorption,
    materialScattering,
    materialExtinction,
    materialScatteringPhaseFunction,
    materialEmissivity,
    materialEmissionDirectedness,
    materialSensitivity,
    materialSensor,
    isInteracting,
    isScattering,
    isAbsorbing,
    isEmitting,
    isSensing
  ) where
  import Data.Monoid
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  import PIRaTE.Texture
  import PIRaTE.PhaseFunction
  import PIRaTE.Sensor
  
  -- Material contains local absorption and scattering properties
  data Material = Material {
        materialAbsorption              :: Texture Double,
        materialScattering              :: Texture Double,
        materialScatteringPhaseFunction :: WeightedPhaseFunction,
        materialEmissivity              :: Texture Double,
        materialEmissionDirectedness    :: WeightedPhaseFunction,
        materialSensitivity             :: Texture Double,
        materialSensor                  :: WeightedSensor
    }

  materialExtinction :: Material -> Texture Double
  materialExtinction m = (materialAbsorption m) `mappend` (materialScattering m)

  isInteracting :: Material -> Bool
  isInteracting m = not $ (materialExtinction m)==mempty
  
  isScattering :: Material -> Bool
  isScattering m = not $ (materialScattering m)==mempty
  
  isAbsorbing :: Material -> Bool
  isAbsorbing m = not $ (materialAbsorption m)==mempty
  
  isEmitting :: Material -> Bool
  isEmitting m = not $ (materialEmissivity m)==mempty
  
  isSensing :: Material -> Bool
  isSensing m = not $ (materialSensitivity m)==mempty

  toHomogenousInteractingMaterial :: Double -> Double -> (Int,PhaseFunction) -> Material
  toHomogenousInteractingMaterial kappa sigma ipf@(index,pf) =
    Material kappatex sigmatex pftex mempty mempty mempty mempty
    where kappatex = Homogenous kappa
          sigmatex = Homogenous sigma
          pftex | sigma==0  = WS.empty
                | otherwise = WS.singleton ipftex
          ipftex = IndexedPhaseFunction ipf

  toHomogenousEmittingMaterial :: Double -> (Int,PhaseFunction) -> Material
  toHomogenousEmittingMaterial epsilon ipf@(index,pf) =
    Material mempty mempty mempty epsilontex pftex mempty mempty
    where epsilontex = Homogenous epsilon
          pftex | epsilon==0 = WS.empty
                | otherwise  = WS.singleton ipftex
          ipftex = IndexedPhaseFunction ipf
          
  toHomogenousSensingMaterial :: Double -> (Int,PhaseFunction,SensorLogger) -> Material
  toHomogenousSensingMaterial zeta ist@(index,pf,sl) =
    Material mempty mempty mempty mempty mempty zetatex wsens
    where zetatex = Homogenous zeta
          wsens | zeta==0   = WS.empty
                | otherwise = WS.singleton indexedsensor
          indexedsensor = IndexedSensor ist -- indexed sensor triple

  instance Show Material where
    show m =  "kappa="     ++ show (materialAbsorption m) ++
              ", sigma="   ++ show (materialScattering m) ++
              ", phi_sca=" ++ show (materialScatteringPhaseFunction m) ++
              ", epsilon=" ++ show (materialEmissivity m) ++
              ", phi_emi=" ++ show (materialEmissionDirectedness m) ++
              ", zeta="    ++ show (materialSensitivity m) ++
              ", phi_sen=" ++ show (materialSensor m)

  instance Monoid Double where
    mempty = 0
    {-# INLINE mempty #-}
    mappend = (+)
    {-# INLINE mappend #-}

  instance Monoid Material where
    mempty = Material mempty mempty mempty mempty mempty mempty mempty
    {-# INLINE mempty #-}
    mappend = addTexturedMaterials
    {-# INLINE mappend #-}

  addTexturedMaterials (Material kappa1 sigma1 scawpf1 epsilon1 emiwpf1 zeta1 senwpf1)
                       (Material kappa2 sigma2 scawpf2 epsilon2 emiwpf2 zeta2 senwpf2) =
    let kappa'   = kappa1 `mappend` kappa2
        sigma'   = sigma1 `mappend` sigma2
        scawpf'  = scawpf1 `mappend` scawpf2
        epsilon' = epsilon1 `mappend` epsilon2
        emiwpf'  = emiwpf1 `mappend` emiwpf2
        zeta'    = zeta1 `mappend` zeta2
        senwpf'  = senwpf1 `mappend` senwpf2
    in Material kappa' sigma' scawpf' epsilon' emiwpf' zeta' senwpf'
  {-# INLINE addTexturedMaterials #-}
