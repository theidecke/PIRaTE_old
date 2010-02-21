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
        materialScatteringPhaseFunction :: IndexedPhaseFunction,
        materialEmissivity              :: Texture Double,
        materialEmissionDirectedness    :: IndexedPhaseFunction,
        materialSensitivity             :: Texture Double,
        materialSensor                  :: IndexedSensor
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
    Material kappatex sigmatex pftex mempty undefined mempty undefined
    where kappatex = Homogenous kappa
          sigmatex = Homogenous sigma
          pftex | sigma==0  = undefined
                | otherwise = ipftex
          ipftex = IndexedPhaseFunction ipf

  toHomogenousEmittingMaterial :: Double -> (Int,PhaseFunction) -> Material
  toHomogenousEmittingMaterial epsilon ipf@(index,pf) =
    Material mempty mempty undefined epsilontex pftex mempty undefined
    where epsilontex = Homogenous epsilon
          pftex | epsilon==0 = undefined
                | otherwise  = ipftex
          ipftex = IndexedPhaseFunction ipf
          
  toHomogenousSensingMaterial :: Double -> (Int,PhaseFunction,SensorLogger) -> Material
  toHomogenousSensingMaterial zeta ist@(index,pf,sl) =
    Material mempty mempty undefined mempty undefined zetatex wsens
    where zetatex = Homogenous zeta
          wsens | zeta==0   = undefined
                | otherwise = indexedsensor
          indexedsensor = IndexedSensor ist -- indexed sensor triple

  instance Show Material where
    show m =  "kappa="     ++ show (materialAbsorption m) ++
              ", sigma="   ++ show sigma ++
              ", phi_sca=" ++ pfscastring ++
              ", epsilon=" ++ show epsilon ++
              ", phi_emi=" ++ pfemistring ++
              ", zeta="    ++ show zeta ++
              ", phi_sen=" ++ pfsenstring where
                sigma = (materialScattering m)
                epsilon = (materialEmissivity m)
                zeta = (materialSensitivity m)
                pfscastring | isScattering m = show (materialScatteringPhaseFunction m)
                            | otherwise      = "undefined"
                pfemistring | isEmitting m   = show (materialEmissionDirectedness m)
                            | otherwise      = "undefined"
                pfsenstring | isSensing m    = show (materialSensor m)
                            | otherwise      = "undefined"

  instance Monoid Double where
    mempty = 0
    {-# INLINE mempty #-}
    mappend = (+)
    {-# INLINE mappend #-}

