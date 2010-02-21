{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module PIRaTE.Scene where
  import Data.Vector ((|*),vmag,Vector3(..))
  import Data.Monoid
  import Data.Maybe (fromMaybe,fromJust,isNothing,isJust)
  import Data.Array.Vector (singletonU)
  import qualified Data.List as L
  import qualified Data.Set as S
  import Control.Monad (foldM,liftM,liftM2)
  import Control.Monad.ST (ST,runST)
  import PIRaTE.SpatialTypes
  import PIRaTE.Confineable
  import PIRaTE.Container
  import PIRaTE.Container.Sphere
  import PIRaTE.PhaseFunction
  import PIRaTE.PhaseFunction.Isotropic
  import PIRaTE.PhaseFunction.ZCone
  import PIRaTE.Texture
  import PIRaTE.Material
  import PIRaTE.UtilityFunctions (infinity,edgeMap)
  import PIRaTE.Sampleable
  import PIRaTE.RandomSample
  import PIRaTE.Sensor
  import System.Random.MWC (Gen,uniform,initialize)
  import Test.QuickCheck hiding (Gen)
  import qualified Test.QuickCheck as QC (Gen)
  
  import Debug.Trace
  
  -- an Entity is a container filled with light-influencing material
  data Entity = Entity {
      entityContainer::Container,
      entityMaterials::[Material]
    }
  
  entityFromContainerAndMaterials :: Container -> [Material] -> Entity
  entityFromContainerAndMaterials container materials = Entity container materials
  
  instance Show Entity where
    show (Entity container materials) = "Entity contained by a " ++ (show container) ++
                                        " filled with " ++ (show materials)
  
  randomPointInEntities :: [Entity] -> Gen s -> ST s Point
  randomPointInEntities entities g = do
    entity <- randomSampleFrom entities g
    let container = entityContainer entity
    randomSampleFrom container g
  
  isEmitter :: Entity -> Bool
  isEmitter (Entity _ materials) = any isEmitting materials
  {-# INLINE isEmitter #-}
  
  isInteractor :: Entity -> Bool
  isInteractor (Entity _ materials) = any isInteracting materials
  {-# INLINE isInteractor #-}
  
  isScatterer :: Entity -> Bool
  isScatterer (Entity _ materials) = any isScattering materials
  {-# INLINE isScatterer #-}
  
  isAbsorber :: Entity -> Bool
  isAbsorber (Entity _ materials) = any isAbsorbing materials
  {-# INLINE isAbsorber #-}
  
  isSensor :: Entity -> Bool
  isSensor (Entity _ materials) = any isSensing materials
  {-# INLINE isSensor #-}
  
  containing :: [Entity] -> Point -> [Entity]
  containing entities point = filter ((`contains` point).entityContainer) entities
  
  summedMaterialAt :: [Entity] -> Point -> Material
  summedMaterialAt entities point = mconcat . concatMap entityMaterials $ entities `containing` point
  {-# INLINE summedMaterialAt #-}

  propertyAt :: (Material->Texture a) -> [Entity] -> Point -> a
  propertyAt getpropfrom entities point = getpropfrom summat `evaluatedAt` point
    where summat = summedMaterialAt entities point
  {-# INLINE propertyAt #-}
    
  absorptionAt :: [Entity] -> Point -> Double
  absorptionAt = propertyAt materialAbsorption
  {-# INLINE absorptionAt #-}
    
  scatteringAt :: [Entity] -> Point -> Double
  scatteringAt = propertyAt materialScattering
  {-# INLINE scatteringAt #-}

  extinctionAt :: [Entity] -> Point -> Double
  extinctionAt = propertyAt materialExtinction
  {-# INLINE extinctionAt #-}

  emissivityAt :: [Entity] -> Point -> Double
  emissivityAt = propertyAt materialEmissivity
  {-# INLINE emissivityAt #-}

  sensitivityAt :: [Entity] -> Point -> Double
  sensitivityAt = propertyAt materialSensitivity
  {-# INLINE sensitivityAt #-}

  -- a Scene contains all entities
  sceneFromEntities entities = Scene {
    sceneEntities    = entities,
    sceneEmitters    = filter isEmitter entities,
    sceneInteractors = filter isInteractor entities,
    sceneScatterers  = filter isScatterer entities,
    sceneAbsorbers   = filter isAbsorber entities,
    sceneSensors     = filter isSensor entities
  }
  
  data Scene = Scene {
      sceneEntities::[Entity],
      sceneEmitters::[Entity],
      sceneInteractors::[Entity],
      sceneScatterers::[Entity],
      sceneAbsorbers::[Entity],
      sceneSensors::[Entity]
    } deriving Show

  instance Arbitrary Scene where
    arbitrary = (standardScene . abs) `fmap` arbitrary where
      standardScene sigma = let
          lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.01
          lightsourcematerial = toHomogenousEmittingMaterial 1.0 (1, PhaseFunction $ Isotropic)
          lightsourceentity = entityFromContainerAndMaterials lightsourcecontainer [lightsourcematerial]
          scatteringcontainer = Container $ Sphere (Vector3 0 0 0) 1
          scatteringmaterial = toHomogenousInteractingMaterial 0 sigma (1,PhaseFunction Isotropic)
          scatteringentity = entityFromContainerAndMaterials scatteringcontainer [scatteringmaterial]
          sensorcontainer = Container $ Sphere (Vector3 0 0 (-3)) 1.1
          sensormaterial = toHomogenousSensingMaterial 1.0 (1, PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
          sensorangle = 1 * degree
          sensorentity = entityFromContainerAndMaterials sensorcontainer [sensormaterial]
          entities = [lightsourceentity, scatteringentity,sensorentity]
        in sceneFromEntities entities

  -- pick a sensor in the scene, choose a point in the sensor and a direction TODO
  --instance Sampleable Scene SensorRay where
      
  
  -- the Bool is used to represent if the IntervalLimiter is the begin of an interval
  data IntervalLimiter a = IntervalLimiter {
      intervalLimiterKey::a,
      isIntervalLimiterBegin::Bool,
      intervalLimiterPosition::Double
    }
  instance (Show a) => Show (IntervalLimiter a) where
    show (IntervalLimiter key isbegin pos) =
      (if isbegin then "Begin(" else "End(") ++ (show key) ++ "@" ++ (show pos) ++ ")"
  -- we use the equality just in terms of position (for Ord)
  instance Eq (IntervalLimiter a) where
    (==) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1==pos2
    {-# INLINE (==) #-}
  -- we order IntervalLimiters by position
  instance Ord (IntervalLimiter a) where
    (<=) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1<=pos2
    {-# INLINE (<=) #-}
    
  
  fromKeyInterval :: a -> Interval -> [IntervalLimiter a]
  fromKeyInterval key (start,end) = [IntervalLimiter key  True start,
                                     IntervalLimiter key False   end]
  {-# INLINE fromKeyInterval #-}
                                  
  -- transforms a list of tagged intervals possibly overlapping and with coinciding
  -- endpoints into a list of disjoint intervals with taglist
  cutOverlaps :: (Ord a) => [IntervalLimiter a] -> [(Interval,S.Set a)]
  cutOverlaps limiters = cutOverlaps' S.empty sortedLimiters
    where sortedLimiters = L.sort limiters
          cutOverlaps' :: (Ord a) => (S.Set a) -> [IntervalLimiter a] -> [(Interval, S.Set a)]
          cutOverlaps' active         [] = []
          cutOverlaps' active (l1:l2:ls)
            | S.null newactive || l1==l2 = rest
            | otherwise = ((intervalLimiterPosition l1,
                            intervalLimiterPosition l2), newactive):rest
            where newactive = getNewActive l1
                  rest = cutOverlaps' newactive (l2:ls)
                  getNewActive (IntervalLimiter key isbegin _) =
                    (if isbegin then S.insert else S.delete) key active
          cutOverlaps' active ((IntervalLimiter _ isbegin _):[]) =
            if isbegin then error "last intervallimiter shouldn't be a begin" else []  
  
  {--
  cutoverlapstestcase = concat.map (uncurry fromInterval) $
    zip [(1,5),(1,7),(2,6),(2,5),(3,4),(1,5)] [1,2,3,4,5,6]
  testCutOverlaps = cutOverlaps cutoverlapstestcase == [
      ((1.0,2.0),S.fromList [1,2,6])      ,((2.0,3.0),S.fromList [1,2,3,4,6]),
      ((3.0,4.0),S.fromList [1,2,3,4,5,6]),((4.0,5.0),S.fromList [1,2,3,4,6]),
      ((5.0,6.0),S.fromList [2,3])        ,((6.0,7.0),S.fromList [2])
    ]
    --}


  disjunctIntervalsWithCondensedMaterials :: [Entity] -> Ray -> [(Interval,Material)]
  disjunctIntervalsWithCondensedMaterials entities ray =
    zip disjointintervals condensedintervalmaterials
    where
      disjointintervals = fst $ unzip taggeddisjointintervals
      condensedintervalmaterials = getCondensedIntervalMaterials entities taggeddisjointintervals
      taggeddisjointintervals    = getTaggedDisjointIntervals entities ray      

  -- | returns a list of pairs of disjoint intervals with a set of entityindices which are contained in the interval
  getTaggedDisjointIntervals :: [Entity] -> Ray -> [(Interval, S.Set Int)]
  getTaggedDisjointIntervals entities ray =
    cutOverlaps . concat $ concatMap distributeEntityTag nestedindexedintervals
    where
      distributeEntityTag (entityindex, intervals) = map (fromKeyInterval entityindex) intervals
      nestedindexedintervals = zip [(0::Int)..] entityintersections
      entityintersections = [entityContainer entity `intersectedBy` ray | entity<-entities]

  -- | returns in listform for every disjoint interval the sum of materials of all entities present in the interval
  getCondensedIntervalMaterials :: [Entity] -> [(Interval, S.Set Int)] -> [Material]
  getCondensedIntervalMaterials entities taggeddisjointintervals =
    map mconcat intervalmaterials
    where
      intervalmaterials = [concat [entitymateriallists!!entityindex | entityindex<-entityindexset] | entityindexset <- intervalentityindexsets]
      intervalentityindexsets = map (S.toList . snd) taggeddisjointintervals
      entitymateriallists = map entityMaterials entities

  -- sort out intervals that are before the ray starts or further away than maxDist
  -- and clip intervals that span across these bounds
  clipAndFilterIntervalsWithMaterial :: Double -> [(Interval,Material)] -> [(Interval,Material)]
  clipAndFilterIntervalsWithMaterial maxDist intervalswithmaterials = let
      maxDist' = max 0 maxDist
      outsideOfInterest = uncurry (\x y -> x>=maxDist' || y<=0) . fst
      filterIntervals = filter (not.outsideOfInterest)
      clipInterval (x,y) = (max 0 x, min maxDist' y)
      clipIntervals = map (uncurry (\x y -> (clipInterval x, y)))
    in filterIntervals . clipIntervals $ intervalswithmaterials
  
  
  data ProbeResult = MaxDepthAtDistance Double | MaxDistAtDepth Double deriving (Show)
  getProbeResultDepth (MaxDistAtDepth depth) = Just depth
  getProbeResultDepth _ = Nothing
  {-# INLINE getProbeResultDepth #-}
  getProbeResultDist (MaxDepthAtDistance distance) = Just distance
  getProbeResultDist _ = Nothing
  {-# INLINE getProbeResultDist #-}
  
  consumeIntervals :: (Material -> Texture Double) -> Ray -> Double -> Double -> [(Interval,Material)] -> ProbeResult
  consumeIntervals propertyof ray maxDepth accumulatedDepth [] = MaxDistAtDepth accumulatedDepth
  consumeIntervals propertyof ray maxDepth accumulatedDepth (((a,b), m):rest) = let
      remainingDepth = maxDepth - accumulatedDepth
      intervalLength = b - a
      scalarvalue = propertyof m `evaluatedAt` undefined -- only works for Homogenous Materials
      intervalDepth = scalarvalue * intervalLength
    in if remainingDepth > intervalDepth
         then consumeIntervals propertyof ray maxDepth (accumulatedDepth + intervalDepth) rest
         else let neededDist = remainingDepth / scalarvalue
              in MaxDepthAtDistance (a+neededDist)
  
  -- casts a Ray through a list of entities until either a maximum optical depth
  -- or a maximum distance is reached
  probePropertyOfEntitiesWithRay :: (Material -> Texture Double) -> [Entity] -> Ray -> Double -> Double -> ProbeResult
  probePropertyOfEntitiesWithRay propertyof entities ray maxDist maxDepth =
    probePropertyOfEntitiesWithRayClosure propertyof entities ray (maxDist,maxDepth)

  probePropertyOfEntitiesWithRayClosure :: (Material -> Texture Double) -> [Entity] -> Ray -> ((Double,Double) -> ProbeResult)
  probePropertyOfEntitiesWithRayClosure propertyof entities ray = let
      refinedintervalswithtextures = disjunctIntervalsWithCondensedMaterials entities ray
    in \(maxDist,maxDepth) -> (consumeIntervals propertyof ray maxDepth 0 (clipAndFilterIntervalsWithMaterial maxDist refinedintervalswithtextures))
  
  depthOfBetween :: (Material -> Texture Double) -> [Entity] -> Point -> Point -> Double
  depthOfBetween propertyof entities v w = let
      distance = vmag $ w - v
      ray = Ray v (Direction $ (1/distance)|*(w-v))
      proberesult = probePropertyOfEntitiesWithRay propertyof entities ray distance infinity
    in fromJust $ getProbeResultDepth proberesult

  opticalDepthBetween :: [Entity] -> Point -> Point -> Double
  opticalDepthBetween = depthOfBetween materialExtinction
  {-# INLINE opticalDepthBetween #-}


  samplingNothingError name = error $ "don't know " ++ name ++ " probability of sampling Nothing."
  
  --data PointSampler = forall s . (Sampleable s (Maybe Point)) => PointSampler s
  newtype SensationPointSampler = SensationPointSampler Scene
  instance Show SensationPointSampler where
    show (SensationPointSampler scene) = "SensationPointSampler in Scene: " ++ show scene
  instance Sampleable SensationPointSampler (Maybe Point) where
    randomSampleFrom (SensationPointSampler scene) g
      | null sensors = return Nothing
      | otherwise = do entity <- randomSampleFrom sensors g
                       let container = entityContainer entity
                       origin <- randomSampleFrom container g
                       return (Just origin)
      where sensors = sceneSensors scene

    sampleProbabilityOf (SensationPointSampler scene) (Just origin)
      | null sensors = 0
      | otherwise = sum [(sampleProbabilityOf (entityContainer sensor) origin) *
                         (sampleProbabilityOf sensors sensor) | sensor <- sensors]
      where sensors = sceneSensors scene `containing` origin
    sampleProbabilityOf (SensationPointSampler scene) Nothing =
      samplingNothingError "SensationPointSampler"

  instance Arbitrary SensationPointSampler where
    arbitrary = SensationPointSampler `fmap` arbitrary

  prop_SensationPointSampler_nonzeroProb :: SensationPointSampler -> Int -> Property
  prop_SensationPointSampler_nonzeroProb sampler@(SensationPointSampler scene) seedint =
    isJust mpoint ==> f sampleprob
    where f | any (`contains` point) sensors = (>0)
            | otherwise                      = (==0)
          point = fromJust mpoint
          sampleprob = sampleProbabilityOf sampler mpoint
          mpoint = runRandomSampler sampler seedint
          sensors = map entityContainer $ sceneSensors scene


  newtype EmissionPointSampler = EmissionPointSampler Scene
  instance Show EmissionPointSampler where
    show (EmissionPointSampler scene) = "EmissionPointSampler in Scene: " ++ show scene
  instance Sampleable EmissionPointSampler (Maybe Point) where
    randomSampleFrom (EmissionPointSampler scene) g
      | null emitters = return Nothing
      | otherwise = do entity <- randomSampleFrom emitters g
                       let container = entityContainer entity
                       origin <- randomSampleFrom container g
                       return (Just origin)
      where emitters = sceneEmitters scene

    sampleProbabilityOf (EmissionPointSampler scene) (Just origin)
      | null emitters = 0
      | otherwise = sum [(sampleProbabilityOf (entityContainer emitter) origin) *
                         (sampleProbabilityOf emitters emitter) | emitter <- emitters]
      where emitters = sceneEmitters scene `containing` origin
    sampleProbabilityOf (EmissionPointSampler scene) Nothing =
      samplingNothingError "EmissionPointSampler"

  instance Arbitrary EmissionPointSampler where
    arbitrary = EmissionPointSampler `fmap` arbitrary

  prop_EmissionPointSampler_nonzeroProb :: EmissionPointSampler -> Int -> Property
  prop_EmissionPointSampler_nonzeroProb sampler@(EmissionPointSampler scene) seedint =
    isJust mpoint ==> f sampleprob
    where f | any (`contains` point) emitters = (>0)
            | otherwise                       = (==0)
          point = fromJust mpoint
          sampleprob = sampleProbabilityOf sampler mpoint
          mpoint = runRandomSampler sampler seedint
          emitters = map entityContainer $ sceneEmitters scene

  newtype ScatteringPointSampler = ScatteringPointSampler Scene
  instance Show ScatteringPointSampler where
    show (ScatteringPointSampler scene) = "ScatteringPointSampler in Scene: " ++ show scene
  instance Sampleable ScatteringPointSampler (Maybe Point) where
    randomSampleFrom (ScatteringPointSampler scene) g
      | null scatterers = return Nothing
      | otherwise = do entity <- randomSampleFrom scatterers g
                       let container = entityContainer entity
                       origin <- randomSampleFrom container g
                       return (Just origin)
      where scatterers = sceneScatterers scene

    sampleProbabilityOf (ScatteringPointSampler scene) (Just origin)
      | null scatterers = 0
      | otherwise = sum [(sampleProbabilityOf (entityContainer scatterer) origin) *
                         (sampleProbabilityOf scatterers scatterer) | scatterer <- scatterers]
      where scatterers = sceneScatterers scene `containing` origin
    sampleProbabilityOf (ScatteringPointSampler scene) Nothing =
      samplingNothingError "ScatteringPointSampler"

  instance Arbitrary ScatteringPointSampler where
    arbitrary = ScatteringPointSampler `fmap` arbitrary

  prop_ScatteringPointSampler_nonzeroProb :: ScatteringPointSampler -> Int -> Property
  prop_ScatteringPointSampler_nonzeroProb sampler@(ScatteringPointSampler scene) seedint =
    isJust mpoint ==> f sampleprob
    where f | any (`contains` point) scatterers = (>0)
            | otherwise                         = (==0)
          point = fromJust mpoint
          sampleprob = sampleProbabilityOf sampler mpoint
          mpoint = runRandomSampler sampler seedint
          scatterers = map entityContainer $ sceneScatterers scene
    
  -- Direction Samplers
  data DirectionSampler = forall s . (IsDirSampler s, Sampleable s (Maybe Direction)) => DirectionSampler s

  instance Sampleable DirectionSampler (Maybe Direction) where
    randomSampleFrom (DirectionSampler ds) = randomSampleFrom ds
    sampleProbabilityOf (DirectionSampler ds) = sampleProbabilityOf ds

  class IsDirSampler a where
    dirSamplerOrigin :: a -> Point
  
  instance IsDirSampler DirectionSampler where dirSamplerOrigin (DirectionSampler ds)   = dirSamplerOrigin ds
  instance IsDirSampler SensationDirectionSampler  where dirSamplerOrigin (SensationDirectionSampler  (_,origin))   = origin
  instance IsDirSampler EmissionDirectionSampler   where dirSamplerOrigin (EmissionDirectionSampler   (_,origin))   = origin
  instance IsDirSampler ScatteringDirectionSampler where dirSamplerOrigin (ScatteringDirectionSampler (_,origin,_)) = origin

  newtype SensationDirectionSampler = SensationDirectionSampler (Scene, Point)
  instance Show SensationDirectionSampler where
    show (SensationDirectionSampler (scene,origin)) = "SensationDirectionSampler @" ++ showVector3 origin ++
                                                      "in Scene: " ++ show scene
  instance Sampleable SensationDirectionSampler (Maybe Direction) where
    randomSampleFrom (SensationDirectionSampler (scene,origin)) g
      | null sensors = return Nothing
      | otherwise = do direction <- randomSampleFrom (weightedsensor,origin) g
                       return (Just direction)
      where weightedsensor = materialSensor originmat
            originmat = summedMaterialAt sensors origin
            sensors = sceneSensors scene `containing` origin
            --TODO: remove double call to `containing` hidden in summedMaterialAt

    sampleProbabilityOf (SensationDirectionSampler (scene,origin)) (Just direction)
      | null sensors = 0
      | otherwise = sampleProbabilityOf (weightedsensor, origin) direction
      where weightedsensor = materialSensor originmat
            originmat = summedMaterialAt sensors origin
            sensors = sceneSensors scene `containing` origin
    sampleProbabilityOf (SensationDirectionSampler (scene,origin)) Nothing =
      samplingNothingError "SensationDirectionSampler"

  prop_SensationDirectionSampler_nonzeroProb :: Scene -> Int -> Property
  prop_SensationDirectionSampler_nonzeroProb scene seedint =
    isJust mpoint && isJust mdir ==> sampleprob > 0
    where sampleprob = sampleProbabilityOf dirsampler mdir
          mdir = (runRandomSampler dirsampler seedint)::(Maybe Direction)
          dirsampler = SensationDirectionSampler (scene,point)
          point = fromJust mpoint
          mpoint = runRandomSampler pointsampler seedint
          pointsampler = SensationPointSampler scene
 
  newtype EmissionDirectionSampler = EmissionDirectionSampler (Scene, Point)
  instance Show EmissionDirectionSampler where
    show (EmissionDirectionSampler (scene,origin)) = "EmissionDirectionSampler @" ++ showVector3 origin ++
                                                     "in Scene: " ++ show scene
  instance Sampleable EmissionDirectionSampler (Maybe Direction) where
    randomSampleFrom (EmissionDirectionSampler (scene,origin)) g
      | null emitters = return Nothing
      | otherwise = do direction <- randomSampleFrom (weightedphasefunction, Ray origin undefined) g
                       return (Just direction)
      where weightedphasefunction = materialEmissionDirectedness originmat
            originmat = summedMaterialAt emitters origin
            emitters = sceneEmitters scene `containing` origin
            
    sampleProbabilityOf (EmissionDirectionSampler (scene,origin)) (Just direction)
      | null emitters = 0
      | otherwise = sampleProbabilityOf (weightedphasefunction, Ray origin undefined) direction
      where weightedphasefunction = materialEmissionDirectedness originmat
            originmat = summedMaterialAt emitters origin
            emitters = sceneEmitters scene `containing` origin
    sampleProbabilityOf (EmissionDirectionSampler (scene,origin)) Nothing =
      samplingNothingError "EmissionDirectionSampler"

  prop_EmissionDirectionSampler_nonzeroProb :: Scene -> Int -> Property
  prop_EmissionDirectionSampler_nonzeroProb scene seedint =
    isJust mpoint && isJust mdir ==> sampleprob > 0
    where sampleprob = sampleProbabilityOf dirsampler mdir
          mdir = (runRandomSampler dirsampler seedint)::(Maybe Direction)
          dirsampler = EmissionDirectionSampler (scene,point)
          point = fromJust mpoint
          mpoint = runRandomSampler pointsampler seedint
          pointsampler = EmissionPointSampler scene


  newtype ScatteringDirectionSampler = ScatteringDirectionSampler (Scene, Point, Direction)
  instance Show ScatteringDirectionSampler where
    show (ScatteringDirectionSampler (scene,origin,Direction dir)) =
      "ScatteringDirectionSampler @" ++ showVector3 origin ++
      "->@" ++ showVector3 dir ++
      "in Scene: " ++ show scene
  instance Sampleable ScatteringDirectionSampler (Maybe Direction) where
    randomSampleFrom (ScatteringDirectionSampler (scene,origin,win)) g
      | null scatterers = return Nothing
      | otherwise = do wout <- randomSampleFrom (weightedphasefunction, Ray origin win) g
                       return (Just wout)
      where weightedphasefunction = materialScatteringPhaseFunction originmat
            originmat = summedMaterialAt scatterers origin
            scatterers = sceneScatterers scene `containing` origin
            
    sampleProbabilityOf (ScatteringDirectionSampler (scene,origin,win)) (Just wout)
      | null scatterers = 0
      | otherwise = sampleProbabilityOf (weightedphasefunction, Ray origin win) wout
      where weightedphasefunction = materialScatteringPhaseFunction originmat
            originmat = summedMaterialAt scatterers origin
            scatterers = sceneScatterers scene `containing` origin
    sampleProbabilityOf (ScatteringDirectionSampler (scene,origin,win)) Nothing =
      samplingNothingError "ScatteringDirectionSampler"

  prop_ScatteringDirectionSampler_nonzeroProb :: Scene -> Int -> Property
  prop_ScatteringDirectionSampler_nonzeroProb scene seedint =
    isJust mpoint && isJust mdir ==> sampleprob > 0
    where sampleprob = sampleProbabilityOf dirsampler mdir
          mdir = (runRandomSampler dirsampler (seedint+2))::(Maybe Direction)
          dirsampler = ScatteringDirectionSampler (scene,point,olddir)
          olddir = (runRandomSampler olddirsampler (seedint+1))::Direction
          olddirsampler = (Isotropic,undefined::Ray)
          point = fromJust mpoint
          mpoint = runRandomSampler pointsampler seedint
          pointsampler = ScatteringPointSampler scene

  -- Distance Samplers
  data DistanceSampler = forall s . (Sampleable s (Maybe Double)) => DistanceSampler s

  instance Sampleable DistanceSampler (Maybe Double) where
    randomSampleFrom (DistanceSampler ds) = randomSampleFrom ds
    sampleProbabilityOf (DistanceSampler ds) = sampleProbabilityOf ds

  newtype SensationDistanceSampler = SensationDistanceSampler (Scene,Point,Direction)
  instance Show SensationDistanceSampler where
    show (SensationDistanceSampler (scene,origin,Direction dir)) =
      "SensationDistanceSampler @" ++ showVector3 origin ++
      "->@" ++ showVector3 dir ++
      "in Scene: " ++ show scene
  instance Sampleable SensationDistanceSampler (Maybe Double) where
    randomSampleFrom (SensationDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = getSensationDistanceSampler scene origin direction
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (SensationDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = getSensationDistanceSampler scene origin direction
    {-# INLINE sampleProbabilityOf #-}
    
  getSensationDistanceSampler scene origin direction = UniformDepthDistanceSampleable (sensors, property, ray) where
    sensors = sceneSensors scene
    property = materialSensitivity
    ray = Ray origin direction
  {-# INLINE getSensationDistanceSampler #-}

  prop_SensationDistanceSampler_nonzeroProb :: Scene -> Int -> Property
  prop_SensationDistanceSampler_nonzeroProb scene seedint =
    isJust mdist ==> sampleprob > 0
    where sampleprob = sampleProbabilityOf distsampler mdist
          mdist = (runRandomSampler distsampler (seedint+2))::(Maybe Double)
          dir   = (runRandomSampler  dirsampler (seedint+1))::Direction
          point  = runRandomSampler pointsampler seedint
          distsampler = SensationDistanceSampler (scene,point,dir)
          dirsampler = (Isotropic,undefined::Ray)
          pointsampler = Exponential3DPointSampler 1.0

  newtype EmissionDistanceSampler = EmissionDistanceSampler (Scene,Point,Direction)
  instance Show EmissionDistanceSampler where
    show (EmissionDistanceSampler (scene,origin,Direction dir)) =
      "EmissionDistanceSampler @" ++ showVector3 origin ++
      "->@" ++ showVector3 dir ++
      "in Scene: " ++ show scene
  instance Sampleable EmissionDistanceSampler (Maybe Double) where
    randomSampleFrom (EmissionDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = getEmissionDistanceSampler scene origin direction
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (EmissionDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = getEmissionDistanceSampler scene origin direction
    {-# INLINE sampleProbabilityOf #-}

  getEmissionDistanceSampler scene origin direction = UniformDepthDistanceSampleable (emitters, property, ray) where
    emitters = sceneEmitters scene
    property = materialEmissivity
    ray = Ray origin direction
  {-# INLINE getEmissionDistanceSampler #-}

  prop_EmissionDistanceSampler_nonzeroProb :: Scene -> Int -> Property
  prop_EmissionDistanceSampler_nonzeroProb scene seedint =
    isJust mdist ==> sampleprob > 0
    where sampleprob = sampleProbabilityOf distsampler mdist
          mdist = (runRandomSampler distsampler (seedint+2))::(Maybe Double)
          dir   = (runRandomSampler  dirsampler (seedint+1))::Direction
          point  = runRandomSampler pointsampler seedint
          distsampler = EmissionDistanceSampler (scene,point,dir)
          dirsampler = (Isotropic,undefined::Ray)
          pointsampler = Exponential3DPointSampler 1.0

  newtype ScatteringDistanceSampler = ScatteringDistanceSampler (Scene,Point,Direction)
  instance Show ScatteringDistanceSampler where
    show (ScatteringDistanceSampler (scene,origin,Direction dir)) =
      "ScatteringDistanceSampler @" ++ showVector3 origin ++
      "->@" ++ showVector3 dir ++
      "in Scene: " ++ show scene
  instance Sampleable ScatteringDistanceSampler (Maybe Double) where
    randomSampleFrom (ScatteringDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = getScatteringDistanceSampler scene origin direction
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (ScatteringDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = getScatteringDistanceSampler scene origin direction
    {-# INLINE sampleProbabilityOf #-}

  getScatteringDistanceSampler scene origin direction = UniformAttenuation2Sampleable (scatterers, property, ray) where
    scatterers = sceneScatterers scene
    property = materialScattering
    ray = Ray origin direction
  {-# INLINE getScatteringDistanceSampler #-}
  
  prop_ScatteringDistanceSampler_nonzeroProb :: Scene -> Int -> Property
  prop_ScatteringDistanceSampler_nonzeroProb scene seedint =
    isJust mdist ==> sampleprob > 0
    where sampleprob = sampleProbabilityOf distsampler mdist
          mdist = (runRandomSampler distsampler (seedint+2))::(Maybe Double)
          dir   = (runRandomSampler  dirsampler (seedint+1))::Direction
          point  = runRandomSampler pointsampler seedint
          distsampler = ScatteringDistanceSampler (scene,point,dir)
          dirsampler = (Isotropic,undefined::Ray)
          pointsampler = Exponential3DPointSampler 1.0
  
  type DistanceSamplerParameters = ([Entity], Material -> Texture Double, Ray)
  
  newtype UniformAttenuationDistanceSampleable = UniformAttenuationDistanceSampleable DistanceSamplerParameters
  instance Sampleable UniformAttenuationDistanceSampleable (Maybe Double) where
    randomSampleFrom (UniformAttenuationDistanceSampleable (entities,materialproperty,ray)) g = do
      u1 <- uniform g
      let depth = negate $ log (u1::Double)
          proberesult = probePropertyOfEntitiesWithRay materialproperty entities ray infinity depth
      return (getProbeResultDist proberesult)

    sampleProbabilityOf (UniformAttenuationDistanceSampleable (entities,
                                                              materialproperty,
                                                              Ray origin (Direction direction)
                                                              ))
                        (Just distance) =
      let endpoint = origin + distance |* direction
          depth = depthOfBetween materialproperty entities origin endpoint
          endpointvalue = propertyAt materialproperty entities endpoint
      in endpointvalue * (exp (-depth))
    sampleProbabilityOf _ Nothing = samplingNothingError "UniformAttenuationDistanceSampleable"

  newtype UniformAttenuation2Sampleable = UniformAttenuation2Sampleable DistanceSamplerParameters
  instance Sampleable UniformAttenuation2Sampleable (Maybe Double) where
    randomSampleFrom (UniformAttenuation2Sampleable (entities,materialproperty,ray)) g
      | totaldepth==0 = return Nothing
      | otherwise = do u1 <- uniform g
                       let absorptionatinfinity = (1 - (exp (-totaldepth)))
                           randomdepth = negate $ log (1 - absorptionatinfinity * (u1::Double))
                           proberesult = probeMedia (infinity, randomdepth)
                           distance = getProbeResultDist proberesult
                       return distance
      where totaldepth = fromJust $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = probeMedia (infinity, infinity)
            probeMedia = probePropertyOfEntitiesWithRayClosure materialproperty entities ray

    sampleProbabilityOf (UniformAttenuation2Sampleable (entities,
                                                        materialproperty,
                                                        ray@(Ray origin (Direction direction))
                                                        ))
                        (Just distance)
      = --trace ("totaldepth="++show totaldepth++" ,endpointvalue="++show endpointvalue) $
        endpointvalue * (exp (-endpointdepth)) / absorptionatinfinity
        where absorptionatinfinity = (1 - (exp (-totaldepth)))
              endpointvalue = propertyAt materialproperty entities endpoint
              endpoint = origin + distance |* direction
              endpointdepth = fromJust . getProbeResultDepth $ probeMedia (distance, infinity)
              totaldepth    = fromJust . getProbeResultDepth $ probeMedia (infinity, infinity)
              probeMedia = probePropertyOfEntitiesWithRayClosure materialproperty entities ray
    sampleProbabilityOf _ Nothing = samplingNothingError "UniformAttenuation2Sampleable"

  newtype UniformDepthDistanceSampleable = UniformDepthDistanceSampleable DistanceSamplerParameters
  instance Sampleable UniformDepthDistanceSampleable (Maybe Double) where
    randomSampleFrom (UniformDepthDistanceSampleable (entities,materialproperty,ray)) g
      | totaldepth==0 = return Nothing
      | otherwise = do u1 <- uniform g
                       let randomdepth = totaldepth * (u1::Double)
                           proberesult = probeMedia (infinity, randomdepth)
                       return (getProbeResultDist proberesult)
      where totaldepth = fromJust $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = probeMedia (infinity, infinity)
            probeMedia = probePropertyOfEntitiesWithRayClosure materialproperty entities ray

    sampleProbabilityOf (UniformDepthDistanceSampleable (entities,
                                                         materialproperty,
                                                         ray@(Ray origin (Direction direction))
                                                         ))
                        (Just distance)
      = --trace ("totaldepth="++show totaldepth++" ,endpointvalue="++show endpointvalue) $
        endpointvalue / totaldepth
        where endpointvalue = propertyAt materialproperty entities endpoint
              endpoint = origin + distance |* direction
              totaldepth = fromJust $ getProbeResultDepth totaldepthproberesult
              totaldepthproberesult = probePropertyOfEntitiesWithRay materialproperty entities ray infinity infinity
    sampleProbabilityOf _ Nothing = samplingNothingError "UniformDepthDistanceSampleable"
