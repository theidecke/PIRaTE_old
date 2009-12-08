{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module PIRaTE.Scene where
  import Data.Vector ((*<>),vmag,Vector3(..))
  import Data.Monoid
  import Data.Maybe (fromMaybe,fromJust,isNothing)
  import Data.Array.Vector (singletonU)
  import qualified Data.List as L
  import qualified Data.Set as S
  import Control.Monad (foldM,liftM)
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
  import Statistics.RandomVariate (Gen,uniform,initialize)
  import Test.QuickCheck hiding (Gen)
  import qualified Test.QuickCheck as QC (Gen)
  
  -- an Entity is a container filled with light-influencing material
  data Entity = Entity {
      entityContainer::Container,
      entityMaterials::[Material]
    }
    
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
  data Scene = Scene {
      sceneEntities::[Entity]
    } deriving Show

  sceneEmitters :: Scene -> [Entity]
  sceneEmitters = filter isEmitter . sceneEntities
  
  sceneInteractors :: Scene -> [Entity]
  sceneInteractors = filter isInteractor . sceneEntities
  
  sceneScatterers :: Scene -> [Entity]
  sceneScatterers = filter isScatterer . sceneEntities
  
  sceneAbsorbers :: Scene -> [Entity]
  sceneAbsorbers = filter isAbsorber . sceneEntities

  sceneSensors :: Scene -> [Entity]
  sceneSensors = filter isSensor . sceneEntities
  
  instance Arbitrary Scene where
    arbitrary = (standardScene . abs) `fmap` arbitrary where
      standardScene sigma = let
          lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.01
          lightsourcematerial = toHomogenousEmittingMaterial 1.0 (1, PhaseFunction $ Isotropic)
          lightsourceentity = Entity lightsourcecontainer [lightsourcematerial]
          scatteringcontainer = Container $ Sphere (Vector3 0 0 0) 1
          scatteringmaterial = toHomogenousInteractingMaterial 0 sigma (1,PhaseFunction Isotropic)
          scatteringentity = Entity scatteringcontainer [scatteringmaterial]
          sensorcontainer = Container $ Sphere (Vector3 0 0 (-3)) 1.1
          sensormaterial = toHomogenousSensingMaterial 1.0 (1, PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
          sensorangle = 1 * degree
          sensorentity = Entity sensorcontainer [sensormaterial]
          entities = [lightsourceentity, scatteringentity,sensorentity]
        in Scene entities
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
    
  
  fromInterval :: Interval -> a -> [IntervalLimiter a]
  fromInterval (start,end) key = [IntervalLimiter key  True start,
                                  IntervalLimiter key False   end]
  {-# INLINE fromInterval #-}
                                  
  -- transforms a list of tagged intervals possibly overlapping and with coinciding
  -- endpoints into a list of disjoint intervals with taglist
  cutOverlaps :: (Ord a) => [IntervalLimiter a] -> [(Interval,S.Set a)]
  cutOverlaps limiters = cutOverlaps' S.empty sortedLimiters
    where sortedLimiters = L.sort limiters
          cutOverlaps' :: (Ord a) => (S.Set a) -> [IntervalLimiter a] -> [(Interval, S.Set a)]
          cutOverlaps' active         [] = []
          cutOverlaps' active (l1:l2:ls)
            | l1==l2    = rest
            | otherwise = ((intervalLimiterPosition l1,
                            intervalLimiterPosition l2), newactive):rest
            where getNewActive (IntervalLimiter key isbegin _)
                    | isbegin   = S.insert key active
                    | otherwise = S.delete key active
                  --getNewActive (IntervalLimiter key isbegin _) =
                  --  (if isbegin then S.insert else S.delete) key active
                  newactive = getNewActive l1
                  rest = cutOverlaps' newactive (l2:ls)
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
  disjunctIntervalsWithCondensedMaterials entities ray = let
      intervals = [entityContainer entity `intersectedBy` ray | entity<-entities]
      entityindices = [(0::Int)..(length entities -1)]
      nestedindexedintervals = zip intervals entityindices
      indexedintervals = concat [map (\x -> (x, snd ii)) (fst ii) | ii<-nestedindexedintervals]
      taggeddisjointintervals = cutOverlaps.concat $ map (uncurry fromInterval) indexedintervals
      intervalmaterialindices = map (S.toList . snd) taggeddisjointintervals
      entitymateriallists = map entityMaterials entities
      intervalmaterials = [concatMap (entitymateriallists!!) materialindices | materialindices<-intervalmaterialindices]
      condensedintervalmaterials = map mconcat intervalmaterials
      disjointintervals = fst $ unzip taggeddisjointintervals
      refinedintervalswithmaterials = zip disjointintervals condensedintervalmaterials
    in refinedintervalswithmaterials
    

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
  probePropertyOfEntitiesWithRay propertyof entities ray maxDist maxDepth = let
      refinedintervalswithtextures = disjunctIntervalsWithCondensedMaterials entities ray
      clippedintervals = clipAndFilterIntervalsWithMaterial maxDist refinedintervalswithtextures
    in consumeIntervals propertyof ray maxDepth 0 clippedintervals
  
  depthOfBetween :: (Material -> Texture Double) -> [Entity] -> Point -> Point -> Double
  depthOfBetween propertyof entities v w = let
      distance = vmag $ w - v
      ray = Ray v (Direction $ (1/distance)*<>(w-v))
      proberesult = probePropertyOfEntitiesWithRay propertyof entities ray distance infinity
    in fromJust $ getProbeResultDepth proberesult

  probeExtinctionWithRay :: [Entity] -> Ray -> Double -> Double -> ProbeResult
  probeExtinctionWithRay = probePropertyOfEntitiesWithRay materialExtinction

  opticalDepthBetween :: [Entity] -> Point -> Point -> Double
  opticalDepthBetween = depthOfBetween materialExtinction
  {-# INLINE opticalDepthBetween #-}


  newtype SensationPoint  = SensationPoint  Point
  newtype EmissionPoint   = EmissionPoint   Point
  newtype ScatteringPoint = ScatteringPoint Point
  instance Show SensationPoint  where show  (SensationPoint p) = "Sen@" ++ showVector3 p
  instance Show EmissionPoint   where show   (EmissionPoint p) = "Emi@" ++ showVector3 p
  instance Show ScatteringPoint where show (ScatteringPoint p) = "Sca@" ++ showVector3 p

  class IsEPoint a where
    getPoint :: a -> Point
    getDirectionSampler :: Scene -> Direction -> a -> DirectionSampler
    getDistanceSamplerConstructor :: a -> ((Scene,Point,Direction) -> DistanceSampler)

  instance IsEPoint SensationPoint  where
    getPoint (SensationPoint  p) = p
    getDirectionSampler scene   _ (SensationPoint  origin) = DirectionSampler $ SensationDirectionSampler  (scene, origin)
    getDistanceSamplerConstructor (SensationPoint  origin) = DistanceSampler . SensationDistanceSampler
  instance IsEPoint EmissionPoint   where
    getPoint (EmissionPoint   p) = p
    getDirectionSampler scene   _ (EmissionPoint   origin) = DirectionSampler $ EmissionDirectionSampler   (scene, origin)
    getDistanceSamplerConstructor (EmissionPoint   origin) = DistanceSampler  . EmissionDistanceSampler
  instance IsEPoint ScatteringPoint where
    getPoint (ScatteringPoint p) = p
    getDirectionSampler scene win (ScatteringPoint origin) = DirectionSampler $ ScatteringDirectionSampler (scene, origin, win)
    getDistanceSamplerConstructor (ScatteringPoint origin) = DistanceSampler  . ScatteringDistanceSampler
  instance IsEPoint EPoint where
    getPoint (EPoint ep) = getPoint ep
    getDirectionSampler scene win (EPoint ep) = getDirectionSampler scene win ep
    getDistanceSamplerConstructor (EPoint ep) = getDistanceSamplerConstructor ep
  
  instance Show EPoint where
    show (EPoint ep) = show ep

  data EPoint = forall p . (IsEPoint p, Show p) => EPoint p --EntityPoint
  data EPointDummy = Sen | Emi | Sca
  type TPath = [EPoint] --TypedPath
  type ERay = (EPoint, Direction)

  samplingNothingError name = error $ "don't know " ++ name ++ " probability of sampling Nothing."
  
  -- Point Samplers
  newtype RecursivePathSampler = RecursivePathSampler (Scene,EPoint,Direction,[EPointDummy])
  instance Sampleable RecursivePathSampler (Maybe TPath) where
    randomSampleFrom (RecursivePathSampler (scene,startpoint,_,[])) g = return $ Just [startpoint]
    randomSampleFrom (RecursivePathSampler (scene,startpoint,startwin,sampleplan)) gen =
      liftM (liftM (map fst) . sequence) $ foldM (step gen) [Just (startpoint,startwin)] sampleplan
      where step :: Gen s -> [Maybe ERay] -> EPointDummy -> ST s [Maybe ERay]
            step g eraysdone dummy = do
              let lastmaybeeray = last eraysdone
                  appenderay newmaybeeray = eraysdone ++ [newmaybeeray]
              if (isNothing lastmaybeeray)
                then return $ appenderay Nothing
                else do let (prevepoint,win) = fromJust lastmaybeeray
                        maybenewpoint <- raycastbyplan scene win g prevepoint dummy
                        if (isNothing maybenewpoint)
                          then return $ appenderay Nothing
                          else do let newepoint = fromJust maybenewpoint
                                      oldpoint = getPoint prevepoint
                                      newpoint = getPoint newepoint
                                      wout = fromEdge (newpoint - oldpoint)
                                  return . appenderay $ Just (newepoint,wout)

    sampleProbabilityOf (RecursivePathSampler (scene,startpoint,startwin,sampleplan)) (Just tpath)
      | any (==0) edgeprobs = 0
      | otherwise           = product edgeprobs
      where edgeprobs = edgeMap getedgeprob erays
            erays = (startpoint,startwin):eraystail
            eraystail = zip (tail tpath) dirtail
            dirtail = edgeMap (\u v -> fromEdge (v-u)) $ map getPoint tpath
            getedgeprob (ep1,d1) (ep2,d2) = sampleProbabilityOf raycastsampler (Just $ getPoint ep2)
              where raycastsampler = RaycastPointSampler (dirsampler,dir2distsampler)
                    dirsampler = getDirectionSampler scene d1 ep1
                    dir2distsampler dir = (getDistanceSamplerConstructor ep1) (scene,getPoint ep1,dir)

    sampleProbabilityOf (RecursivePathSampler (scene,startpoint,startwin,sampleplan)) Nothing =
      samplingNothingError "RecursivePathSampler"

  raycastbyplan :: Scene -> Direction -> Gen s -> EPoint -> EPointDummy -> ST s (Maybe EPoint)
  raycastbyplan scene win g (EPoint ep) dummy = liftPoint $ randomSampleFrom (RaycastPointSampler (dirsampler,dir2distsampler)) g
    where dirsampler = getDirectionSampler scene win ep
          dir2distsampler = \dir -> distancesamplerfactory (scene,getPoint ep,dir)
          distancesamplerfactory = case dummy of
            Sen -> DistanceSampler . SensationDistanceSampler
            Emi -> DistanceSampler . EmissionDistanceSampler
            Sca -> DistanceSampler . ScatteringDistanceSampler
          liftPoint = liftM (liftM epointfactory)
          epointfactory = case dummy of
            Sen -> EPoint . SensationPoint
            Emi -> EPoint . EmissionPoint
            Sca -> EPoint . ScatteringPoint


  newtype RaycastPointSampler = RaycastPointSampler (DirectionSampler,Direction->DistanceSampler)
  instance Sampleable RaycastPointSampler (Maybe Point) where
    randomSampleFrom (RaycastPointSampler (dirsampler,dir2distsampler)) g = do
      maybedir <- randomSampleFrom dirsampler g
      if (isNothing maybedir)
        then return Nothing
        else do let dir = fromJust maybedir
                maybedist <- randomSampleFrom (dir2distsampler dir) g
                if (isNothing maybedist)
                  then return Nothing
                  else do let origin = dirSamplerOrigin dirsampler
                              dist = fromJust maybedist
                          return $ Just ((Ray origin dir) `followFor` dist)

    sampleProbabilityOf (RaycastPointSampler (dirsampler,dir2distsampler)) (Just p) =
      dirprob * distprob / (dist^2)
      where dirprob  = sampleProbabilityOf  dirsampler (Just  dir)
            distprob = sampleProbabilityOf distsampler (Just dist)
            distsampler = dir2distsampler dir
            dir = Direction $ (1/dist) *<> edge
            dist = vmag edge
            edge = p - origin
            origin = dirSamplerOrigin dirsampler
    sampleProbabilityOf (RaycastPointSampler (dirsampler,dir2distsampler)) Nothing =
      samplingNothingError "RaycastPointSampler"
      
  
  newtype SensationPointSampler = SensationPointSampler Scene
  instance Show SensationPointSampler where
    show (SensationPointSampler scene) = "SensationPointSampler for Scene: " ++ show scene
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

  prop_SensationPointSampler_nonzeroProb sampler@(SensationPointSampler scene) seedint
    | any (`contains` point) sensors = sampleprob  > 0
    | otherwise                      = sampleprob == 0
    where point = maybe (Vector3 0 0 0) id mpoint
          sampleprob = sampleProbabilityOf sampler mpoint
          mpoint = runRandomSampler sampler seedint
          sensors = map entityContainer $ sceneSensors scene


  newtype EmissionPointSampler = EmissionPointSampler Scene
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


  newtype ScatteringPointSampler = ScatteringPointSampler Scene
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


  newtype EmissionDirectionSampler = EmissionDirectionSampler (Scene, Point)
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


  newtype ScatteringDirectionSampler = ScatteringDirectionSampler (Scene, Point, Direction)
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


  -- Distance Samplers
  data DistanceSampler = forall s . (Sampleable s (Maybe Double)) => DistanceSampler s

  instance Sampleable DistanceSampler (Maybe Double) where
    randomSampleFrom (DistanceSampler ds) = randomSampleFrom ds
    sampleProbabilityOf (DistanceSampler ds) = sampleProbabilityOf ds

  newtype SensationDistanceSampler = SensationDistanceSampler (Scene,Point,Direction)
  instance Sampleable SensationDistanceSampler (Maybe Double) where
    randomSampleFrom (SensationDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = UniformDepthDistanceSampleable (sensors, materialSensitivity, Ray origin direction)
            sensors = sceneSensors scene
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (SensationDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = UniformDepthDistanceSampleable (sensors, materialSensitivity, Ray origin direction)
            sensors = sceneSensors scene
    {-# INLINE sampleProbabilityOf #-}


  newtype EmissionDistanceSampler = EmissionDistanceSampler (Scene,Point,Direction)
  instance Sampleable EmissionDistanceSampler (Maybe Double) where
    randomSampleFrom (EmissionDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = UniformDepthDistanceSampleable (emitters, materialEmissivity, Ray origin direction)
            emitters = sceneEmitters scene
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (EmissionDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = UniformDepthDistanceSampleable (emitters, materialEmissivity, Ray origin direction)
            emitters = sceneEmitters scene
    {-# INLINE sampleProbabilityOf #-}


  newtype ScatteringDistanceSampler = ScatteringDistanceSampler (Scene,Point,Direction)
  instance Sampleable ScatteringDistanceSampler (Maybe Double) where
    randomSampleFrom (ScatteringDistanceSampler (scene,origin,direction)) g =
      randomSampleFrom distsampler g
      where distsampler = UniformAttenuationDistanceSampleable (scatterers, materialScattering, Ray origin direction)
            scatterers = sceneScatterers scene
    {-# INLINE randomSampleFrom #-}

    sampleProbabilityOf (ScatteringDistanceSampler (scene,origin,direction)) distance = 
      sampleProbabilityOf distsampler distance
      where distsampler = UniformAttenuationDistanceSampleable (scatterers, materialScattering, Ray origin direction)
            scatterers = sceneScatterers scene
    {-# INLINE sampleProbabilityOf #-}
      
  
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
      let endpoint = origin + distance *<> direction
          depth = depthOfBetween materialproperty entities origin endpoint
          endpointvalue = propertyAt materialproperty entities endpoint
      in endpointvalue * (exp (-depth))
    sampleProbabilityOf _ Nothing = samplingNothingError "UniformAttenuationDistanceSampleable"


  newtype UniformDepthDistanceSampleable = UniformDepthDistanceSampleable DistanceSamplerParameters
  instance Sampleable UniformDepthDistanceSampleable (Maybe Double) where
    randomSampleFrom (UniformDepthDistanceSampleable (entities,materialproperty,ray)) g
      | totaldepth==0 = return Nothing
      | otherwise = do u1 <- uniform g
                       let randomdepth = totaldepth * (u1::Double)
                           proberesult = probeToInfinity randomdepth
                       return (getProbeResultDist proberesult)
      where totaldepth = fromJust $ getProbeResultDepth totaldepthproberesult
            totaldepthproberesult = probeToInfinity infinity
            probeToInfinity = probePropertyOfEntitiesWithRay materialproperty entities ray infinity

    sampleProbabilityOf (UniformDepthDistanceSampleable (entities,
                                                         materialproperty,
                                                         ray@(Ray origin (Direction direction))
                                                         ))
                        (Just distance)
      = endpointvalue / totaldepth
        where endpointvalue = propertyAt materialproperty entities endpoint
              endpoint = origin + distance *<> direction
              totaldepth = fromJust $ getProbeResultDepth totaldepthproberesult
              totaldepthproberesult = probePropertyOfEntitiesWithRay materialproperty entities ray infinity infinity
    sampleProbabilityOf _ Nothing = samplingNothingError "UniformDepthDistanceSampleable"
