{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ExistentialQuantification #-}

module PIRaTE.Path where
  import Data.Vector (Vector3(..),vmag,(*<>))
  import Data.Maybe (isNothing,fromJust,isJust)
  import Data.List (isPrefixOf)
  import Control.Monad (replicateM,sequence,liftM,liftM2,foldM)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq,normalize,edgeMap)
  import PIRaTE.Sampleable
  import Statistics.RandomVariate (Gen)
  import Control.Monad.ST (ST)
  import PIRaTE.RandomSample (runRandomSampler,Exponential3DPointSampler(..))
  import PIRaTE.PhaseFunction.Isotropic (Isotropic(..))
  import PIRaTE.Scene
  import Test.QuickCheck hiding (Gen)
  import qualified Test.QuickCheck as QC (Gen)
  
  --
  -- path stuff
  --
  
  measurementContribution :: Scene -> Path -> Double
  measurementContribution     _   [] = 0
  measurementContribution scene path
    | any (==0) pointfactors = 0
    | otherwise = (product pointfactors) * exp (-opticaldepth) / geometricfactors
    where pointfactors = [emissivity,emissionpf,sensitivity,sensationpf] ++ scattercrosssections ++ scatteringpfs
          emissivity  = emitters `emissivityAt` emissionpoint
          sensitivity = sensors `sensitivityAt` sensationpoint
          scattercrosssections = map (scatterers `scatteringAt`) scatterpoints
          emissionpf  = sampleProbabilityOf (EmissionDirectionSampler  (scene,  emissionpoint)) (Just  emissiondir)
          sensationpf = sampleProbabilityOf (SensationDirectionSampler (scene, sensationpoint)) (Just sensationdir)
          scatteringpfs = [sampleProbabilityOf (ScatteringDirectionSampler (scene,p,win)) (Just wout)
                           | p <- scatterpoints
                           | (win,wout) <- scatterdirpairs]
          opticaldepth = sum $ edgeMap (opticalDepthBetween interactors) path
          geometricfactors = product . map normsq $ edges
          (emissionpoint,scatterpoints,sensationpoint) = trisect path
          emissiondir = head directions
          sensationdir = (negate `appliedToDirection`) . last $ directions
          scatterdirpairs = edgeMap (,) directions
          directions = map fromEdge edges
          edges = edgeMap (\u v -> v-u) path
          emitters    = sceneEmitters    scene
          scatterers  = sceneScatterers  scene
          sensors     = sceneSensors     scene
          interactors = sceneInteractors scene
  
  trisect :: [a] -> (a,[a],a)
  trisect [] = error "cannot trisect empty list"
  trisect (x:xs)
    | null xs = (x,[],x)
    | otherwise = (start,middle,end)
    where start  = x
          middle = init xs
          end    = last xs

  newtype RaytracingPathSampler = RaytracingPathSampler (Scene,Int)
  instance Sampleable RaytracingPathSampler (Maybe Path) where
    randomSampleFrom (RaytracingPathSampler (scene,pl)) g = do
      msensationpoint <- randomSampleFrom (SensationPointSampler scene) g
      if (isNothing msensationpoint)
        then return Nothing
        else do
          let sensationpoint = fromJust msensationpoint
              esenspoint = EPoint . SensationPoint $ sensationpoint
              sampleplan = SamplePlan $ replicate (pl-1) Sca
              recursivesensingsampler = RecursivePathSampler (scene,esenspoint,undefined,sampleplan)
          mrecursiveepoints <- randomSampleFrom recursivesensingsampler g
          if (isNothing (mrecursiveepoints::(Maybe TPath)))
            then return Nothing
            else do
              let scatterpoints = reverse . map getPoint . tail . fromJust $ mrecursiveepoints
              memissionpoint <- randomSampleFrom (EmissionPointSampler scene) g
              if (isNothing memissionpoint)
                then return Nothing
                else do
                  let emissionpoint = fromJust memissionpoint
                      path = emissionpoint:(scatterpoints++[sensationpoint])
                  return $ Just path

    sampleProbabilityOf (RaytracingPathSampler (scene,pl)) (Just path)
      | pathLength path /= pl = 0
      | otherwise = sensationprob * scatterprobs * emissionprob
      where sensationprob = sampleProbabilityOf (SensationPointSampler scene) (Just sensationpoint)
            scatterprobs  = sampleProbabilityOf scatterpointsampler (Just recursiveepoints)
            emissionprob  = sampleProbabilityOf (EmissionPointSampler  scene) (Just emissionpoint)
            scatterpointsampler = RecursivePathSampler (scene,esenspoint,undefined,sampleplan)
            recursiveepoints = esenspoint:(map (EPoint . ScatteringPoint) . reverse $ scatterpoints)
            esenspoint = EPoint . SensationPoint $ sensationpoint
            sampleplan = SamplePlan $ replicate (pl-1) Sca
            (emissionpoint,scatterpoints,sensationpoint) = trisect path
    sampleProbabilityOf _ Nothing = undefined


  newtype SimplePathSampler = SimplePathSampler (Scene,Int)
  instance Sampleable SimplePathSampler (Maybe Path) where
    randomSampleFrom (SimplePathSampler (scene,pl)) g = do
      emissionpoint <- randomSampleFrom (EmissionPointSampler scene) g
      scatterpoints <- replicateM (pl-1) . randomSampleFrom (ScatteringPointSampler scene) $ g
      sensationpoint <- randomSampleFrom (SensationPointSampler scene) g
      return . sequence $ [emissionpoint]++scatterpoints++[sensationpoint]
    
    sampleProbabilityOf (SimplePathSampler (scene,pl)) (Just path)
      | pathLength path /= pl = 0
      | otherwise = emissionprob * scatterprobs * sensationprob
      where emissionprob  = sampleProbabilityOf (EmissionPointSampler  scene) (Just emissionpoint)
            scatterprobs  = product $ map ((sampleProbabilityOf (ScatteringPointSampler scene)).Just) scatterpoints
            sensationprob = sampleProbabilityOf (SensationPointSampler scene) (Just sensationpoint)
            (emissionpoint,scatterpoints,sensationpoint) = trisect path
    sampleProbabilityOf _ Nothing = undefined
  
  randomPathOfLength :: Scene -> Int -> Gen s -> ST s Path
  randomPathOfLength scene n g = do
    maybecompletepath <- randomSampleFrom (RaytracingPathSampler (scene,n)) g
    if (isNothing maybecompletepath)
      then randomPathOfLength scene n g
      else do
        let completepath = fromJust maybecompletepath
        if (measurementContribution scene completepath)==0
          then randomPathOfLength scene n g
          else return completepath





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
    getDistanceSamplerConstructor (SensationPoint  target) = DistanceSampler  . SensationDistanceSampler
  instance IsEPoint EmissionPoint   where
    getPoint (EmissionPoint   p) = p
    getDirectionSampler scene   _ (EmissionPoint   origin) = DirectionSampler $ EmissionDirectionSampler   (scene, origin)
    getDistanceSamplerConstructor (EmissionPoint   target) = DistanceSampler  . EmissionDistanceSampler
  instance IsEPoint ScatteringPoint where
    getPoint (ScatteringPoint p) = p
    getDirectionSampler scene win (ScatteringPoint origin) = DirectionSampler $ ScatteringDirectionSampler (scene, origin, win)
    getDistanceSamplerConstructor (ScatteringPoint target) = DistanceSampler  . ScatteringDistanceSampler
  instance IsEPoint EPoint where
    getPoint (EPoint ep) = getPoint ep
    getDirectionSampler scene win (EPoint ep) = getDirectionSampler scene win ep
    getDistanceSamplerConstructor (EPoint ep) = getDistanceSamplerConstructor ep
  
  instance Show EPoint where
    show (EPoint ep) = show ep

  data EPoint = forall p . (IsEPoint p, Show p) => EPoint p --EntityPoint
  instance Arbitrary EPoint where
    arbitrary = oneof [fmap (EPoint .  SensationPoint) arbitrary,
                       fmap (EPoint .   EmissionPoint) arbitrary,
                       fmap (EPoint . ScatteringPoint) arbitrary]

  data EPointDummy = Sen | Emi | Sca deriving Show
  instance Arbitrary EPointDummy where
    arbitrary = oneof [return Sen, return Emi, return Sca]
  newtype SamplePlan = SamplePlan [EPointDummy] deriving Show
  instance Arbitrary SamplePlan where
    arbitrary = frequency
      [(1, return $ SamplePlan []),
       (5, fmap (SamplePlan.(:[])) arbitrary)
      ]

  type TPath = [EPoint] --TypedPath
  fromPointList :: Path -> TPath
  fromPointList [] = error "fromPointList: path should have at least two vertices."
  fromPointList (_:[]) = fromPointList []
  fromPointList (p:ps) = (EPoint $ EmissionPoint p) : fplTail ps where
    fplTail (p:[]) = (EPoint $  SensationPoint p) : []
    fplTail (p:ps) = (EPoint $ ScatteringPoint p) : fplTail ps
  toPointList :: TPath -> Path
  toPointList = map getPoint
  planFromPath :: Path -> [EPointDummy]
  planFromPath (emissionpoint:rest) = Emi : planFromPath' rest where
    planFromPath' (sensorpoint:[]) = [Sen]
    planFromPath' (scatterpoint:pts) = Sca : planFromPath' pts
  planFromNodeCount n
    | n>=2 = [Emi] ++ (replicate (n-2) Sca) ++ [Sen]
    | otherwise = error "planFromNodeCount: cannot make sampleplan for paths with less than two nodes."
  typifyPath :: [EPointDummy] -> Path -> TPath
  typifyPath = zipWith typifyPoint
  typifyPoint :: EPointDummy -> Point -> EPoint
  typifyPoint Emi = EPoint .   EmissionPoint
  typifyPoint Sca = EPoint . ScatteringPoint
  typifyPoint Sen = EPoint .  SensationPoint

  type ERay = (EPoint, Direction)

  -- Point Samplers
  newtype RecursivePathSampler2 = RecursivePathSampler2 (Scene,Maybe ERay,SamplePlan)
  instance Show RecursivePathSampler2 where
    show (RecursivePathSampler2 (scene, Nothing, SamplePlan dummylist)) =
      "RecursivePathSampler " ++
      "to sample:" ++ show dummylist ++
      "in Scene: " ++ show scene
    show (RecursivePathSampler2 (scene, Just (eorigin,Direction win), SamplePlan dummylist)) =
      "RecursivePathSampler @" ++ show eorigin ++
      "->@" ++ showVector3 win ++
      "to sample:" ++ show dummylist ++
      "in Scene: " ++ show scene

  instance Sampleable RecursivePathSampler2 (Maybe Path) where
    randomSampleFrom (RecursivePathSampler2 (scene,_,SamplePlan [])) g = return (Just [])
    -- no startpoint provided, so we generate one ourselves according to the head of the given sampleplan
    randomSampleFrom (RecursivePathSampler2 (scene,  Nothing,SamplePlan sampleplan)) g = do
        let (startpointdummy:restplan) = sampleplan
        mstartpoint <- samplePoint startpointdummy g
        if (isNothing mstartpoint)
          then return Nothing
          else do
            let startpoint = fromJust mstartpoint
                startwin = undefined --if we start with Emi or Sen we don't need an \omega_in
                startepoint = typifyPoint startpointdummy startpoint
                innersampler = RecursivePathSampler2 (scene, Just (startepoint,startwin),SamplePlan restplan)
            mrestpath <- randomSampleFrom innersampler g
            return $ liftM2 (:) mstartpoint mrestpath
      where samplePoint :: EPointDummy -> Gen s -> ST s (Maybe Point)
            samplePoint Sen = randomSampleFrom ( SensationPointSampler scene)
            samplePoint Emi = randomSampleFrom (  EmissionPointSampler scene)
            samplePoint Sca = undefined --randomSampleFrom (ScatteringPointSampler scene) -- not supported because \omega_in is needed
    -- we have a startpoint and startdirection. recursively sample new points according to the sampleplan
    randomSampleFrom (RecursivePathSampler2 (scene,mstarteray,SamplePlan sampleplan)) gen =
      liftM (liftM (map (getPoint . fst)) . sequence . tail) $ foldM (step gen) [mstarteray] sampleplan
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

    sampleProbabilityOf (RecursivePathSampler2 _) Nothing = samplingNothingError "RecursivePathSampler2"
    sampleProbabilityOf (RecursivePathSampler2 (scene,  _,SamplePlan [])) (Just []) = 1
    sampleProbabilityOf (RecursivePathSampler2 (scene,Nothing,SamplePlan (startpointdummy:restplan))) (Just (startpoint:restpath)) =
      startpointprob * restprob
      where restprob = sampleProbabilityOf innersampler (Just restpath)
            startpointprob = samplePointProb startpointdummy startpoint
            innersampler = RecursivePathSampler2 (scene, Just (startepoint,startwin),SamplePlan restplan)
            startepoint = typifyPoint startpointdummy startpoint
            startwin = undefined
            samplePointProb :: EPointDummy -> Point -> Double
            samplePointProb Sen p = sampleProbabilityOf ( SensationPointSampler scene) (Just p)
            samplePointProb Emi p = sampleProbabilityOf (  EmissionPointSampler scene) (Just p)
            samplePointProb Sca p = undefined --sampleProbabilityOf (  ScatteringPointSampler scene) (Just p) -- not supported because \omega_in is needed
    sampleProbabilityOf (RecursivePathSampler2 (scene,Just starteray,SamplePlan sampleplan)) (Just restpath)
      | any (==0) edgeprobs = 0
      | otherwise           = product edgeprobs
      where edgeprobs = edgeMap getedgeprob erays
            erays = starteray:eraystail
            eraystail = zip trestpath dirtail
            dirtail = edgeMap (\u v -> fromEdge (v-u)) $ path
            path = startpoint:restpath
            startpoint = getPoint startepoint
            trestpath = typifyPath sampleplan restpath
            (startepoint,startwin) = starteray
            getedgeprob (eorigin,originwin) (etarget,_) = sampleProbabilityOf raycastsampler (Just $ getPoint etarget)
              where raycastsampler = RaycastPointSampler (dirsampler,dir2distsampler)
                    dirsampler = getDirectionSampler scene originwin eorigin
                    dir2distsampler dir = getDistanceSamplerConstructor etarget $ (scene,getPoint eorigin,dir)

    

  newtype RecursivePathSampler = RecursivePathSampler (Scene,EPoint,Direction,SamplePlan)
  instance Show RecursivePathSampler where
    show (RecursivePathSampler (scene,eorigin,Direction win,SamplePlan dummylist)) =
      "RecursivePathSampler @" ++ show eorigin ++
      "->@" ++ showVector3 win ++
      "to sample:" ++ show dummylist ++
      "in Scene: " ++ show scene
  instance Sampleable RecursivePathSampler (Maybe TPath) where
    randomSampleFrom (RecursivePathSampler (scene,startpoint,_,SamplePlan [])) g = return $ Just [startpoint]
    randomSampleFrom (RecursivePathSampler (scene,startpoint,startwin,SamplePlan sampleplan)) gen =
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

    sampleProbabilityOf (RecursivePathSampler (scene,startpoint,startwin,SamplePlan sampleplan)) (Just tpath)
      | any (==0) edgeprobs = 0
      | otherwise           = product edgeprobs
      where edgeprobs = edgeMap getedgeprob erays
            erays = (startpoint,startwin):eraystail
            eraystail = zip (tail tpath) dirtail
            dirtail = edgeMap (\u v -> fromEdge (v-u)) $ map getPoint tpath
            getedgeprob (eorigin,originwin) (etarget,_) = sampleProbabilityOf raycastsampler (Just $ getPoint etarget)
              where raycastsampler = RaycastPointSampler (dirsampler,dir2distsampler)
                    dirsampler = getDirectionSampler scene originwin eorigin
                    dir2distsampler dir = (getDistanceSamplerConstructor etarget) (scene,getPoint eorigin,dir)

    sampleProbabilityOf (RecursivePathSampler (scene,startpoint,startwin,sampleplan)) Nothing =
      samplingNothingError "RecursivePathSampler"

  prop_RecursivePathSampler_nonzeroProb :: Scene -> EPoint -> Direction -> SamplePlan -> Int -> Property
  prop_RecursivePathSampler_nonzeroProb scene startpoint startwin (SamplePlan sampleplan) seedint =
    isJust mtpath ==> sampleprob > 0
    where sampleprob = sampleProbabilityOf pointsampler mtpath
          mtpath = (runRandomSampler pointsampler (seedint+2))::(Maybe TPath)
          pointsampler = RecursivePathSampler (scene,startpoint,startwin,SamplePlan sampleplan)

  raycastbyplan :: Scene -> Direction -> Gen s -> EPoint -> EPointDummy -> ST s (Maybe EPoint)
  raycastbyplan scene win g (EPoint ep) dummy =
    liftPoint $ randomSampleFrom (RaycastPointSampler (dirsampler,dir2distsampler)) g
    where dirsampler = getDirectionSampler scene win ep
          dir2distsampler = \dir -> distanceSamplerFactory dummy (scene,getPoint ep,dir)
          liftPoint = liftM (liftM epointfactory)
          epointfactory = case dummy of
            Sen -> EPoint . SensationPoint
            Emi -> EPoint . EmissionPoint
            Sca -> EPoint . ScatteringPoint

  distanceSamplerFactory dummy = case dummy of
    Sen -> DistanceSampler . SensationDistanceSampler
    Emi -> DistanceSampler . EmissionDistanceSampler
    Sca -> DistanceSampler . ScatteringDistanceSampler

  prop_RaycastPointSampler_nonzeroProb :: Scene -> EPoint -> EPointDummy -> Int -> Property
  prop_RaycastPointSampler_nonzeroProb scene epoint dummy seedint =
    isJust mpoint ==> sampleprob > 0
    where sampleprob = sampleProbabilityOf pointsampler mpoint
          mpoint = (runRandomSampler pointsampler (seedint+2))::(Maybe Point)
          pointsampler = RaycastPointSampler (dirsampler,dir2distsampler)
          dirsampler = getDirectionSampler scene win epoint
          dir2distsampler = \dir -> distanceSamplerFactory dummy (scene,getPoint epoint,dir)
          win   = (runRandomSampler  winsampler (seedint+1))::Direction
          origin  = (runRandomSampler originsampler seedint)::Point
          winsampler = (Isotropic,undefined::Ray)
          originsampler = Exponential3DPointSampler 1.0

  newtype RaycastPointSampler = RaycastPointSampler (DirectionSampler,Direction->DistanceSampler)
  instance Show RaycastPointSampler where
    show (RaycastPointSampler (_,_)) = "RaycastPointSampler"
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
            dir = Direction $ (1/dist) *<> edge --fromEdge edge
            dist = vmag edge
            edge = p - origin
            origin = dirSamplerOrigin dirsampler
    sampleProbabilityOf (RaycastPointSampler (dirsampler,dir2distsampler)) Nothing =
      samplingNothingError "RaycastPointSampler"
  