{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ExistentialQuantification #-}

module PIRaTE.Path where
  import Data.ACVector (Vector3(..),vmag,(|*))
  import Data.Maybe (isNothing,fromJust,isJust)
  import Data.List (isPrefixOf)
  import Control.Monad (replicateM,sequence,liftM,liftM2,foldM)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq,normalize,edgeMap)
  import PIRaTE.Sampleable
  import System.Random.MWC (Gen)
  import Control.Monad.ST (ST)
  import PIRaTE.RandomSample (runRandomSampler,Exponential3DPointSampler(..))
  import PIRaTE.PhaseFunction.Isotropic (Isotropic(..))
  import PIRaTE.Scene
  import Test.QuickCheck hiding (Gen)
  import qualified Test.QuickCheck as QC (Gen)
  
  import Debug.Trace
  --
  -- path stuff
  --
  
  measurementContribution :: Scene -> Path -> Double
  measurementContribution     _   [] = 0
  measurementContribution scene path
    | any (==0) pointfactors = 0
    | otherwise = (product pointfactors) * exp (-opticaldepth) / geometricfactors
    where pointfactors = [emissivity,emissionpf,sensitivity,sensationpf] ++ scattercrosssections ++ scatteringpfs
          emissivity  = scene `emissivityAt` emissionpoint
          sensitivity = scene `sensitivityAt` sensationpoint
          scattercrosssections = map (scene `scatteringAt`) scatterpoints
          emissionpf  = sampleProbabilityOf (EmissionDirectionSampler  (scene,  emissionpoint)) (Just  emissiondir)
          sensationpf = sampleProbabilityOf (SensationDirectionSampler (scene, sensationpoint)) (Just sensationdir)
          scatteringpfs = [sampleProbabilityOf (ScatteringDirectionSampler (scene,p,win)) (Just wout)
                           | p <- scatterpoints
                           | (win,wout) <- scatterdirpairs]
          opticaldepth = sum $ edgeMap (opticalDepthBetween scene) path
          geometricfactors = product . map normsq $ edges
          (emissionpoint,scatterpoints,sensationpoint) = trisect path
          emissiondir = head directions
          sensationdir = (negate `appliedToDirection`) . last $ directions
          scatterdirpairs = edgeMap (,) directions
          directions = map fromEdge edges
          edges = edgeMap (\u v -> v-u) path
  
  type MCFIngredients = ([Double],[Double],[Double],[Double])
  type AugmentedState = (MLTState, MCFIngredients)
  
  measurementContributionHelper :: Scene -> Path -> MCFIngredients
  measurementContributionHelper scene path = (crosssections,phasefunctions,squareddists,opticaldepths) where
    crosssections = [emissivity]++scattercrosssections++[sensitivity]
    phasefunctions = [emissionpf]++scatteringpfs++[sensationpf]
    squareddists = map normsq edges
    opticaldepths = edgeMap (opticalDepthBetween scene) path
    emissivity  = scene `emissivityAt` emissionpoint
    sensitivity = scene `sensitivityAt` sensationpoint
    scattercrosssections = map (scene `scatteringAt`) scatterpoints
    emissionpf  = sampleProbabilityOf (EmissionDirectionSampler  (scene,  emissionpoint)) (Just  emissiondir)
    sensationpf = sampleProbabilityOf (SensationDirectionSampler (scene, sensationpoint)) (Just sensationdir)
    scatteringpfs = [sampleProbabilityOf (ScatteringDirectionSampler (scene,p,win)) (Just wout)
                     | p <- scatterpoints
                     | (win,wout) <- scatterdirpairs]
    (emissionpoint,scatterpoints,sensationpoint) = trisect path
    emissiondir = head directions
    sensationdir = (negate `appliedToDirection`) . last $ directions
    scatterdirpairs = edgeMap (,) directions
    directions = map fromEdge edges
    edges = edgeMap (\u v -> v-u) path
    
  
  measurementContributionQuotient :: Scene -> AugmentedState -> AugmentedState -> Double
  measurementContributionQuotient scene (oldstate,oldingredients) (newstate,newingredients)
    | unchangedpath = 1
    | any (==0) (newcrosssections' ++ newphasefunctions') = 0
    | otherwise = nominator / denominator
    where nominator   = product $ newcrosssections' ++ newphasefunctions' ++ oldsquareddists' ++ [exp deltatau]
          denominator = product $ oldcrosssections' ++ oldphasefunctions' ++ newsquareddists'
          deltatau = (sum oldopticaldepths') - (sum newopticaldepths')
          newcrosssections'  =  take (n' - sharednodes )     . drop r  $ newcrosssections
          newphasefunctions' =  take (n' - sharednodes')     . drop r' $ newphasefunctions
          newsquareddists'   =  take ((n'-1) - sharednodes') . drop r' $ newsquareddists
          newopticaldepths'  =  take ((n'-1) - sharednodes') . drop r' $ newopticaldepths
          oldcrosssections'  =  take (n  - sharednodes )     . drop r  $ oldcrosssections
          oldphasefunctions' =  take (n  - sharednodes')     . drop r' $ oldphasefunctions
          oldsquareddists'   =  take ((n -1) - sharednodes') . drop r' $ oldsquareddists
          oldopticaldepths'  =  take ((n -1) - sharednodes') . drop r' $ oldopticaldepths
          sharednodes  = r  + s
          sharednodes' = r' + s'
          r' = max 0 (r - 1)
          s' = max 0 (s - 1)
          unchangedpath = r == n && s == n --oldpath==newpath
          r = length . map fst . takeWhile (uncurry (==)) $ zip oldpath newpath
          s = length . map fst . takeWhile (uncurry (==)) $ zip (reverse oldpath) (reverse newpath)
          n  = pathNodeCount oldpath
          n' = pathNodeCount newpath
          (newcrosssections,newphasefunctions,newsquareddists,newopticaldepths) = newingredients
          (oldcrosssections,oldphasefunctions,oldsquareddists,oldopticaldepths) = oldingredients
          newpath = mltStatePath newstate
          oldpath = mltStatePath oldstate

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
    randomSampleFrom (RaytracingPathSampler (scene,ns)) g = do
      let sampleplan = SamplePlan $ Sen:(replicate ns Sca)
          recursivesensingsampler = RecursivePathSampler2 (scene,Nothing,sampleplan)
      mrecursivepoints <- randomSampleFrom recursivesensingsampler g
      if (isNothing (mrecursivepoints::(Maybe Path)))
        then return Nothing
        else do
          let pathtail = reverse . fromJust $ mrecursivepoints
          memissionpoint <- randomSampleFrom (EmissionPointSampler scene) g
          if (isNothing memissionpoint)
            then return Nothing
            else do
              let emissionpoint = fromJust memissionpoint
                  path = emissionpoint:pathtail
              return $ Just path

    sampleProbabilityOf (RaytracingPathSampler (scene,ns)) (Just path)
      | pathLength path /= (ns+1) = 0
      | any (==0) probs           = 0
      | otherwise                 = product probs
      where probs = [pathtailprob, emissionprob]
            pathtailprob  = sampleProbabilityOf revpathtailsampler (Just revpathtail)
            emissionprob  = sampleProbabilityOf (EmissionPointSampler scene) (Just emissionpoint)
            revpathtailsampler = RecursivePathSampler2 (scene,Nothing,sampleplan)
            revpathtail = reverse . tail $ path
            emissionpoint = head path
            sampleplan = SamplePlan $ Sen:(replicate ns Sca)
    sampleProbabilityOf _ Nothing = undefined

  newtype SimpleBidirPathSampler = SimpleBidirPathSampler (Scene,Int)
  instance Sampleable SimpleBidirPathSampler (Maybe Path) where
    randomSampleFrom (SimpleBidirPathSampler (scene,ns)) g = do
      let nodecount = 2 + ns
          plans = simpleBidirPlansFromNodeCounts . simpleBidirDivideNodes $ nodecount
          (recursiveemittingsampler, recursivesensingsampler) = simpleBidirSamplersFromPlans scene plans
      msensorpoints <- randomSampleFrom recursivesensingsampler g
      if (isNothing (msensorpoints::(Maybe Path)))
        then return Nothing
        else do
          mlightpoints <- randomSampleFrom recursiveemittingsampler g
          if (isNothing mlightpoints)
            then return Nothing
            else do
              let lightpath  = fromJust mlightpoints
                  sensorpath = fromJust msensorpoints
                  path = lightpath++(reverse sensorpath)
              return $ Just path

    sampleProbabilityOf (SimpleBidirPathSampler (scene,ns)) (Just path)
      | pathLength path /= (ns+1) = 0
      | any (==0) probs           = 0
      | otherwise                 = product probs
      where probs = [sensorpathprob, lightpathprob]
            sensorpathprob = sampleProbabilityOf recursivesensingsampler  (Just sensorpath)
            sensorpath = take sensornodes . reverse $ path
            lightpathprob  = sampleProbabilityOf recursiveemittingsampler (Just lightpath)
            lightpath  = take lightnodes path
            (recursiveemittingsampler, recursivesensingsampler) = simpleBidirSamplersFromPlans scene plans
            plans = simpleBidirPlansFromNodeCounts (lightnodes,sensornodes)
            (lightnodes, sensornodes) = simpleBidirDivideNodes nodecount
            nodecount = 2 + ns
    sampleProbabilityOf _ Nothing = undefined

  simpleBidirSamplersFromPlans scene (lightplan,sensorplan) = (recursiveemittingsampler, recursivesensingsampler) where
    recursivesensingsampler  = RecursivePathSampler2 (scene,Nothing,SamplePlan $ sensorplan)
    recursiveemittingsampler = RecursivePathSampler2 (scene,Nothing,SamplePlan $ lightplan)

  simpleBidirPlansFromNodeCounts (lightnodes,sensornodes) = (lightplan,sensorplan) where
    lightplan  = take lightnodes sampleplan
    sensorplan = take sensornodes . reverse $ sampleplan
    sampleplan = planFromNodeCount nodecount
    nodecount = lightnodes + sensornodes

  simpleBidirDivideNodes nodecount = (lightnodes,sensornodes) where
    lightnodes  = nodecount - sensornodes
    sensornodes = max 2 ((nodecount + 1) `div` 2) --max 2 (nodecount - 1) --2
    
  newtype SimplePathSampler = SimplePathSampler (Scene,Int)
  instance Sampleable SimplePathSampler (Maybe Path) where
    randomSampleFrom (SimplePathSampler (scene,ns)) g = do
      emissionpoint <- randomSampleFrom (EmissionPointSampler scene) g
      scatterpoints <- replicateM ns . randomSampleFrom (ScatteringPointSampler scene) $ g
      sensationpoint <- randomSampleFrom (SensationPointSampler scene) g
      return . sequence $ [emissionpoint]++scatterpoints++[sensationpoint]
    
    sampleProbabilityOf (SimplePathSampler (scene,ns)) (Just path)
      | pathLength path /= (ns+1) = 0
      | otherwise = emissionprob * scatterprobs * sensationprob
      where emissionprob  = sampleProbabilityOf (EmissionPointSampler  scene) (Just emissionpoint)
            scatterprobs  = product $ map ((sampleProbabilityOf (ScatteringPointSampler scene)).Just) scatterpoints
            sensationprob = sampleProbabilityOf (SensationPointSampler scene) (Just sensationpoint)
            (emissionpoint,scatterpoints,sensationpoint) = trisect path
    sampleProbabilityOf _ Nothing = undefined
  
  randomPathOfLength :: Scene -> Int -> Gen s -> ST s Path
  randomPathOfLength scene pl g = do
    let ns = pl+1
    maybecompletepath <- randomSampleFrom (RaytracingPathSampler (scene,ns)) g
    if (isNothing maybecompletepath)
      then randomPathOfLength scene pl g
      else do
        let completepath = fromJust maybecompletepath
        if (measurementContribution scene completepath)==0
          then randomPathOfLength scene pl g
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
            dir = fromEdge edge
            dist = vmag edge
            edge = p - origin
            origin = dirSamplerOrigin dirsampler
    sampleProbabilityOf (RaycastPointSampler (dirsampler,dir2distsampler)) Nothing =
      samplingNothingError "RaycastPointSampler"
  