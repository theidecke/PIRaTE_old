{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}

module PIRaTE.Path where
  import Data.Vector (Vector3(..))
  import Data.Maybe (isNothing,fromJust)
  import Data.List (isPrefixOf)
  import Control.Monad (replicateM,sequence)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq,normalize,edgeMap)
  import PIRaTE.Sampleable
  import Statistics.RandomVariate (Gen)
  import Control.Monad.ST (ST)
  import PIRaTE.Scene
  
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
    maybecompletepath <- randomSampleFrom (SimplePathSampler (scene,n)) g
    if (isNothing maybecompletepath)
      then randomPathOfLength scene n g
      else do
        let completepath = fromJust maybecompletepath
        if (measurementContribution scene completepath)==0
          then randomPathOfLength scene n g
          else return completepath


  newtype LightSubPathSampler = LightSubPathSampler (Scene,Path,Int)
  instance Sampleable LightSubPathSampler (Maybe Path) where
    randomSampleFrom (LightSubPathSampler (scene,startpath,scatternodecount)) g
      | null startpath = do
          maybeemissionpoint <- randomSampleFrom (EmissionPointSampler scene) g
          if (isNothing maybeemissionpoint)
            then return Nothing
            else do
              let temissionpoint = EPoint . EmissionPoint $ fromJust maybeemissionpoint
                  recsampler = RecursivePathSampler (scene,temissionpoint,undefined,sampleplan)
              mrecpoints <- randomSampleFrom recsampler g
              if (isNothing (mrecpoints::(Maybe TPath)))
                then return Nothing
                else do
                  let lightsubpath = map getPoint . fromJust $ mrecpoints
                  return . Just $ lightsubpath
      | tail startpath == [] = do
          let (emissionpoint:_) = startpath
              temissionpoint = EPoint . EmissionPoint $ emissionpoint
              recsampler = RecursivePathSampler (scene,temissionpoint,undefined,sampleplan)
          mrecpoints <- randomSampleFrom recsampler g
          if (isNothing mrecpoints)
            then return Nothing
            else do
              let lightsubpath = startpath ++ (tail . map getPoint . fromJust $ (mrecpoints::(Maybe TPath)))
              return . Just $ lightsubpath
      | otherwise = do
          -- we have at least two nodes (and the last one is a scattering one)
          let [lastscatterpoint,ntlastscatterpoint] = take 2 (reverse startpath)
              startpoint = EPoint . ScatteringPoint $ lastscatterpoint
              startdir = fromEdge $ lastscatterpoint - ntlastscatterpoint
              recsampler = RecursivePathSampler (scene,startpoint,startdir,sampleplan)
          mrecpoints <- randomSampleFrom recsampler g
          if (isNothing (mrecpoints::(Maybe TPath)))
            then return Nothing
            else do
              let lightsubpath = startpath ++ (tail . map getPoint . fromJust $ mrecpoints)
              return . Just $ lightsubpath
      where sampleplan = SamplePlan $ replicate scatternodecount Sca

    sampleProbabilityOf (LightSubPathSampler _) Nothing = undefined
    sampleProbabilityOf (LightSubPathSampler _) (Just []) = undefined
    sampleProbabilityOf (LightSubPathSampler (scene,startpath,scatternodecount)) (Just lightsubpath)
      | not $ startpath `isPrefixOf` lightsubpath = 0
      | null startpath = let
          emissionpointprob = sampleProbabilityOf (EmissionPointSampler scene) (Just emissionpoint)
          scatterpointsprob = sampleProbabilityOf recsampler (Just tpath)
          recsampler = RecursivePathSampler (scene,temissionpoint,undefined,sampleplan)
          temissionpoint = EPoint . EmissionPoint $ emissionpoint
          (emissionpoint:_) = lightsubpath
        in emissionpointprob * scatterpointsprob
      | tail startpath == [] = let
          scatterpointsprob = sampleProbabilityOf recsampler (Just tpath)
          temissionpoint = EPoint . EmissionPoint $ emissionpoint
          (emissionpoint:_) = startpath
          recsampler = RecursivePathSampler (scene,temissionpoint,undefined,sampleplan)
        in scatterpointsprob
      | otherwise = let
          scatterpointsprob = sampleProbabilityOf recsampler (Just tpath)
          [lastscatterpoint,ntlastscatterpoint] = take 2 (reverse startpath)
          startpoint = EPoint . ScatteringPoint $ lastscatterpoint
          startdir = fromEdge $ lastscatterpoint - ntlastscatterpoint
          recsampler = RecursivePathSampler (scene,startpoint,startdir,sampleplan)
        in scatterpointsprob
      where sampleplan = SamplePlan $ replicate scatternodecount Sca
            tpath = lightSubpathFromPntList recpoints
            recpoints = drop (pathNodeCount startpath - 1) lightsubpath


  lightSubpathFromPntList [] = []
  lightSubpathFromPntList (p:ps) = (EPoint $ EmissionPoint p) : fplTail ps where
    fplTail [] = []
    fplTail (p:ps) = (EPoint $ ScatteringPoint p) : fplTail ps


  newtype SensorSubPathSampler = SensorSubPathSampler (Scene,Path,Int)
  instance Sampleable SensorSubPathSampler (Maybe Path) where
    randomSampleFrom (SensorSubPathSampler (scene,startpath,scatternodecount)) g
      | null startpath = do
          maybesensationpoint <- randomSampleFrom (SensationPointSampler scene) g
          if (isNothing maybesensationpoint)
            then return Nothing
            else do
              let tsensationpoint = EPoint . SensationPoint $ fromJust maybesensationpoint
                  recsampler = RecursivePathSampler (scene,tsensationpoint,undefined,sampleplan)
              mrecpoints <- randomSampleFrom recsampler g
              if (isNothing (mrecpoints::(Maybe TPath)))
                then return Nothing
                else do
                  let sensorsubpath = map getPoint . fromJust $ mrecpoints
                  return . Just $ sensorsubpath
      | tail startpath == [] = do
          let (sensationpoint:_) = startpath
              tsensationpoint = EPoint . SensationPoint $ sensationpoint
              recsampler = RecursivePathSampler (scene,tsensationpoint,undefined,sampleplan)
          mrecpoints <- randomSampleFrom recsampler g
          if (isNothing mrecpoints)
            then return Nothing
            else do
              let sensorsubpath = startpath ++ (tail . map getPoint . fromJust $ (mrecpoints::(Maybe TPath)))
              return . Just $ sensorsubpath
      | otherwise = do
          -- we have at least two nodes (and the last one is a scattering one)
          let [lastscatterpoint,ntlastscatterpoint] = take 2 (reverse startpath)
              startpoint = EPoint . ScatteringPoint $ lastscatterpoint
              startdir = fromEdge $ lastscatterpoint - ntlastscatterpoint
              recsampler = RecursivePathSampler (scene,startpoint,startdir,sampleplan)
          mrecpoints <- randomSampleFrom recsampler g
          if (isNothing (mrecpoints::(Maybe TPath)))
            then return Nothing
            else do
              let sensorsubpath = startpath ++ (tail . map getPoint . fromJust $ mrecpoints)
              return . Just $ sensorsubpath
      where sampleplan = SamplePlan $ replicate scatternodecount Sca

    sampleProbabilityOf (SensorSubPathSampler _) Nothing = undefined
    sampleProbabilityOf (SensorSubPathSampler _) (Just []) = undefined
    sampleProbabilityOf (SensorSubPathSampler (scene,startpath,scatternodecount)) (Just sensorsubpath)
      | not $ startpath `isPrefixOf` sensorsubpath = 0
      | null startpath = let
          sensationpointprob = sampleProbabilityOf (SensationPointSampler scene) (Just sensationpoint)
          scatterpointsprob = sampleProbabilityOf recsampler (Just tpath)
          recsampler = RecursivePathSampler (scene,tsensationpoint,undefined,sampleplan)
          tsensationpoint = EPoint . SensationPoint $ sensationpoint
          (sensationpoint:_) = sensorsubpath
        in sensationpointprob * scatterpointsprob
      | tail startpath == [] = let
          scatterpointsprob = sampleProbabilityOf recsampler (Just tpath)
          tsensationpoint = EPoint . SensationPoint $ sensationpoint
          (sensationpoint:_) = startpath
          recsampler = RecursivePathSampler (scene,tsensationpoint,undefined,sampleplan)
        in scatterpointsprob
      | otherwise = let
          scatterpointsprob = sampleProbabilityOf recsampler (Just tpath)
          [lastscatterpoint,ntlastscatterpoint] = take 2 (reverse startpath)
          startpoint = EPoint . ScatteringPoint $ lastscatterpoint
          startdir = fromEdge $ lastscatterpoint - ntlastscatterpoint
          recsampler = RecursivePathSampler (scene,startpoint,startdir,sampleplan)
        in scatterpointsprob
      where sampleplan = SamplePlan $ replicate scatternodecount Sca
            tpath = sensorSubpathFromPntList recpoints
            recpoints = drop (pathNodeCount startpath - 1) sensorsubpath


  sensorSubpathFromPntList [] = []
  sensorSubpathFromPntList (p:ps) = (EPoint $ SensationPoint p) : fplTail ps where
    fplTail [] = []
    fplTail (p:ps) = (EPoint $ ScatteringPoint p) : fplTail ps