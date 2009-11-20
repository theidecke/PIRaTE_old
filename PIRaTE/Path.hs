{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}

module PIRaTE.Path where
  import Data.Vector (Vector3(..))
  import Data.Maybe (isNothing,fromJust)
  import Control.Monad (replicateM,sequence)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq,normalize)
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
  
  edgeMap :: (a->a->b) -> [a] -> [b]
  edgeMap f     (a:[]) = []
  edgeMap f (a:b:rest) = f a b : edgeMap f (b:rest)

  trisect :: [a] -> (a,[a],a)
  trisect [] = error "cannot trisect empty list"
  trisect (x:xs)
    | null xs = (x,[],x)
    | otherwise = (start,middle,end)
    where start  = x
          middle = init xs
          end    = last xs


  newtype SimplePathSampler = SimplePathSampler (Scene,Int)
  instance Sampleable SimplePathSampler (Maybe Path) where
    randomSampleFrom (SimplePathSampler (scene,n)) g = do
      emissionpoint <- randomSampleFrom (EmissionPointSampler scene) g
      scatterpoints <- replicateM (n-1) . randomSampleFrom (ScatteringPointSampler scene) $ g
      sensationpoint <- randomSampleFrom (SensationPointSampler scene) g
      return . sequence $ [emissionpoint]++scatterpoints++[sensationpoint]
    
    sampleProbabilityOf (SimplePathSampler (scene,n)) (Just path)
      | pathLength path /= n = 0
      | otherwise = emissionprob * scatterprobs * sensationprob
      where emissionprob  = sampleProbabilityOf (EmissionPointSampler  scene) (Just emissionpoint)
            scatterprobs  = product $ map ((sampleProbabilityOf (ScatteringPointSampler scene)).Just) scatterpoints
            sensationprob = sampleProbabilityOf (SensationPointSampler scene) (Just sensationpoint)
            (emissionpoint,scatterpoints,sensationpoint) = trisect path
    sampleProbabilityOf _ Nothing = undefined
  
  randomPathOfLength :: Scene -> Int -> Gen s -> ST s Path
  randomPathOfLength scene n g = do
    completepath <- randomSampleFrom (SimplePathSampler (scene,n)) g
    if (isNothing completepath)
      then randomPathOfLength scene n g
      else return (fromJust completepath)
