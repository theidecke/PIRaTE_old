{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Path where
  import Data.Vector (Vector3(..))
  import Data.Maybe (isNothing,fromJust)
  import Control.Monad (replicateM,sequence)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq)
  import PIRaTE.Sampleable
  import Statistics.RandomVariate (Gen)
  import Control.Monad.ST (ST)
  import PIRaTE.Scene
  
  --
  -- path stuff
  --
    
  addLightSourceNode :: Path -> Path
  addLightSourceNode = ((Vector3 0 0 0):)
  addSensorNode :: Path -> Path
  addSensorNode path = path ++ [((Vector3 1 1 0)*(last path) + (Vector3 0 0 1))]
  finalizePath :: Path -> Path
  finalizePath = addSensorNode . addLightSourceNode
  
  measurementContribution :: Scene -> Path -> Double
  measurementContribution     _   [] = 0
  measurementContribution scene path =
    let interactors = sceneInteractors scene
        scatterers = sceneScatterers scene
        scattercrosssections = map (scatterers `scatteringAt`) path
    in if any (==0) scattercrosssections
         then 0
         else let scatterfactor = product $ map (*(1/(4*pi))) scattercrosssections
                  completepath = finalizePath path
                  opticaldepth = sum $ edgeMap (opticalDepthBetween interactors) completepath
                  edges = edgeMap (-) completepath
                  geometricfactors = product $ map normsq (init edges)
              in scatterfactor * exp (-opticaldepth) / geometricfactors
                       
  
  edgeMap :: (a->a->b) -> [a] -> [b]
  edgeMap f     (a:[]) = []
  edgeMap f (a:b:rest) = f a b : edgeMap f (b:rest)
  
  
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
            (emissionpoint:rest) = path
            scatterpoints = init rest
            sensationpoint = last rest
    sampleProbabilityOf _ Nothing = undefined
  
  randomPathOfLength :: Scene -> Int -> Gen s -> ST s Path
  randomPathOfLength scene n g = do
    completepath <- randomSampleFrom (SimplePathSampler (scene,n)) g
    if (isNothing completepath)
      then randomPathOfLength scene n g
      else return (fromJust completepath)
