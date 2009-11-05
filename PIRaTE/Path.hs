module PIRaTE.Path where
  import Data.Vector (Vector3(..))
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq)
  import PIRaTE.Scene (Scene(..),scatteringAt,opticalDepthBetween)
  
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
  measurementContribution                             _   [] = 0
  measurementContribution (Scene lightsources entities) path =
    let scattercrosssections = map (entities `scatteringAt`) path
    in if any (==0) scattercrosssections
         then 0
         else let scatterfactor = product $ map (*(1/(4*pi))) scattercrosssections
                  completepath = finalizePath path
                  opticaldepth = sum $ edgeMap (opticalDepthBetween entities) completepath
                  edges = edgeMap (-) completepath
                  geometricfactors = product $ map normsq (init edges)
              in scatterfactor * exp (-opticaldepth) / geometricfactors
                       
  
  edgeMap :: (a->a->b) -> [a] -> [b]
  edgeMap f     (a:[]) = []
  edgeMap f (a:b:rest) = f a b : edgeMap f (b:rest)
  