module Main where
  import Data.Vector (Vector3(..),v3x,v3y)
  import qualified Data.WeighedSet as WS
  import qualified Data.List as L (intersperse,foldl')
  import System.Environment
  import Text.Printf
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normalize)
  import PIRaTE.Confineable
  import PIRaTE.Container (Container(..))
  import PIRaTE.Container.Sphere (Sphere(..))
  import PIRaTE.PhaseFunction (PhaseFunction(..))
  import PIRaTE.PhaseFunction.Isotropic (Isotropic(..))
  import PIRaTE.PhaseFunction.ZCone (fromApexAngle)
  import PIRaTE.Texture (Texture(..))
  import PIRaTE.Material (toHomogenousInteractingMaterial,
                          toHomogenousSensingMaterial,
                          toHomogenousEmittingMaterial)
  import PIRaTE.Scene (Entity(..),Scene(..))
  import PIRaTE.Sensor (SensorResult(..))
  import PIRaTE.MCMC (mltAction)
  import PIRaTE.Mutation (
      Mutation(..),
      ExponentialScatteringNodeTranslation(..),
      ExponentialImageNodeTranslation(..),
      IncDecPathLength(..),
      NewEmissionPoint(..),
      RandomPathLength(..)
    )


  -- test-stuff

  testEntities = let cont1 = Container $ Sphere (Vector3 0.3 0 0) 0.5
                     cont2 = Container $ Sphere (Vector3 (-0.5) 0 0) 0.3
                     cont3 = Container $ Sphere (Vector3 0.2 0.1 (-0.15)) 0.1
                     cont4 = Container $ Sphere (Vector3 (-0.35) (-0.7) 0.0) 0.25
                     mat1 = toHomogenousInteractingMaterial  3  4 (1,PhaseFunction Isotropic)
                     mat2 = toHomogenousInteractingMaterial  0  7 (1,PhaseFunction Isotropic)
                     mat3 = toHomogenousInteractingMaterial 40  0 (1,PhaseFunction Isotropic)
                     mat4 = toHomogenousInteractingMaterial  0 40 (1,PhaseFunction Isotropic)
                     ent1 = Entity cont1 [mat1]
                     ent2 = Entity cont2 [mat2]
                     ent3 = Entity cont3 [mat3]
                     ent4 = Entity cont4 [mat4]
                     sensorcontainer = Container $ Sphere (Vector3 0 0 (-3)) 1.1
                     sensormaterial = toHomogenousSensingMaterial 1.0 (1, PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
                     sensorangle = 1 * arcmin
                     sensorentity = Entity sensorcontainer [sensormaterial]
                     lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.01
                     lightsourcematerial = toHomogenousEmittingMaterial 1.0 (1, PhaseFunction Isotropic)
                     lightsourceentity = Entity lightsourcecontainer [lightsourcematerial]
                 in [ent1,ent2,ent3,ent4,sensorentity,lightsourceentity]

  testRay = Ray (Vector3 0 0 0) (Direction $ normalize (Vector3 0 0 1))
  testScene = Scene testEntities
  
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
  
  
  showSample (x,y) = printf "{%f,%f}" x y
  showSamplesForMathematica :: [(Double,Double)] -> String
  showSamplesForMathematica samples = "testsamples={" ++ concat (L.intersperse ",\n" $ map showSample samples) ++ "};"
  
  showListForMathematica showelement list = "{" ++ concat (L.intersperse "," $ map showelement list) ++ "}\n"
  showGrid2DForMathematica = showListForMathematica (showListForMathematica show)
  
  binSamplesInGrid :: [(Double,Double)] -> Int -> [[Int]]
  binSamplesInGrid samples n = let
      pairMap f (x,y) = (f x, f y)

      gridIndices :: Int -> [[(Int,Int)]]
      gridIndices n = [[(i,j) | i<-indices1d] | j <- reverse indices1d]
        where indices1d = [0..n-1]

      toUnitSquare :: [(Double,Double)] -> [(Double,Double)]
      toUnitSquare samples = map (pairMap (\x -> 0.5*(x+1))) samples

      coordsToGridIndex :: Int -> (Double,Double) -> (Int,Int)
      coordsToGridIndex n point = pairMap (\x -> truncate $ (fromIntegral n)*x) point

      emptybins = WS.empty
      sampleindices = map (coordsToGridIndex n) (toUnitSquare samples)
      fullbins = L.foldl' WS.increaseWeight emptybins sampleindices
    in map (map $ round . WS.weightOf fullbins) (gridIndices n)
  
  binSamplesRadially :: [(Double,Double)] -> Int -> [Int]
  binSamplesRadially samples n = let
      gridIndices :: Int -> [Int]
      gridIndices n = [0..n-1]

      toCenterDistance :: [(Double,Double)] -> [Double]
      toCenterDistance samples = map (\(x,y) -> sqrt (x*x+y*y)) samples

      centerDistanceToBinIndex :: Int -> Double -> Int
      centerDistanceToBinIndex n distance = truncate $ (fromIntegral n)*distance

      emptybins = WS.empty
      sampleindices = map (centerDistanceToBinIndex n) (toCenterDistance samples)
      fullbins = L.foldl' WS.increaseWeight emptybins sampleindices
    in map (round . WS.weightOf fullbins) (gridIndices n)
  
  putGridBinnedPhotonCounts gridsize samples = do
    let photonbincounts = binSamplesInGrid samples gridsize
    putStrLn $ "binnedphotons=" ++ init (showGrid2DForMathematica photonbincounts) ++ ";\n"
    
  putRadiallyBinnedPhotonCounts gridsize samples = do
    let photonbincounts = binSamplesRadially samples gridsize
    putStrLn $ "radialphotoncounts=" ++ init (showListForMathematica show photonbincounts) ++ ";\n"
  
  putPhotonList = putStrLn.showSamplesForMathematica
  
  putPathLengthList = putStrLn.show.reduceList where
    reduceList :: [Int] -> [Int]
    reduceList (x:y:xs)
      | x==y      = reduceList (x:xs)
      | otherwise = x:reduceList (y:xs)
    reduceList (x:[]) = x:[]
    reduceList [] = []
    
  main = do
    [gridsize,n] <- map read `fmap` getArgs
    let mutations = [(Mutation $ ExponentialImageNodeTranslation 0.1       , 10)
                    ,(Mutation $ ExponentialScatteringNodeTranslation 0.1  , 10)
                    ,(Mutation $ NewEmissionPoint                          , 1)
                    ,(Mutation $ RandomPathLength 3.0                      , 3)
                    ]
        extractor = (\v -> (v3x v, v3y v)) . last . mltStatePath
        --extractor = mltStatePathLength
        chunksize = min 2500 n
        sigma = 1.0
        scene = testScene --standardScene sigma
        samples = mltAction scene mutations extractor 9811372 n chunksize
    --putRadiallyBinnedPhotonCounts gridsize samples
    putGridBinnedPhotonCounts gridsize samples
    --putPhotonList samples
    --putPathLengthList samples
