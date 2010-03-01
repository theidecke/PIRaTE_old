module Main where
  import Data.Vector (Vector3(..),v3x,v3y,vmag)
  import qualified Data.WeighedSet as WS
  import qualified Data.List as L (intersperse,foldl',sortBy)
  import qualified Data.Map as M
  import qualified Data.EmpiricalDiscreteDistribution as EDD (Tree, toWeightList)
  import System.Environment
  import Text.Printf
  import Control.Parallel
  import Control.Parallel.Strategies
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normalize)
  import PIRaTE.Confineable
  import PIRaTE.Container (Container(..))
  import PIRaTE.Container.Sphere (Sphere(..))
  import PIRaTE.Container.Box (fromCorners)
  import PIRaTE.PhaseFunction (PhaseFunction(..))
  import PIRaTE.PhaseFunction.Isotropic (Isotropic(..))
  import PIRaTE.PhaseFunction.ZCone (fromApexAngle)
  import PIRaTE.Texture (Texture(..))
  import PIRaTE.Material (toHomogenousInteractingMaterial,
                          toCustomInteractingMaterial,
                          toHomogenousSensingMaterial,
                          toHomogenousEmittingMaterial)
  import PIRaTE.Scene (Entity,entityFromContainerAndMaterials,Scene,sceneFromEntities)
  import PIRaTE.Sensor (SensorResult(..))
  import PIRaTE.MCMC (mltAction,MutationMemory)
  import PIRaTE.Mutation (
      Mutation(..),
      ExponentialScatteringNodeTranslation(..),
      ExponentialImageNodeTranslation(..),
      NewEmissionPoint(..),
      SimpleRandomPathLength(..),
      RaytracingRandomPathLength(..),
      SimpleBidirRandomPathLength(..),
      BidirPathSub(..),
      getStandardBidirPathSub
    )

  import Debug.Trace
  -- test-stuff

  testEntities = let cont1 = Container $ Sphere (Vector3 0.3 0 0) 0.5
                     cont2 = Container $ Sphere (Vector3 (-0.5) 0 0) 0.3
                     cont3 = Container $ Sphere (Vector3 0.2 0.1 (-0.15)) 0.1
                     cont4 = Container $ Sphere (Vector3 (-0.35) (-0.7) 0.0) 0.25
                     emissionphasefunction   = (1,PhaseFunction Isotropic)
                     scatteringphasefunction = (1,PhaseFunction Isotropic)
                     sensationphasefunction  = (1,PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
                     mat1 = toHomogenousInteractingMaterial  3  4 scatteringphasefunction
                     mat2 = toHomogenousInteractingMaterial  0  7 scatteringphasefunction
                     mat3 = toHomogenousInteractingMaterial 40  0 scatteringphasefunction
                     mat4 = toHomogenousInteractingMaterial  0 40 scatteringphasefunction
                     ent1 = entityFromContainerAndMaterials cont1 [mat1]
                     ent2 = entityFromContainerAndMaterials cont2 [mat2]
                     ent3 = entityFromContainerAndMaterials cont3 [mat3]
                     ent4 = entityFromContainerAndMaterials cont4 [mat4]
                     sensorcontainer = Container $ fromCorners (Vector3 (-1) (-1) (-4.01)) (Vector3 1 1 (-3.99))
                     sensormaterial = toHomogenousSensingMaterial 1.0 sensationphasefunction
                     sensorangle = 1 * arcmin
                     sensorentity = entityFromContainerAndMaterials sensorcontainer [sensormaterial]
                     lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.01
                     lightsourcematerial = toHomogenousEmittingMaterial 1.0 emissionphasefunction
                     lightsourceentity = entityFromContainerAndMaterials lightsourcecontainer [lightsourcematerial]
                 in [ent1,ent2,ent3,ent4,sensorentity,lightsourceentity]

  testRay = Ray (Vector3 0 0 0) (Direction $ normalize (Vector3 0 0 1))
  testScene = sceneFromEntities testEntities
  
  standardScene sigma = let
      emissionphasefunction   = (1,PhaseFunction Isotropic)
      scatteringphasefunction = (1,PhaseFunction Isotropic)
      sensationphasefunction  = (1,PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
      lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.01
      lightsourcematerial = toHomogenousEmittingMaterial 1.0 emissionphasefunction
      lightsourceentity = entityFromContainerAndMaterials lightsourcecontainer [lightsourcematerial]
      scatteringcontainer = Container $ Sphere (Vector3 0 0 0) 1
      scatteringmaterial = toHomogenousInteractingMaterial 0 sigma scatteringphasefunction
      scatteringentity = entityFromContainerAndMaterials scatteringcontainer [scatteringmaterial]
      sensorcontainer = Container $ fromCorners (Vector3 (-1) (-1) (-4.01)) (Vector3 1 1 (-3.99))
      sensormaterial = toHomogenousSensingMaterial 1.0 sensationphasefunction
      sensorangle = 1 * arcmin
      sensorentity = entityFromContainerAndMaterials sensorcontainer [sensormaterial]
      entities = [lightsourceentity, scatteringentity,sensorentity]
    in sceneFromEntities entities
  
  inhomScene sigma = let
      emissionphasefunction   = (1,PhaseFunction Isotropic)
      scatteringphasefunction = (1,PhaseFunction Isotropic)
      sensationphasefunction  = (1,PhaseFunction $ fromApexAngle sensorangle, PathLength . mltStatePathLength)
      lightsourcecontainer = Container $ Sphere (Vector3 0 0 0) 0.01
      lightsourcematerial = toHomogenousEmittingMaterial 1.0 emissionphasefunction
      lightsourceentity = entityFromContainerAndMaterials lightsourcecontainer [lightsourcematerial]
      scatteringcontainer = Container $ Sphere (Vector3 0 0 0) 1
      scatteringmaterial = toCustomInteractingMaterial Empty (Inhomogenous sigmafun) scatteringphasefunction
      sigmafun = simpleDisc sigma 0.1 1.0 0.25
      simpleDisc m eps so a = rho where
        rho p = if s < 0.01 then 0 else c / ((exp (0.5*(z/(eps*s))^2))*(a^2+s^2)) where {z=v3y p; s=sqrt ((v3x p)^2+(v3z p)^2)}
        c = m / ((2*pi)**1.5 * eps * (so - a*(atan (so/a))))
      scatteringentity = entityFromContainerAndMaterials scatteringcontainer [scatteringmaterial]
      sensorcontainer = Container $ fromCorners (Vector3 (-1) (-1) (-4.01)) (Vector3 1 1 (-3.99))
      sensormaterial = toHomogenousSensingMaterial 1.0 sensationphasefunction
      sensorangle = 1 * arcmin
      sensorentity = entityFromContainerAndMaterials sensorcontainer [sensormaterial]
      entities = [lightsourceentity, scatteringentity,sensorentity]
    in sceneFromEntities entities
  
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
  
  putPathLengthList :: [Int] -> IO ()
  putPathLengthList = putStrLn.("pathlengths="++).showListForMathematica show where
    reduceList :: [Int] -> [Int]
    reduceList (x:y:xs)
      | x==y      = reduceList (x:xs)
      | otherwise = x:reduceList (y:xs)
    reduceList (x:[]) = x:[]
    reduceList [] = []

  putMutMemStats :: [MutationMemory] -> IO ()
  putMutMemStats = putStrLn . showMList . map toSortedList . M.toList . last where
    showMList = showListForMathematica (\x->show x++"\n")
    toSortedList (k,t) = (k, reverse . sortByAccProb $ EDD.toWeightList t)
    sortByAccProb = L.sortBy compareAccProbs
    compareAccProbs (_,p1) (_,p2) = compare p1 p2

  main = do
    args <- getArgs
    --[gridsize,n] <- map read `fmap` args
    let (gridsize,n,meankd,sigma) = ((read (args!!0))::Int
                                    ,(read (args!!1))::Int
                                    ,(read (args!!2))::Double
                                    ,(read (args!!3))::Double)
    let mutations1 = [(Mutation $ RaytracingRandomPathLength  avgscatternodes ,  1)]
        mutations1b= [(Mutation $ SimpleBidirRandomPathLength avgscatternodes ,  1)]
        mutations2 = [(Mutation $ ExponentialImageNodeTranslation 0.1         , 10)
                     ,(Mutation $ ExponentialScatteringNodeTranslation 0.1    , 10)
                     ,(Mutation $ SimpleBidirRandomPathLength avgscatternodes ,  5)
                     ,(Mutation $ RaytracingRandomPathLength  avgscatternodes ,  3)
                     ,(Mutation $ NewEmissionPoint                            ,  1)
                     ]
        mutations3 = [(Mutation $ (getStandardBidirPathSub meankd) ,  0.5)]
        mutations4 = [(Mutation $ ExponentialImageNodeTranslation 0.05          , 0.3)
                     ,(Mutation $ ExponentialScatteringNodeTranslation 0.05      , 0.3)
                     ,(Mutation $ SimpleBidirRandomPathLength avgscatternodes   , 0.3)
                     ,(Mutation $ (getStandardBidirPathSub meankd)              , 1.0)
                     ]
        avgscatternodes = 2.0*sigma --shouldn't be less than or equal 0.0
        --meankd = 3.0
        extractor = (\v -> (v3x v, v3y v)) . last . mltStatePath
        --extractor = mltStatePathLength
        getChunksize = min 100
        chunksize = getChunksize n
        --sigma = 5.0
        --scene = standardScene sigma
        scene = inhomScene sigma
        --scene = testScene
        sessionsize = min 20000 n --n
        sessioncount = n `div` sessionsize
        initmutmem = M.empty
        startSampleSession size seed = mltAction scene mutations4 initmutmem extractor seed size (getChunksize size)
        samplesessions = map (startSampleSession sessionsize) [1..sessioncount]
        samples = concat (map fst samplesessions `using` parList rdeepseq)
        mutmems = map snd samplesessions
    --putRadiallyBinnedPhotonCounts gridsize samples
    putGridBinnedPhotonCounts gridsize samples
    --putPhotonList samples
    --putPathLengthList samples
    --putMutMemStats mutmems
