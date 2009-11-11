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
  import PIRaTE.Texture (Texture(..))
  import PIRaTE.Material (toHomogenousInteractingMaterial)
  import PIRaTE.Scene (Entity(..),Scene(..))
  import PIRaTE.MCMC (mltAction)
  import PIRaTE.Mutation (
      Mutation(..),
      ExponentialNodeTranslation(..),
      ExponentialImageNodeTranslation(..),
      IncDecPathLength(..)
    )


  -- test-stuff

  testEntities = let cont1 = Container $ Sphere (Vector3 0.3 0 0) 0.5
                     cont2 = Container $ Sphere (Vector3 (-0.5) 0 0) 0.3
                     cont3 = Container $ Sphere (Vector3 0.2 0.1 0.15) 0.1
                     cont4 = Container $ Sphere (Vector3 (-0.35) (-0.7) 0.0) 0.25
                     mat1 = toHomogenousInteractingMaterial  3  4 (1,PhaseFunction Isotropic)
                     mat2 = toHomogenousInteractingMaterial  0  7 (1,PhaseFunction Isotropic)
                     mat3 = toHomogenousInteractingMaterial 40  0 (1,PhaseFunction Isotropic)
                     mat4 = toHomogenousInteractingMaterial  0 40 (1,PhaseFunction Isotropic)
                     ent1 = Entity cont1 [mat1]
                     ent2 = Entity cont2 [mat2]
                     ent3 = Entity cont3 [mat3]
                     ent4 = Entity cont4 [mat4]
                 in [ent1,ent2,ent3,ent4]

  testRay = Ray (Vector3 0 0 0) (Direction $ normalize (Vector3 0 0 1))
  testScene = Scene testEntities
  
  standardScene sigma = let
      container = Container $ Sphere (Vector3 0 0 0) 1
      material = toHomogenousInteractingMaterial 0 sigma (1,PhaseFunction Isotropic)
      entities = [Entity container [material]]
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
    
  main = do
    [gridsize,n] <- map read `fmap` getArgs
    let mutations = [(Mutation $ ExponentialNodeTranslation 0.1      , 3),
                     (Mutation $ ExponentialImageNodeTranslation 0.3 , 4),
                     (Mutation $ IncDecPathLength 0.1                , 3)]
        extractor = (\v -> (v3x v, v3y v)) . last
        --extractor = (subtract 1).length.finalizePath
        chunksize = min 2500 n
        sigma = 10
        scene = testScene --standardScene sigma
        samples = mltAction scene mutations extractor 19912 n chunksize
    --putRadiallyBinnedPhotonCounts gridsize samples
    putGridBinnedPhotonCounts gridsize samples
    --putPhotonList samples
    --putStrLn.show $ samples

    
  --main = putStrLn "Hi there!"
    
  -- ghc -O2 -fexcess-precision -funfolding-use-threshold=48 --make HMLT.hs -fforce-recomp
  -- ghc -O2 -fexcess-precision -funfolding-use-threshold=48 --make HMLT.hs -prof -auto-all -caf-all -fforce-recomp
  --}