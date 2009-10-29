{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PIRaTE.RandomSample where
  import Control.Monad
  import Control.Monad.ST  
  import Statistics.RandomVariate
  import Statistics.Distribution (quantile)
  import Statistics.Distribution.Exponential (fromLambda)
  import Data.Vector (Vector3(..),(*<>))
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq)
  import PIRaTE.Confineable
  import PIRaTE.Container
  import PIRaTE.PhaseFunction (PhaseFunction(..),WeightedPhaseFunction(..),scatterPDF)
  import PIRaTE.Scene (entityContainer,sceneEntities)
  import PIRaTE.Sampleable
  --
  -- random-dependent stuff starts here!
  --
  
  instance Sampleable Container Point where
    --probabilityDensityOf :: (Sampleable a) => a -> Point -> Double
    probabilityDensityOf (Container c) point = probabilityDensityOf c point
    {-# INLINE probabilityDensityOf #-}
    --randomSampleFrom :: (Sampleable a) => a -> Gen s -> ST s Point
    randomSampleFrom     (Container c)     g = randomSampleFrom c g
    {-# INLINE randomSampleFrom #-}
    
  randomDirectionFrom :: PhaseFunction -> Direction -> Gen s -> ST s Direction
  randomDirectionFrom       Isotropic   _ g = randomIsotropicDirection g
  randomDirectionFrom (Anisotropic _) win g = undefined
  {-# INLINE randomDirectionFrom #-}
  
  instance Sampleable (PhaseFunction,Direction) Direction where
    probabilityDensityOf (pf,win) wout = scatterPDF pf win wout
    {-# INLINE probabilityDensityOf #-}
    randomSampleFrom     (pf,win)    g = randomDirectionFrom pf win g
    {-# INLINE randomSampleFrom #-}
  
  instance Sampleable (WeightedPhaseFunction,Direction) Direction where
    probabilityDensityOf (WeightedPhaseFunction (w,pf),win) wout = (probabilityDensityOf (pf,win) wout)/w
    randomSampleFrom     (WeightedPhaseFunction (_,      Isotropic),  _) g = randomIsotropicDirection g
    randomSampleFrom     (WeightedPhaseFunction (w,Anisotropic phi),win) g = undefined
    {-# INLINE randomSampleFrom #-}
    
  startSeed = runST $ create >>= save

  -- generates a uniformly distributed random Int in the interval (a,b)
  randomIntInRange :: (Int,Int) -> Gen s -> ST s Int
  randomIntInRange (a,b) g =
    if a==b
      then return a
      else do
        u1 <- uniform g
        let rnddbl = u1::Double
            ad = fromIntegral a
            bd = fromIntegral b
        return $! a + truncate ((bd-ad+1)*rnddbl)

  randomListIndex :: [a] -> Gen s -> ST s Int
  randomListIndex [] g = error "cannot choose an elementindex in an empty list"
  randomListIndex l g = randomIntInRange (0,length l - 1) g

  -- chooses uniformly random a list-element
  randomChoice :: [a] -> Gen s -> ST s a
  randomChoice [] g = error "cannot choose an element of an empty list"
  randomChoice choices g = do
    rndindex <- randomListIndex choices g
    return $ choices!!rndindex
    
  randomWeightedChoice :: [(Double,a)] -> Gen s -> ST s a
  randomWeightedChoice weightedchoices g = do
    let (weights,_) = unzip weightedchoices
        totalweight = sum weights
        step ((     _,choice):[])  _ = choice
        step ((weight,choice):wcs) remainingweight
          | remainingweight > weight = step wcs (remainingweight-weight)
          | otherwise = choice
    u1 <- uniform g
    let randomweight = totalweight*(u1::Double)
    return $ step weightedchoices randomweight

  randomWeightedChoices weightedchoices n g =
    replicateM n $ randomWeightedChoice weightedchoices g

  -- generates a d-distributed sample
  distributionSample d g = do
    u1 <- uniform g
    return $ quantile d (u1::Double)

  -- generates a random direction vector with length one
  randomIsotropicDirection :: Gen s -> ST s Direction
  randomIsotropicDirection g = do
    u1 <- uniform g
    u2 <- uniform g
    let z = 2*(u1::Double) - 1
        phi = 2*pi*(u2::Double)
        rho = sqrt (1 - z*z)
    return $ Vector3 (rho * cos phi) (rho * sin phi) z

  randomExponential3D :: Double -> Gen s -> ST s Point
  randomExponential3D lambda g = do
    rdir <- randomIsotropicDirection g
    rexp <- distributionSample (fromLambda (1/lambda)) g
    return $ rexp *<> rdir
    
  randomPointInUnitSphere :: Gen s -> ST s Point
  randomPointInUnitSphere g = do
    u1 <- uniform g
    u2 <- uniform g
    u3 <- uniform g
    let x = 2*(u1::Double)-1
        y = 2*(u2::Double)-1
        z = 2*(u3::Double)-1
        v = Vector3 x y z
    if normsq v <= 1
      then return v
      else randomPointInUnitSphere g
  
  randomPointInEntities entities g = do
    entity <- randomChoice entities g
    let container = entityContainer entity
    randomSampleFrom container g
  
  randomPathOfLength scene n g = do
    let entities = sceneEntities scene
    replicateM n $ randomPointInEntities entities g
  
  randomIsotropicDirections n g =
    replicateM n $ randomIsotropicDirection g

  runRandomActions seed = runST $ do
    g <- restore seed
    replicateM 10 $ randomIsotropicDirection g
    