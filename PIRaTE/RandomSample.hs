{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PIRaTE.RandomSample where
  import Control.Monad
  import Control.Monad.ST
  import Data.Maybe (fromJust,fromMaybe)
  import Statistics.RandomVariate
  import Statistics.Distribution (quantile)
  import Statistics.Distribution.Exponential (fromLambda)
  import Data.Vector (Vector3(..),(*<>),vmag)
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq,infinity)
  import PIRaTE.Confineable
  import PIRaTE.Sampleable
  import PIRaTE.PhaseFunction.Isotropic (randomIsotropicDirection)
  
  --
  -- random-dependent stuff starts here!
  --
  
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
  instance Sampleable [a] a where
    sampleProbabilityOf choices _ = 1 / (fromIntegral $ length choices)
    --unsafe, should be:
    --sampleProbabilityOf choices choice = if choice `elem` choices then 1 / (fromIntegral $ length choices) else 0
    --requires (Eq a) =>
    randomSampleFrom = randomChoice
    
  randomChoice :: [a] -> Gen s -> ST s a
  randomChoice choices g = do
    rndindex <- randomListIndex choices g
    return $ choices!!rndindex
    
  randomWeightedChoice :: [(a,Double)] -> Gen s -> ST s a
  randomWeightedChoice weightedchoices g = do
    let weights = snd $ unzip weightedchoices
        totalweight = sum weights
        step                   []  _ = error "cannot choose element from an empty list"
        step ((choice,     _):[])  _ = choice
        step ((choice,weight):wcs) remainingweight
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

  newtype Exponential3DPointSampler = Exponential3DPointSampler Double
  instance Sampleable Exponential3DPointSampler Point where
    randomSampleFrom (Exponential3DPointSampler lambda) g = do
      (Direction rdir) <- randomIsotropicDirection g
      rexp <- distributionSample (fromLambda (1/lambda)) g
      return $ rexp *<> rdir

    sampleProbabilityOf (Exponential3DPointSampler lambda) p = (exp (-r/lambda))/(4*pi*lambda*r^2)
                                                               where r = vmag p


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
  
  randomIsotropicDirections n g =
    replicateM n $ randomIsotropicDirection g

  runRandomActions seed = runST $ do
    g <- restore seed
    replicateM 10 $ randomIsotropicDirection g
    