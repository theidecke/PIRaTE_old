{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module PIRaTE.RandomSample where
  import Control.Monad
  import Control.Monad.ST
  import Data.Maybe (fromJust,fromMaybe)
  import Data.Array.Vector (singletonU)
  import Statistics.RandomVariate
  import Statistics.Distribution (quantile)
  import Statistics.Distribution.Exponential (fromLambda)
  import Data.Vector (Vector3(..),(*<>),vmag)
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (normsq,infinity)
  import PIRaTE.Confineable
  import PIRaTE.Sampleable
  import PIRaTE.PhaseFunction.Isotropic (Isotropic(..),randomIsotropicDirection)
  import Test.QuickCheck (Arbitrary,Property,arbitrary)
  import Debug.Trace
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

  data DiscreteDistribution = forall s. (Sampleable s Int,Show s) => DiscreteDistribution s
  instance Show DiscreteDistribution where
    show (DiscreteDistribution d) = "DiscreteDistribution("++show d++")"

  newtype UniformDistribution = UniformDistribution (Int,Int)
  instance Sampleable UniformDistribution Int where
    randomSampleFrom (UniformDistribution (a,b)) g = do
      u <- uniform g
      let k = (a+) . truncate $ (fromIntegral (b-a+1))*(1-(u::Double))
      return k
    
    sampleProbabilityOf (UniformDistribution (a,b)) k
      | k>=a && k<=b = 1 / fromIntegral (b-a+1)
      | otherwise    = 0
  
  newtype GeometricDistribution = GeometricDistribution Double deriving Show
  geometricDistributionFromMean mu = GeometricDistribution (1 / (mu + 1))
  instance Sampleable GeometricDistribution Int where
    randomSampleFrom (GeometricDistribution p') g = geometricSample' p' 0 g where
      geometricSample' p n g = do
        u1 <- uniform g
        let bernoullisuccess = (u1::Double) <= p
        if bernoullisuccess
          then return n
          else geometricSample' p (n+1) g

    sampleProbabilityOf (GeometricDistribution p) n
      | n>=0      = (1-p)^n * p
      | otherwise = 0

  newtype BinomialDistribution = BinomialDistribution (Int,Double) deriving Show
  binomialDistributionFromNMean n mean = BinomialDistribution (n,p) where
    p = min 1 (mean / fromIntegral n)
  instance Sampleable BinomialDistribution Int where
    randomSampleFrom (BinomialDistribution (n,p)) g = do
      us <- replicateM n . uniform $ g
      let successes = length . filter (<=p) $ (us::[Double])
      return successes

    sampleProbabilityOf (BinomialDistribution (n,p)) k =
        (fromIntegral $ binomial n k) * p^k * (1-p)^(n-k)
  
  binomial n k = foldl (\i (l,m) -> i*l `div` m) 1 $ zipWith (,) [n-k+1..n] [1..k]
  
  -- distribution for length of unchanged light-/sensornodes (r,s) for known nodecount n and mean deleted node count mu
  newtype DelBoundsDist = DelBoundsDist (Int,Double)
  instance Sampleable DelBoundsDist (Int,Int) where
    randomSampleFrom (DelBoundsDist (n,p)) g = do
      let kddist = BinomialDistribution (n,p)
      kd <- randomSampleFrom kddist g
      let ks = n - kd
          rmax = min ks (n-1) --r (or s) mustn't be equal to n, because we shouldn't sample the lightsubpath starting at the sensationpoint
          rdist = UniformDistribution (0,rmax)
      r <- randomSampleFrom rdist g
      let s = ks - r
      return (r,s)
    
    sampleProbabilityOf (DelBoundsDist (n,p)) (r,s)
      | any (==0) probs = 0
      | otherwise = product probs 
      where
        --probs  = trace ("|kddist="++show kddist++",kdprob="++show kdprob++",rprob="++show rprob++"|") [kdprob,rprob]
        probs  = [kdprob,rprob]
        kdprob = sampleProbabilityOf kddist kd
        rprob  = sampleProbabilityOf rdist   r
        kddist = BinomialDistribution (n,p)
        rdist  = UniformDistribution (0,rmax)
        rmax = min ks (n-1)
        kd = n - ks
        ks = r + s

  
  -- distribution for number of added light-/sensorsubpath-scatteringnodes for mean added node count mu
  newtype AddBoundsDist = AddBoundsDist Double
  instance Sampleable AddBoundsDist (Int,Int) where
    randomSampleFrom (AddBoundsDist mu) g = do
      let kadist = geometricDistributionFromMean mu
      ka <- randomSampleFrom kadist g
      let idist = UniformDistribution (0,ka)
      i <- randomSampleFrom idist g
      let j = ka - i
      return (i,j)
    
    sampleProbabilityOf (AddBoundsDist mu) (i,j)
      | any (==0) probs = 0
      | otherwise = product probs 
      where
        probs = [kaprob,iprob]
        kaprob = sampleProbabilityOf kadist ka
        iprob  = sampleProbabilityOf idist   i
        kadist = geometricDistributionFromMean mu
        idist = UniformDistribution (0,ka)
        ka = i + j
  
  newtype RIJSDist = RIJSDist (Int,Double)
  instance Sampleable RIJSDist (Int,Int,Int,Int) where
    randomSampleFrom (RIJSDist (n,p)) g = do
      let deldist = DelBoundsDist (n,p)
      (r,s) <- randomSampleFrom deldist g
      let kd = n - r - s
          meanka = max 1 (fromIntegral kd)
          adddist = AddBoundsDist meanka
      (i',j') <- randomSampleFrom adddist g
      let i | r==0 && i'==0 = 1
            | otherwise     = i'
          j | s==0 && j'==0 = 1
            | otherwise     = j'
      return (r,i,j,s)
    
    sampleProbabilityOf (RIJSDist (n,p)) (r,i,j,s)
      | any (==0) probs = 0
      | otherwise = product probs
      where
        probs = [rsprob,ijprob]
        rsprob = sampleProbabilityOf deldist (r,s)
        ijprob = sum [sampleProbabilityOf adddist (i',j') | i'<-is, j'<-js]
        is | r==0 && i==1 = [0,1] -- because we originally could've sampled an i' of zero and would still have returned i=1
           | otherwise    = [i]
        js | s==0 && j==1 = [0,1] -- because we originally could've sampled an j' of zero and would still have returned j=1
           | otherwise    = [j]
        deldist = DelBoundsDist (n,p)
        adddist = AddBoundsDist meanka
        meanka = max 1 (fromIntegral kd)
        kd = n - r - s

  {--prop_RIJSDist_nonzeroProb :: RIJSDist -> Int -> Property
  prop_RIJSDist_nonzeroProb rijsdist seedint = sampleprob>0
    where sampleprob = sampleProbabilityOf rijsdist rijs
          rijs = runRandomSampler rijsdist seedint--}

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

  runRandomSampler :: (Sampleable a b) => a -> Int -> b
  runRandomSampler sampler seedint = runST $ do
    gen <- initialize . singletonU $ fromIntegral seedint
    randomSampleFrom sampler gen
  
  instance Arbitrary Point where
    arbitrary = runRandomSampler expsampler `fmap` arbitrary where
      expsampler = Exponential3DPointSampler 1.0
  
  instance Arbitrary Direction where
    arbitrary = runRandomSampler  (Isotropic,undefined::Ray) `fmap` arbitrary