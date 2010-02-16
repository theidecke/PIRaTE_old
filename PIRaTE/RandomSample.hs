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
  import System.Random.MWC
  import Data.Vector (Vector3(..),(|*),vmag)
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
      | k<a || k>b = 0
      | otherwise  = 1 / fromIntegral (b-a+1)
  
  newtype GeometricDistribution = GeometricDistribution Double deriving Show
  geometricDistributionFromMean mu
    | mu >=0    = GeometricDistribution (1 / (mu + 1))
    | otherwise = error ("geometricDistributionFromMean: mu<0, mu="++show mu)

  instance Sampleable GeometricDistribution Int where
    randomSampleFrom (GeometricDistribution p') g = geometricSample' p' 0 g where
      geometricSample' p n g = do
        u1 <- uniform g
        let bernoullisuccess = (u1::Double) <= p
        if bernoullisuccess
          then return n
          else geometricSample' p (n+1) g

    sampleProbabilityOf (GeometricDistribution p) n
      | p<0 || p>1 || n<0 = 0
      | otherwise         = (1-p)^n * p

  newtype PoissonDistribution = PoissonDistribution Double deriving Show
  instance Sampleable PoissonDistribution Int where
    randomSampleFrom (PoissonDistribution lambda) g = poissonSample 0 1 g where
      poissonSample :: Int -> Double -> Gen s -> ST s Int
      poissonSample k p g = do
        u1 <- uniform g
        let p' = (u1::Double) * p
        if (p'<=l)
          then return k
          else poissonSample (k+1) p' g
      l = exp (-lambda)
    -- TODO: numerically unstable pdf. Idea: compute logarithmised expression first
    sampleProbabilityOf (PoissonDistribution lambda) k = undefined
    {--
      | k>=0      = lambda^k * (exp (-lambda)) / (fromIntegral $ fact k)
      | otherwise = 0
      where
        fact k | k>1       = k * (fact (k-1)) 
               | otherwise = 1--}

  newtype BinomialDistribution = BinomialDistribution (Int,Double) deriving Show
  binomialDistributionFromNMean n mean = BinomialDistribution (n,p) where
    p = min 1 (mean / fromIntegral n)
  instance Sampleable BinomialDistribution Int where
    randomSampleFrom (BinomialDistribution (n,p)) g = do
      us <- replicateM n . uniform $ g
      let successes = length . filter (<=p) $ (us::[Double])
      return successes

    sampleProbabilityOf (BinomialDistribution (n,p)) k
      | k<0 || k>n || n<0 || p<0 || p>1 = 0
      | otherwise = (binomial n k) * p^k * (1 - p)^(n-k)
  
  binomial :: Int -> Int -> Double
  binomial n' k'
    | k'<0 || k'>n' = 0
    | 2*k' > n'     = binomial n' (n'-k')
    | otherwise     = foldl (\i (l,m) -> i*l / m) 1 $ zipWith (,) [n-k+1..n] [1..k] where
                        n = fromIntegral n'
                        k = fromIntegral k'
  
  
  newtype StandardRIJSDist = StandardRIJSDist (Int,Double)
  instance Sampleable StandardRIJSDist (Int,Int,Int,Int) where
    randomSampleFrom (StandardRIJSDist (n,meankd)) g = do
      let kddist = binomialDistributionFromNMean n meankd
      kd <- randomSampleFrom kddist g
      let ks = n - kd
          (rmin,rmax) | kd==0     = (1,ks-1)
                      | otherwise = (0,ks)
          --r (or s) mustn't be equal to n, because we shouldn't sample the lightsubpath starting at the sensationpoint
          rdist = UniformDistribution (rmin,rmax)
      r <- randomSampleFrom rdist g
      let s = ks - r
          meanka' = max 1 (fromIntegral kd)
          ka'dist = geometricDistributionFromMean meanka'
      ka' <- randomSampleFrom ka'dist g
      let changeguarantee | ks==n     = 1
                          | otherwise = 0
          twonodeguarantee = length . filter (==0) $ [r,s]
          deltaka = max changeguarantee twonodeguarantee
          ka = ka' + deltaka
          idist = UniformDistribution (0,ka)
      i <- randomSampleFrom idist g
      let j = ka - i
      if badRIJS n (r,i,j,s)
        then fail $ "StandardRIJSDist: Assertion failed: n="++show n++", (r,i,j,s)="++show (r,i,j,s)
        else return (r,i,j,s)
      
    
    sampleProbabilityOf (StandardRIJSDist (n,meankd)) (r,i,j,s)
      | bad_rijs = 0
      | any (==0) probs = 0
      | otherwise = product probs
      where
        bad_rijs = badRIJS n (r,i,j,s)
        probs = [kdprob,rprob,ka'prob,iprob]
        kdprob  = sampleProbabilityOf  kddist  kd
        rprob   = sampleProbabilityOf   rdist   r
        ka'prob = sampleProbabilityOf ka'dist ka'
        iprob   = sampleProbabilityOf   idist   i
        kddist = binomialDistributionFromNMean n meankd
        rdist = UniformDistribution (rmin,rmax)
        (rmin,rmax) | kd==0     = (1,ks-1)
                    | otherwise = (0,ks)
        ka'dist = geometricDistributionFromMean meanka'
        meanka' = max 1 (fromIntegral kd)
        ka' = ka - deltaka
        deltaka = max changeguarantee twonodeguarantee
        changeguarantee | ks==n     = 1
                        | otherwise = 0
        twonodeguarantee = length . filter (==0) $ [r,s]
        idist = UniformDistribution (0,ka)
        kd = n - ks
        ka = i + j
        ks = r + s


  -- tells, if (r,i,j,s) is legal for given n
  badRIJS n (r,i,j,s) = or
    [r < 0, r >= n
    ,s < 0, s >= n
    ,i < 0, j < 0
    ,r+s > n
    ,i+j < minka
    ]
    where minka = length . filter (==0) $ [r,s]

  {--prop_StandardRIJSDist_nonzeroProb :: StandardRIJSDist -> Int -> Property
  prop_StandardRIJSDist_nonzeroProb rijsdist seedint = sampleprob>0
    where sampleprob = sampleProbabilityOf rijsdist rijs
          rijs = runRandomSampler rijsdist seedint--}

  newtype ExponentialSampler = ExponentialSampler Double
  instance Sampleable ExponentialSampler Double where
    randomSampleFrom (ExponentialSampler lambda) g = do
      u <- uniform g
      let p = u::Double
          rexp = - (log (1-p))*lambda
      return rexp
    {-# INLINE randomSampleFrom #-}
    
    sampleProbabilityOf (ExponentialSampler lambda) r
      | r<0 || lambda<=0 = 0
      | otherwise = invlambda*(exp (-invlambda*r)) where invlambda = 1/lambda
    {-# INLINE sampleProbabilityOf #-}

  newtype Exponential3DPointSampler = Exponential3DPointSampler Double
  instance Sampleable Exponential3DPointSampler Point where
    randomSampleFrom (Exponential3DPointSampler lambda) g = do
      (Direction rdir) <- randomIsotropicDirection g
      rexp <- randomSampleFrom (ExponentialSampler lambda) g
      return $ rexp |* rdir

    sampleProbabilityOf (Exponential3DPointSampler lambda) p = rprob / shellarea where
      rprob = sampleProbabilityOf expsampler r
      expsampler = ExponentialSampler lambda
      shellarea = 4*pi*r^2
      r = vmag p

  newtype Exponential2DPointSampler = Exponential2DPointSampler Double
  instance Sampleable Exponential2DPointSampler Point where
    randomSampleFrom (Exponential2DPointSampler lambda) g = do
      u <- uniform g
      let phi = 2*pi*(u::Double)
          rdir = Vector3 (cos phi) (sin phi) 0
      rexp <- randomSampleFrom (ExponentialSampler lambda) g
      return $ rexp |* rdir

    sampleProbabilityOf (Exponential2DPointSampler lambda) p = rprob / shellarea where
      rprob = sampleProbabilityOf expsampler r
      expsampler = ExponentialSampler lambda
      shellarea = 2*pi*r
      r = vmag p

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
  
  takeRandomSamples :: (Sampleable a b) => a -> Int -> Int -> [b]
  takeRandomSamples sampler seedint n = runST $ do
    gen <- initialize . singletonU $ fromIntegral seedint
    (replicateM n . randomSampleFrom sampler) gen
  
  instance Arbitrary Point where
    arbitrary = runRandomSampler expsampler `fmap` arbitrary where
      expsampler = Exponential3DPointSampler 1.0
  
  instance Arbitrary Direction where
    arbitrary = runRandomSampler  (Isotropic,undefined::Ray) `fmap` arbitrary