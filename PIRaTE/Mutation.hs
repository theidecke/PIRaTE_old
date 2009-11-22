{-# LANGUAGE ExistentialQuantification #-}

module PIRaTE.Mutation where
  import Data.Vector (Vector3(..),(*<>),vmag)
  import Data.Maybe (fromMaybe)
  import qualified Data.List as L (findIndex)
  import Statistics.RandomVariate (Gen,uniform)
  import Control.Monad.ST (ST)
  import PIRaTE.SpatialTypes (MLTState,mltStatePath,mltStatePathLength,mltStateSubstitutePath)
  import PIRaTE.UtilityFunctions (mapAt)
  import PIRaTE.Sampleable
  import PIRaTE.RandomSample
  import PIRaTE.Scene (Scene)
  import PIRaTE.Path (measurementContribution)
  
  -- the standard (possibly wasteful) way to compute the acceptance probability
  -- f x should return the probability density for state x
  -- t x y should return the transition probability from state x to state y
  defautAcceptanceProbability :: (a -> Double) -> (a -> a -> Double) -> a -> a -> Double
  defautAcceptanceProbability f t oldstate newstate =
    (/) ((f newstate) * (t newstate oldstate))
        ((f oldstate) * (t oldstate newstate))
        
  translationInvariant _ _ = 1
        
  {--
    -- Mutations should adher to this type class:
    -- gives the acceptance probability for the transition (with parameters of type a)
    -- to a new state
    acceptanceProbabilityOf :: a -> Path -> Path -> Double
    -- mutates 
    mutateWith :: a -> Path -> Gen s -> ST s Path
  --}
  class Mutating a where
    mutateWith              :: a -> Scene -> MLTState -> Gen s -> ST s (Maybe MLTState)
    acceptanceProbabilityOf :: a -> Scene -> MLTState -> MLTState -> Double
    
  -- define algebraic datatype which can hold all Mutations which adher to the 'Mutating' type class,
  data Mutation = forall s. (Mutating s) => Mutation s

  instance Mutating Mutation where
    mutateWith              (Mutation m) = mutateWith m
    {-# INLINE mutateWith #-}
    acceptanceProbabilityOf (Mutation m) = acceptanceProbabilityOf m
    {-# INLINE acceptanceProbabilityOf #-}
  
  -- implemented Mutations
  data ExponentialNodeTranslation = ExponentialNodeTranslation Double
  instance Mutating ExponentialNodeTranslation where
    mutateWith (ExponentialNodeTranslation l) scene oldstate g = do
      let oldpath = mltStatePath oldstate
      rndindex <- randomListIndex oldpath g
      rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
      let newpath = mapAt rndindex (+rndtranslation) oldpath
      return . Just $ mltStateSubstitutePath oldstate newpath
    acceptanceProbabilityOf (ExponentialNodeTranslation l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) translationInvariant oldstate newstate


  data ExponentialImageNodeTranslation = ExponentialImageNodeTranslation Double
  instance Mutating ExponentialImageNodeTranslation where
    mutateWith (ExponentialImageNodeTranslation l) scene oldstate g = do
      rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
      let oldpath = mltStatePath oldstate
          oldpathlength = mltStatePathLength oldstate
          newpath = mapAt oldpathlength (+rndtranslation) oldpath
      return . Just $ mltStateSubstitutePath oldstate newpath
    acceptanceProbabilityOf (ExponentialImageNodeTranslation l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) translationInvariant oldstate newstate


  data IncDecPathLength = IncDecPathLength Double  
  instance Mutating IncDecPathLength where
    mutateWith (IncDecPathLength l) scene oldstate g = do
      u <- uniform g
      let coinflip = u::Bool
          oldpath = mltStatePath oldstate
          oldnodecount = length oldpath
      if coinflip
        then do -- add node
          rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
          addindex <- randomIntInRange (0,oldnodecount) g
          let (prelist,postlist) = splitAt addindex oldpath
              anchor
                | addindex==0            = if null postlist then Vector3 0 0 0 else head postlist
                | addindex==oldnodecount = if null  prelist then Vector3 0 0 0 else last  prelist
                | otherwise              = let prenode = last prelist
                                               postnode = head postlist
                                           in 0.5*<>(prenode + postnode)
              newnode = anchor + rndtranslation
              newpath = prelist ++ [newnode] ++ postlist
          return . Just $ mltStateSubstitutePath oldstate newpath
        else if oldnodecount > 1
          then do -- delete node
            delindex <- randomListIndex oldstate g
            let (prelist,postlist) = splitAt delindex oldstate
                newpath = prelist ++ (tail postlist)
            return . Just $ mltStateSubstitutePath oldstate newpath
          else
            return Nothing
    acceptanceProbabilityOf (IncDecPathLength l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) (incDecPathLengthTransitionProbability l) oldstate newstate


  incDecPathLengthTransitionProbability :: Double -> MLTState -> MLTState -> Double
  incDecPathLengthTransitionProbability lambda oldstate newstate =
    let oldpath = mltStatePath oldstate
        newpath = mltStatePath newstate
        oldpathlength = length oldpath
        newpathlength = length newpath
    in 0.5 * if newpathlength > oldpathlength
      then -- added node
        let newnodeindex = fromMaybe oldpathlength $ L.findIndex (uncurry (/=)) $ zip oldpath newpath
            newnode = newpath!!newnodeindex
            anchor
              | newnodeindex==0             = if null oldpath then Vector3 0 0 0 else head oldpath
              | newnodeindex==oldpathlength = if null oldpath then Vector3 0 0 0 else last oldpath
              | otherwise                   = 0.5*<>(sum . take 2 $ drop (newnodeindex-1) oldpath)
            exp3dpointsample = newnode - anchor
            exp3dprob = sampleProbabilityOf (Exponential3DPointSampler lambda) exp3dpointsample
        in exp3dprob / fromIntegral (length newpath)
      else -- deleted node
        1 / max 1 (fromIntegral oldpathlength)
        