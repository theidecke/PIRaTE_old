{-# LANGUAGE ExistentialQuantification #-}

module PIRaTE.Mutation where
  import Data.Vector (Vector3(..),(*<>),vmag)
  import Data.Maybe (fromMaybe,isNothing,fromJust)
  import qualified Data.List as L (findIndex)
  import Statistics.RandomVariate (Gen,uniform)
  import Control.Monad (liftM)
  import Control.Monad.ST (ST)
  import PIRaTE.SpatialTypes (MLTState,mltStatePath,mltStatePathLength,mltStateSubstitutePath,pathNodeCount)
  import PIRaTE.UtilityFunctions (mapAt)
  import PIRaTE.Sampleable
  import PIRaTE.RandomSample
  import PIRaTE.Scene
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
  data ExponentialScatteringNodeTranslation = ExponentialScatteringNodeTranslation Double
  instance Mutating ExponentialScatteringNodeTranslation where
    mutateWith (ExponentialScatteringNodeTranslation l) scene oldstate g
      | nodecount<=2 = return Nothing
      | otherwise = do
          rndindex <- randomIntInRange (1,nodecount-1) g
          rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
          let newpath = mapAt rndindex (+rndtranslation) oldpath
          return . Just $ mltStateSubstitutePath oldstate newpath
      where nodecount = pathNodeCount oldpath
            oldpath   = mltStatePath oldstate

    acceptanceProbabilityOf (ExponentialScatteringNodeTranslation l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) translationInvariant oldstate newstate


  data ExponentialImageNodeTranslation = ExponentialImageNodeTranslation Double
  instance Mutating ExponentialImageNodeTranslation where
    mutateWith (ExponentialImageNodeTranslation l) scene oldstate g = do
        rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
        let newsensorpoint = (last pathtail) + rndtranslation
            newesensorpoint = EPoint . SensationPoint $ newsensorpoint
            dummy = if nodecount==2 then Emi else Sca
        maybenewreversetail <- randomSampleFrom (RecursivePathSampler (scene,newesensorpoint,undefined,[dummy])) g
        if (isNothing maybenewreversetail)
          then return Nothing
          else do
            let nexttolastpoint = getPoint . last . fromJust $ (maybenewreversetail::(Maybe TPath))
                newtail = [nexttolastpoint,newsensorpoint]
                newpath = fixedpath ++ newtail
            return . Just $ mltStateSubstitutePath oldstate newpath
      where (fixedpath,pathtail) = splitAt (nodecount-2) oldpath
            nodecount = pathNodeCount oldpath
            oldpath = mltStatePath oldstate

    acceptanceProbabilityOf (ExponentialImageNodeTranslation l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene)
                                  (expImgNodeTrlTransitionProbability scene l)
                                  oldstate
                                  newstate
      where
        expImgNodeTrlTransitionProbability :: Scene -> Double -> MLTState -> MLTState -> Double
        expImgNodeTrlTransitionProbability scene lambda oldstate newstate =
           sensprob * ntlprob
          where ntlprob = sampleProbabilityOf recsampler recsample
                recsampler = RecursivePathSampler (scene,newesensorpoint,undefined,[dummy])
                recsample = Just [newesensorpoint, epfactory newntl]
                newesensorpoint = EPoint $ SensationPoint newsens
                dummy = if newnodecount==2 then Emi else Sca
                epfactory = if newnodecount==2 then EPoint . EmissionPoint else EPoint . ScatteringPoint
                sensprob = sampleProbabilityOf (Exponential3DPointSampler lambda) sensdelta
                sensdelta = newsens - oldsens
                [oldntl,oldsens] = snd . splitAt (oldnodecount - 2) $ oldpath
                [newntl,newsens] = snd . splitAt (newnodecount - 2) $ newpath
                oldnodecount = pathNodeCount oldpath
                newnodecount = pathNodeCount newpath
                oldpath = mltStatePath oldstate
                newpath = mltStatePath newstate


  data ResampleSensorDirection = ResampleSensorDirection
  instance Mutating ResampleSensorDirection where
    mutateWith ResampleSensorDirection scene oldstate g =
      do return undefined

    acceptanceProbabilityOf ResampleSensorDirection scene =
      defautAcceptanceProbability (measurementContribution scene) t
      where t = undefined
      

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
            delindex <- randomListIndex oldpath g
            let (prelist,postlist) = splitAt delindex oldpath
                newpath = prelist ++ (tail postlist)
            return . Just $ mltStateSubstitutePath oldstate newpath
          else
            return Nothing
    acceptanceProbabilityOf (IncDecPathLength l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene)
                                  (incDecPathLengthTransitionProbability l)
                                  oldstate
                                  newstate
      where
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
        