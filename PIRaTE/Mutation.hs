{-# LANGUAGE ExistentialQuantification #-}

module PIRaTE.Mutation where
  import Data.Vector (Vector3(..),(*<>),vmag)
  import Data.Maybe (fromMaybe,isNothing,fromJust)
  import qualified Data.List as L (findIndex)
  import Statistics.RandomVariate (Gen,uniform)
  import Control.Monad (liftM)
  import Control.Monad.ST (ST)
  import PIRaTE.SpatialTypes (MLTState,mltStatePath,mltStatePathLength,mltStateSubstitutePath,pathNodeCount,pathLength)
  import PIRaTE.UtilityFunctions (mapAt)
  import PIRaTE.Sampleable
  import PIRaTE.RandomSample
  import PIRaTE.Scene
  import PIRaTE.Path (measurementContribution,trisect,RaytracingPathSampler(..))
  
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
  data Mutation = forall s. (Show s, Mutating s) => Mutation s

  instance Mutating Mutation where
    mutateWith              (Mutation m) = mutateWith m
    {-# INLINE mutateWith #-}
    acceptanceProbabilityOf (Mutation m) = acceptanceProbabilityOf m
    {-# INLINE acceptanceProbabilityOf #-}
  
  instance Show Mutation where
    show (Mutation m) = show m

  -- implemented Mutations
  data NewEmissionPoint = NewEmissionPoint
  instance Show NewEmissionPoint where
    show NewEmissionPoint = "NewEmissionPoint"
  instance Mutating NewEmissionPoint where
    mutateWith NewEmissionPoint scene oldstate g = do
        maybenewemissionpoint <- randomSampleFrom (EmissionPointSampler scene) g
        if (isNothing maybenewemissionpoint)
          then return Nothing
          else do
            let newemissionpoint = fromJust maybenewemissionpoint
                newpath = newemissionpoint : tail oldpath
            return . Just $ mltStateSubstitutePath oldstate newpath
      where oldpath = mltStatePath oldstate

    acceptanceProbabilityOf (NewEmissionPoint) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) translationInvariant oldstate newstate
      where
        newEmissionPointTransitionProbability :: Scene -> MLTState -> MLTState -> Double
        newEmissionPointTransitionProbability scene _ newstate =
          let newemissionpoint = head $ mltStatePath newstate
          in sampleProbabilityOf (EmissionPointSampler scene) (Just newemissionpoint)

  data ExponentialScatteringNodeTranslation = ExponentialScatteringNodeTranslation Double
  instance Show ExponentialScatteringNodeTranslation where
    show (ExponentialScatteringNodeTranslation lambda) = "ExponentialScatteringNodeTranslation(" ++ (show lambda) ++ ")"
  instance Mutating ExponentialScatteringNodeTranslation where
    mutateWith (ExponentialScatteringNodeTranslation l) scene oldstate g
      | nodecount<=2 = return Nothing
      | otherwise = do
          rndindex <- randomIntInRange (1,nodecount-2) g
          rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
          let newpath = mapAt rndindex (+rndtranslation) oldpath
          return . Just $ mltStateSubstitutePath oldstate newpath
      where nodecount = pathNodeCount oldpath
            oldpath   = mltStatePath oldstate

    acceptanceProbabilityOf (ExponentialScatteringNodeTranslation l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) translationInvariant oldstate newstate


  data ExponentialImageNodeTranslation = ExponentialImageNodeTranslation Double
  instance Show ExponentialImageNodeTranslation where
    show (ExponentialImageNodeTranslation lambda) = "ExponentialImageNodeTranslation(" ++ (show lambda) ++ ")"
  instance Mutating ExponentialImageNodeTranslation where
    mutateWith (ExponentialImageNodeTranslation l) scene oldstate g = do
        rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
        let newsensorpoint = (last pathtail) + rndtranslation
            newesensorpoint = EPoint . SensationPoint $ newsensorpoint
            dummy = if nodecount==2 then Emi else Sca
        maybenewreversetail <- randomSampleFrom (RecursivePathSampler (scene,newesensorpoint,undefined,SamplePlan [dummy])) g
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
                recsampler = RecursivePathSampler (scene,newesensorpoint,undefined,SamplePlan [dummy])
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


  data RandomPathLength = RandomPathLength Double
  instance Show RandomPathLength where
    show (RandomPathLength l) = "RandomPathLength(" ++ (show l) ++ ")"
  instance Mutating RandomPathLength where
    mutateWith (RandomPathLength l) scene oldstate g = do
        geomdistsample <- randomSampleFrom geomdist g
        let newpathlength = 1 + geomdistsample
            simplepathsampler = RaytracingPathSampler (scene,newpathlength)
        mnewpath <- randomSampleFrom simplepathsampler g
        if (isNothing mnewpath)
          then return Nothing
          else return . Just $ mltStateSubstitutePath oldstate (fromJust mnewpath)
      where geomdist = geometricDistributionFromMean (l-1)

    acceptanceProbabilityOf (RandomPathLength l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) t oldstate newstate where
        t _ newstate = pathlengthprob * simplepathprob where
          simplepathprob = sampleProbabilityOf simplepathsampler (Just newpath)
          simplepathsampler = RaytracingPathSampler (scene,newpathlength)
          pathlengthprob = sampleProbabilityOf geomdist geomdistsample
          geomdistsample = newpathlength - 1
          geomdist = geometricDistributionFromMean (l-1)
          newpathlength = pathLength newpath
          newpath = mltStatePath newstate        
          
        
      

  data IncDecPathLength = IncDecPathLength Double
  instance Show IncDecPathLength where
    show (IncDecPathLength lambda) = "IncDecPathLength(" ++ (show lambda) ++ ")"
  instance Mutating IncDecPathLength where
    mutateWith (IncDecPathLength l) scene oldstate g = do
      u <- uniform g
      let coinflip = u::Bool
          oldpath = mltStatePath oldstate
          (eminode,scatternodes,sensnode) = trisect oldpath
          scatternodecount = length scatternodes
      if coinflip
        then if scatternodecount==0 -- add scatter node
          then do -- sample new scatterpoint independently
            maybenewnode <- randomSampleFrom (ScatteringPointSampler scene) g
            if (isNothing maybenewnode)
              then return Nothing
              else do
                let newnode = fromJust maybenewnode
                    newpath = [eminode,newnode,sensnode]
                return . Just $ mltStateSubstitutePath oldstate newpath
          else do -- sample new scatterpoint by jittered bisection
            rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
            addindex <- randomIntInRange (0,scatternodecount) g
            let (prelist,postlist) = splitAt addindex scatternodes
                anchor
                  | addindex==0                = head postlist
                  | addindex==scatternodecount = last  prelist
                  | otherwise              = let prenode = last prelist
                                                 postnode = head postlist
                                             in 0.5*<>(prenode + postnode)
                newnode = anchor + rndtranslation
                newpath = [eminode] ++ prelist ++ [newnode] ++ postlist ++ [sensnode]
            return . Just $ mltStateSubstitutePath oldstate newpath
        else if scatternodecount > 0
          then do -- delete scatter node
            delindex <- randomListIndex scatternodes g
            let (prelist,postlist) = splitAt delindex scatternodes
                newpath = [eminode] ++ prelist ++ (tail postlist) ++ [sensnode]
            return . Just $ mltStateSubstitutePath oldstate newpath
          else
            return Nothing

    acceptanceProbabilityOf (IncDecPathLength l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene)
                                  (incDecPathLengthTransitionProbability scene l)
                                  oldstate
                                  newstate
      where
        incDecPathLengthTransitionProbability :: Scene -> Double -> MLTState -> MLTState -> Double
        incDecPathLengthTransitionProbability scene lambda oldstate newstate =
          let oldpath = mltStatePath oldstate
              newpath = mltStatePath newstate
              (_,oldscatternodes,_) = trisect oldpath
              (_,newscatternodes,_) = trisect newpath
              oldscatternodecount = length oldscatternodes
              newscatternodecount = length newscatternodes
          in 0.5 * if newscatternodecount > oldscatternodecount
            then let
                newnodeindex = fromMaybe oldscatternodecount .
                               L.findIndex (uncurry (/=)) $ zip oldscatternodes newscatternodes
                newnode = newscatternodes!!newnodeindex                
              in if oldscatternodecount==0-- added node
                then -- new scatterpoint sampled independently
                  sampleProbabilityOf (ScatteringPointSampler scene) (Just newnode)
                else let -- new scatterpoint sampled by jittered bisection
                    anchor
                      | newnodeindex==0                   = head oldpath
                      | newnodeindex==oldscatternodecount = last oldpath
                      | otherwise                   = 0.5*<>(sum . take 2 $ drop (newnodeindex-1) oldscatternodes)
                    exp3dprob = sampleProbabilityOf (Exponential3DPointSampler lambda) (newnode - anchor)
                  in exp3dprob / (fromIntegral newscatternodecount)
            else -- deleted node
              1 / max 1 (fromIntegral oldscatternodecount)
        