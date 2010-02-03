{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}

module PIRaTE.Mutation where
  import Data.Vector (Vector3(..),(|*),vmag)
  import Data.Maybe (fromMaybe,isNothing,fromJust,listToMaybe)
  import qualified Data.List as L (findIndex)
  import Statistics.RandomVariate (Gen,uniform)
  import Control.Monad (liftM)
  import Control.Monad.ST (ST)
  import PIRaTE.SpatialTypes
  import PIRaTE.UtilityFunctions (mapAt,edgeMap,efficientProduct)
  import PIRaTE.Sampleable
  import PIRaTE.RandomSample
  import PIRaTE.Scene
  import PIRaTE.Path
  
  import Debug.Trace
  
  -- the standard (possibly wasteful) way to compute the acceptance probability
  -- f x should return the probability density for state x
  -- t x y should return the transition probability from state x to state y
  defautAcceptanceProbability :: (a -> Double) -> (a -> a -> Double) -> a -> a -> Double
  defautAcceptanceProbability f t oldstate newstate
    | any (==0) [a,c] = 0
    | otherwise = (a*c)/(b*d)
    where a = f newstate
          b = f oldstate
          c = t newstate oldstate
          d = t oldstate newstate
  
  {--defautAcceptanceProbability :: (a -> Double) -> (a -> a -> Double) -> a -> a -> Double
  defautAcceptanceProbability f t oldstate newstate =
    (/) ((f newstate) * (t newstate oldstate))
        ((f oldstate) * (t oldstate newstate))--}
        
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
      | nodecount<=3 = return Nothing
      | otherwise = do
          rndindex <- randomIntInRange (1,nodecount-3) g
          rndtranslation <- randomSampleFrom (Exponential3DPointSampler l) g
          let newpath = mapAt rndindex (+rndtranslation) oldpath
          return . Just $ mltStateSubstitutePath oldstate newpath
      where nodecount = pathNodeCount oldpath
            oldpath   = mltStatePath oldstate

    acceptanceProbabilityOf (ExponentialScatteringNodeTranslation l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) translationInvariant oldstate newstate


  --TODO: instead of a randomtranslation that may end up in the void, use a UniformAttenuation2DistanceSampler to get the new sensationpoint
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

  data SimpleRandomPathLength = SimpleRandomPathLength Double
  instance Show SimpleRandomPathLength where
    show (SimpleRandomPathLength l) = "SimpleRandomPathLength(" ++ (show l) ++ ")"
  instance Mutating SimpleRandomPathLength where
    mutateWith (SimpleRandomPathLength l) scene oldstate g = do
        geomdistsample <- randomSampleFrom geomdist g
        let newpathlength = 1 + geomdistsample
            simplepathsampler = SimplePathSampler (scene,newpathlength)
        mnewpath <- randomSampleFrom simplepathsampler g
        if (isNothing mnewpath)
          then return Nothing
          else return . Just $ mltStateSubstitutePath oldstate (fromJust mnewpath)
      where geomdist = geometricDistributionFromMean (l-1)

    acceptanceProbabilityOf (SimpleRandomPathLength l) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) t oldstate newstate where
        t _ newstate = pathlengthprob * simplepathprob where
          simplepathprob = sampleProbabilityOf simplepathsampler (Just newpath)
          simplepathsampler = SimplePathSampler (scene,newpathlength)
          pathlengthprob = sampleProbabilityOf geomdist geomdistsample
          geomdistsample = newpathlength - 1
          geomdist = geometricDistributionFromMean (l-1)
          newpathlength = pathLength newpath
          newpath = mltStatePath newstate


  data RaytracingRandomPathLength = RaytracingRandomPathLength Double
  instance Show RaytracingRandomPathLength where
    show (RaytracingRandomPathLength ns) = "RaytracingRandomPathLength(" ++ (show ns) ++ ")"
  instance Mutating RaytracingRandomPathLength where
    mutateWith (RaytracingRandomPathLength ns) scene oldstate g = do
        geomdistsample <- randomSampleFrom geomdist g
        let newns = geomdistsample
            simplepathsampler = RaytracingPathSampler (scene,newns)
        mnewpath <- randomSampleFrom simplepathsampler g
        if (isNothing mnewpath)
          then return Nothing
          else return . Just $ mltStateSubstitutePath oldstate (fromJust mnewpath)
      where geomdist = geometricDistributionFromMean ns

    acceptanceProbabilityOf (RaytracingRandomPathLength ns) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) t oldstate newstate where
        t _ newstate = pathlengthprob * simplepathprob where
          simplepathprob = sampleProbabilityOf simplepathsampler (Just newpath)
          simplepathsampler = RaytracingPathSampler (scene,newns)
          pathlengthprob = sampleProbabilityOf geomdist geomdistsample
          geomdistsample = newns
          geomdist = geometricDistributionFromMean ns
          newns = (pathNodeCount newpath) - 2
          newpath = mltStatePath newstate

  data SimpleBidirRandomPathLength = SimpleBidirRandomPathLength Double
  instance Show SimpleBidirRandomPathLength where
    show (SimpleBidirRandomPathLength ns) = "SimpleBidirRandomPathLength(" ++ (show ns) ++ ")"
  instance Mutating SimpleBidirRandomPathLength where
    mutateWith (SimpleBidirRandomPathLength ns) scene oldstate g = do
        newns <- randomSampleFrom geomdist g
        let simplepathsampler = SimpleBidirPathSampler (scene,newns)
        mnewpath <- randomSampleFrom simplepathsampler g
        if (isNothing mnewpath)
          then return Nothing
          else return . Just $ mltStateSubstitutePath oldstate (fromJust mnewpath)
      where geomdist = geometricDistributionFromMean ns

    acceptanceProbabilityOf (SimpleBidirRandomPathLength ns) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) t oldstate newstate where
        t _ newstate = pathlengthprob * simplepathprob where
          simplepathprob = sampleProbabilityOf simplepathsampler (Just newpath)
          simplepathsampler = SimpleBidirPathSampler (scene,newns)
          pathlengthprob = sampleProbabilityOf geomdist newns
          geomdist = geometricDistributionFromMean ns
          newns = (pathNodeCount newpath) - 2
          newpath = mltStatePath newstate

  data BidirPathSub = BidirPathSub Double
  instance Show BidirPathSub where
    show (BidirPathSub mu) = "BidirPathSub:("++show mu++")"
  instance Mutating BidirPathSub where
    mutateWith (BidirPathSub mu) scene oldstate g = do
        let rijsdist = RIJSDist (n,mu)
        (r,i,j,s) <- randomSampleFrom rijsdist g
        let lightstartpath  = take r oldpath
            sensorstartpath = take s (reverse oldpath)
            n' = r + i + j + s
            newpathplan = planFromNodeCount n'
            lightstartepoint  = fmap (flip typifyPoint (last  lightstartpath)) $ listToMaybe (reverse $ take r newpathplan)
            sensorstartepoint = fmap (flip typifyPoint (last sensorstartpath)) $ listToMaybe (reverse $ take s (reverse newpathplan))
            lightstartwin  |     r < 2 = undefined
                           | otherwise = last . edgeMap (\u v -> fromEdge (v-u)) $ lightstartpath
            sensorstartwin |     s < 2 = undefined
                           | otherwise = last . edgeMap (\u v -> fromEdge (v-u)) $ sensorstartpath
            lightstarteray  = fmap (\ep -> (ep, lightstartwin)) lightstartepoint
            sensorstarteray = fmap (\ep -> (ep,sensorstartwin)) sensorstartepoint
            lightsubpathplan  = take i . drop r $ newpathplan
            sensorsubpathplan = take j . drop s . reverse $ newpathplan
            lightsubpathsampler  = --trace ("  n="++show n++" n'="++show n'++"  rijs="++show (r,i,j,s)) $
                                   RecursivePathSampler2 (scene, lightstarteray,SamplePlan  lightsubpathplan)
            sensorsubpathsampler = RecursivePathSampler2 (scene,sensorstarteray,SamplePlan sensorsubpathplan)
        mlightsubpath <- randomSampleFrom lightsubpathsampler g
        if (isNothing mlightsubpath)
          then return Nothing
          else do
            msensorsubpath <- randomSampleFrom sensorsubpathsampler g
            if (isNothing msensorsubpath)
              then return Nothing
              else do
                let lightsubpath  = fromJust (mlightsubpath::(Maybe Path))
                    sensorsubpath = fromJust (msensorsubpath::(Maybe Path))
                    newpath = lightstartpath ++ lightsubpath ++ (reverse sensorsubpath) ++ (reverse sensorstartpath)
                return . Just $ mltStateSubstitutePath oldstate newpath
      where n = pathNodeCount oldpath
            oldpath = mltStatePath oldstate

    acceptanceProbabilityOf (BidirPathSub mu) scene oldstate newstate =
      defautAcceptanceProbability (measurementContribution scene) (bidirPathSubTransProb scene mu) oldstate newstate
  
  bidirPathSubTransProb scene meandeletednodes oldstate newstate
    | unchangedpath = --mytrace $ 
                      sum $ map getRIJSProb rijss
    | otherwise     = --mytrace $ 
                      sum [efficientProduct [getRIJSProb rijs, getSamplingProb rijs] | rijs<-rijss]
    where
    mytrace = trace ("  n="++show n++" n'="++show n'++" (r',s')="++show (r',s')++"\n  rijss="++show rijss)
    getSamplingProb (r,i,j,s) = lightsubpathprob * sensorsubpathprob where
      lightsubpathprob  = sampleProbabilityOf  lightsubpathsampler (Just  lightsubpath)
      sensorsubpathprob = sampleProbabilityOf sensorsubpathsampler (Just sensorsubpath)
      lightsubpathsampler  = RecursivePathSampler2 (scene, lightstarteray,SamplePlan  lightsubpathplan)
      sensorsubpathsampler = RecursivePathSampler2 (scene,sensorstarteray,SamplePlan sensorsubpathplan)
      lightsubpath  = take i . drop r $ newpath
      sensorsubpath = take j . drop s . reverse $ newpath
      lightsubpathplan  = take i . drop r $ newpathplan
      sensorsubpathplan = take j . drop s . reverse $ newpathplan
      lightstarteray  = fmap (\ep -> (ep, lightstartwin)) lightstartepoint
      sensorstarteray = fmap (\ep -> (ep,sensorstartwin)) sensorstartepoint
      lightstartepoint  = fmap (flip typifyPoint (last lstartpath)) $ listToMaybe (reverse $ take r newpathplan)
      sensorstartepoint = fmap (flip typifyPoint (last sstartpath)) $ listToMaybe (reverse $ take s (reverse newpathplan))
      lightstartwin  = getStartWin r lstartpath
      sensorstartwin = getStartWin s sstartpath
      getStartWin rs startpath |    rs < 2 = undefined
                               | otherwise = last . edgeMap (\u v -> fromEdge (v-u)) $ startpath
      lstartpath = take r newpath
      sstartpath = take s $ reverse newpath
      newpathplan = planFromNodeCount n'
    getRIJSProb = sampleProbabilityOf (RIJSDist (n,meandeletednodes))
    rijss | unchangedpath = [(r,0,0,s) | r<-[1..n-1]   , s<-[n-r]]
          | otherwise     = [(r,i,j,s) | r<-[r']       , s<-[s']
                                       , i<-[0..n'-r-s], j<-[n'-r-s-i]
                                       , i+j >= (length . filter (==0) $ [r,s])]
    unchangedpath = oldpath==newpath --r' == n && s' == n
    r' = length lightstartpath
    s' = length sensorstartpath
    lightstartpath  = map fst . takeWhile (uncurry (==)) $ zip oldpath newpath
    sensorstartpath = map fst . takeWhile (uncurry (==)) $ zip (reverse oldpath) (reverse newpath)
    n  = pathNodeCount oldpath
    n' = pathNodeCount newpath
    oldpath = mltStatePath oldstate
    newpath = mltStatePath newstate
  --(\n n'->[(r,i,j,s)|r<-[0..n],s<-[0..n-r],i<-[0..n'-r-s],j<-[n'-r-s-i],r>0 || i>0,s>0 || j>0]) 3 3
