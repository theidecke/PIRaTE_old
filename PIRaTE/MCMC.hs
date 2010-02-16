module PIRaTE.MCMC where
  import Data.Array.Vector (singletonU)
  import Data.Maybe
  import qualified Data.Map as M
  import Control.Monad.ST (ST,runST)
  import Statistics.RandomVariate (Gen,Seed,initialize,save,restore,uniform)
  import PIRaTE.SpatialTypes
  import Data.EmpiricalDiscreteDistribution
  import PIRaTE.RandomSample (randomWeightedChoice,StandardRIJSDist(..))
  import PIRaTE.Scene (Scene)
  import PIRaTE.Path (randomPathOfLength,measurementContributionHelper,MCFIngredients,AugmentedState)
  import PIRaTE.Mutation

  import Debug.Trace
  import Text.Printf
  --
  -- metropolis stuff
  --
  
  type MutationList = [(Mutation,Double)]
  type MutationMemory = M.Map Int (Tree (Int,Int,Int,Int))
  type StepInfo = (AugmentedState, MutationMemory)
  
  metropolisStep :: Scene -> MutationList -> StepInfo -> Gen s -> ST s StepInfo
  metropolisStep scene mutations (old@(oldstate,oldingredients),mutmem) g = do
    let mutations' = (getEmpiricalBidirPathSub mutmem, 1.0):mutations
    mutation <- randomWeightedChoice mutations' g
    (maybenewstate,mfeedback) <- mutateWith mutation scene oldstate g
    let n = pathNodeCount . mltStatePath $ oldstate
        mfeedback' a = (\x -> (x,a,n)) `fmap` mfeedback
        mutmem'mutfail = updateMutationMemory (mfeedback' 0) mutmem
    if (isNothing maybenewstate)
      then metropolisStep scene mutations (old, mutmem'mutfail) g
      else do let newstate = fromJust maybenewstate
                  newingredients = measurementContributionHelper scene newstate
                  new = (newstate,newingredients)
                  accprob = --trace =<< (((show mutation++"["++show mfeedback++"] : ")++). printf "%f") $ 
                            acceptanceProbabilityOf mutation scene old new
                  mutmem'accprob = updateMutationMemory (mfeedback' accprob) mutmem
              if accprob==0
                then return (old, mutmem'accprob)
                else do rnd <- uniform g
                        if rnd<=accprob
                          then return (new, mutmem'accprob)
                          else return (old, mutmem'accprob)
  
  updateMutationMemory :: (Maybe (MutationFeedback,Double,Int)) -> MutationMemory -> MutationMemory
  --updateMutationMemory _ mutmem = mutmem
  updateMutationMemory Nothing mutmem = mutmem
  updateMutationMemory (Just (SampledRIJS rijs,accprob,n)) mutmem = M.alter updateTree n mutmem where
    updateTree  Nothing = Just $ singleton accprob rijs
    updateTree (Just t) = Just $ insert accprob rijs t

  getEmpiricalBidirPathSub :: MutationMemory -> Mutation
  getEmpiricalBidirPathSub mutmem = Mutation . BidirPathSub $
      \n -> case M.lookup n mutmem of
        Nothing   -> fallbackdist n
        Just tree -> if hasNonzeroSamples tree then RIJSDist $ tree else fallbackdist n
    where fallbackdist n = RIJSDist $ StandardRIJSDist (n,meankd)
          meankd = 4.0
  --lookup :: Ord k => k -> Map k a -> Maybe a
  
  nMLTSteps :: Scene -> MutationList -> Int -> StepInfo -> Gen s -> ST s ([AugmentedState],MutationMemory)
  nMLTSteps scene mutations n stepinfo@(initialaugmentedstate,mutmem) g
    | n==1      = return ([initialaugmentedstate],mutmem)
    | otherwise = do
        newstepinfo <- metropolisStep scene mutations stepinfo g
        (futureresults, mutmem') <- nMLTSteps scene mutations (n-1) newstepinfo g
        return (initialaugmentedstate:futureresults, mutmem')

  mltActionStep :: Scene -> MutationList -> (MLTState -> a) -> Int -> (Seed,MutationMemory,MLTState) -> ((Seed,MutationMemory,MLTState),[a])
  mltActionStep scene mutations extractor n (seed,initialmutmem,initialstate) = runST $ do
    g <- restore seed
    let initialstateingredients = measurementContributionHelper scene initialstate
        initialaugmentedstate = (initialstate, initialstateingredients)
        initialstepinfo = (initialaugmentedstate,initialmutmem)
    (augmentedstates, mutmem') <- nMLTSteps scene
                                            mutations
                                            n
                                            initialstepinfo
                                            g
    g' <- save g
    let states = map fst augmentedstates
    return ((g', mutmem', last states), map extractor states)
  
  mltAction :: Scene -> MutationList -> MutationMemory -> (MLTState -> a) -> Int -> Int -> Int -> ([a],MutationMemory)
  mltAction scene mutations mutmem extractor seedint n chunksize =
    let ipl = 2 -- initial path length
        (seed,initialpath) = runST $ do
                               g <- initialize $ singletonU $ fromIntegral seedint
                               ip <- randomPathOfLength scene ipl g
                               s <- save g
                               return (s, fromPath ip)
        chunks = n `div` chunksize
        step k old@(s,mem,ip)
          | k==0      = ([],mem)
          | otherwise = (chunk ++ futurechunks,futuremutmem) where
              (futurechunks,futuremutmem) = step (k-1) newactionstepinfo
              (newactionstepinfo@(seed',mutmem',initialpath'),chunk) = mltActionStep scene mutations extractor chunksize old
    in step chunks (seed,mutmem,initialpath)
  
