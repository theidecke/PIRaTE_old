module PIRaTE.MCMC where
  import Data.Array.Vector (singletonU)
  import Data.Maybe
  import Control.Monad.ST (ST,runST)
  import Statistics.RandomVariate (Gen,Seed,initialize,save,restore,uniform)
  import PIRaTE.SpatialTypes
  import PIRaTE.RandomSample (randomWeightedChoice)
  import PIRaTE.Scene (Scene)
  import PIRaTE.Path (randomPathOfLength,measurementContributionHelper,MCFIngredients,AugmentedState)
  import PIRaTE.Mutation

  import Debug.Trace
  import Text.Printf
  --
  -- metropolis stuff
  --
  
  type MutationList = [(Mutation,Double)]
  
  metropolisStep :: Scene -> MutationList -> AugmentedState -> Gen s -> ST s AugmentedState
  metropolisStep scene mutations old@(oldstate,oldingredients) g = do
    mutation <- randomWeightedChoice mutations g
    maybenewstate <- mutateWith mutation scene oldstate g
    if (isNothing maybenewstate)
      then metropolisStep scene mutations old g
      else do let newstate = fromJust maybenewstate
                  newingredients = measurementContributionHelper scene newstate
                  new = (newstate,newingredients)
                  accprob = --trace =<< (printf "%f") $ 
                            acceptanceProbabilityOf mutation scene old new
              if accprob==0
                then return old
                else do rnd <- uniform g
                        if rnd<=accprob
                          then return new
                          else return old

  nMLTSteps :: Scene -> MutationList -> Int -> AugmentedState -> Gen s -> ST s [AugmentedState]
  nMLTSteps scene mutations n initialaugmentedstate g
    | n==1      = return [initialaugmentedstate]
    | otherwise = do
        newaugmentedstate <- metropolisStep scene mutations initialaugmentedstate g
        futureresults <- nMLTSteps scene mutations (n-1) newaugmentedstate g
        return (initialaugmentedstate:futureresults)

  mltActionStep :: Scene -> MutationList -> (MLTState -> a) -> Int -> (Seed,MLTState) -> ((Seed,MLTState),[a])
  mltActionStep scene mutations extractor n (seed,initialstate) = runST $ do
    g <- restore seed
    let initialstateingredients = measurementContributionHelper scene initialstate
    augmentedstates <- nMLTSteps scene
                                 mutations
                                 n
                                 (initialstate, initialstateingredients)
                                 g
    g' <- save g
    let states = map fst augmentedstates
    return ((g',last states), map extractor states)
  
  mltAction :: Scene -> MutationList -> (MLTState -> a) -> Int -> Int -> Int -> [a]
  mltAction scene mutations extractor seedint n chunksize =
    let ipl = 2 -- initial path length
        (seed,initialpath) = runST $ do
                               g <- initialize $ singletonU $ fromIntegral seedint
                               ip <- randomPathOfLength scene ipl g
                               s <- save g
                               return (s,ip)
        chunks = n `div` chunksize
        step k oldstate
          | k==0      = []
          | otherwise = chunk ++ step (k-1) newstate
                        where (newstate,chunk) = mltActionStep scene mutations extractor chunksize oldstate
    in step chunks (seed,initialpath)
  