module PIRaTE.MCMC where
  import Data.Array.Vector (singletonU)
  import Data.Maybe
  import Control.Monad.ST (ST,runST)
  import Statistics.RandomVariate (Gen,Seed,initialize,save,restore,uniform)
  import PIRaTE.SpatialTypes
  import PIRaTE.RandomSample (randomWeightedChoice)
  import PIRaTE.Scene (Scene)
  import PIRaTE.Path (randomPathOfLength)
  import PIRaTE.Mutation

  import Debug.Trace
  import Text.Printf
  --
  -- metropolis stuff
  --
  
  type MutationList = [(Mutation,Double)]
  
  metropolisStep :: Scene -> MutationList -> MLTState -> Gen s -> ST s MLTState
  metropolisStep scene mutations oldstate g = do
    mutation <- randomWeightedChoice mutations g
    maybenewstate <- mutateWith mutation scene oldstate g
    if (isNothing maybenewstate)
      then metropolisStep scene mutations oldstate g
      else do let newstate = fromJust maybenewstate
                  accprob = --trace =<< (printf "%f") $ 
                            acceptanceProbabilityOf mutation scene oldstate newstate
              if accprob==0
                then return oldstate
                else do rnd <- uniform g
                        if rnd<=accprob
                          then return newstate
                          else return oldstate

  nMLTSteps :: Scene -> MutationList -> Int -> MLTState -> Gen s -> ST s [MLTState]
  nMLTSteps scene mutations n initialstate g
    | n==1      = return [initialstate]
    | otherwise = do
        newpath <- metropolisStep scene mutations initialstate g
        futureresults <- nMLTSteps scene mutations (n-1) newpath g
        return (initialstate:futureresults)

  mltActionStep :: Scene -> MutationList -> (MLTState -> a) -> Int -> (Seed,MLTState) -> ((Seed,MLTState),[a])
  mltActionStep scene mutations extractor n (seed,initialpath) = runST $ do
    g <- restore seed
    states <- nMLTSteps scene
                        mutations
                        n
                        initialpath
                        g
    g' <- save g
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
  