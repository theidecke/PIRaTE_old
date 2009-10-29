module PIRaTE.UtilityFunctions where
  import Data.Vector
  
  -- some utility functions
  normsq v = v `vdot` v
  normalize v = (1/vmag v) *<> v
  
  -- applies f only at the kth element of the list and let's the rest untouched
  mapAt :: Int -> (a->a) -> [a] -> [a]
  mapAt _ _ [] = []
  mapAt k f (x:xs)
    | k==0      = f x : xs
    | otherwise = x : mapAt (k-1) f xs
    