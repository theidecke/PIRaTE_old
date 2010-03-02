module PIRaTE.UtilityFunctions where
  import Data.ACVector
  
  -- some utility functions
  normsq v = v `vdot` v
  normalize v = (1/vmag v) |* v
  
  -- applies f only at the kth element of the list and let's the rest untouched
  mapAt :: Int -> (a->a) -> [a] -> [a]
  mapAt _ _ [] = []
  mapAt k f (x:xs)
    | k==0      = f x : xs
    | otherwise = x : mapAt (k-1) f xs
  
  edgeMap :: (a->a->b) -> [a] -> [b]
  edgeMap f     (a:[]) = []
  edgeMap f (a:b:rest) = f a b : edgeMap f (b:rest)

  efficientProduct :: [Double] -> Double
  efficientProduct factors
    | any (==0) factors = 0
    | otherwise         = product factors

  infinity = 1/(0::Double)