{-# LANGUAGE TypeSynonymInstances #-}

module Data.WeighedSet (
    show,
    fromWeightList,
    toWeightList,
    empty,
    mempty,
    mappend,
    mconcat,
    increaseWeightBy,
    increaseWeight,
    weightOf
  ) where
  import qualified Data.Map as M (Map,empty,insertWith',findWithDefault,unionWith,unionsWith,assocs,fromList)
  import Data.Monoid
  
  newtype WeighedSet a = WeighedSet (M.Map a Double)
  unwrap (WeighedSet ws) = ws
  {-# INLINE unwrap #-}
  
  instance (Show a) => Show (WeighedSet a) where
    show (WeighedSet ws) = show ws

  fromWeightList :: (Ord a) => [(a, Double)] -> (WeighedSet a)
  fromWeightList = WeighedSet . M.fromList
  
  toWeightList :: WeighedSet a -> [(a, Double)]
  toWeightList = M.assocs . unwrap
  
  empty = WeighedSet M.empty
  
  instance (Ord a) => Monoid (WeighedSet a) where
    mempty = empty
    mappend ws1 ws2 = WeighedSet $ M.unionWith (+) (unwrap ws1) (unwrap ws2)
    mconcat wss = WeighedSet $ M.unionsWith (+) (map unwrap wss)
    
  increaseWeightBy :: (Ord a) => Double -> (WeighedSet a) -> a -> (WeighedSet a)
  increaseWeightBy inc ws index = WeighedSet $ M.insertWith' (+) index inc (unwrap ws)
  
  increaseWeight :: (Ord a) => (WeighedSet a) -> a -> (WeighedSet a)
  increaseWeight = increaseWeightBy 1
  
  weightOf :: (Ord a) => (WeighedSet a) -> a -> Double
  weightOf ws index = M.findWithDefault 0 index (unwrap ws)
  
  -- mappend (fromWeightList [('a',1.0),('b',13.0),('c',0.5)]) (fromWeightList [('c',1.7),('d',11.0)])
  
  {--incBin :: M.Map Int Int -> Int -> M.Map Int Int
  incBin oldmap index = M.insertWith' (+) index 1 oldmap

  getBinCount :: M.Map Int Int -> Int -> Int
  getBinCount fullbins index = M.findWithDefault 0 index fullbins--}