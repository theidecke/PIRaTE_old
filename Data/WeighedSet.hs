{-# LANGUAGE TypeSynonymInstances #-}

module Data.WeighedSet (
    WeighedSet,
    show,
    fromWeightList,
    toWeightList,
    empty,
    singleton,
    foldWithKey,
    mempty,
    mappend,
    mconcat,
    increaseWeightBy,
    increaseWeight,
    weightOf
  ) where
  import qualified Data.Map as M (Map,empty,insertWith',findWithDefault,unionWith,unionsWith,assocs,fromList,foldWithKey)
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
  
  singleton :: (Ord a) => a -> WeighedSet a
  singleton x = increaseWeight empty x
  
  foldWithKey :: (Ord k) => (k->Double->a->a) -> a -> (WeighedSet k) -> a
  foldWithKey f x0 ws = M.foldWithKey f x0 (unwrap ws)
  
  instance (Ord a) => Monoid (WeighedSet a) where
    mempty = empty
    mappend ws1 ws2 = WeighedSet $ M.unionWith (+) (unwrap ws1) (unwrap ws2)
    mconcat = WeighedSet . M.unionsWith (+) . map unwrap
    
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