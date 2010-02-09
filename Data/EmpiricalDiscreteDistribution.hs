{-# LANGUAGE MultiParamTypeClasses #-}

module Data.EmpiricalDiscreteDistribution (
    empty,singleton,insert,randomSampleFrom,sampleProbabilityOf
  ) where
  import Statistics.RandomVariate
  import Control.Monad.ST
  import PIRaTE.Sampleable
  
  data Tree a = Empty
              | Branch (Tree a) Double Integer a (Tree a)
              deriving Show

  empty :: (Ord a) => Tree a
  empty = Empty

  singleton :: (Ord a) => Double -> a -> (Tree a)
  singleton weight item = Branch Empty weight 1 item Empty
  
  insert :: (Ord a) => Double -> a -> (Tree a) -> (Tree a)
  insert weight item Empty = singleton weight item
  insert weight item (Branch left tw n pivot right) = case itemorder of
      EQ -> Branch left  tw' n' pivot right
      LT -> Branch left' tw' n' pivot right
      GT -> Branch left  tw' n' pivot right'
    where itemorder = compare item pivot
          tw' = tw + weight
          n' = n + 1
          left'  = insert weight item left
          right' = insert weight item right

  isEmpty Empty = True
  isEmpty     _ = False
  
  isLeaf (Branch Empty _ _ _ Empty) = True
  isLeaf                          _ = False
  
  getWeight Empty = 0
  getWeight (Branch _ w _ _ _) = w
  
  getItem Empty = undefined
  getItem (Branch _ _ _ i _) = i
  

  instance (Ord a) => Sampleable (Tree a) (Maybe a) where
    randomSampleFrom Empty g = return Nothing
    randomSampleFrom tree@(Branch l w n p r) g
      | isLeaf tree = return (Just p)
      | otherwise   = do
          u <- uniform g
          let x = w*(u::Double)
              wl = getWeight l
              wr = getWeight r
          if x<=wl
            then randomSampleFrom l g
            else do
              let rest = x - wl
              if rest<=wr
                then randomSampleFrom r g
                else return (Just p) 
    
    sampleProbabilityOf Empty Nothing  = 1
    sampleProbabilityOf Empty (Just _) = 0
    sampleProbabilityOf (Branch _ _ _ _ _) Nothing = 0
    sampleProbabilityOf tree@(Branch l w n p r) ji@(Just i)
      | isLeaf tree = if i==p then 1 else 0
      | otherwise = case itemorder of
          EQ -> pivotprob
          LT -> leftprob
          GT -> rightprob
      where itemorder = compare i p
            pivotprob = max 0 $ (w-wl-wr)/w
            leftprob  = (wl/w) * (sampleProbabilityOf l ji)
            rightprob = (wr/w) * (sampleProbabilityOf r ji)
            wl = getWeight l
            wr = getWeight r




