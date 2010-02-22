{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.EmpiricalDiscreteDistribution (
    Tree,empty,singleton,insert,randomSampleFrom,sampleProbabilityOf,hasNonzeroSamples,toWeightList
  ) where
  import Data.Maybe(fromJust,isJust)
  import System.Random.MWC (uniform)
  import Control.Monad.ST
  import Control.DeepSeq
  import PIRaTE.Sampleable
  
  data Tree a = Empty
              | Branch (Tree a) (Tree a) !a !Double !Integer !Double

  instance (Show a) => Show (Tree a) where
    show Empty = ""
    show (Branch l r p pw pn nw) = "("++show p++" "++show pw++" "++show pn++" "++show nw++" L"++show l++" "++" R"++show r++")"
    --show (Branch l r p pw pn nw) = show l++" "++show p++" "++show pw++" "++show pn++" "++show nw++" "++show r

  toWeightList :: (Tree a) -> [(a, Double)]
  toWeightList Empty = []
  toWeightList (Branch l r p pw _ _) = (p,pw):(toWeightList l ++ toWeightList r)

  instance  (NFData a) => NFData (Tree a)  where
    rnf Empty                   = ()
    rnf (Branch l r p pw pn nw) = rnf (l,r,p,pw,pn,nw)

  empty :: (Ord a) => Tree a
  empty = Empty

  singleton :: (Ord a) => Double -> a -> (Tree a)
  singleton weight item = Branch Empty Empty item weight 1 weight
  
  insert :: (Ord a, Show a) => Double -> a -> (Tree a) -> (Tree a)
  insert weight item tree = fst $ insert' weight item tree
  
  insert' :: (Ord a) => Double -> a -> (Tree a) -> (Tree a, Double)
  insert' weight item Empty = (singleton weight item, weight)
  insert' weight item (Branch l r p pw pn nw) = (updatedbranch,deltanw) where
    (updatedbranch,deltanw) = case itemorder of
      EQ -> (Branch l  r  p pw' pn' pnw, pdeltanw)
      LT -> (Branch l' r  p pw  pn  lnw, ldeltanw)
      GT -> (Branch l  r' p pw  pn  rnw, rdeltanw)
    itemorder = compare item p
    (l',ldeltanw) = insert' weight item l
    (r',rdeltanw) = insert' weight item r
    pw' = pw + pdeltanw
    pnw = nw + pdeltanw
    lnw = nw + ldeltanw
    rnw = nw + rdeltanw
    pdeltanw = (weight - pw) / (fromIntegral $ pn')
    pn' = pn + 1
  
  isEmpty Empty = True
  isEmpty     _ = False
  
  isLeaf (Branch Empty Empty _ _ _ _) = True
  isLeaf                            _ = False
  
  hasNonzeroSamples Empty = False
  hasNonzeroSamples branch = (getNormWeight branch)>0
  
  getPivotWeight Empty = 0
  getPivotWeight (Branch _ _ _ pw _ _) = pw
  
  getNormWeight Empty = 0
  getNormWeight (Branch _ _ _ _ _ nw) = nw
  
  getPivot Empty = undefined
  getPivot (Branch _ _ p _ _ _) = p
  
  instance (Ord a) => Sampleable (Tree a) (Maybe a) where
    randomSampleFrom Empty g = return Nothing
    randomSampleFrom tree@(Branch l r p pw pn nw) g
      | nw==0       = return Nothing
      | isLeaf tree = return (Just p)
      | otherwise   = do
          u <- uniform g
          let x = nw*(u::Double)
              wl = getNormWeight l
              wr = getNormWeight r
          if x<=wl
            then randomSampleFrom l g
            else do
              let rest = x - wl
              if rest<=wr
                then randomSampleFrom r g
                else return (Just p)
    
    sampleProbabilityOf Empty Nothing  = 1
    sampleProbabilityOf Empty (Just _) = 0
    sampleProbabilityOf (Branch _ _ _ _ _ nw) Nothing = if nw==0 then 1 else 0
    sampleProbabilityOf tree@(Branch l r p pw pn nw) ji@(Just i)
      | nw==0       = 0
      | isLeaf tree = if i==p then 1 else 0
      | otherwise   = case itemorder of
          EQ -> pivotprob
          LT -> leftprob
          GT -> rightprob
      where itemorder = compare i p
            pivotprob = pw/nw
            leftprob  = (wl/nw) * (sampleProbabilityOf l ji)
            rightprob = (wr/nw) * (sampleProbabilityOf r ji)
            wl = getNormWeight l
            wr = getNormWeight r
  
  instance (Ord a,Show a) => Sampleable (Tree a) a where
    randomSampleFrom Empty g = return undefined
    randomSampleFrom tree g = do
      ma <- randomSampleFrom tree g
      if (isJust ma)
        then return $ fromJust ma
        else error $ "randomSampleFrom tree failed:"++show tree
      
    sampleProbabilityOf Empty _ = 0
    sampleProbabilityOf tree i = sampleProbabilityOf tree (Just i)
