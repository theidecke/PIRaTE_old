{-# LANGUAGE FlexibleInstances #-}

module PIRaTE.Texture where
  import Data.Monoid
  import qualified Data.List as L
  import PIRaTE.SpatialTypes
  import PIRaTE.PhaseFunction
  
    -- a Texture contains the spatial dependency of 'a'
  data Texture a = Empty
                 | Homogenous a
                 | Inhomogenous (Point->a)

  instance (Show a) => Show (Texture a) where
    show            Empty = "Empty"
    show   (Homogenous x) = show x
    show (Inhomogenous _) = "Inhomogenous"

  instance (Eq a) => Eq (Texture a) where
    (==) Empty Empty = True
    (==) (Homogenous x1) (Homogenous x2) = (==) x1 x2
    (==) _ _ = False
    {-# INLINE (==) #-}

  isHomogenous (Homogenous _) = True
  isHomogenous _ = False
  {-# INLINE isHomogenous #-}
  
  isFilled Empty = False
  isFilled     _ = True
  {-# INLINE isFilled #-}

  -- evaluates a texture of type a at a specific point in space
  evaluatedAt :: (Monoid a) => Texture a -> Point -> a
  evaluatedAt            Empty _ = mempty
  evaluatedAt   (Homogenous x) _ = x
  evaluatedAt (Inhomogenous f) p = f p
  {-# INLINE evaluatedAt #-}

  addTexturedMonoid Empty t = t
  addTexturedMonoid t Empty = t
  addTexturedMonoid (Homogenous x) (Homogenous y) = Homogenous $ mappend x y
  addTexturedMonoid (Homogenous x) (Inhomogenous f) = Inhomogenous (\p -> mappend x (f p))
  addTexturedMonoid (Inhomogenous f) (Homogenous x) = Inhomogenous (\p -> mappend (f p) x)
  addTexturedMonoid (Inhomogenous f1) (Inhomogenous f2) = Inhomogenous (\p -> mappend (f1 p) (f2 p))
  {-# INLINE addTexturedMonoid #-}
  
  instance (Monoid a) => Monoid (Texture a) where
    mempty  = Empty
    mappend = addTexturedMonoid
