{-# LANGUAGE FlexibleInstances #-}

module PIRaTE.Texture where
  import Data.Monoid
  import qualified Data.List as L
  import PIRaTE.SpatialTypes
  import PIRaTE.PhaseFunction
  
    -- a Texture contains the spatial dependency of 'a'
  data Texture a = Homogenous a
                 | Inhomogenous (Point->a)

  instance (Show a) => Show (Texture a) where
    show   (Homogenous x) = show x
    show (Inhomogenous _) = "Inhomogenous"

  isHomogenous (Homogenous _) = True
  isHomogenous _ = False
  {-# INLINE isHomogenous #-}

  -- evaluates a texture of type a at a specific point in space
  evaluatedAt :: Texture a -> Point -> a
  evaluatedAt   (Homogenous x) _ = x
  evaluatedAt (Inhomogenous f) p = f p
  {-# INLINE evaluatedAt #-}

  addTexturedMonoid t1 t2
    | isHomogenous t1 && isHomogenous t2 = Homogenous $ mappend (t1 `evaluatedAt` undefined) (t2 `evaluatedAt` undefined)
    | otherwise = Inhomogenous (\p -> mappend (t1 `evaluatedAt` p) (t2 `evaluatedAt` p))
  {-# INLINE addTexturedMonoid #-}
  
  instance (Monoid a) => Monoid (Texture a) where
    mempty = Homogenous mempty
    mappend = addTexturedMonoid
