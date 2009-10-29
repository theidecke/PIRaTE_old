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

  addTexturedDoubles :: Texture Double -> Texture Double -> Texture Double
  addTexturedDoubles    (Homogenous x)    (Homogenous y) = Homogenous (x+y)
  addTexturedDoubles    (Homogenous x) (Inhomogenous  f) = Inhomogenous (\p -> x + f p )
  addTexturedDoubles (Inhomogenous  f)    (Homogenous x) = Inhomogenous (\p -> x + f p )
  addTexturedDoubles (Inhomogenous f1) (Inhomogenous f2) = Inhomogenous (\p -> f1 p + f2 p)
  {-# INLINE addTexturedDoubles #-}

  instance Monoid (Texture Double) where
    mempty = Homogenous 0
    mappend = addTexturedDoubles

  {--
  weighPhaseFunctions :: [(Double,PhaseFunction)] -> PhaseFunction
  weighPhaseFunctions phiswithweights' = let
      phiswithweights = filter ((>0).fst) phiswithweights'
      (isophiswithweights,aniphiswithweights) = L.partition (isIsotropic.snd) phiswithweights
      aniweightsum = sum $ map fst aniphiswithweights
    in if aniweightsum==0
         then Isotropic
         else let isoweightsum = sum $ map fst isophiswithweights
                  normweight = 1 / (isoweightsum + aniweightsum)
              in Anisotropic (\win wout -> normweight*(
                      isoweightsum/(4*pi) +
                      aniweightsum * sum (map ((\phi -> scatterPDF phi win wout).snd) aniphiswithweights)
                    )
                  )

  --}
  addTexturedPhaseFunctions :: [(Texture Double, Texture PhaseFunction)] -> Texture PhaseFunction
  addTexturedPhaseFunctions sigmaphitexturepairs = 
    let (sigmatexts,phitexts) = unzip sigmaphitexturepairs
        (homsigmas,inhomsigmas) = L.partition isHomogenous sigmatexts
        (  homphis,  inhomphis) = L.partition isHomogenous phitexts
        nohomsigmas   = null homsigmas
        noinhomsigmas = null inhomsigmas
        nohomphis     = null homphis
        noinhomphis   = null inhomphis
        weighPhaseFunctions :: [(Double,PhaseFunction)] -> WeightedPhaseFunction
        weighPhaseFunctions phiswithweights = mconcat $ map WeightedPhaseFunction phiswithweights
    in if noinhomsigmas && noinhomphis
        then let sigmas = map (`evaluatedAt` undefined) homsigmas
                 phis   = map (`evaluatedAt` undefined) homphis
             in Homogenous . toPhaseFunction $ weighPhaseFunctions $ zip sigmas phis
        else let sigmasat p = map (`evaluatedAt` p) sigmatexts
                 phisat   p = map (`evaluatedAt` p) phitexts
             in Inhomogenous (\p -> toPhaseFunction . weighPhaseFunctions $ zip (sigmasat p) (phisat p))
  