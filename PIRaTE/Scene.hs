module PIRaTE.Scene where
  import Data.Vector ((*<>),vmag)
  import Data.Monoid
  import qualified Data.List as L
  import qualified Data.Set as S
  import PIRaTE.SpatialTypes
  import PIRaTE.Confineable
  import PIRaTE.Container
  import PIRaTE.PhaseFunction
  import PIRaTE.Texture
  import PIRaTE.Material
  
  -- an Entity is a container filled with light-influencing material
  data Entity = Entity {
      entityContainer::Container,
      entityMaterials::[Material]
    }
    
  instance Show Entity where
    show (Entity container materials) = "Entity contained by a " ++ (show container) ++
                                        " filled with " ++ (show materials)
  
  isLightsource :: Entity -> Bool
  isLightsource (Entity _ materials) = any isEmitting materials
  {-# INLINE isLightsource #-}
  
  isInteractor :: Entity -> Bool
  isInteractor (Entity _ materials) = any isInteracting materials
  {-# INLINE isInteractor #-}
  
  isSensor :: Entity -> Bool
  isSensor (Entity _ materials) = any isSensing materials
  {-# INLINE isSensor #-}
  
  summedMaterialAt :: [Entity] -> Point -> Material
  summedMaterialAt entities point = boiledMaterial
    where
      relevantEntities = filter ((`contains` point).entityContainer) entities
      boiledMaterial = mconcat . concatMap entityMaterials $ relevantEntities
  {-# INLINE summedMaterialAt #-}

  propertyAt :: (Material->Texture a) -> [Entity] -> Point -> a
  propertyAt getpropfrom entities point = getpropfrom summat `evaluatedAt` point
    where summat = summedMaterialAt entities point
  {-# INLINE propertyAt #-}
    
  absorptionAt :: [Entity] -> Point -> Double
  absorptionAt = propertyAt materialAbsorption
  {-# INLINE absorptionAt #-}
    
  scatteringAt :: [Entity] -> Point -> Double
  scatteringAt = propertyAt materialScattering
  {-# INLINE scatteringAt #-}

  extinctionAt :: [Entity] -> Point -> Double
  extinctionAt = propertyAt materialExtinction
  {-# INLINE extinctionAt #-}

  emissivityAt :: [Entity] -> Point -> Double
  emissivityAt = propertyAt materialEmissivity
  {-# INLINE emissivityAt #-}

  sensitivityAt :: [Entity] -> Point -> Double
  sensitivityAt = propertyAt materialSensitivity
  {-# INLINE sensitivityAt #-}

  scatteringPhaseFunctionAt :: [Entity] -> Point -> WeightedPhaseFunction
  scatteringPhaseFunctionAt = propertyAt materialScatteringPhaseFunction
  {-# INLINE scatteringPhaseFunctionAt #-}

  emissionDirectednessAt :: [Entity] -> Point -> WeightedPhaseFunction
  emissionDirectednessAt = propertyAt materialEmissionDirectedness
  {-# INLINE emissionDirectednessAt #-}

  sensationDirectednessAt :: [Entity] -> Point -> WeightedPhaseFunction
  sensationDirectednessAt = propertyAt materialSensationDirectedness
  {-# INLINE sensationDirectednessAt #-}

  -- a Scene contains all entities
  data Scene = Scene {
      sceneEntities::[Entity]
    }
    
  -- the Bool is used to represent if the IntervalLimiter is the begin of an interval
  data IntervalLimiter a = IntervalLimiter {
      intervalLimiterKey::a,
      isIntervalLimiterBegin::Bool,
      intervalLimiterPosition::Double
    }
  instance (Show a) => Show (IntervalLimiter a) where
    show (IntervalLimiter key isbegin pos) =
      (if isbegin then "Begin(" else "End(") ++ (show key) ++ "@" ++ (show pos) ++ ")"
  -- we use the equality just in terms of position (for Ord)
  instance Eq (IntervalLimiter a) where
    (==) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1==pos2
    {-# INLINE (==) #-}
  -- we order IntervalLimiters by position
  instance Ord (IntervalLimiter a) where
    (<=) (IntervalLimiter _ _ pos1) (IntervalLimiter _ _ pos2) = pos1<=pos2
    {-# INLINE (<=) #-}
    
  
  fromInterval :: Interval -> a -> [IntervalLimiter a]
  fromInterval (start,end) key = [IntervalLimiter key  True start,
                                  IntervalLimiter key False   end]
  {-# INLINE fromInterval #-}
                                  
  -- transforms a list of tagged intervals possibly overlapping and with coinciding
  -- endpoints into a list of disjoint intervals with taglist
  cutOverlaps :: (Ord a) => [IntervalLimiter a] -> [(Interval,S.Set a)]
  cutOverlaps limiters = cutOverlaps' S.empty sortedLimiters
    where sortedLimiters = L.sort limiters
          cutOverlaps' :: (Ord a) => (S.Set a) -> [IntervalLimiter a] -> [(Interval, S.Set a)]
          cutOverlaps' active         [] = []
          cutOverlaps' active (l1:l2:ls)
            | l1==l2    = rest
            | otherwise = ((intervalLimiterPosition l1,
                            intervalLimiterPosition l2), newactive):rest
            where getNewActive (IntervalLimiter key isbegin _)
                    | isbegin   = S.insert key active
                    | otherwise = S.delete key active
                  --getNewActive (IntervalLimiter key isbegin _) =
                  --  (if isbegin then S.insert else S.delete) key active
                  newactive = getNewActive l1
                  rest = cutOverlaps' newactive (l2:ls)
          cutOverlaps' active ((IntervalLimiter _ isbegin _):[]) =
            if isbegin then error "last intervallimiter shouldn't be a begin" else []  
  
  {--
  cutoverlapstestcase = concat.map (uncurry fromInterval) $
    zip [(1,5),(1,7),(2,6),(2,5),(3,4),(1,5)] [1,2,3,4,5,6]
  testCutOverlaps = cutOverlaps cutoverlapstestcase == [
      ((1.0,2.0),S.fromList [1,2,6])      ,((2.0,3.0),S.fromList [1,2,3,4,6]),
      ((3.0,4.0),S.fromList [1,2,3,4,5,6]),((4.0,5.0),S.fromList [1,2,3,4,6]),
      ((5.0,6.0),S.fromList [2,3])        ,((6.0,7.0),S.fromList [2])
    ]
    --}


  disjunctIntervalsWithCondensedMaterials :: [Entity] -> Ray -> [(Interval,Material)]
  disjunctIntervalsWithCondensedMaterials entities ray = let
      intervals = [entityContainer entity `intersectedBy` ray | entity<-entities]
      entityindices = [(0::Int)..(length entities -1)]
      nestedindexedintervals = zip intervals entityindices
      indexedintervals = concat [map (\x -> (x, snd ii)) (fst ii) | ii<-nestedindexedintervals]
      taggeddisjointintervals = cutOverlaps.concat $ map (uncurry fromInterval) indexedintervals
      intervalmaterialindices = map (S.toList . snd) taggeddisjointintervals
      entitymateriallists = map entityMaterials entities
      intervalmaterials = [concatMap (entitymateriallists!!) materialindices | materialindices<-intervalmaterialindices]
      condensedintervalmaterials = map mconcat intervalmaterials
      disjointintervals = fst $ unzip taggeddisjointintervals
      refinedintervalswithmaterials = zip disjointintervals condensedintervalmaterials
    in refinedintervalswithmaterials
    

  -- sort out intervals that are before the ray starts or further away than maxDist
  -- and clip intervals that span across these bounds
  clipAndFilterIntervalsWithMaterial :: Double -> [(Interval,Material)] -> [(Interval,Material)]
  clipAndFilterIntervalsWithMaterial maxDist intervalswithmaterials = let
      maxDist' = max 0 maxDist
      outsideOfInterest = uncurry (\x y -> x>=maxDist' || y<=0) . fst
      filterIntervals = filter (not.outsideOfInterest)
      clipInterval (x,y) = (max 0 x, min maxDist' y)
      clipIntervals = map (uncurry (\x y -> (clipInterval x, y)))
    in filterIntervals . clipIntervals $ intervalswithmaterials
  
  
  data ProbeResult = MaxDepthAtDistance Double | MaxDistAtDepth Double deriving (Show)
  getProbeResultDepth (MaxDistAtDepth depth) = depth
  {-# INLINE getProbeResultDepth #-}
  
  consumeIntervals :: (Material -> Texture Double) -> Ray -> Double -> Double -> [(Interval,Material)] -> ProbeResult
  consumeIntervals propertyof ray maxDepth accumulatedDepth [] = MaxDistAtDepth accumulatedDepth
  consumeIntervals propertyof ray maxDepth accumulatedDepth (((a,b), m):rest) = let
      remainingDepth = maxDepth - accumulatedDepth
      intervalLength = b - a
      scalarvalue = propertyof m `evaluatedAt` undefined -- only works for Homogenous Materials
      intervalDepth = scalarvalue * intervalLength
    in if remainingDepth > intervalDepth
         then consumeIntervals propertyof ray maxDepth (accumulatedDepth + intervalDepth) rest
         else let neededDist = remainingDepth / scalarvalue
              in MaxDepthAtDistance (a+neededDist)
  
  -- casts a Ray through a list of entities until either a maximum optical depth
  -- or a maximum distance is reached
  probePropertyOfEntitiesWithRay :: (Material -> Texture Double) -> [Entity] -> Ray -> Double -> Double -> ProbeResult
  probePropertyOfEntitiesWithRay propertyof entities ray maxDist maxDepth = let
      refinedintervalswithtextures = disjunctIntervalsWithCondensedMaterials entities ray
      clippedintervals = clipAndFilterIntervalsWithMaterial maxDist refinedintervalswithtextures
    in consumeIntervals propertyof ray maxDepth 0 clippedintervals
  
  depthOfBetween :: (Material -> Texture Double) -> [Entity] -> Point -> Point -> Double
  depthOfBetween propertyof entities v w = let
      infinity = 1/(0::Double)
      distance = vmag $ w - v
      ray = Ray v ((1/distance)*<>(w-v))
    in getProbeResultDepth $ probePropertyOfEntitiesWithRay propertyof entities ray distance infinity
            
  opticalDepthBetween :: [Entity] -> Point -> Point -> Double
  opticalDepthBetween = depthOfBetween materialExtinction
  {-# INLINE opticalDepthBetween #-}

  