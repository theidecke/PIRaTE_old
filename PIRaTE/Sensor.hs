--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PIRaTE.Sensor where
  import PIRaTE.SpatialTypes
  import qualified Data.WeighedSet as WS
  import PIRaTE.SpatialTypes
  import PIRaTE.PhaseFunction
  import PIRaTE.Sampleable
  import PIRaTE.RandomSample (randomWeightedChoice)
  
  -- the kinds of sensor data we might want to log  
  data SensorResult = Grid2DPhoton (Int,Int) Double
  
  type SensorLogger = MLTState -> SensorResult
  newtype IndexedSensor = IndexedSensor {indexedSensorTripleForm :: (Int,PhaseFunction,SensorLogger)}
  
  indexedSensorLogger :: IndexedSensor -> SensorLogger
  indexedSensorLogger (IndexedSensor (_,_,sl)) = sl
  indexedSensorDirectedness :: IndexedSensor -> PhaseFunction
  indexedSensorDirectedness (IndexedSensor (_,pf,_)) = pf
  
  instance Show IndexedSensor where
    show (IndexedSensor (index,_,_)) = "Sensor " ++ show index
  instance Eq IndexedSensor where
    (==) (IndexedSensor (index1,_,_)) (IndexedSensor (index2,_,_)) = index1==index2
    {-# INLINE (==) #-}
  instance Ord IndexedSensor where
    (<=) (IndexedSensor (index1,_,_)) (IndexedSensor (index2,_,_)) = index1 <= index2
    {-# INLINE (<=) #-}
    
    
  type WeightedSensor = WS.WeighedSet IndexedSensor
  
  instance Sampleable (WeightedSensor,Point) Direction where
    sampleProbabilityOf (wsens,point) wout = let
        step ipf w (tp,tw) = (tp',tw')
          where tp' = tp + (sampleProbabilityOf (pf,inray) wout)
                tw' = tw + w
                pf  = indexedSensorDirectedness ipf
                inray = Ray point undefined
        (totalprob,totalweight) = WS.foldWithKey step (0,0) wsens
      in if totalweight>0
        then totalprob / totalweight
        else 0
    randomSampleFrom     (wsens,point) g = do
      indexedphasefunction <- randomWeightedChoice (WS.toWeightList wsens) g
      let pf = indexedSensorDirectedness indexedphasefunction
          inray = Ray point undefined
      randomSampleFrom (pf,inray) g
    {-# INLINE randomSampleFrom #-}
    

