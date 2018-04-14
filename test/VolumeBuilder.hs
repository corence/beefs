
module VolumeBuilder where

import Achikaps
import AchiTask
import AchiVolume
import qualified Data.Map.Strict as Map
import Data.Function((&))
import qualified Interval
import Interval(Interval(..))
import Data.Bifunctor
import qualified RTree
import RTree(RTree)
import qualified Data.Text as Text
import Data.Monoid

bigDouble :: Double
bigDouble = 999999999999

convertAtoB :: [Key] -> [Key] -> (Double, Double) -> (AchiVolume, AchiTask)
convertAtoB sources dests (x, y)
  = (makeUnitVolume prereqs outcomes,
    AchiTask (Text.pack $ "convert " <> show sources <> " to " <> show dests))
       where prereqs
               =  map (\source -> (source, 1)) sources
             outcomes
               =  [(X, x), (Y, y)]
               ++ map (\source -> (source, -1)) sources
               ++ map (\dest -> (dest, 1)) dests

convertInv :: [ItemType] -> [ItemType] -> (Double, Double) -> (AchiVolume, AchiTask)
convertInv sourceTypes destTypes
  = convertAtoB
      (map (Item Inventory) sourceTypes)
      (map (Item Inventory) destTypes)

pickup :: ItemType -> (Double, Double) -> (AchiVolume, AchiTask)
pickup itemType = convertAtoB [Item Floor itemType] [Item Inventory itemType]

addTask :: (AchiVolume, AchiTask) -> Achikaps.Tasks -> Achikaps.Tasks
addTask = RTree.insert

prereqPositive :: Key -> AchiVolume -> AchiVolume
prereqPositive key = prereqRange key 1 bigDouble

availablePositive :: Key -> AchiVolume -> AchiVolume
availablePositive key = availableRange key 1 bigDouble

prereqRange :: Key -> Double -> Double -> AchiVolume -> AchiVolume
prereqRange key low high (AchiVolume p o) = Map.insert key value p & (\p -> AchiVolume p o)
  where value = Interval low high

availableRange :: Key -> Double -> Double -> AchiVolume -> AchiVolume
availableRange key low high (AchiVolume p o) = Map.insert key value o & AchiVolume p
  where value = Interval low high

makeUnitVolume :: [(Key, Double)] -> [(Key, Double)] -> AchiVolume
makeUnitVolume prereqs outcomes = makeVolume (unit prereqs) (unit outcomes)
  where unit = map (second Interval.unit)

makeVolume :: [(Key, Interval Double)] -> [(Key, Interval Double)] -> AchiVolume
makeVolume p o = AchiVolume (Map.fromList p) (Map.fromList o)

startQuery :: AchiVolume
startQuery = AchiVolume.empty

emptyVolume :: AchiVolume
emptyVolume = AchiVolume.empty

addUnitAvailable :: Key -> Double -> AchiVolume -> AchiVolume
addUnitAvailable key value (AchiVolume p o)
  = AchiVolume p (Map.insert key interval o)
  where interval = Interval value value

addUnitPrereq :: Key -> Double -> AchiVolume -> AchiVolume
addUnitPrereq key value (AchiVolume p o)
  = AchiVolume (Map.insert key interval p) o
  where interval = Interval value value

addRangeOutcome :: Key -> Double -> Double -> AchiVolume -> AchiVolume
addRangeOutcome key value0 value1 (AchiVolume p o)
  = AchiVolume p (Map.insert key interval o)
  where interval = Interval value0 value1

addRangePrereq :: Key -> Double -> Double -> AchiVolume -> AchiVolume
addRangePrereq key value0 value1 (AchiVolume p o)
  = AchiVolume (Map.insert key interval p) o
  where interval = Interval value0 value1
