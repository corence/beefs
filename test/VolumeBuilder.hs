
module VolumeBuilder where

import AchiTask
import AchiVolume
import qualified Data.Map.Strict as Map
import Data.Function((&))
import qualified Interval
import Interval(Interval(..))
import Data.Bifunctor

bigDouble :: Double
bigDouble = 999999999999

queryPrereqPositive :: Key -> AchiVolume -> AchiVolume
queryPrereqPositive key = queryPrereqRange key 1 bigDouble

queryOutcomePositive :: Key -> AchiVolume -> AchiVolume
queryOutcomePositive key = queryOutcomesRange key 1 bigDouble

queryPrereqRange :: Key -> Double -> Double -> AchiVolume -> AchiVolume
queryPrereqRange key low high (AchiVolume p o) = Map.insert key value p & (\p -> AchiVolume p o)
  where value = Interval low high

queryOutcomesRange :: Key -> Double -> Double -> AchiVolume -> AchiVolume
queryOutcomesRange key low high (AchiVolume p o) = Map.insert key value o & AchiVolume p
  where value = Interval low high

makeUnitVolume :: [(Key, Double)] -> [(Key, Double)] -> AchiVolume
makeUnitVolume prereqs outcomes = makeVolume (unit prereqs) (unit outcomes)
  where unit = map (second Interval.unit)

makeVolume :: [(Key, Interval Double)] -> [(Key, Interval Double)] -> AchiVolume
makeVolume p o = AchiVolume (Map.fromList p) (Map.fromList o)

startQuery :: AchiVolume
startQuery = AchiVolume Map.empty Map.empty
