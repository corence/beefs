
module Zone where

import Data.Function((&))

type Pos = [Double]
type Interval = (Double, Double) -- min and max in a single dimension
data Zone = ZVoid | Zone [Interval]

intervalIncludes :: Double -> Interval -> Bool
intervalIncludes value (mini, maxi) = value >= mini && value <= maxi

extendInterval :: Double -> Interval -> Interval
extendInterval value (mini, maxi) = (min value mini, max value maxi)

intervalsOverlap :: Interval -> Interval -> Bool
intervalsOverlap (a0, a1) (b0, b1)
  | a0 > b1 = False
  | b0 > a1 = False
  | a1 < b0 = False
  | b1 < a0 = False
  | otherwise = True

mergeIntervals :: Interval -> Interval -> Interval
mergeIntervals (a0, a1) (b0, b1) = (min a0 b0, max a1 b1)

contains :: Pos -> Zone -> Bool
contains _ ZVoid = False
contains pos (Zone intervals) = all id (zipWith intervalIncludes pos intervals)

overlaps :: Zone -> Zone -> Bool
overlaps ZVoid _ = False
overlaps _ ZVoid = False
overlaps (Zone za) (Zone zb) = all id (zipWith intervalsOverlap za zb)

extend :: Pos -> Zone -> Zone
extend pos ZVoid = unitZone pos
extend pos (Zone intervals) = zipWith extendInterval pos intervals & Zone

merge :: Zone -> Zone -> Zone
merge ZVoid zb = zb
merge za ZVoid = za
merge (Zone intervalsA) (Zone intervalsB) = zipWith mergeIntervals intervalsA intervalsB & Zone

unitZone :: Pos -> Zone
unitZone pos = zip pos pos & Zone
