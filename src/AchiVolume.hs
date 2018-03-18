
module AchiVolume where

import Volume
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Interval
import Interval(Interval)
import AchiTask(Key)
import Data.Function((&))

data AchiVolume = AchiVolume {
  prerequisites :: Map Key (Interval Double),
  outcomes :: Map Key (Interval Double)
  }

instance Eq AchiVolume where
  (==) (AchiVolume prereq1 outcomes1) (AchiVolume prereq2 outcomes2)
    = Map.toList prereq1 == Map.toList prereq2
    && Map.toList outcomes1 == Map.toList outcomes2

instance Volume AchiVolume where
  merge (AchiVolume prereq1 outcomes1) (AchiVolume prereq2 outcomes2)
    = AchiVolume
      (Map.unionWith Interval.merge prereq1 prereq2)
      (Map.unionWith Interval.merge outcomes1 outcomes2)
  intersects (AchiVolume queryPrereq queryOutcomes) (AchiVolume prereq outcomes)
     = allRightKeysIntersect queryPrereq (Map.toList prereq)
    && allRightKeysIntersect outcomes (Map.toList queryOutcomes)
    where allRightKeysIntersect lefts = all (isGood lefts)
          isGood lefts (key, interval) = maybe False (Interval.intersects interval) (Map.lookup key lefts)
  {- This implementation is _probably_ less efficient because queryOutcomes and prereq are gonna have few keys.
     It's complicated and hard to read, too!
  intersects (AchiVolume queryPrereq queryOutcomes) (AchiVolume prereq outcomes)
     = allLeftKeysIntersect (Map.toList prereq) (Map.toList queryPrereq)
    && allLeftKeysIntersect (Map.toList queryOutcomes) (Map.toList outcomes)
    where allLeftKeysIntersect [] _ = True
          allLeftKeysIntersect _ [] = False
          allLeftKeysIntersect (l:ls) (r:rs)
            = if lk == rk
                then Interval.intersects li ri && allLeftKeysIntersect ls rs
                else allLeftKeysIntersect (l:ls) rs
            where (lk, li) = l
                  (rk, ri) = r
  -}
  contains (AchiVolume prereq1 outcomes1) (AchiVolume prereq2 outcomes2)
    = mapContains prereq1 prereq2 && mapContains outcomes1 outcomes2
      where mapContains map1 map2
              = let combo = Map.intersectionWith Interval.contains map1 map2 & Map.toList & map snd in
                      all id combo
                      && length combo == Map.size map1
                      && length combo == Map.size map2
