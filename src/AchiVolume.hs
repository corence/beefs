
module AchiVolume where

import Volume
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.Map.Strict(Map)
import qualified Interval
import Interval(Interval(..))
import AchiTask(Key)
import Data.Function((&))

data AchiVolume = AchiVolume {
  aPrerequisites :: Map Key (Interval Double),
  aAvailables :: Map Key (Interval Double)
  } deriving (Show)

empty :: AchiVolume
empty = AchiVolume Map.empty Map.empty

instance Eq AchiVolume where
  (==) (AchiVolume prereqs1 availables1) (AchiVolume prereqs2 availables2)
    = Map.toList prereqs1 == Map.toList prereqs2
    && Map.toList availables1 == Map.toList availables2

instance Volume AchiVolume where

  -- We need to obey the merge law
  merge (AchiVolume prereqs1 availables1) (AchiVolume prereqs2 availables2)
    = AchiVolume prereqs availables
      where
            prereqs
              = Merge.merge
                Merge.dropMissing
                Merge.dropMissing
                (Merge.zipWithMatched (const Interval.merge))
                prereqs1
                prereqs2
            availables = Map.unionWith Interval.merge availables1 availables2

  extents (AchiVolume prereqs1 availables1) (AchiVolume prereqs2 availables2)
    = AchiVolume prereqs availables
      where
            prereqs = Map.unionWith Interval.merge prereqs1 prereqs2
            availables = Map.unionWith Interval.merge availables1 availables2

  (AchiVolume prereqs1 availables1) `intersects` (AchiVolume prereqs2 availables2)
     = all (intersectsWith availables1) (Map.toList prereqs2)
    && all (intersectsWith availables2) (Map.toList prereqs1)
    where intersectsWith availables (key, interval) = maybe False (Interval.intersects interval) (Map.lookup key availables)

  {- This implementation is _probably_ less efficient because queryOutcomes and prereq are gonna have few keys.
     It's complicated and hard to read, too!
     Oh and it's not up to date with the current behaviour of the method, so don't use it as-is.
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

  contains (AchiVolume prereqs1 availables1) (AchiVolume prereqs2 availables2)
    = mapContains prereqs1 prereqs2 && mapContains availables1 availables2
      where mapContains map1 map2
              = let combo = Map.intersectionWith Interval.contains map1 map2 & Map.toList & map snd in
                      and combo
                      && length combo == Map.size map1
                      && length combo == Map.size map2
