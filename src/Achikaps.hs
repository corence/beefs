
module Achikaps where

import Debug.Trace

import AchiTask(AchiTask)
import AchiVolume
import qualified Interval
import Interval(Interval(..))
import RTree(RTree)
import qualified Data.Map.Merge.Strict as Merge
import qualified RTree

-- task: "i want to burn food at the Altar." (x: altar.x, y: altar.y, food: -1, divine favour: +1)
-- task: "haul food to the altar." (x: altar.x, y: altar.y. Relative priorities inherited from the above)
-- task: "convert meat into food at any Kitchen." (meat: -1, food: +1) (how do we mark meat as a non-negative resource? I guess we could just never match against it, but it seems flakey and wasteful... Maybe it's a new Interval type? (IntInterval + CountingInterval))
-- task: "convert meat into food at this specific Kitchen." (x: altar.x, y: altar.y, relatives inherited from the above)
-- task: "forage for berries." (food: +1)
-- task: "forage for berries at this berry bush out in woop woop." (x: bush.x, y: bush.y, inherited)
-- task: "create a new kitchen." (food +.1, civic +1, urgency -1, votes +1)

type Tasks = RTree AchiVolume AchiTask

chooseTask :: AchiVolume -> Tasks -> [(AchiVolume, AchiTask)]
chooseTask query tasks
  = RTree.lookup query tasks

{-
chooseTask :: AchiVolume -> Tasks -> [(AchiVolume, AchiTask)]
chooseTask query tasks
  = trace ("chooseTask " ++ show query) $ if null discoveredTasks
      then case RTree.rBoundary tasks of
        Just boundary -> chooseTask (halfwayToward boundary query) tasks
        Nothing -> []
      else discoveredTasks
  where discoveredTasks = RTree.lookup query tasks

halfwayToward :: AchiVolume -> AchiVolume -> AchiVolume
halfwayToward (AchiVolume pDest aDest) (AchiVolume p a)
  = AchiVolume (toward pDest p) (toward aDest a)
  where toward
          = Merge.merge
            Merge.dropMissing
            Merge.preserveMissing
            (Merge.zipWithMatched (const averageIntervals))
        averageIntervals (Interval a b) (Interval x y)
          = Interval (mean [a, x]) (mean [b, y])

mean :: [Double] -> Double
mean inputs = sum inputs / fromIntegral (length inputs)
-}
