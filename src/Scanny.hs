
module Scanny where

import Data.Function((&))
import SimsKeys
import qualified Interval
import Interval(Interval)
import qualified Task
import Task(Task)
import qualified SolutionChain
import SolutionChain(SolutionChain(..), Target)

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map(Map)

type Tasks = [Task]
data Available = Available {
  amount :: Double,
  valuableness :: Double
  }

-- remember. all we need to find is the cheapest task that will bring us progress toward that goal.
-- there is totally a risk of getting stuck in local maxima. we are ignoring that risk.
-- Actually, we can mitigate it if we prioritize large target ranges before smaller ones, imo.
-- so, scan through all tasks until we find the cheapest single task that will improve our standing.
-- we DO need a Closed set though, so that we don't endless loop within this function.

{-
findNextStep :: Tasks -> Target -> SolutionChain -> [SolutionChain]
findNextStep allTasks target@(key, range) chain
  = allTasks
  & filter (yields target)
  & filter (not . alreadyInChain chain)
  & map (makeChain chain)
  where yields target task = undefined
        alreadyInChain chain task = Set.member (SolutionChain.closed chain) (Task.name task)

extend :: [Task] -> Map Key Available -> SolutionChain -> [SolutionChain]
extend allTasks availables successor
  = allTasks
  & filter (yields target)
  & map (makeChain allTasks availables successor)
    where tasks = undefined

makeChain :: [Task] -> Map Key Available -> SolutionChain -> Task -> SolutionChain
makeChain allTasks availables successor task
  = SolutionChain {
      task = task,
      cost = knownCost availables task,
      successor = successor,
      closed = Set.insert (SolutionChain.closed successor) (Task.name task)
    }
-}

extendChain :: [Task] -> Map Key Available -> SolutionChain -> [SolutionChain]
extendChain allTasks availables successor
  -- find all of the keys that need fulfilling in the successor
  = SolutionChain.openPrereqs successor
  -- that was easy.
  -- now, extend for each prereq
  & map (extendOnTarget allTasks availables successor)
  & concat

extendOnTarget :: [Task] -> Map Key Available -> SolutionChain -> Target -> [SolutionChain]
extendOnTarget allTasks availables successor target
  -- find all of the tasks that might fulfill this target
  = allTasks
  & filter (modifiesKey (fst target))

  -- drop the ones that are already in the chain
  & filter (not . alreadyInChain successor)

  -- of these tasks, there are 3 groups:
  --  * already fulfilled
  --  * solvable with fixed known cost
  --  * solvable via further tasks
  & List.partition 

  concatMap (extendWithChainOrDont availables successor target) relevantTasks
  where alreadyInChain chain task = Set.member (Task.name task) (SolutionChain.closed chain)

modifiesKey :: Key -> Task -> Bool
modifiesKey key task
  =  Map.member key (Task.adjustments task)
  || Map.member key (Task.assignments task)
