
import SimsKeys
import qualified Interval
import Interval(Interval)
import qualified Task
import Task(Task)

type Target = (Key, Interval Double)
type Tasks = [Task]

-- remember. all we need to find is the cheapest task that will bring us progress toward that goal.
-- there is totally a risk of getting stuck in local maxima. we are ignoring that risk.
-- Actually, we can mitigate it if we prioritize large target ranges before smaller ones, imo.
-- so, scan through all tasks until we find the cheapest single task that will improve our standing.
-- we DO need a Closed set though, so that we don't endless loop within this function.

findNextStep :: {- WorldState -> -} Tasks -> Target -> SolutionChain -> [SolutionChain]
findNextStep tasks target@(key, range) chain
  = tasks
  & filter (yields target)
  & filter (not . alreadyInChain chain)
  & map (makeChain chain)
  where yields target task = undefined
        alreadyInChain chain task = Set.member (SolutionChain.closed chain) (Task.name task)

makeChain :: SolutionChain -> Task -> SolutionChain
makeChain successor task
  = SolutionChain {
    task = task,
    successor = successor,
    closed = Set.insert (SolutionChain.closed successor) (Task.name task),
    cost = (SolutionChain.cost successor) + calculateCosts task
    }

data SolutionChain = SolutionChain {
  task :: Task,
  successor :: Maybe SolutionChain,
  closed :: Set String, -- names of  the tasks that are already in this chain
  cost :: Double
  }
