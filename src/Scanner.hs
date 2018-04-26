module Scanner where

import Data.Function((&))
import SimsKeys
import qualified Interval
import Interval(Interval)
import qualified Task
import Task(Task, Need)
import qualified SolutionNode
import SolutionNode(SolutionNode(..))
import Data.Maybe(fromMaybe)
import qualified ScanFactors
import ScanFactors(ScanFactors)

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import qualified Data.List as List

findPredecessors :: ScanFactors -> SolutionNode -> [SolutionNode]
findPredecessors factors successor
  = SolutionNode.task successor
  & Task.needs
  & Set.toList
  & map (solveNeed factors)
  & concat

solveNeed :: ScanFactors -> Need -> [SolutionNode]
solveNeed factors need
  = if needCanBeSolvedDirectly factors need
      then [solveNeedDirectly]
      else solveNeedWithTasks
  where solves need task = Set.member need (Task.outcomes task)
        solveNeedWithTasks = ScanFactors.allTasks factors
                           & filter (solves need)
                           & map (solveNeedWithTask need)
        solveNeedDirectly = DirectNode (costToSolveDirectly factors need)

solveNeedWithTask :: Need -> Task -> SolutionNode
solveNeedWithTask
  = task
  & Task.needs
  & Set.toList

needCanBeSolvedDirectly :: ScanFactors -> Need -> Bool
needCanBeSolvedDirectly factors need
  = Map.member need (ScanFactors.directPrices factors)

costToSolveDirectly :: ScanFactors -> Need -> Double
costToSolveDirectly factors need
  = fromMaybe
      (error "but i can't solve that")
      (Map.lookup need (ScanFactors.directPrices factors)) -- for now, we're pricing as if you always need to add one of the Need

modifiesKey :: Key -> Task -> Bool
modifiesKey key task
  =  Set.member key (Task.outcomes task)
