module Scanful where

import Data.Function((&))
import SimsKeys
import qualified Interval
import Interval(Interval)
import qualified Task
import Task(Task(..), Need)
import Data.Maybe(fromMaybe)
import qualified ScanFactors
import ScanFactors(ScanFactors)
import qualified SolutionNode
import SolutionNode(SolutionNode(..))

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import qualified Data.Map.Lazy as LMap
import qualified Data.List as List

-- to find actual tasks ready to start, filter this map to only contain nodes with tasks with no needs
findCompleteSolutions :: ScanFactors -> Need -> LMap.Map Double SolutionNode
findCompleteSolutions factors need
  = foldr solveStep LMap.empty terminalNodes
  where terminalTasks = findSolutions factors need
        terminalNodes = map (taskToNode factors) terminalTasks
        solveStep task results
          = findPredecessors factors task
          & map (\node -> (cost node, node))
          & addAll results

addAll :: Ord k => LMap.Map k v -> [(k, v)] -> LMap.Map k v
addAll = foldr (uncurry LMap.insert)

findPredecessors :: ScanFactors -> SolutionNode -> [SolutionNode]
findPredecessors factors successor
  = SolutionNode.task successor
  & Task.needs
  & Set.toList
  & map (findSolutions factors)
  & concat
  & map (taskToNode factors)

canSolveANeedOf :: SolutionNode -> Task -> Bool
canSolveANeedOf needyNode providingTask = undefined

taskToNode :: ScanFactors -> Task -> SolutionNode
taskToNode factors task
  = let (directNeeds, indirectNeeds)
          = Task.needs task
          & Set.partition (needCanBeSolvedDirectly factors)
        directCost
          = directNeeds
          & Set.map (costToSolveNeed factors)
          & sum
        newTask = task { needs = indirectNeeds }
        in SolutionNode directCost newTask

findSolutions :: ScanFactors -> Need -> [Task]
findSolutions factors need
  = ScanFactors.allTasks factors
  & filter (Set.member need . Task.outcomes)

needCanBeSolvedDirectly :: ScanFactors -> Need -> Bool
needCanBeSolvedDirectly factors need
  = Map.member need (ScanFactors.directPrices factors)

costToSolveNeed :: ScanFactors -> Need -> Double
costToSolveNeed factors need
  = fromMaybe
      (error "but i can't solve that directly")
      (Map.lookup need (ScanFactors.directPrices factors)) -- for now, we're pricing as if you always need to add one of the Need

modifiesKey :: Key -> Task -> Bool
modifiesKey key task
  =  Set.member key (Task.outcomes task)
