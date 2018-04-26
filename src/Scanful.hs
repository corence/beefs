module Scanful where

import Data.Function((&))
import SimsKeys
import qualified Interval
import Interval(Interval)
import qualified Task
import Task(Task, Need)
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

findCompleteSolutions :: ScanFactors -> Need -> LMap.Map Double SolutionNode
findCompleteSolutions factors need
  = foldr solveStep LMap.empty terminalTasks
  where terminalTasks = findSolutions factors need
        solveStep task results
          = findPrecursors factors task
          & map (\node -> (cost node, node))
          & addAll results

addAll :: Ord k => LMap.Map k v -> [(k, v)] -> LMap.Map k v
addAll = foldr (uncurry LMap.insert)

findPrecursors :: ScanFactors -> Task -> [SolutionNode]
findPrecursors factors nextTask
  = let (directNeeds, indirectNeeds)
          = Task.needs nextTask
          & Set.toList
          & List.partition (needCanBeSolvedDirectly factors)
        directCost
          = directNeeds
          & map (costToSolveDirectly factors)
          & sum
  in
    if null indirectNeeds
      then [SolutionNode directCost Nothing]
      else indirectNeeds
        & map (findSolutions factors)
        & concat
        & map (SolutionNode directCost . Just)

findSolutions :: ScanFactors -> Need -> [Task]
findSolutions factors need
  = ScanFactors.allTasks factors
  & filter (Set.member need . Task.outcomes)

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
