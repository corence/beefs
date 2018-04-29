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
import Debug.Trace

type LMap = LMap.Map

-- to find actual tasks ready to start, filter this map to only contain nodes with tasks with no needs
-- TODO: this is totally fucked because it's hard-coded to loop 2 levels deep.
-- needs to scan indefinitely deeply
findCompleteSolutions :: ScanFactors -> Need -> LMap Double SolutionNode
findCompleteSolutions factors need
  = foldr solveStep LMap.empty terminalNodes
  where terminalTasks = findSolutions factors need
        terminalNodes = map (taskToNode factors) terminalTasks
        solveStep task results
          = findPredecessors factors task
          & map (\node -> (cost node, node))
          & addAll results

findAnswers :: ScanFactors -> Need -> [SolutionNode]
findAnswers factors terminalNeed
  = findAnswersRecursive factors (mappy, [])
    where mappy
            = Task "terminal" (Set.singleton terminalNeed) Set.empty
            & SolutionNode (-999)
            & (\node -> (-999, node))
            & pure
            & LMap.fromList

fA :: ScanFactors -> Need -> [SolutionNode]
fA factors terminalNeed
  = fAR factors (mappy, [])
    where mappy
            = Task "terminal" (Set.singleton terminalNeed) Set.empty
            & SolutionNode (-999)
            & (\node -> (-999, node))
            & pure
            & LMap.fromList

findAnswersRecursive :: ScanFactors -> (LMap Double SolutionNode, [SolutionNode]) -> [SolutionNode]
findAnswersRecursive factors (successors, answers)
  | LMap.null successors = answers
  | isComplete successor = findAnswersRecursive factors (otherSuccessors, successor : answers)
  | otherwise = (addAll otherSuccessors predecessorEntries, answers) & findAnswersRecursive factors
    where successor = LMap.findMin successors & snd
          otherSuccessors = LMap.deleteMin successors
          predecessors = findPredecessors factors successor
          predecessorEntries = map (\node -> (SolutionNode.cost node, node)) predecessors

fAR :: ScanFactors -> (LMap Double SolutionNode, [SolutionNode]) -> [SolutionNode]
fAR factors (successors, answers)
  = case trace (show (successors, answers)) (findAnswersStep factors (successors, answers)) of
      Nothing -> []
      Just newy@(newSuccessors, newAnswers) -> fAR factors newy ++ answers

findAnswersStep :: ScanFactors -> (LMap Double SolutionNode, [SolutionNode]) -> Maybe (LMap Double SolutionNode, [SolutionNode])
findAnswersStep factors (successors, answers)
  | LMap.null successors = Nothing
  | isComplete successor = Just (otherSuccessors, successor : answers)
  | otherwise = Just (addAll otherSuccessors predecessorEntries, answers)
    where successor = LMap.findMin successors & snd
          otherSuccessors = LMap.deleteMin successors
          predecessors = findPredecessors factors successor
          predecessorEntries = map (\node -> (SolutionNode.cost node, node)) predecessors

isComplete :: SolutionNode -> Bool
isComplete = Set.null . Task.needs . SolutionNode.task

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
