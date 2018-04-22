
module SolutionNode where

import Interval(Interval)
import SimsKeys(Key)
import Task
import qualified Data.Set as Set
import Data.Set(Set)

data SolutionNode = TaskNode {
  task :: Task,
  successor :: SolutionNode,
  closed :: Set String, -- names of  the tasks that are already in this chain
  needs :: [Need]
  }
  | DirectNode {
  expense :: Double
  }
  | TerminalNode

nodeIsComplete :: SolutionNode -> Bool
nodeIsComplete TerminalNode = False
nodeIsComplete DirectNode{} = True
nodeIsComplete TaskNode{} = undefined

cost :: SolutionNode -> Double
cost TerminalNode = 0
cost TaskNode{} = 0
cost node@DirectNode{} = expense node
