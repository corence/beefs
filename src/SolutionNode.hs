
module SolutionNode where

import Interval(Interval)
import SimsKeys(Key)
import Task
import qualified Data.Set as Set
import Data.Set(Set)

data SolutionNode = SolutionNode {
  cost :: Double,
  task :: Maybe Task
  } deriving (Show, Eq)
