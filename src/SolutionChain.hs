
module SolutionChain where

import Interval(Interval)
import SimsKeys(Key)
import Task
import qualified Data.Set as Set
import Data.Set(Set)

type Target = (Key, Interval Double)

data SolutionChain = SolutionChain {
  task :: Task,
  successor :: Maybe SolutionChain,
  closed :: Set String, -- names of  the tasks that are already in this chain
  cost :: Double,
  openPrereqs :: [Target]
  }
