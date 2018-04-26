
module TestFunctions where

import qualified Task
import Task(Task, Need)
import qualified ScanFactors
import ScanFactors(ScanFactors(..))
import Data.Function((&))
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import SimsKeys
import qualified Scanful
import qualified SolutionNode
import Data.Maybe(fromJust)

addTask :: Task -> ScanFactors -> ScanFactors
addTask task factors = factors { allTasks = task : allTasks factors }

addPrice :: Need -> Double -> ScanFactors -> ScanFactors
addPrice need price factors
  = factors { directPrices = Map.insert need price (ScanFactors.directPrices factors) }

bestTask :: ScanFactors -> Task
bestTask factors
  = Scanful.findCompleteSolutions factors Victory
  & Map.toList
  & head
  & snd
  & SolutionNode.task
  & fromJust
