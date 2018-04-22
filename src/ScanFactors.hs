
module ScanFactors where

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import qualified Task
import Task(Task, Need)

data ScanFactors = ScanFactors {
  allTasks :: [Task],
  directPrices :: Map Need Double
}
