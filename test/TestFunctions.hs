
module TestFunctions where

import qualified Task
import Task(Task(..), Need)
import qualified ScanFactors
import ScanFactors(ScanFactors(..))
import Data.Function((&))
import Data.Map(Map)
import SimsKeys
import qualified Scanful
import qualified SolutionNode
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

standardFactors :: ScanFactors
standardFactors
  = let victory = Set.fromList [Victory]
    in
          ScanFactors.empty
        & addPrice X 1000
        & addPrice (Item Floor Fridge) 12
        & addPrice (Item Floor Food) 120
        & addTask (Task "fly" (Set.fromList [X]) (Set.fromList [Y]))
        & addTask (Task "pump iron" (Set.fromList [Item Floor Fridge]) (Set.fromList [Item Inventory Fridge]))
        & addTask (Task "gather food" (Set.fromList [Item Floor Food]) (Set.fromList [Item Inventory Food]))
        & addTask (Task "enjoy being high" (Set.fromList [Y]) victory)
        & addTask (Task "enjoy being strong" (Set.fromList [Item Inventory Fridge]) victory)
        & addTask (Task "chow down" (Set.fromList [Item Inventory Food]) victory)

addTask :: Task -> ScanFactors -> ScanFactors
addTask task factors = factors { allTasks = task : allTasks factors }

addPrice :: Need -> Double -> ScanFactors -> ScanFactors
addPrice need price factors
  = factors { directPrices = Map.insert need price (ScanFactors.directPrices factors) }

bestTask :: ScanFactors -> Task
bestTask factors
  = Scanful.findCompleteSolutions factors Victory
  & Map.toList
  & map snd
  & map SolutionNode.task
  & filter (null . Task.needs)
  & head
