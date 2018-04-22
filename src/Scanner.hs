
module Scanner where

import Data.Bifunctor
import Data.Function((&))
import SimsKeys
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified ScanNode
import ScanNode(ScanNode(..))

 -- we want 1 or more Victory, as cheaply as possible
 -- 1: list all Tasks with Victory as a direct outcome
 -- 2: find the known Cost of each Task
 -- 3: add them all to a Heap, called the Open heap
 -- 4: for the cheapest one: if it's complete,
 --      then return it
 --      else add more entries to the current open heap, hierarchically

-- sample tasks:

-- Task
--   "bake"
-- prerequisites@{ Item Floor Oven, 1+ }
--               { Item Inventory ChoppedIngredients, 1+ }
--               { X, (22~52) }
--               { Y, (12~42) }
--   adjustments@{ Item Inventory Food, 1 }
--   assignments@{ X, 37 }
--   assignments@{ Y, 27 }

-- Task
--   "pickup Food"
-- prerequisites@{ Item Floor a, 1+ }
--   adjustments@{ Item Inventory a, 1 }

isComplete :: ScanNode -> Bool

scan :: ScanNode -> [ScanNode]

makeScanNode :: Map Key Double -> Task -> ScanNode
makeScanNode availables task
  = ScanNode {
    task = task,
    cost = 0,
    subNodes = [],
    unmetRequirements = Map.toList (Task.prerequisites task)
  }

consumeAvailables :: (Map Key Double, Map Key (Interval Double)) -> (Map Key Double, Map Key (Interval Double))
consumeAvailables (availables, prereqs)
  = consumeAvailablesFromLists (Map.toList availables, Map.toList prereqs)
