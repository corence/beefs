
module ScanNode where

import Data.Function((&))
import SimsKeys
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

data ScanNode = ScanNode {
  task :: Task,
  cost :: Double,
  subNodes :: [ScanNode],
  unmetRequirements :: [(Key, Interval Double)],
  availableBefore :: Map Key Double,
  availableAfter :: Map Key Double
  }
