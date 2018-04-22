
module Task where

import SimsKeys
import Interval(Interval)

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

data Task = Task {
  name :: String,
  prerequisites :: Map Key (Interval Double),
  adjustments :: Map Key Double,
  assignments :: Map Key Double
  }
