
module Task where

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

data Task = Task {
  name :: String,
  prerequisites :: Map Key (Interval Double),
  outcomes :: Map Key Double
  }
