
module Task where

import SimsKeys
import Interval(Interval)

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import Data.Set(Set)

type Need = Key

-- simplified this for first prototype
data Task = Task {
  name :: String,
  needs :: Set Need,
  outcomes :: Set Key
} deriving (Show, Eq)
