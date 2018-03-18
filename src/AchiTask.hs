
module AchiTask where

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Function((&))
import Volume
import Data.Text(Text)
import qualified Interval
import Interval(Interval)

data ItemType
  = Food
  | Meat
  | Pearl
  | Metal
  | Debris
  | Gum
  | Victory
  deriving (Show, Eq, Ord)

data ItemPosition
  = Floor
  | Inventory
  deriving (Show, Eq, Ord)

data Key
  = X
  | Y
  | Z
  | NumSlotsAvailable
  | Item ItemPosition ItemType
  deriving (Show, Eq, Ord)

newtype AchiTask = AchiTask {
  name :: Text
  -- prereqs :: Ord ord => Map Key (Interval ord),
  -- ‎outcomes :: Map Key Value, -- these will actually be stored as intervals so let's encapsulate it with a setter
  -- actually, we don't store these anymore... now they're just a key in an RTree
  } deriving Show

data Value = IntValue Int | DoubleValue Double deriving (Show, Ord, Eq)
