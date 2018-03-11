
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
  deriving (Show, Ord, Eq)

data Key
  = X
  | Y
  | Z
  | NumSlotsAvailable
  | ItemAvailable ItemType
  | ItemInInventory ItemType
  deriving (Show, Ord, Eq)

newtype AchiTask = AchiTask {
  name :: Text
  -- prereqs :: Ord ord => Map Key (Interval ord),
  -- ‎outcomes :: Map Key Value, -- these will actually be stored as intervals so let's encapsulate it with a setter
  -- actually, we don't store these anymore... now they're just a key in an RTree
  }

data Value = IntValue Int | DoubleValue Double deriving (Show, Ord, Eq)
newtype MapVolume = MapVolume (Map Key (Interval Value))

instance Volume MapVolume where
  merge (MapVolume vol1) (MapVolume vol2) = Map.unionWith Interval.merge vol1 vol2 & MapVolume
  intersects (MapVolume vol1) (MapVolume vol2)
    = Map.intersectionWith Interval.intersects vol1 vol2 -- for all keys that exist in both Maps, create a new Map that records whether there's an intersection between the Intervals at that key
    & Map.toList -- make it a list
    & all snd -- make sure that every value from that intersected map is True
  contains m1@(MapVolume vol1) m2@(MapVolume vol2)
    = intersects m1 m2 && null (Map.difference vol2 vol1)
