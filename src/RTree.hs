
module RTree where

import RNode
import qualified Volume
import Volume(Volume)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe(maybe)
import Data.Function((&))

data RTree k v
  = RTree {
    rNodeCapacity :: Int,
    rRootNode :: RNode k v,
    rBoundary :: Maybe k
    }

create :: Int -> RTree k v
create nodeCapacity
  = RTree {
    rNodeCapacity = nodeCapacity,
    rRootNode = RNode.REmptyNode,
    rBoundary = Nothing
  }

insert :: Volume k => (k, v) -> RTree k v -> RTree k v
insert entry@(volume, _) (RTree nodeCapacity rootNode extents)
  = RTree nodeCapacity newRootNode newExtents
  where newRootNode = RNode.insert nodeCapacity entry rootNode
        newExtents = Just $ maybe volume (Volume.extents volume) extents

lookup :: Volume k => k -> RTree k v -> [(k, v)]
lookup key tree = RNode.lookup key (rRootNode tree)

size :: RNode k v -> Int
size REmptyNode = 0
size (RLeaf _ _) = 1
size (RNode _ numLeafs _) = numLeafs

dump :: (Show k, Show v) => Int -> RTree k v -> [String]
dump indent tree = RNode.dump indent (rRootNode tree)
