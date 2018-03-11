
module RTree where

import qualified Volume
import Volume(Volume)

import Data.Function((&))

type NodeTransition k v a = RTree k v -> (a, RTree k v)

data RTree k v
  = NoRTree
  | RNode {
    nVolume :: k,
    nNumLeafs :: Int,
    nChilds :: [RTree k v] -- sorted by nNumLeafs ascending
    }
  | RLeaf {
    nVolume :: k,
    nValue :: v
    }

-- inserting:
-- 1) navigate past all of the appropriate ancestors, keeping them in the call stack
-- 1a) if this node can take a child, add one more
-- 1b) from the childs that contain the pos, pick the one with the least values
-- 1c) otherwise, feed it into the first child
-- 2) create the leaf
-- 3) for each ancestor, return a new copy of itself holding the new value
insert :: Volume k => Int -> (k, v) -> RTree k v -> RTree k v
insert _ (volume, value) NoRTree = RLeaf volume value
insert _ (volume, value) tLeaf@(RLeaf tVolume _)
  = RNode (Volume.merge volume tVolume) 2 [tLeaf, RLeaf volume value]
insert maxChilds leaf@(volume, value) node@(RNode _ _ tChilds)
  = if length tChilds < maxChilds
      then insertChild node (RLeaf volume value)
      else insert maxChilds leaf selectedChild & insertChild (node { nChilds = otherChilds })
  where (firstMatchingChild, unmatchingChilds) = removeFirstMatch (Volume.intersects volume . nVolume) tChilds
        (selectedChild, otherChilds) = maybe (head tChilds, tail tChilds) (\target -> (target, unmatchingChilds)) firstMatchingChild
        insertChild (RNode pVolume pNumLeafs pChilds) newNode@(RNode cVolume _ _)
          = RNode (Volume.merge pVolume cVolume) (pNumLeafs + 1) (sortedInsertOn nNumLeafs newNode pChilds)
        insertChild (RNode pVolume pNumLeafs pChilds) newLeaf@(RLeaf lVolume _)
          = RNode (Volume.merge pVolume lVolume) (pNumLeafs + 1) (sortedInsertOn nNumLeafs newLeaf pChilds)
        insertChild _ _ = error "what? can't insertChild into that"

removeFirstMatch :: (a -> Bool) -> [a] -> (Maybe a, [a])
removeFirstMatch predicate
  = foldr removeNextMatch (Nothing, [])
  where removeNextMatch a (Just match, as) = (Just match, a : as)
        removeNextMatch a (Nothing, as) = if predicate a then (Just a, as) else (Nothing, a : as)

sortedInsertOn :: Ord a => (x -> a) -> x -> [x] -> [x]
sortedInsertOn _ a [] = [a]
sortedInsertOn func a (b:bs)
  = if func a <= func b
      then a : b : bs
      else b : sortedInsertOn func a bs

query :: Volume k => k -> RTree k v -> [(k, v)]
query _ NoRTree = []
query volume (RLeaf lVolume lValue)
  = if Volume.intersects volume lVolume
      then [(lVolume, lValue)]
      else []
query volume (RNode tVolume _ tChilds)
  = if Volume.intersects volume tVolume
      then concatMap (query volume) tChilds
      else []
