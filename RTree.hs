
module RTree where

import qualified Zone as Zone
import Zone(Pos(..), Zone(..))

import Data.Function((&))
import Data.Maybe(isJust, fromJust)

data RTree a = NoRTree | RTreeNode {
  nPos :: Pos,
  nValue :: a,
  nZone :: Zone,
  nNumValues :: Int,
  nChilds :: [RTree a] -- sorted by nNumValues ascending
  }

-- inserting: how to choose a destination child
-- 1) from the childs that contain the pos, pick the one with the least values
-- 2) if this node can take another child, do that
-- 3) otherwise, feed it into the first child
insert :: Int -> Pos -> a -> RTree a -> RTree a
insert _ pos value NoRTree = RTreeNode pos value (Zone.unitZone pos) 1 []
insert maxChilds pos value tree
  | isJust targetChild = insertInto (fromJust targetChild) & insertInto (treeWithChilds otherChilds)
  | length tChilds < maxChilds = insertInto tree
  | otherwise = insert maxChilds pos value (head tChilds) & insertInto (treeWithChilds (tail tChilds))
  where (RTreeNode tPos tValue tZone tNumValues tChilds) = tree
        (targetChild, otherChilds) = removeFirstMatch (\node -> Zone.contains pos (nZone node)) tChilds
        treeWithChilds = RTreeNode tPos tValue tZone tNumValues -- note: we don't shrink the parent rectangle or tNumValues, because they're about to be restored anyway
        newNode = RTreeNode pos value (Zone.unitZone pos) 1 []
        insertInto (RTreeNode pPos pValue pZone pNumValues pChilds)
          = RTreeNode pPos pValue (Zone.extend pos pZone) (pNumValues + 1) (sortedInsert newNode pChilds)

removeFirstMatch :: (a -> Bool) -> [a] -> (a, [a])
removeFirstMatch predicate
  = foldr removeNextMatch (Nothing, [])
  where removeNextMatch a (Just match, as) = (Just match, a : as)
        removeNextMatch a (Nothing, as) = if predicate a then (Just a, as) else (Nothing, a : as)

sortedInsert :: Ord a => a -> [a] -> [a]
sortedInsert a [] = [a]
sortedInsert a (b:bs)
  = if a <= b
      then a : b : bs
      else b : sortedInsert a bs

query :: Zone -> RTree a -> [a]
query _ NoRTree = []
query zone tree
  = if Zone.overlaps zone tZone
      then tValue : concatMap (query zone) tChilds
      else []
  where (RTreeNode tPos tValue tZone tNumValues tChilds) = tree
