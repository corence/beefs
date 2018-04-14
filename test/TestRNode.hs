{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.QuickCheck
import qualified RNode
import RNode(RNode(..))
import Data.Function((&))
import qualified Volume
import Volume(Volume)
import qualified Interval
import Interval(Interval(..))
import System.Random
import Rect
import Arbs

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

import AchiVolume

makeTree :: Volume k => [(k, v)] -> RNode k v
makeTree = foldr (RNode.insert 3) RNode.REmptyNode

switchie :: AchiVolume -> AchiVolume
switchie (AchiVolume prereqs availables) = AchiVolume availables prereqs

main :: IO ()
main = hspec $ do
  describe "Interval Double" $ do
    it "can only generate valid intervals" $
      property $ \(Interval a z) -> (a :: Double) < z `shouldBe` True
    it "can only generate valid rects" $
      property $ \(Rect (Interval a b) (Interval c d)) -> do
        a < b `shouldBe` True
        c < d `shouldBe` True
    it "can only generate valid rect lists" $
      property $ \rects ->
        let checkRect (Rect (Interval a b) (Interval c d))
                = do
                    a < b `shouldBe` True
                    c < d `shouldBe` True
          in mapM_ checkRect (rects :: [Rect])
  describe "RNode Rect a" $ do
    it "contains n elements after inserting n things" $
      property $ \xs -> RNode.size (makeTree xs :: RNode Rect String) `shouldBe` length xs
    it "always makes sure parent node volumes contain child node volumes" $
      property $ \xs -> rTreeDescendantsAreContained (makeTree xs :: RNode Rect String) `shouldBe` True
    it "contains the elements that it contains" $
      property $ \rect entries -> do
        let rtree = makeTree entries :: RNode Rect Int
        let matchingEntries = filter (\(r, i) -> Volume.intersects rect r) entries
        RNode.lookup rect rtree `shouldMatchList` matchingEntries

  describe "RNode AchiVolume a" $ do
    it "can contain multiple unrelated volumes" $
      property $ \keys values -> do
        let entries = zip (List.nub keys) values :: [(AchiVolume, Int)]
        let rtree = makeTree entries :: RNode AchiVolume Int
        -- all ((== 1) . length . (`RNode.lookup` rtree) . switchie . fst) entries `shouldBe` True
        let expectedMatchResults = map (const True) entries
        let matchResults
              = entries
              & map fst
              & map switchie
              & map (`RNode.lookup` rtree)
              & map length
              & map (> 0)
        matchResults `shouldBe` expectedMatchResults
    it "can contain multiple overlapping volumes" $
      property $ \entries -> do
        let rtree = makeTree entries :: RNode AchiVolume Int
        any (null . (`RNode.lookup` rtree) . switchie . fst) entries `shouldBe` False
    it "contains n elements after inserting n things" $
      property $ \xs -> RNode.size (makeTree xs :: RNode AchiVolume String) `shouldBe` length xs
    it "contains the elements that it contains" $
      property $ \vol entries -> do
        let rtree = makeTree entries :: RNode AchiVolume Int
        let matchingEntries = filter (\(v, _) -> Volume.intersects vol v) entries
        RNode.lookup vol rtree `shouldMatchList` matchingEntries


rTreeDescendantsAreContained :: Volume k => RNode k v -> Bool
rTreeDescendantsAreContained REmptyNode = True
rTreeDescendantsAreContained (RLeaf _ _) = True
rTreeDescendantsAreContained (RNode vol _ childs)
  =  all (Volume.contains vol . getVolume) childs
  && all rTreeDescendantsAreContained childs

getVolume :: RNode k v -> k
getVolume REmptyNode = error "hey wait what but that doesn't even"
getVolume (RLeaf key _) = key
getVolume (RNode key _ _) = key
