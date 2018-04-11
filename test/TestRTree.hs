{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.QuickCheck
import qualified RTree
import RTree(RTree(..))
import Data.Function((&))
import qualified Volume
import Volume(Volume)
import qualified Interval
import Interval(Interval(..))
import System.Random
import Rect

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

import AchiVolume
import AchiTask(Key)

instance Arbitrary Key where

instance Arbitrary (Interval Double) where
  arbitrary = do
    x <- choose (-100, 50)
    y <- choose (x, 100)
    pure $ Interval x y

instance Arbitrary Rect where
  arbitrary = Rect <$> arbitrary <*> arbitrary
  shrink (Rect x y) = Rect <$> shrink x <*> shrink y

instance Arbitrary (Map Key (Interval Double)) where
  arbitrary = Map.fromList <$> arbitrary
  shrink = map Map.fromList . shrink . Map.toList

instance Arbitrary AchiVolume where
  arbitrary = AchiVolume <$> arbitrary <*> arbitrary
  shrink (AchiVolume p a) = AchiVolume <$> shrink p <*> shrink a

makeTree :: Volume k => [(k, v)] -> RTree k v
makeTree = foldr (RTree.insert 3) RTree.NoRTree

main :: IO ()
main = hspec $ do
  describe "Rect RTree" $ do
    it "contains n elements after inserting n things" $
      property $ \xs -> RTree.size (makeTree xs :: RTree Rect String) `shouldBe` length xs
    it "always makes sure parent node volumes contain child node volumes" $
      property $ \xs -> rTreeDescendantsAreContained (makeTree xs :: RTree Rect String) `shouldBe` True
    it "contains the elements that it contains" $
      property $ \rect entries -> do
        let rtree = makeTree entries :: RTree Rect Int
        let matchingEntries = filter (\(r, i) -> Volume.intersects rect r) entries
        RTree.query rect rtree `shouldBe` matchingEntries

  describe "AchiVolume RTree" $ do
    it "can contain multiple unrelated volumes" $
      property $ \keys values -> do
        let entries = zip (List.nub keys) values :: [(AchiVolume, Int)]
        let rtree = makeTree entries :: RTree AchiVolume Int
        all ((== 1) . length . (`RTree.query` rtree) . fst) entries `shouldBe` True
    it "can contain multiple overlapping volumes" $
      property $ \entries -> do
        let rtree = makeTree entries :: RTree AchiVolume Int
        any (null . (`RTree.query` rtree) . fst) entries `shouldBe` False
    it "contains n elements after inserting n things" $
      property $ \xs -> RTree.size (makeTree xs :: RTree AchiVolume String) `shouldBe` length xs
    it "contains the elements that it contains" $
      property $ \vol entries -> do
        let rtree = makeTree entries :: RTree AchiVolume Int
        let matchingEntries = filter (\(v, i) -> Volume.intersects vol v) entries
        RTree.query vol rtree `shouldBe` matchingEntries


rTreeDescendantsAreContained :: Volume k => RTree k v -> Bool
rTreeDescendantsAreContained NoRTree = True
rTreeDescendantsAreContained (RLeaf _ _) = True
rTreeDescendantsAreContained (RNode vol _ childs)
  =  all (Volume.contains vol . getVolume) childs
  && all rTreeDescendantsAreContained childs

getVolume :: RTree k v -> k
getVolume NoRTree = error "hey wait what but that doesn't even"
getVolume (RLeaf key _) = key
getVolume (RNode key _ _) = key
