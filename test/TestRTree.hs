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

instance Arbitrary (Interval Double) where
  arbitrary = do
    x <- choose (-100, 50)
    y <- choose (x, 100)
    pure $ Interval x y

instance Arbitrary Rect where
  arbitrary = Rect <$> arbitrary <*> arbitrary
  shrink (Rect x y) = Rect <$> shrink x <*> shrink y

makeTree :: [(Rect, String)] -> RTree Rect String
makeTree = foldr (RTree.insert 3) RTree.NoRTree

main :: IO ()
main = hspec $ do
  describe "Achikaps" $ do
    it "contains n elements after inserting n things" $ do
      property $ \xs -> RTree.size (makeTree xs) `shouldBe` length xs
    it "always makes sure parent node volumes contain child node volumes" $ do
      property $ \xs -> rTreeDescendantsAreContained (makeTree xs) `shouldBe` True

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
