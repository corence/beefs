{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.QuickCheck
import qualified RTree
import RTree(RTree)
import Data.Function((&))
import qualified Volume
import Volume(Volume)
import qualified Interval
import Interval(Interval(..))
import System.Random

data Rect = Rect (Interval Double) (Interval Double) deriving (Show)

instance Volume Rect where
  intersects (Rect x1 y1) (Rect x2 y2) = Interval.intersects x1 x2 && Interval.intersects y1 y2
  merge (Rect x1 y1) (Rect x2 y2) = Rect (Interval.merge x1 y1) (Interval.merge x2 y2)

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
