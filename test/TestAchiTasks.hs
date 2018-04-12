{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import Test.Hspec
import AchiTask
import qualified Achikaps
import qualified Data.Map.Strict as Map
import Data.Function((&))
import qualified RTree
import RTree(RTree)
import qualified Interval
import Interval(Interval(..))
import Data.Tuple(swap)
import Data.Monoid
import qualified Data.Text as Text
import AchiVolume
import VolumeBuilder

deriving instance Eq AchiTask

main :: IO ()
main = hspec $ do
  describe "Achikaps" $ do
    it "matches against subsets of the tasks" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Pearl] [Metal] (5, 5))
            & addTask (convertInv [Meat, Metal] [Food] (9, 9))
      let q = startQuery
            & prereqPositive (Item Inventory Metal)
            & availablePositive (Item Inventory Pearl)
            & availablePositive (Item Inventory Meat)
            & availableRange X (-99999999) 999999999
            & availableRange Y (-99999999) 999999999
      length (RTree.query q tasks) `shouldBe` 1

    it "matches no task (on the first attempt) if none falls within the x/y bounds" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & prereqPositive (Item Inventory Victory)
            & availableRange X (-50) 50
            & availableRange Y (-50) 50
      map snd (Achikaps.chooseTask q tasks) `shouldBe` []

    it "matches a viable task right away if it falls within the x/y bounds" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & prereqPositive (Item Inventory Victory)
            & availableRange X (-500) 500
            & availableRange Y (-500) 500
            & availablePositive (Item Inventory Food)
      map snd (Achikaps.chooseTask q tasks) `shouldBe` map snd [convertInv [Food] [Victory] (100, 100)]

    it "matches the most viable task by scanning" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & prereqPositive (Item Inventory Victory)
            & availableRange X (-50) 50
            & availableRange Y (-50) 50
      map snd (Achikaps.chooseTask q tasks) `shouldBe` map snd [pickup Food (100, 9999)]
