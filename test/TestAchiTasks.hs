{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import Test.Hspec
-- import Test.QuickCheck
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

convertAtoB :: [Key] -> [Key] -> (Double, Double) -> (AchiVolume, AchiTask)
convertAtoB sources dests (x, y)
  = (makeUnitVolume prereqs outcomes,
    AchiTask (Text.pack $ "convert " <> show sources <> " to " <> show dests))
       where prereqs
               =  map (\source -> (source, 1)) sources
             outcomes
               =  [(X, x), (Y, y)]
               ++ map (\source -> (source, -1)) sources
               ++ map (\dest -> (dest, 1)) dests

convertInv :: [ItemType] -> [ItemType] -> (Double, Double) -> (AchiVolume, AchiTask)
convertInv sourceTypes destTypes
  = convertAtoB
      (map (Item Inventory) sourceTypes)
      (map (Item Inventory) destTypes)

pickup :: ItemType -> (Double, Double) -> (AchiVolume, AchiTask)
pickup itemType = convertAtoB [Item Floor itemType] [Item Inventory itemType]

addTask :: (AchiVolume, AchiTask) -> Achikaps.Tasks -> Achikaps.Tasks
addTask = RTree.insert 3

main :: IO ()
main = hspec $ do
  describe "Achikaps" $ do
    it "matches against subsets of the tasks" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Pearl] [Metal] (5, 5))
            & addTask (convertInv [Meat, Metal] [Food] (9, 9))
      let q = startQuery
            & queryOutcomePositive (Item Inventory Metal)
            & queryPrereqPositive (Item Inventory Pearl)
            & queryPrereqPositive (Item Inventory Meat)
      length (RTree.query q tasks) `shouldBe` 1

    --it "matches nearby tasks before faraway tasks" $ do

    it "matches no task (on the first attempt) if none falls within the x/y bounds" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & queryOutcomePositive (Item Inventory Victory)
            & queryPrereqRange X (-50) 50
            & queryPrereqRange Y (-50) 50
      map snd (Achikaps.chooseTask q tasks) `shouldBe` []

    it "matches a viable task right away if it falls within the x/y bounds" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & queryOutcomePositive (Item Inventory Victory)
            & queryPrereqRange X (-500) 500
            & queryPrereqRange Y (-500) 500
      map snd (Achikaps.chooseTask q tasks) `shouldBe` map snd [pickup Food (100, 9999)]

    it "matches the most viable task by scanning" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & queryOutcomePositive (Item Inventory Victory)
            & queryPrereqRange X (-50) 50
            & queryPrereqRange Y (-50) 50
      map snd (Achikaps.chooseTask q tasks) `shouldBe` map snd [pickup Food (100, 9999)]
