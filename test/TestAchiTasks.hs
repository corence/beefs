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
import Data.Bifunctor
import qualified Interval
import Interval(Interval(..))
import Data.Tuple(swap)
import Data.Monoid
import qualified Data.Text as Text

deriving instance Eq AchiTask

convertAtoB :: [Key] -> [Key] -> (Int, Int) -> (MapVolume, AchiTask)
convertAtoB sources dests (x, y)
  = swap (AchiTask (Text.pack $ "convert " <> show sources <> " to " <> show dests),
     makeUnitVolume $
       [(X, x), (Y, y)]
       ++ map (\source -> (source, -1)) sources
       ++ map (\dest -> (dest, 1)) dests)

convertInv :: [ItemType] -> [ItemType] -> (Int, Int) -> (MapVolume, AchiTask)
convertInv sourceTypes destTypes
  = convertAtoB
      (map (\sourceType -> Item Inventory sourceType) sourceTypes)
      (map (\destType -> Item Inventory destType) destTypes)

pickup :: ItemType -> (Int, Int) -> (MapVolume, AchiTask)
pickup itemType = convertAtoB [Item Floor itemType] [Item Inventory itemType]

queryPositive :: Key -> MapVolume
queryPositive key = makeVolume [(key, Interval (IntValue 1) (IntValue maxBound))]

makeUnitVolume :: [(Key, Int)] -> MapVolume
makeUnitVolume = makeVolume . map (second (Interval.unit . IntValue))

makeVolume :: [(Key, Interval Value)] -> MapVolume
makeVolume = MapVolume . Map.fromList

addTask :: (MapVolume, AchiTask) -> Achikaps.Tasks -> Achikaps.Tasks
addTask = RTree.insert 3

main :: IO ()
main = hspec $ do
  describe "Achikaps" $ do
    it "matches against subsets of the tasks" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Pearl] [Metal] (5, 5))
            & addTask (convertInv [Meat, Metal] [Food] (9, 9))
      let q = queryPositive (Item Inventory Metal)
      length (RTree.query q tasks) `shouldBe` 1

    --it "matches nearby tasks before faraway tasks" $ do

    it "scans for a viable task" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = queryPositive (Item Inventory Victory)
      map snd (Achikaps.chooseTask q tasks) `shouldBe` map snd [pickup Food (100, 9999)]
