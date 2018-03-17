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

queryPositive :: Key -> MapVolume -> MapVolume
queryPositive key = queryRange key 1 maxBound

queryRange :: Key -> Int -> Int -> MapVolume -> MapVolume
queryRange key low high (MapVolume m) = Map.insert key value m & MapVolume
    where value = Interval (IntValue low) (IntValue high)

makeUnitVolume :: [(Key, Int)] -> MapVolume
makeUnitVolume = makeVolume . map (second (Interval.unit . IntValue))

makeVolume :: [(Key, Interval Value)] -> MapVolume
makeVolume = MapVolume . Map.fromList

startQuery :: MapVolume
startQuery = MapVolume $ Map.empty

addTask :: (MapVolume, AchiTask) -> Achikaps.Tasks -> Achikaps.Tasks
addTask = RTree.insert 3

main :: IO ()
main = hspec $ do
  describe "Achikaps" $ do
    it "matches against subsets of the tasks" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Pearl] [Metal] (5, 5))
            & addTask (convertInv [Meat, Metal] [Food] (9, 9))
      let q = startQuery & queryPositive (Item Inventory Metal)
      length (RTree.query q tasks) `shouldBe` 1

    --it "matches nearby tasks before faraway tasks" $ do

    it "matches no task (on the first attempt) if none falls within the x/y bounds" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & queryPositive (Item Inventory Victory)
            & queryRange X (-50) 50
            & queryRange Y (-50) 50
      map snd (Achikaps.chooseTask q tasks) `shouldBe` []

    it "matches a viable task right away if it falls within the x/y bounds" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & queryPositive (Item Inventory Victory)
            & queryRange X (-500) 500
            & queryRange Y (-500) 500
      map snd (Achikaps.chooseTask q tasks) `shouldBe` map snd [pickup Food (100, 9999)]

    it "matches the most viable task by scanning" $ do
      let tasks = RTree.NoRTree
            & addTask (convertInv [Food] [Victory] (100, 100))
            & addTask (pickup Food (100, 9999))
      let q = startQuery
            & queryPositive (Item Inventory Victory)
            & queryRange X (-50) 50
            & queryRange Y (-50) 50
      map snd (Achikaps.chooseTask q tasks) `shouldBe` map snd [pickup Food (100, 9999)]
