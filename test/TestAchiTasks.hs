{-# LANGUAGE OverloadedStrings #-}

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

convertAtoB :: [ItemType] -> [ItemType] -> (Int, Int) -> (AchiTask, MapVolume)
convertAtoB sourceTypes destTypes (x, y)
  = (AchiTask (Text.pack $ "convert " <> show sourceTypes <> " to " <> show destTypes),
     makeUnitVolume $
       [(X, x), (Y, y)]
       ++ map (\sourceType -> (ItemAvailable sourceType, -1)) sourceTypes
       ++ map (\destType -> (ItemAvailable destType, 1)) destTypes)

makeUnitVolume :: [(Key, Int)] -> MapVolume
makeUnitVolume = makeVolume . map (second (Interval.unit . IntValue))

makeVolume :: [(Key, Interval Value)] -> MapVolume
makeVolume = MapVolume . Map.fromList

addTask :: (AchiTask, MapVolume) -> Achikaps.Tasks -> Achikaps.Tasks
addTask = RTree.insert 3 . swap

tasks :: RTree MapVolume AchiTask
tasks = RTree.NoRTree
  & addTask (convertAtoB [Pearl] [Metal] (5, 5))
  & addTask (convertAtoB [Meat, Metal] [Food] (9, 9))

main :: IO ()
main = hspec $ do
  describe "Achikaps" $ do
    it "matches against subsets of the tasks" $ do
      let q = makeVolume [(ItemAvailable Metal, Interval (IntValue 1) (IntValue maxBound))]
      length (RTree.query q tasks) `shouldBe` 1
