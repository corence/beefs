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

convertPearlToMetal :: (AchiTask, MapVolume)
convertPearlToMetal = (AchiTask "convert pearl to metal",
  makeUnitVolume [
  (X, 5),
  (Y, 5),
  (ItemAvailable Pearl, -1),
  (ItemAvailable Metal, 1)])

makeUnitVolume :: [(Key, Int)] -> MapVolume
makeUnitVolume = makeVolume . map (second (Interval.unit . IntValue))

makeVolume :: [(Key, Interval Value)] -> MapVolume
makeVolume = MapVolume . Map.fromList

cookMeat :: (AchiTask, MapVolume)
cookMeat = (AchiTask "cook meat",
  makeUnitVolume [
    (ItemAvailable Meat, -1),
    (ItemAvailable Metal, -1),
    (ItemAvailable Food, 1)])

addTask :: (AchiTask, MapVolume) -> Achikaps.Tasks -> Achikaps.Tasks
addTask = RTree.insert 3 . swap

tasks :: RTree MapVolume AchiTask
tasks = RTree.NoRTree
  & addTask convertPearlToMetal
  & addTask cookMeat

main :: IO ()
main = hspec $ do
  describe "Achikaps" $ do
    it "matches against subsets of the tasks" $ do
      let q = makeVolume [(ItemAvailable Meat, Interval (IntValue 1) (IntValue maxBound))]
      length (RTree.query q tasks) `shouldBe` 2
