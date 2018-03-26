{-# LANGUAGE OverloadedStrings #-}

import AchiTask
import AchiVolume
import VolumeBuilder

import qualified Volume

import qualified Data.Text as Text
import Data.Function((&))

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "AchiVolume" $ do
    it "doesn't match if prereq key doesn't exist" $ do
      let taskVolume = emptyVolume
            & addUnitPrereq X 3
      let q = startQuery
            & addUnitAvailable Y 12
      Volume.intersects q taskVolume `shouldBe` False
    it "matches the simplest possible task example" $ do
      let taskVolume = emptyVolume
            & addUnitPrereq X 3
      let q = startQuery
            & addUnitAvailable X 3
      q `shouldIntersect` taskVolume
    it "matches against a pretty complicated volume" $ do
      let taskVolume = emptyVolume
            & addUnitPrereq (Item Inventory Pearl) 1
            & addUnitAvailable (Item Inventory Metal) 1
      let q = startQuery
            & prereqPositive (Item Inventory Metal)
            & availablePositive (Item Inventory Pearl)
            & availablePositive (Item Inventory Meat)
            & availableRange X (-99999999) 999999999
            & availableRange Y (-99999999) 999999999
      q `shouldIntersect` taskVolume

shouldIntersect :: AchiVolume -> AchiVolume -> IO ()
shouldIntersect a b = do
  Volume.intersects a b `shouldBe` True
  Volume.intersects b a `shouldBe` True
