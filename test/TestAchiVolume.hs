{-# LANGUAGE OverloadedStrings #-}

import AchiTask
import AchiVolume
import VolumeBuilder

import Test.QuickCheck

import qualified Volume

import qualified Data.Text as Text
import Data.Function((&))

import Arbs

import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $
  describe "AchiVolume" $ do
    it "matches commutatively" $
      property $ \vol1 vol2 ->
        Volume.intersects vol1 vol2 `shouldBe` Volume.intersects vol2 (vol1 :: AchiVolume)
    it "matches associatively" $
      property $ \vol1 vol2 vol3 ->
        let match12 = Volume.intersects vol1 vol2
            match23 = Volume.intersects vol2 vol3
            match13 = Volume.intersects vol1 (vol3 :: AchiVolume)
        in match13 `shouldBe` match12 && match23
    modifyMaxDiscardRatio (const 20) $
      it "anything that matches against a volume should match against its merged form too" $
        property $ \vol1 vol2 vol3 -> do
          let merged = Volume.merge vol1 vol2 :: AchiVolume
          Volume.intersects vol1 vol3 ==> Volume.intersects merged vol3
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
