{-# LANGUAGE FlexibleInstances #-}

module Arbs where

import Test.QuickCheck
import AchiTask
import Data.Function((&))
import qualified Interval
import Interval(Interval(..))
import Rect
import AchiVolume

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

instance Arbitrary Key where
  arbitrary = oneof [
    elements [
      X,
      Y,
      Z,
      NumSlotsAvailable
      ],
    Item <$> itemPosition <*> itemType
    ]
    where itemPosition = elements [Floor, Inventory]
          itemType = elements [
                      Food,
                      Meat,
                      Pearl,
                      Metal,
                      Debris,
                      Gum,
                      Victory
                      ]

instance Arbitrary (Interval Double) where
  arbitrary = do
    x <- choose (-100, 50)
    y <- choose (x, 100)
    pure $ Interval x y
  shrink (Interval x y) = Interval <$> shrink x <*> shrink y

instance Arbitrary Rect where
  arbitrary = Rect <$> arbitrary <*> arbitrary
  shrink (Rect x y) = Rect <$> shrink x <*> shrink y

instance Arbitrary (Map Key (Interval Double)) where
  arbitrary = Map.fromList <$> arbitrary
  shrink = map Map.fromList . shrink . Map.toList

instance Arbitrary AchiVolume where
  arbitrary = AchiVolume <$> arbitrary <*> arbitrary
  shrink (AchiVolume p a) = AchiVolume <$> shrink p <*> shrink a
