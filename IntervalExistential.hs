
{-# LANGUAGE ExistentialQuantification #-}

module Interval where

import qualified Data.Text as Text
import Data.Text(Text)

data Interval = forall a . Ord a => Interval a a

pointInt :: Int -> Interval
pointInt i = IntInterval i i

pointDouble :: Double -> Interval
pointDouble d = Doublterval d d

pointText :: Text -> Interval
pointText t = Texterval t t

overlaps :: Interval -> Interval -> Bool
overlaps (IntInterval a0 a1) (IntInterval b0 b1) = tupleOverlaps (a0, a1) (b0, b1)
overlaps (Doublterval a0 a1) (Doublterval b0 b1) = tupleOverlaps (a0, a1) (b0, b1)
overlaps (Texterval a0 a1) (Texterval b0 b1) = tupleOverlaps (a0, a1) (b0, b1)

tupleOverlaps :: Ord a => (a, a) -> (a, a) -> Bool
tupleOverlaps (a0, a1) (b0, b1)
  | a0 > b1 = False
  | b0 > a1 = False
  | a1 < b0 = False
  | b1 < a0 = False
  | otherwise = True

merge :: Interval -> Interval -> Interval
merge (IntInterval a1 a2) (IntInterval b1 b2) = IntInterval (min a1 b1) (max a2 b2)
merge (Doublterval a1 a2) (Doublterval b1 b2) = Doublterval (min a1 b1) (max a2 b2)
merge (Texterval a1 a2) (Texterval b1 b2) = Texterval (min a1 b1) (max a2 b2)
