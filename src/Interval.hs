
module Interval where

import Debug.Trace

data Interval a = Interval a a

instance Show a => Show (Interval a) where
  show (Interval a b) = "(" ++ show a ++ "-" ++ show b ++ ")"

unit :: a -> Interval a
unit a = Interval a a

interval :: Ord a => a -> a -> Interval a
interval a0 a1 = if a0 <= a1 then Interval a0 a1 else error "but this interval is bass-ackwards"

merge :: (Show a, Ord a) => Interval a -> Interval a -> Interval a
merge (Interval a0 a1) (Interval b0 b1)
  = trace ("{{merging " ++ show a0 ++ "," ++ show a1 ++ " with " ++ show b0 ++ "," ++ show b1 ++ "}}")
  $ Interval (min a0 b0) (max a1 b1)

intersects :: Ord a => Interval a -> Interval a -> Bool
intersects (Interval a0 a1) (Interval b0 b1) = b1 >= a0 && b0 <= a1

contains :: Ord a => Interval a -> Interval a -> Bool
contains (Interval a0 a1) (Interval b0 b1) = a0 <= b0 && a1 >= b1
