
module Interval where

data Interval a = Interval a a

instance Show a => Show (Interval a) where
  show (Interval a b) = "((" ++ show a ++ ")~(" ++ show b ++ "))"

instance Eq a => Eq (Interval a) where
  (Interval min0 max0) == (Interval min1 max1) = min0 == min1 && max0 == max1

unit :: a -> Interval a
unit a = Interval a a

interval :: Ord a => a -> a -> Interval a
interval a0 a1 = if a0 <= a1 then Interval a0 a1 else error "but this interval is bass-ackwards"

merge :: Ord a => Interval a -> Interval a -> Interval a
merge (Interval a0 a1) (Interval b0 b1) = Interval (min a0 b0) (max a1 b1)

intersects :: Ord a => Interval a -> Interval a -> Bool
intersects (Interval a0 a1) (Interval b0 b1) = b1 >= a0 && b0 <= a1

contains :: Ord a => Interval a -> Interval a -> Bool
contains (Interval a0 a1) (Interval b0 b1) = a0 <= b0 && a1 >= b1
