
-- A default volume type for RTree.

module Rect where
import Volume(Volume)
import Interval(Interval)
import qualified Volume
import qualified Interval

data Rect = Rect (Interval Double) (Interval Double) deriving (Show, Eq)

instance Volume Rect where
  intersects (Rect x1 y1) (Rect x2 y2) = Interval.intersects x1 x2 && Interval.intersects y1 y2
  merge (Rect x1 y1) (Rect x2 y2) = Rect (Interval.merge x1 x2) (Interval.merge y1 y2)
  contains (Rect x1 y1) (Rect x2 y2) = Interval.contains x1 x2 && Interval.contains y1 y2
