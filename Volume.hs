
module Volume where

class Volume a where
  merge :: a -> a -> a
  intersects :: a -> a -> Bool
