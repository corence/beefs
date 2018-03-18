
module Volume where

class Eq a => Volume a where
  merge :: a -> a -> a
  intersects :: a -> a -> Bool
  contains :: a -> a -> Bool
