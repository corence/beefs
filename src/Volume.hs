
module Volume where

class Eq a => Volume a where
  -- Merge law:
  -- If a is merged with b to produce c, then,
  --  any q that would intersect with a, must also intersect with c
  merge :: a -> a -> a

  -- Intersects law:
  -- Intersect is commutative and transitive.
  -- TODO: transitive? imo that isn't necessary
  intersects :: a -> a -> Bool

  -- Contains law:
  -- Contains is transitive.
  -- If a contains b, then a intersects b and b intersects a.
  contains :: a -> a -> Bool
