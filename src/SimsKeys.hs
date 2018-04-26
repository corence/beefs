
module SimsKeys where

import Data.Function((&))

data ItemType
  = Food
  | ChoppedIngredients
  | RawIngredients
  | Oven
  | Fridge
  deriving (Show, Eq, Ord)

data ItemPosition
  = Floor
  | Inventory
  deriving (Show, Eq, Ord)

data Key
  = X
  | Y
  | Z
  | Victory
  | Item ItemPosition ItemType
  | Cash
  | Delay
  deriving (Show, Eq, Ord)
