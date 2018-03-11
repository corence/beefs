
module Value where

import Data.Text(Text)
import Count(Count)

data Value = IntValue Int | DoubleValue Double | TextValue Text | CountValue Count deriving (Ord, Eq)
