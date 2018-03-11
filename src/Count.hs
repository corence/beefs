
module Count
( Count
, count
, merge
) where

newtype Count = Count Int deriving (Ord, Eq, Show)

count :: Int -> Count
count n = if n < 0
            then error ("count can't be negative, but i was given " ++ show n)
            else Count n

merge :: Count -> Count -> Count
merge (Count a) (Count b) = count (a + b)
