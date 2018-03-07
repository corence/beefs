
module Volume where

import qualified Interval as Interval
import Interval(Interval)
import qualified Data.Text as Text
import Data.Text(Text)

data Volume = Volume [Interval]

pointInt :: Int -> Interval
pointInt i = IntInterval i i

pointDouble :: Double -> Interval
pointDouble d = Doublterval d d

pointText :: Text -> Interval
pointText t = Texterval t t
