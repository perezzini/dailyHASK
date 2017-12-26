module Date
    (
    Date
    , getCurrentDate
    ) where

import Data.Time.Clock as Clock
import Data.Time.Calendar as Calendar

type Date = (Integer, Int, Int)

getCurrentDate :: IO Date
getCurrentDate = do
  now <- Clock.getCurrentTime
  let t @ (year, month, day) = Calendar.toGregorian $ Clock.utctDay now
  return t
