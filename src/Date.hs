module Date
    (
    today
    , getCurrentTimeFromServer
    ) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

getCurrentTimeFromServer :: IO UTCTime
getCurrentTimeFromServer = do
  tz <- getCurrentTimeZone
  now @ (UTCTime day _) <- getCurrentTime
  let localDiffTime = timeOfDayToTime $ localTimeOfDay $ utcToLocalTime tz now :: DiffTime
  return $ UTCTime day localDiffTime

today :: IO String
today = do
  UTCTime day _ <- getCurrentTime
  return $ show $ day
