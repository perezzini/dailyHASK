{-|
Module      : Date

Abstraction of 'Date.Time' library: definition of certain functions concerning actual project
-}

module Date
    (
    today
    , getCurrentTimeFromServer
    ) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

-- |'getCurrentTimeFromServer' returns current time of server using actual time zone, and converts it to 'UTCTime' format
getCurrentTimeFromServer :: IO UTCTime
getCurrentTimeFromServer = do
  tz <- getCurrentTimeZone
  now @ (UTCTime day _) <- getCurrentTime
  let localDiffTime = timeOfDayToTime $ localTimeOfDay $ utcToLocalTime tz now :: DiffTime
  return $ UTCTime day localDiffTime

-- |'today' returns current y-m-d format
today :: IO String
today = do
  UTCTime day _ <- getCurrentTime
  return $ show $ day
