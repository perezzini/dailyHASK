{-|
Module      : Schedule

Abstraction of 'System.Cron' library: definition of certain functions concerning actual project
-}

module Schedule
    (
    createScheduleFromText
    ) where

import System.Cron.Parser (parseCronSchedule)
import System.Cron.Types (CronSchedule)
import Data.Text (Text)

-- |The 'createScheduleFromText' function takes a text corresponding to a
-- specification of a cron job and maybe converts it to a 'CronSchedule'
-- type value. It returns 'Nothing' in case parsing fails
createScheduleFromText :: Text -> Maybe CronSchedule
createScheduleFromText spec = case parseCronSchedule spec of
  Right cron -> Just cron
  otherwise -> Nothing
