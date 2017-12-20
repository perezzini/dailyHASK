module Main where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import System.Cron

main :: IO ()
main = do
  putStrLn "getUserData()"
  putStrLn "updateDB()"
  putStrLn "Want to create another user? [Y/N]"
  line <- getLine
  if line == "Y"
    then main
    else forever $ do
      now <- getCurrentTime
      when (scheduleMatches schedule now) doWork
      putStrLn "Sleeping..."
      --threadDelay 10000000
    where
      doWork = putStrLn "Do the real work: iterate DB, retrieve information, and send e-mails, etc."
      schedule = daily
