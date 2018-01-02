{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (last)

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import System.Cron

import Data.Maybe as M

import User
import Interest
import Mail
import Location
import Html
import Error as E

createNewGeoLoc :: String -> IO Location.GeoLoc
createNewGeoLoc address = do
  geoLocAttempt <- Location.getGeoLocFromString address
  if M.isNothing geoLocAttempt
    then E.callError "Error. Main. createNewGeoLoc: failed when requesting location coordinates from address"
    else return $ M.fromJust geoLocAttempt

createNewUser :: IO User
createNewUser = do
  putStrLn "** NEW USER **"
  putStrLn "> First Name:"
  userFirstName <- getLine
  putStrLn "> Last Name:"
  userLastName <- getLine
  let userName = User.newName userFirstName userLastName
  putStrLn "> E-mail Address:"
  userEmail <- getLine
  let userEmail' = Mail.stringToAddress userEmail
  putStrLn "> User's location (e.g: 'rosario, santa fe, argentina'):"
  userLocationAddress <- getLine
  userGeoLoc <- createNewGeoLoc userLocationAddress
  putStrLn "Interests (keywords separated by a space):"
  userInterestsString <- getLine
  let userInterests = Interest.toDataType $ words userInterestsString
  newUser <- User.newUser userName userEmail' userGeoLoc userInterests
  return $ newUser

sendWelcomeMailToUser :: User -> IO ()
sendWelcomeMailToUser user = do
  let userEmailAddress = User.getEmail user
  conn <- Mail.connect
  Mail.auth conn
  Mail.send conn userEmailAddress "Welcome to dailyHASK" "plain text body" (Html.renderWelcomeMailTemplate user)
  Mail.closeConnection conn
  return $ ()

main :: IO ()
main = do
  putStrLn ".::. Welcome to dailyHASK .::."
  newUser <- createNewUser
  sendWelcomeMailToUser newUser
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
