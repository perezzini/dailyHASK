{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (last)

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Time.Clock (getCurrentTime)
import System.Cron as Cron (daily, scheduleMatches)

import Data.Maybe as M
import Data.Text as Text hiding (words, unword, map)
import qualified Data.Bson as Bson
import Network.HaskellNet.SMTP.SSL as SMTP

import User
import Interest
import Mail
import News
import Location
-- import Weather
import Html
import Database as DB
import Error as E
import Config

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
  return ()

collection :: IO Text
collection = do
  value <- Config.getValue "database.usersCollection"
  let value' = M.fromJust value
  return $ Text.pack value'

doWork :: IO ()
doWork = let
  workActions :: SMTPConnection -> Bson.Document -> IO ()
  workActions conn user = do
    let email = Bson.typed $ Bson.valueAt "email" user :: Text
    let interests = Bson.typed $ Bson.valueAt "interests" user :: [Interest]
    news <- News.getNews interests
    if M.isNothing news
      then E.callError "Error. Main: couldn't retrive news articles. Aborting..."
      else let
        news' = M.fromJust news
        in Mail.send conn email "Your dailyHASK" "plain text body" (Html.renderDailyMailTemplate news')

  work :: [Bson.Document] -> SMTPConnection -> IO ()
  work users conn = do
    mapM_ (workActions conn) users
  in do
    putStrLn "doWork started..."
    collection <- collection
    pipe <- DB.open
    users <- DB.findAll pipe [] collection
    DB.close pipe
    conn <- Mail.connect
    Mail.auth conn
    work users conn
    Mail.closeConnection conn
    putStrLn "doWork finished."

main :: IO ()
main = do
  putStrLn ".::. Welcome to dailyHASK .::."
  newUser <- createNewUser
  sendWelcomeMailToUser newUser
  putStrLn "Want to create another user? [Y/N]"
  line <- getLine
  if line == "Y" || line == "y"
    then main
    else forever $ do
      now <- getCurrentTime
      when (scheduleMatches schedule now) doWork
    where
      schedule = Cron.daily
