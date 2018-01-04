{-# LANGUAGE OverloadedStrings #-}

module Mail
    (
    Address
    , Subject
    , stringToAddress
    , addressToString
    , connect
    , closeConnection
    , auth
    , send
    , isAddressValid
    ) where

import Data.Text as Text
import qualified Data.Text.Lazy as L
import Data.Maybe as M
import Text.Html.Email.Validate as EValidate

import Config
import Error as E

import Network.HaskellNet.SMTP.SSL as SMTP

type Address = Text
type Subject = Text

stringToAddress :: String -> Address
stringToAddress = Text.pack

addressToString :: Address -> String
addressToString = Text.unpack

isAddressValid :: Address -> Bool
isAddressValid address = EValidate.isValidEmail address

smtpHostname :: IO String
smtpHostname = do
  value <- Config.getValue "smtp.hostname"
  if M.isNothing value
    then E.callError "Error: smtp.hostname config value not found"
    else return $ M.fromJust value

smtpUserName :: IO String
smtpUserName = do
  value <- Config.getValue "smtp.user.name.address"
  if M.isNothing value
    then E.callError "Error: smtp.user.name.address config value not found"
    else return $ M.fromJust value

smtpUserNamePassword :: IO String
smtpUserNamePassword = do
  value <- Config.getValue "smtp.user.password"
  if M.isNothing value
    then E.callError "Error: smtp.user.password config value not found"
    else return $ M.fromJust value

smtpMailAddressAlias :: IO String
smtpMailAddressAlias = do
  value <- Config.getValue "smtp.mail.address.alias"
  if M.isNothing value
    then E.callError "Error: smtp.mail.address.alias config value not found"
    else return $ M.fromJust value

connect :: IO SMTPConnection
connect = do
  hostname <- smtpHostname
  conn <- SMTP.connectSMTPSSL hostname
  return $ conn

closeConnection :: SMTPConnection -> IO ()
closeConnection conn = SMTP.closeSMTP conn

auth :: SMTPConnection -> IO ()
auth conn = do
  userName <- smtpUserName
  userNamePassword <- smtpUserNamePassword
  status <- SMTP.authenticate LOGIN userName userNamePassword conn
  if not status
    then E.callError "Mail. Auth denied. Aborting..."
    else return $ ()

send :: SMTPConnection -> Text -> Text -> Text -> String -> IO ()
send conn receiver subject plainTextBody htmlBody = do
  let receiver' = Text.unpack receiver :: String
  senderAlias <- smtpMailAddressAlias
  let subject' = Text.unpack subject :: String
  let plainTextBody' = L.pack $ Text.unpack plainTextBody :: L.Text
  let htmlBody' = L.pack htmlBody :: L.Text
  SMTP.sendMimeMail receiver' senderAlias subject' plainTextBody' htmlBody' [] conn
