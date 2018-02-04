{-|
Module      : Mail

HaskellNet-SSL package abstraction
-}

{-# LANGUAGE OverloadedStrings #-}

module Mail
    (
    Address
    , Subject
    , stringToAddress
    , addressToString
    , connect
    , connectAndLogin
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

-- |The 'stringToAddress' function maps a string to an 'Address' value
stringToAddress :: String -> Address
stringToAddress = Text.pack

-- |The 'addressToString' function maps an 'Address' value to a string
addressToString :: Address -> String
addressToString = Text.unpack

-- |The 'isAddressValid' function takes an 'Address' and validates it according
-- to the HTML standard
isAddressValid :: Address -> Bool
isAddressValid address = EValidate.isValidEmail address

smtpHostname :: IO String
smtpHostname = do
  value <- Config.getValue "smtp.hostname"
  return $ value

smtpUserName :: IO String
smtpUserName = do
  value <- Config.getValue "smtp.user.name.address"
  return $ value

smtpUserNamePassword :: IO String
smtpUserNamePassword = do
  value <- Config.getValue "smtp.user.password"
  return $ value

smtpMailAddressAlias :: IO String
smtpMailAddressAlias = do
  value <- Config.getValue "smtp.mail.address.alias"
  return $ value

-- |'connect' connects to an SMTP SSL server using a SMTP hostname from the config file
connect :: IO SMTPConnection
connect = do
  hostname <- smtpHostname
  conn <- SMTP.connectSMTPSSL hostname
  return $ conn

-- |'connectAndLogin' connects to an SMTP SSL server using a SMTP hostname, user name and password
-- from the config file, and logs in.
connectAndLogin :: IO SMTPConnection
connectAndLogin = do
  hostname <- smtpHostname
  conn <- SMTP.connectSMTPSSL hostname
  userName <- smtpUserName
  userNamePassword <- smtpUserNamePassword
  status <- SMTP.authenticate LOGIN userName userNamePassword conn
  if not status
    then E.callError "Mail. Auth denied. Aborting..."
    else return $ conn

-- |The 'closeConnection' function takes a SMTP connection and closes it
closeConnection :: SMTPConnection -> IO ()
closeConnection conn = SMTP.closeSMTP conn

-- |The 'auth' function takes a SMTP connection and logs in using predefined SMTP user name, user password, and an
-- established SMTP connection
auth :: SMTPConnection -> IO ()
auth conn = do
  userName <- smtpUserName
  userNamePassword <- smtpUserNamePassword
  status <- SMTP.authenticate LOGIN userName userNamePassword conn
  if not status
    then E.callError "Mail. Auth denied. Aborting..."
    else return $ ()

-- |The 'send' function takes a SMTP connetion, a receiver, a subject, and a HTML body. It sends
-- the respective e-mail
send :: SMTPConnection -> Address -> Subject -> String -> IO ()
send conn receiver subject htmlBody = do
  let receiver' = Text.unpack receiver :: String
  senderAlias <- smtpMailAddressAlias
  let subject' = Text.unpack subject :: String
  let htmlBody' = L.pack htmlBody :: L.Text
  SMTP.sendMimeMail receiver' senderAlias subject' "" htmlBody' [] conn -- do not use plain text body
