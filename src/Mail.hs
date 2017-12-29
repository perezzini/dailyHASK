{-# LANGUAGE OverloadedStrings #-}

module Mail
    (
    Address
    , Subject
    , Host
    , stringToAddress
    , addressToString
    , createMailAndSend
    ) where

import Data.Text as Text
import Data.Maybe as M

import Config
import Error as E

import qualified Network.Mail.SMTP as SMTP
import qualified Network.Mail.Mime as Mime

type Address = Text
type Subject = Text
type Host = (Maybe Text, Address)

stringToAddress :: String -> Address
stringToAddress = Text.pack

addressToString :: Address -> String
addressToString = Text.unpack

hostname :: IO String
hostname = do
  value <- Config.getValue "mail.hostname"
  if M.isNothing value
    then E.callError "Error: mail.hostname config value not found"
    else return $ M.fromJust value

port :: IO Integer
port = do
  value <- Config.getValue "mail.port.number"
  if M.isNothing value
    then E.callError "Error: mail.port.number config value not found"
    else return $ read $ M.fromJust value

createMail :: Host -> Host -> Subject -> Mime.Part -> Mime.Part -> Mime.Mail
createMail (fm, fa) (tm, ta) subject body html = let
  from = SMTP.Address fm fa
  to = [SMTP.Address tm ta]
  in SMTP.simpleMail from to [] [] subject [body, html]

send :: Mime.Mail -> IO ()
send mail = do
  hostName <- hostname
  port <- port
  SMTP.sendMail' hostName (fromInteger port) mail

createMailAndSend :: Host -> Host -> Subject -> Mime.Part -> Mime.Part -> IO ()
createMailAndSend from to subject body html = do
  let mail = createMail from to subject body html
  send mail
