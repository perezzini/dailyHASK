{-# LANGUAGE OverloadedStrings #-}

module Database
    (
    open
    , close
    , insert
    , insertMany
    , count
    , save
    , delete
    ) where

import qualified Database.MongoDB as MongoDB
import qualified Config
import qualified Data.Text as Text
import qualified Data.Maybe as M
import qualified Data.Bson as Bson

import Error as E

server :: IO String
server = do
  value <- Config.getValue "database.server"
  if M.isNothing value
    then E.callError "Error: database.server config value not found"
    else return (M.fromJust value)

port :: IO String
port = do
  value <- Config.getValue "database.port"
  if M.isNothing value
    then E.callError "Error: database.port config value not found"
    else return (M.fromJust value)

db :: IO String
db = do
  value <- Config.getValue "database.db"
  if M.isNothing value
    then E.callError "Error: database.db config file value not found"
    else return (M.fromJust value)

open :: IO MongoDB.Pipe
open = do
  server <- server
  pipe <- MongoDB.connect $ MongoDB.host server
  return (pipe)

close :: MongoDB.Pipe -> IO ()
close pipe = MongoDB.close pipe

run :: MongoDB.Pipe -> MongoDB.Action IO a -> IO a
run pipe act = do
  db <- db
  let db' = Text.pack db
  exec <- MongoDB.access pipe MongoDB.master db' act
  return (exec)

insert :: MongoDB.Pipe -> MongoDB.Collection -> Bson.Document -> IO Bson.Value
insert pipe collection document = do
  exec <- run pipe $ MongoDB.insert collection document
  return (exec)

insertMany :: MongoDB.Pipe -> MongoDB.Collection -> [Bson.Document] -> IO [Bson.Value]
insertMany pipe collection documents = do
  exec <- run pipe $ MongoDB.insertMany collection documents
  return (exec)

count :: MongoDB.Pipe -> MongoDB.Selector -> MongoDB.Collection -> IO Int
count pipe fields collection = do
  exec <- run pipe $ MongoDB.count $ MongoDB.select fields collection
  return (exec)

save :: MongoDB.Pipe -> MongoDB.Collection -> MongoDB.Selector -> IO ()
save pipe collection fields = do
  exec <- run pipe $ MongoDB.save collection fields
  return (exec)

delete :: MongoDB.Pipe -> MongoDB.Selector -> MongoDB.Collection -> IO ()
delete pipe fields collection = do
  exec <- run pipe $ MongoDB.delete $ MongoDB.select fields collection
  return (exec)
