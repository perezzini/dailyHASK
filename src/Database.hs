{-|
Module      : Database

Asbtraction of the Haskell MongoDB driver
-}

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
    , findOne
    , findAll
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
    else return $ M.fromJust value

port :: IO String
port = do
  value <- Config.getValue "database.port"
  if M.isNothing value
    then E.callError "Error: database.port config value not found"
    else return $ M.fromJust value

db :: IO String
db = do
  value <- Config.getValue "database.db"
  if M.isNothing value
    then E.callError "Error: database.db config file value not found"
    else return $ M.fromJust value

-- |'open' returns a TCP connection to a database defined in the
-- /app.cfg file
open :: IO MongoDB.Pipe
open = do
  server <- server
  pipe <- MongoDB.connect $ MongoDB.host server
  return $ pipe

-- |The 'close' function closes a TCP connection to the database opened with 'open'
close :: MongoDB.Pipe -> IO ()
close pipe = MongoDB.close pipe

run :: MongoDB.Pipe -> MongoDB.Action IO a -> IO a
run pipe act = do
  db <- db
  let db' = Text.pack db
  exec <- MongoDB.access pipe MongoDB.master db' act
  return $ exec

-- |The 'insert' function inserts a document into a given collection
insert :: MongoDB.Pipe -> MongoDB.Collection -> Bson.Document -> IO Bson.Value
insert pipe collection document = do
  exec <- run pipe $ MongoDB.insert collection document
  return $ exec

-- |The 'insertMany' function inserts a set of documents into a given collection
insertMany :: MongoDB.Pipe -> MongoDB.Collection -> [Bson.Document] -> IO [Bson.Value]
insertMany pipe collection documents = do
  exec <- run pipe $ MongoDB.insertMany collection documents
  return $ exec

-- |The 'count' function counts documents, from a collection, that preserve some property
count :: MongoDB.Pipe -> MongoDB.Selector -> MongoDB.Collection -> IO Int
count pipe fields collection = do
  exec <- run pipe $ MongoDB.count $ MongoDB.select fields collection
  return $ exec

-- |The 'save' function updates documents, from a collection, that preserve some property
save :: MongoDB.Pipe -> MongoDB.Collection -> MongoDB.Selector -> IO ()
save pipe collection fields = do
  exec <- run pipe $ MongoDB.save collection fields
  return $ exec

-- |The 'delete' function drops documents, from a collection, that preserve some property
delete :: MongoDB.Pipe -> MongoDB.Selector -> MongoDB.Collection -> IO ()
delete pipe fields collection = do
  exec <- run pipe $ MongoDB.delete $ MongoDB.select fields collection
  return $ exec

-- |The 'findOne' function fetches just one document, the first one from a collection, that preserve some property
findOne :: MongoDB.Pipe -> MongoDB.Selector -> MongoDB.Collection -> IO (Maybe Bson.Document)
findOne pipe fields collection = do
  exec <- run pipe $ MongoDB.findOne $ MongoDB.select fields collection
  return $ exec

-- |The 'findAll' function fetches all documents from a given collection, that preserve some property
findAll :: MongoDB.Pipe -> MongoDB.Selector -> MongoDB.Collection -> IO [Bson.Document]
findAll pipe fields collection = do
  exec <- run pipe $ MongoDB.find (MongoDB.select fields collection) >>= MongoDB.rest
  return $ exec
