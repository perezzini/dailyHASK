{-# LANGUAGE OverloadedStrings #-}

module User
    (
    User(..)
    , Name(..)
    , newName
    , getID
    , getName
    , getFirstName
    , getLastName
    , getEmail
    , getLocation
    , getInterests
    , newUser
    , deleteUser
    ) where

import Prelude hiding (last)

import Interest
import Location
import Database as DB
import Config
import Mail

import Data.Bson as Bson
import qualified Data.Maybe as M
import Data.Text as Text hiding (last)

-- ObjectID
type ID = Bson.Value

data Name = Name {
  first :: Text
  , last :: Text
} deriving (Show)

getFirstName :: Name -> Text
getFirstName = first

getLastName :: Name -> Text
getLastName = last

newName :: String -> String -> Name
newName first last = Name {
  first = Text.pack first
  , last = Text.pack last
}

data User = User {
  _id :: ID
  , name :: Name
  , email :: Mail.Address
  , location :: Location.GeoLoc
  , interests :: [Interest]
} deriving (Show)

collection :: IO Text
collection = do
  value <- Config.getValue "database.usersCollection"
  let value' = M.fromJust value
  return (Text.pack value')

getID :: User -> ID
getID = _id

getName :: User -> Name
getName user = name user

getEmail :: User -> Mail.Address
getEmail = email

getLocation :: User -> Location.GeoLoc
getLocation = location

getInterests :: User -> [Interest]
getInterests = interests

newUserData :: ID -> Name -> Mail.Address -> Location.GeoLoc -> [Interest] -> User
newUserData id name email location interests = User {
  _id = id
  , name = name
  , email = email
  , location = location
  , interests = interests
}

newUser :: Name -> Mail.Address -> Location.GeoLoc -> [Interest] -> IO User
newUser name email location interests = do
  let firstName = getFirstName name
  let lastName = getLastName name
  let locationAddress = Location.getAddress location
  let locationLat = Location.getLat location
  let locationLong = Location.getLong location

  let namePost = ["first" =: firstName
                  , "last" =: lastName] :: Bson.Document
  let locationPost = ["address" =: locationAddress
                      , "lat" =: locationLat
                      , "long" =: locationLong] :: Bson.Document
  let post = ["name" =: namePost
              , "email" =: email
              , "location" =: locationPost
              , "interests" =: interests] :: Bson.Document

  collection <- collection
  pipe <- DB.open
  _id <- DB.insert pipe collection post
  DB.close pipe

  let user = newUserData _id name email location interests
  return (user)

deleteUser :: ID -> IO ()
deleteUser _id = do
  let selection = ["_id" =: _id] :: Bson.Document
  collection <- collection
  pipe <- DB.open
  DB.delete pipe selection collection
  DB.close pipe
  return ()
