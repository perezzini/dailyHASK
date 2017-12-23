{-# LANGUAGE OverloadedStrings #-}

module User
    (
    User(..)
    , getID
    , getName
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

import Data.Bson as Bson
import qualified Data.Maybe as M
import Data.Text as Text hiding (last)

-- ObjectID
type ID = Bson.Value

type Email = Text

data Name = Name {
  first :: Text
  , last :: Text
} deriving (Show)

getFirstName :: Name -> Text
getFirstName = first

getLastName :: Name -> Text
getLastName = last

data User = User {
  _id :: ID
  , name :: Name
  , email :: Email
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

getName :: User -> (Text, Text)
getName user = let
  name = name
  in (getFirstName name, getLastName name)

getEmail :: User -> Email
getEmail = email

getLocation :: User -> Location.GeoLoc
getLocation = location

getInterests :: User -> [Interest]
getInterests = interests

newUserData :: ID -> Name -> Email -> Location.GeoLoc -> User
newUserData id name email location = User {
  _id = id
  , name = name
  , email = email
  , location = location
  , interests = []
}

newUser :: Name -> Email -> Location.GeoLoc -> IO User
newUser name email location = do
  let firstName = getFirstName name
  let lastName = getLastName name
  let locationAddress = Location.getAddress location
  let locationLat = Location.getLat location
  let locationLong = Location.getLong location

  let postName = ["firstName" =: firstName
                  , "lastName" =: lastName] :: Bson.Document
  let locationPost = ["address" =: locationAddress
                      , "lat" =: locationLat
                      , "long" =: locationLong] :: Bson.Document
  let post = ["name" =: postName
              , "email" =: email
              , "location" =: locationPost] :: Bson.Document

  collection <- collection
  pipe <- DB.open
  _id <- DB.insert pipe collection post
  DB.close pipe

  let user = newUserData _id name email location
  return (user)

deleteUser :: ID -> IO ()
deleteUser _id = do
  let selection = ["_id" =: _id] :: Bson.Document
  collection <- collection
  pipe <- DB.open
  DB.delete pipe selection collection
  DB.close pipe
  return ()
