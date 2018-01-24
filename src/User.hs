{-|
Module      : User

Definition of the 'User' data type along multiple functions concerning 'User' values. Creates
and uploads new users to database
-}


{-# LANGUAGE OverloadedStrings #-}

module User
    (
    User(..)
    , Name(..)
    , ID
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
import Error as E

import Data.Bson as Bson
import qualified Data.Maybe as M
import Data.Text as Text hiding (last)

-- ObjectID
type ID = Bson.Value

data Name = Name {
  first :: Text
  , last :: Text
} deriving (Show)

-- |The 'getFirstName' function takes a 'Name' and returns its 'first' name
getFirstName :: Name -> Text
getFirstName = first

-- |The 'getLastName' function takes a 'Name' and returns its 'last' name
getLastName :: Name -> Text
getLastName = last

-- |The 'newName' function takes a two strings concerning first, and last name, and returns a 'Name'
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
  return $ Text.pack value

-- |The 'getID' function takes a 'User' and returns its '_id'
getID :: User -> ID
getID = _id

-- |The 'getName' function takes a 'User' and returns its 'name'
getName :: User -> Name
getName user = name user

-- |The 'getEmail' function takes a 'User' and returns its 'email' address
getEmail :: User -> Mail.Address
getEmail = email

-- |The 'getLocation' function takes a 'User' and returns its geographic 'location'
getLocation :: User -> Location.GeoLoc
getLocation = location

-- |The 'getInterests' function takes a 'User' and returns its corresponding 'interests'
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

-- |The 'newUser' function takes a information data about a user, and maybe returns a new 'User'. It first
-- creates a 'User' value, and then uploads it to database
newUser :: Name -> Mail.Address -> Location.GeoLoc -> [Interest] -> IO (Maybe User)
newUser name email location interests = case Mail.isAddressValid email of
  True -> do
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

    putStrLn "Uploading new user data to DB..."
    collection <- collection
    pipe <- DB.open
    _id <- DB.insert pipe collection post
    DB.close pipe
    putStrLn ("User uploaded correctly. ID = " ++ (show _id))

    let user = newUserData _id name email location interests
    return $ Just user
  otherwise -> return $ Nothing

-- |The 'deleteUser' function takes a user's ID, and then deletes it from database
deleteUser :: ID -> IO ()
deleteUser _id = do
  let selection = ["_id" =: _id] :: Bson.Document
  collection <- collection
  pipe <- DB.open
  DB.delete pipe selection collection
  DB.close pipe
  return ()
