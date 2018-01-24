{-|
Module      : Location

Definition of 'GeoLoc' data type along multiple functions concerning 'GeoLoc' values. Defines
-- a GET request to retrieve geographic location from Google Maps API services
-}

{-# LANGUAGE OverloadedStrings #-}

module Location
    (
    GeoLoc(..)
    , Address
    , Latitude
    , Longitude
    , stringToAddress
    , addressToString
    , getAddress
    , getLat
    , getLong
    , getGeoLocFromString
    ) where

import Config
import Url
import Utils
import Http
import Error as E

import Data.Aeson
import qualified Data.Aeson.Lens as Lens (key, _String)
import Network.Wreq
import Control.Lens

import Data.Text as Text
import Data.Maybe as M
import qualified Data.Vector as V

type Latitude = Double
type Longitude = Double
type Address = Text

data GeoLoc = GeoLoc {
  address :: Address
  , lat :: Latitude
  , long :: Longitude
} deriving (Show)

-- Here is where we parse the GET request response object.
-- We just want the following fields: "formatted_address",
-- "lat", and "lng"
instance FromJSON GeoLoc where
  parseJSON = withObject "GeoLoc" $ \v -> do
    results <- v .: "results"
    let resultsItemObject = V.head results
    address <- resultsItemObject .: "formatted_address"
    geometry <- resultsItemObject .: "geometry"
    location <- geometry .: "location"
    lat <- location .: "lat"
    long <- location .: "lng"
    return (GeoLoc address lat long)

-- |The 'getAddress' function takes a 'GeoLoc' value and extracts the address value from it
getAddress :: GeoLoc -> Address
getAddress = address

-- |The 'getLat' function extracts 'lat' value from a 'GeoLoc' value
getLat :: GeoLoc -> Latitude
getLat = lat

-- |The 'getLong' function extracst 'long' value from a 'GeoLoc' value
getLong :: GeoLoc -> Longitude
getLong = long

-- |The 'stringToAddress' function maps a string to an 'Address' value
stringToAddress :: String -> Address
stringToAddress = Text.pack

-- |The 'addressToString' function maps an 'Address' value to a string
addressToString :: Address -> String
addressToString = Text.unpack

endpoint :: IO Url
endpoint = do
  value <- Config.getValue "api.googlemaps.endpoint"
  return $ Text.pack value

key :: IO Text
key = do
  value <- Config.getValue "api.googlemaps.key"
  return $ Text.pack value

apiRequestOk :: Text -> Bool
apiRequestOk t = if t == "OK" || t == "ok"
  then True
  else False

-- |The 'getGeoLocFromString' takes a string geographic location address and makes a
-- GET request to Google Maps API services to retrieve 'lat' and 'long'.
-- It returns 'Nothing' in case the request fails
getGeoLocFromString :: String -> IO (Maybe GeoLoc)
getGeoLocFromString address = do
  putStrLn "Start of GET request from locations API..."
  endpoint <- endpoint
  key <- key
  let address' = Text.pack $ Utils.replaceCharByCharInString ' ' '+' address
  let opts = defaults & param "key" .~ [key] & param "address" .~ [address']
  req <- getWith opts (Text.unpack endpoint)
  let headerStatusCode = req ^. responseStatus . statusCode
  let apiStatus = req ^. responseBody . Lens.key "status" . Lens._String
  putStrLn "End of GET request from locations API"
  if Http.isGETRequestOk headerStatusCode && apiRequestOk apiStatus
    then return (decode $ req ^. responseBody)
    else return $ Nothing
