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
import Json

import Data.Aeson
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

getAddress :: GeoLoc -> Address
getAddress = address

getLat :: GeoLoc -> Latitude
getLat = lat

getLong :: GeoLoc -> Longitude
getLong = long

stringToAddress :: String -> Address
stringToAddress = Text.pack

addressToString :: Address -> String
addressToString = Text.unpack

endpoint :: IO Url
endpoint = do
  value <- Config.getValue "api.googlemaps.endpoint"
  return (M.fromJust value)

key :: IO Text
key = do
  value <- Config.getValue "api.googlemaps.key"
  return (Text.pack $ M.fromJust value)

getGeoLocFromString :: String -> IO (Maybe GeoLoc)
getGeoLocFromString address = do
  endpoint <- endpoint
  key <- key
  let address' = Text.pack $ Utils.replaceCharInString ' ' '+' address
  let opts = defaults & param "key" .~ [key] & param "address" .~ [address']
  req <- getWith opts endpoint
  let headerStatus = req ^. responseStatus . statusCode
  if Json.requestIsOk headerStatus
    then return (decode $ req ^. responseBody)
    else return (Nothing)
