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
  if M.isNothing value
    then E.callError "Error: api.googlemaps.endpoint config value not found"
    else return $ Text.pack $ M.fromJust value

key :: IO Text
key = do
  value <- Config.getValue "api.googlemaps.key"
  if M.isNothing value
    then E.callError "Error: api.googlemaps.key not found"
    else return $ Text.pack $ M.fromJust value

apiRequestOk :: Text -> Bool
apiRequestOk t = if t == "OK" || t == "ok"
  then True
  else False

getGeoLocFromString :: String -> IO (Maybe GeoLoc)
getGeoLocFromString address = do
  endpoint <- endpoint
  key <- key
  let address' = Text.pack $ Utils.replaceCharByCharInString ' ' '+' address
  let opts = defaults & param "key" .~ [key] & param "address" .~ [address']
  req <- getWith opts (Text.unpack endpoint)
  let headerStatusCode = req ^. responseStatus . statusCode
  let apiStatus = req ^. responseBody . Lens.key "status" . Lens._String
  if Json.httpRequestOk headerStatusCode && apiRequestOk apiStatus
    then return (decode $ req ^. responseBody)
    else return $ Nothing
