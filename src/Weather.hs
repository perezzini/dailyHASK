{-|
Module      : Weather

Definition of 'Weather' data type. Definition of functions to retrieve current
-- weather information from Open Weather Map API
-}

{-# LANGUAGE OverloadedStrings #-}

module Weather
    (
    Weather(..)
    , getTemp
    , getPressure
    , getHumidity
    , getDescription
    , getCurrentWeatherFromGeoLoc
    , kelvinToCelsius
    ) where

import Config
import Url
import Location
import Utils
import Http
import Error as E

import Data.Text as Text
import Data.Maybe as M
import qualified Data.Vector as V

import Data.Aeson
import qualified Data.Aeson.Lens as Lens (key, _Integer)
import Network.Wreq
import Control.Lens

data Weather = Weather {
  temp :: Double
  , pressure :: Int
  , humidity :: Int
  , description :: Maybe Text
} deriving (Show)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \v -> do
    main <- v .: "main"
    weather <- v .: "weather"
    let weatherObject = V.head weather
    temp <- main .: "temp"
    pressure <- main .: "pressure"
    humidity <- main .: "humidity"
    description <- weatherObject .: "description"
    return (Weather temp pressure humidity description)

-- |The 'getTemp' function takes a 'Weather' value and returns its 'temp'
getTemp :: Weather -> Double
getTemp = temp

-- |The 'getPressure' function takes a 'Weather' value and returns its 'pressure'
getPressure :: Weather -> Int
getPressure = pressure

-- |The 'getHumidity' function takes a 'Weather' value and returns its 'humidity'
getHumidity :: Weather -> Int
getHumidity = humidity

-- |The 'getHumidity' function takes a 'Weather' value and returns its 'description'
getDescription :: Weather -> Maybe Text
getDescription = description

endpoint :: IO Url
endpoint = do
  value <- Config.getValue "api.owm.endpoint.current"
  return $ Text.pack value

key :: IO Text
key = do
  value <- Config.getValue "api.owm.key"
  return $ Text.pack value

apiRequestOk :: Maybe Integer -> Bool
apiRequestOk (Just n) = if n == 200
  then True
  else False
apiRequestOk _ = False

-- |The 'getCurrentWeatherFromGeoLoc' takes a geographic location and returns its current weather using
-- the Open Weather Map API using latitude and longitude from corresponding location
getCurrentWeatherFromGeoLoc :: Location.GeoLoc -> IO (Maybe Weather)
getCurrentWeatherFromGeoLoc geoLoc = do
  putStrLn "[getCurrentWeatherFromGeoLoc] Start of GET request from weather API..."
  endpoint <- endpoint
  key <- key
  let lat = Text.pack $ show $ Location.getLat geoLoc
  let long = Text.pack $ show $ Location.getLong geoLoc
  let opts = defaults & param "appid" .~ [key]
            & param "lat" .~ [lat]
            & param "lon" .~ [long]
  req <- getWith opts (Text.unpack endpoint)
  let headerStatusCode = req ^. responseStatus . statusCode
  let apiStatus = req ^? responseBody . Lens.key "cod" . Lens._Integer
  putStrLn "[getCurrentWeatherFromGeoLoc] End of GET request from weather API"
  if Http.isGETRequestOk headerStatusCode && apiRequestOk apiStatus
    then return (decode $ req ^. responseBody)
    else return $ Nothing

-- |The 'kelvinToCelsius' function converts Kelvin to Celsius temperature
kelvinToCelsius :: Double -> Double
kelvinToCelsius k = k - 273.5
