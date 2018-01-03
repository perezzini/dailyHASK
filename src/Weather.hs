{-# LANGUAGE OverloadedStrings #-}

module Weather
    (
    Weather(..)
    , getTemp
    , getPressure
    , getHumidity
    , getCurrentWeatherFromGeoLoc
    ) where

import Config
import Url
import Location
import Utils
import Json
import Error as E

import Data.Text as Text
import Data.Maybe as M

import Data.Aeson
import qualified Data.Aeson.Lens as Lens (key, _Integer)
import Network.Wreq
import Control.Lens

data Weather = Weather {
  temp :: Double
  , pressure :: Int
  , humidity :: Int
} deriving (Show)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \v -> do
    main <- v .: "main"
    temp <- main .: "temp"
    pressure <- main .: "pressure"
    humidity <- main .: "humidity"
    return (Weather temp pressure humidity)

getTemp :: Weather -> Double
getTemp = temp

getPressure :: Weather -> Int
getPressure = pressure

getHumidity :: Weather -> Int
getHumidity = humidity

-- getWindSpeed :: Weather -> Double
-- getWindSpeed = windSpeed
--
-- getRain :: Weather -> Int
-- getRain = rain
--
-- getSnow :: Weather -> Int
-- getSnow = snow

endpoint :: IO Url
endpoint = do
  value <- Config.getValue "api.owm.endpoint.current"
  if M.isNothing value
    then E.callError "Error: api.owm.endpoint.current config value not found"
    else return $ Text.pack $ M.fromJust value

key :: IO Text
key = do
  value <- Config.getValue "api.owm.key"
  if M.isNothing value
    then E.callError "Error: api.owm.key not found"
    else return $ Text.pack $ M.fromJust value

apiRequestOk :: Maybe Integer -> Bool
apiRequestOk (Just n) = if n == 200
  then True
  else False
apiRequestOk _ = False

getCurrentWeatherFromGeoLoc :: Location.GeoLoc -> IO (Maybe Weather)
getCurrentWeatherFromGeoLoc geoLoc = do
  endpoint <- endpoint
  key <- key
  let lat = Text.pack $ show $ Location.getLat geoLoc
  let long = Text.pack $ show $ Location.getLong geoLoc
  let opts = defaults & param "APPID" .~ [key]
            & param "lat" .~ [lat]
            & param "lon" .~ [long]
  req <- getWith opts (Text.unpack endpoint)
  let headerStatusCode = req ^. responseStatus . statusCode
  let apiStatus = req ^? responseBody . Lens.key "cod" . Lens._Integer
  if Json.httpRequestOk headerStatusCode && apiRequestOk apiStatus
    then return (decode $ req ^. responseBody)
    else return $ Nothing
