{-# LANGUAGE OverloadedStrings #-}

module Location
    (
    GeoLoc(..)
    , getAddress
    , getLat
    , getLong
    ) where

import Data.Text as Text

type Latitude = Double
type Longitude = Double
type Address = Text

data GeoLoc = GeoLoc {
  address :: Address
  , lat :: Latitude
  , long :: Longitude
} deriving (Show)

getAddress :: GeoLoc -> Address
getAddress = address

getLat :: GeoLoc -> Latitude
getLat = lat

getLong :: GeoLoc -> Longitude
getLong = long
