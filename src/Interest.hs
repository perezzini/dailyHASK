module Interest
    (
    Interest
    , toDataType
    , fromDataType
    ) where

import Data.Text as Text hiding (map)

type Interest = Text

toDataType :: [String] -> [Interest]
toDataType list = map pack list

fromDataType :: [Interest] -> [String]
fromDataType list = map unpack list
