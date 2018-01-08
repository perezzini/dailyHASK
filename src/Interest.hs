{-|
Module      : Interest

Definition of 'Interest' type along multiple functions concerning 'Interest' values
-}

module Interest
    (
    Interest
    , toDataType
    , fromDataType
    ) where

import Data.Text as Text hiding (map)

type Interest = Text

-- |The 'toDataType' maps a list of strings to a list of 'Interest' values
toDataType :: [String] -> [Interest]
toDataType list = map pack list

-- |The 'fromDataType' maps a list of 'Interest' values to a list of strings
fromDataType :: [Interest] -> [String]
fromDataType list = map unpack list
