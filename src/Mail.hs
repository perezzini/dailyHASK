module Mail
    (
    Address
    , stringToAddress
    , addressToString
    ) where

import Data.Text as Text

type Address = Text

stringToAddress :: String -> Address
stringToAddress = Text.pack

addressToString :: Address -> String
addressToString = Text.unpack
