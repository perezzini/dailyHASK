module Utils
    (
    listOfTextsToListOfStrings
    , listOfStringsToListOfText
    , replaceCharByCharInString
    , connectListOfStrings
    , stringToInteger
    , handleNullValue
    ) where

import Data.Text as Text hiding (map)

listOfStringsToListOfText :: [String] -> [Text]
listOfStringsToListOfText list = map Text.pack list

listOfTextsToListOfStrings :: [Text] -> [String]
listOfTextsToListOfStrings list = map Text.unpack list

replaceCharByCharInString :: Char -> Char -> String -> String
replaceCharByCharInString oldChar newChar str = map (\c -> if c == oldChar then newChar else c) str

connectListOfStrings :: [String] -> String -> String
connectListOfStrings (x : []) _ = x
connectListOfStrings (x : xs) repl = x ++ repl ++ (connectListOfStrings xs repl)

stringToInteger :: String -> Integer
stringToInteger s = read s

handleNullValue :: Maybe a -> Either Text a
handleNullValue (Just v) = Right v
handleNullValue _ = Left $ Text.pack "Not available"
