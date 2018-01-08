{-|
Module      : Utils

Definition of several library's utilities
-}

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

-- |The 'listOfStringsToListOfText' function takes a list of strings and maps it to a list
-- of 'Text'
listOfStringsToListOfText :: [String] -> [Text]
listOfStringsToListOfText list = map Text.pack list

-- |The 'listOfTextsToListOfStrings' function takes a list of 'Text' and maps it to a list
-- of strings
listOfTextsToListOfStrings :: [Text] -> [String]
listOfTextsToListOfStrings list = map Text.unpack list

-- |The 'replaceCharByCharInString' function swaps a certain 'Char' value by a new 'Char' value
-- in a given string
replaceCharByCharInString :: Char -> Char -> String -> String
replaceCharByCharInString oldChar newChar str = map (\c -> if c == oldChar then newChar else c) str

-- |The 'connectListOfStrings' function connects strings, in a list of strings, by concatenating a given
-- string
connectListOfStrings :: [String] -> String -> String
connectListOfStrings (x : []) _ = x
connectListOfStrings (x : xs) repl = x ++ repl ++ (connectListOfStrings xs repl)

-- |The 'stringToInteger' function takes a given number formatted in a 'String' value and converts it to an 'Integer'
stringToInteger :: String -> Integer
stringToInteger s = read s

-- |The 'handleNullValue' function handles a JSON null value: in case the input value is Nothing, it just returns
-- "Not available" as a sort of error; otherwise it just returns the given value
handleNullValue :: Maybe a -> Either Text a
handleNullValue (Just v) = Right v
handleNullValue _ = Left $ Text.pack "Not available"
