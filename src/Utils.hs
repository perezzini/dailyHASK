{-|
Module      : Utils

Definition of several utilities
-}

module Utils
    (
    listOfTextsToListOfStrings
    , listOfStringsToListOfText
    , replaceCharByCharInString
    , connectListOfStrings
    , stringToInteger
    , handleNullValue
    , isInt
    , inList
    ) where

import Data.Text as Text hiding (map)
import Text.Read as Read
import Data.Maybe as M

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

-- |The 'isInt' function takes a 'String' value and checks if it represents an Int
isInt :: String -> Bool
isInt s = let
  s' = Read.readMaybe s :: Maybe Int
  in if M.isNothing s'
    then False
    else True

-- |The 'inList' function takes a value, and a list of same type, and checks if
-- the mentioned value exists in input list
inList :: Eq a => a -> [a] -> Bool
inList value list = case Prelude.length $ Prelude.filter (\v -> v == value) list of
  0 -> False
  _ -> True
