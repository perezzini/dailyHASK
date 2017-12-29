module Utils
    (
    listOfTextsToListOfStrings
    , listOfStringsToListOfText
    , replaceCharByCharInString
    , connectListOfStrings
    , stringToInteger
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
