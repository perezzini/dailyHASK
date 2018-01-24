{-|
Module      : Config

Definition of functions to get values of certain keys form a configuration file named app.cfg placed in root folder
-}

module Config
    (
    getValue
    ) where

import qualified Data.TConfig as TConfig
import System.Directory as Dir

import Data.Maybe as M
import qualified Error as E

appConfigFile :: String
appConfigFile = "/app.cfg"

-- |The 'getValue' function takes a key and maybe returns its value.
-- Exits program with error in case key could not be found
getValue :: String -> IO String
getValue key = do
  currDir <- Dir.getCurrentDirectory
  let filePath = currDir ++ appConfigFile
  configFile <- TConfig.readConfig filePath
  let value = TConfig.getValue key configFile
  case value of
    Just v -> return $ v
    otherwise -> E.callError ("Error. Key " ++ key ++ "not found in config file. Aborting...")
