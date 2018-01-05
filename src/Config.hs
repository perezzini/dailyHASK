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

getValue :: String -> IO (Maybe String)
getValue key = do
  currDir <- Dir.getCurrentDirectory
  let filePath = currDir ++ appConfigFile
  configFile <- TConfig.readConfig filePath
  let value = TConfig.getValue key configFile
  return $ value
