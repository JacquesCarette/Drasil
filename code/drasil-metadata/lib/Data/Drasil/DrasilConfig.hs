{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- Contains the current Drasil Version
module Data.Drasil.DrasilConfig (obtainVersion) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Utils.Drasil(drasilConfigPath)
import Data.Maybe(fromMaybe)

newtype DrasilConfig = DrasilConfig {version :: String} deriving Generic

-- The number of folders that need to be exited to obtain the 
-- DrasilConfiguration.json file
defaultFolderVal :: Int
defaultFolderVal = 3

instance FromJSON DrasilConfig

-- Reads the DrasilConfiguration.json file and extracts the data from that file.
obtainVersion :: Maybe Int -> IO String
obtainVersion num = do
  input <- B.readFile $ drasilConfigPath $ fromMaybe defaultFolderVal num
  let dc = decode input :: Maybe DrasilConfig
  case dc of
    Nothing -> return "error parsing JSON"
    Just d -> return ((show.version) d)
