{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- Contains the current Drasil Version
module Data.Drasil.DrasilConfig (obtainVersion, drasilVersion) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Utils.Drasil(drasilConfigPath)

newtype DrasilConfig = DrasilConfig {version :: String} deriving Generic

instance FromJSON DrasilConfig

obtainVersion :: Int -> IO String
obtainVersion num = do
  input <- B.readFile $ drasilConfigPath num
  let dc = decode input :: Maybe DrasilConfig
  case dc of
    Nothing -> return "error parsing JSON"
    Just d -> return ((show.version) d)

-- The current Drasil version
drasilVersion :: String
drasilVersion = "v0.1-alpha"
