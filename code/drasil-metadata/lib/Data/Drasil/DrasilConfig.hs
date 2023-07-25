{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Data.Drasil.DrasilConfig where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Utils.Drasil (drasilConfigPath)

newtype DrasilConfig = DrasilConfig {version :: String} deriving (Generic, Show)

instance FromJSON DrasilConfig

obtainVersion :: Int -> IO DrasilConfig
obtainVersion num = do
  input <- B.readFile $ drasilConfigPath num
  let dc = decode input :: Maybe DrasilConfig
  case dc of
    Nothing -> error "error parsing JSON"
    Just d -> return d
