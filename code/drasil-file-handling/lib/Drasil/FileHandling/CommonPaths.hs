{-# LANGUAGE QuasiQuotes #-}
module Drasil.FileHandling.CommonPaths (
  localPath
) where

import System.OsPath (OsPath, osp)

localPath :: OsPath
localPath = [osp|./|]
