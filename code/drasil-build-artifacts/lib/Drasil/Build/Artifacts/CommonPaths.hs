{-# LANGUAGE QuasiQuotes #-}
module Drasil.Build.Artifacts.CommonPaths (
  localPath
) where

import System.OsPath (OsPath, osp)

localPath :: OsPath
localPath = [osp|./|]
