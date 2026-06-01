{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | External file 'Asset's.
module Drasil.Assets.FromLocalPath
  ( -- * Assets
    Asset,

    -- ** Constructors
    readAsset,

    -- ** Accessors
    content,
    description,

    -- ** File Dumping
    toFile,
  )
where

import Data.Text (Text)
import Language.Haskell.TH (Code (examineCode), Q, liftCode, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject)
import System.Directory.OsPath (doesPathExist)
import System.File.OsPath (readFile')
import System.OsPath (OsPath, decodeUtf, encodeUtf)
import Prelude hiding (readFile)

import Drasil.Assets.Core (Asset, content, description, mkAsset, toFile)

-- | Read a file from disk, providing a description of the asset.
readAsset :: OsPath -> Text -> Code Q Asset
readAsset filePath desc = liftCode $ do
  strPath <- runIO (decodeUtf filePath)
  -- FIXME: HACK: This relies on decoding the path, making it relative, and then
  -- re-encoding. It works, but, when OsPath and OsString is propagated through
  -- GHC core libs more, this will break.
  absPathStr <- makeRelativeToProject strPath
  absPath <- runIO (encodeUtf absPathStr)
  exists <- runIO (doesPathExist absPath)
  if exists
    then do
      fc <- runIO (readFile' absPath)
      let a = mkAsset desc fc
      addDependentFile absPathStr
      examineCode [||a||]
    else error $ "Asset file lookup failed. File does not exist: " ++ absPathStr
