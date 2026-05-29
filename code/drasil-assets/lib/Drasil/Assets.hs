{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | External file 'Asset's.
module Drasil.Assets
  ( -- * Assets
    Asset,

    -- ** Constructors
    readAsset,

    -- ** Accessors
    content,
    description,

    -- ** File Conversion
    toFile,
  )
where

import Data.ByteString qualified as B (ByteString)
import Drasil.Build.Artifacts (FileLayout, PathSegment, exactFile)
import Language.Haskell.TH (Code (examineCode), Q, liftCode, runIO)
import Language.Haskell.TH.Syntax (Lift, addDependentFile, makeRelativeToProject)
import System.Directory.OsPath (doesPathExist)
import System.File.OsPath (readFile')
import System.OsPath (OsPath, decodeUtf, encodeUtf)
import Prelude hiding (readFile)

-- | An arbitrary file asset; a 'B.ByteString' with a description.
data Asset = Asset String B.ByteString
  deriving (Lift)

-- | Get the description of an 'Asset'.
description :: Asset -> String
description (Asset desc _) = desc

-- | Get the raw 'ByteString' content of an 'Asset'.
content :: Asset -> B.ByteString
content (Asset _ c) = c

-- | Convert an 'Asset' to a 'FileLayout' node given a 'PathSegment' (file name)
-- it is to be written to.
toFile :: Asset -> PathSegment -> FileLayout
toFile a = flip exactFile (content a)

-- | Read a file from disk, providing a description of the asset.
readAsset :: OsPath -> String -> Code Q Asset
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
      let a = Asset desc fc
      addDependentFile absPathStr
      examineCode [||a||]
    else error $ "Asset file lookup failed. File does not exist: " ++ absPathStr
