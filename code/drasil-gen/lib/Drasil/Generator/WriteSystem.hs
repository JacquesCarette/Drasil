{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Drasil.Generator.WriteSystem
  ( drasilMakefileReqOpts,
    concretizeAndWrite,
    setSystemLocale,
    WriteOptions (..),
    DebugDataPolicy (..)
  )
where

import Control.Lens ((^.))
import GHC.IO.Encoding (setLocaleEncoding, utf8, TextEncoding)
import System.Environment (lookupEnv)

import Drasil.FileHandling (OverwritePolicy (..), PathSegment, directory, localPath, ps, writeFiles, FileLayout)
import Drasil.System (HasSystemMeta (..), ToFiles (..))
import Language.Drasil (CommonIdea (abrv))

import Drasil.Generator.ChunkDump (buildDebuggingFiles)

-- | Internal: Set system locale encoding to UTF-8.
setSystemLocale :: IO ()
setSystemLocale = setLocaleEncoding utf8

{-# DEPRECATED setSystemLocale
  "Use `concretizeAndWrite` instead of directly setting system locale before file-writing." #-}

-- | When should debugging data be written?
data DebugDataPolicy
  = -- | Always.
    AlwaysWrite
      -- | The name of the directory to carry all debugging data files.
      PathSegment
  | -- | Only write debugging data if the following environment variable ('String') is non-empty.
    CheckEnvVar
      -- | The environment variable name.
      String
      -- | The name of the directory to carry all debugging data files.
      PathSegment
  | -- | Never.
    NeverWrite

-- | Configuration options for writing a repository of software artifacts.
data WriteOptions = WO
  { -- | Are we allowed to overwrite files or not?
    overwritePolicy :: OverwritePolicy,
    -- | What is the name of the subfolder to be created?
    localDirName :: forall sys. HasSystemMeta sys => sys -> PathSegment,
    -- | What is the expected text encoding scheme?
    textEncoding :: TextEncoding,
    -- | Should debugging data be written?
    debugDataPolicy :: DebugDataPolicy
  }

-- | These are the default file-writing 'WriteOptions' required for Drasil (the
-- main Makefile will not play well otherwise).
--
-- Options:
--
-- 1. 'overwritePolicy': Always allow file-overwriting.
-- 2. 'dirName': The example's abbreviation.
-- 3. 'textEncoding': UTF-8.
drasilMakefileReqOpts :: WriteOptions
drasilMakefileReqOpts =
  WO OverwriteAllowed dirName utf8 (CheckEnvVar "DEBUG_ENV" [ps|.drasil|])
  where
    dirName sys = let ab = abrv $ sys ^. systemMeta . sysName
                   in [ps|{ab}|]

-- FIXME: Both `abrv` usage and `sysName` usage above is dubious. We need to
-- replace this field with something better, such as project name and project
-- shortname (repo name).

-- | Internal: 'DebugDataPolicy' eliminator.
debugData :: HasSystemMeta sys => sys -> DebugDataPolicy -> IO (Maybe FileLayout)
debugData _ NeverWrite = pure Nothing
debugData sys (AlwaysWrite dirName) =
  pure $ Just $ directory dirName $ buildDebuggingFiles sys
debugData sys (CheckEnvVar envVar dirName) = do
  maybeDebugging <- lookupEnv envVar
  case maybeDebugging of
    (Just v) | not (null v)
      -> pure $ Just $ directory dirName $ buildDebuggingFiles sys
    _ -> pure Nothing

-- | Concretize a system into a concrete set of files and write them to disk.
--
-- Note: Writes files to a subdirectory of the current working directory.
concretizeAndWrite ::
  (ToFiles sys concOpts) =>
  -- | The system.
  sys ->
  -- | The concretization options.
  concOpts ->
  -- | The file-writing options.
  WriteOptions ->
  -- | Files will be written to a local directory named after the abbreviation
  -- of the system.
  IO ()
concretizeAndWrite sys concOpts WO {..} = do
  setLocaleEncoding textEncoding
  debugFiles <- debugData sys debugDataPolicy
  let artifacts = toFiles sys concOpts
      artifacts' = maybe artifacts (: artifacts) debugFiles
      finalDir = directory (localDirName sys) artifacts'
  writeFiles overwritePolicy localPath finalDir
