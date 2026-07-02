{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Drasil.Generator.WriteSystem
  ( drasilMakefileReqOpts,
    concretizeAndWrite,
    setSystemLocale,
  )
where

import Control.Lens ((^.))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.OsPath (OsPath)

import Drasil.FileHandling (OverwritePolicy (..), directory, localPath, ps, writeFiles)
import Drasil.System (HasSystemMeta (..), ToFiles (..))
import Language.Drasil (CommonIdea (abrv))

-- | Internal: Set system locale encoding to UTF-8.
setSystemLocale :: IO ()
setSystemLocale = setLocaleEncoding utf8

-- | Configuration options for writing a repository.
data WriteOptions = WO {
  -- | Where should we write to?
  basePath :: OsPath,
  -- | Are we allowed to overwrite files or not?
  overwritePolicy :: OverwritePolicy
}

-- | These are the default file-writing 'WriteOptions' required or else the main
-- Makefile will not play well.
drasilMakefileReqOpts :: WriteOptions
drasilMakefileReqOpts = WO localPath OverwriteAllowed

-- | Concretize a system into a concrete set of files and write them to disk.
--
-- Notes:
--
-- 1. Sets system locale to utf8 for cross-platform consistency.
-- 2. Bundles artifacts together into a single directory. Directory name is
--    derived from the abbreviation of the system's 'CI' title.
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
concretizeAndWrite sys concOpts WO{..} = do
  setSystemLocale
  writeFiles overwritePolicy basePath $ directory [ps|{dirName}|] $ toFiles sys concOpts
  where
    dirName = abrv $ sys ^. systemMeta . sysName

-- FIXME: Both `abrv` usage and `sysName` usage above is dubious. We need to
-- replace this field with something better, such as project name and project
-- shortname (repo name).
--
-- FIXME: Rename `System` to `Project`?
