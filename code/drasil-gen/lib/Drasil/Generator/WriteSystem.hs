{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Drasil.Generator.WriteSystem
  ( drasilMakefileReqOpts,
    concretizeAndWrite,
    setSystemLocale,
    WriteOptions (..),
  )
where

import Control.Lens ((^.))
import GHC.IO.Encoding (setLocaleEncoding, utf8, TextEncoding)
import System.OsPath (OsPath)

import Drasil.FileHandling (OverwritePolicy (..), PathSegment, directory, localPath, ps, writeFiles)
import Drasil.System (HasSystemMeta (..), ToFiles (..))
import Language.Drasil (CommonIdea (abrv))

-- | Internal: Set system locale encoding to UTF-8.
setSystemLocale :: IO ()
setSystemLocale = setLocaleEncoding utf8

{-# DEPRECATED setSystemLocale
  "Use `concretizeAndWrite` instead of directly setting system locale before file-writing." #-}

-- | Configuration options for writing a repository of software artifacts.
data WriteOptions = WO
  { -- | Where should we write to?
    basePath :: OsPath,
    -- | Are we allowed to overwrite files or not?
    overwritePolicy :: OverwritePolicy,
    -- | What is the name of the subfolder to be created?
    localDirName :: forall sys. HasSystemMeta sys => sys -> PathSegment,
    -- | What is the expected text encoding scheme?
    textEncoding :: TextEncoding
  }

-- | These are the default file-writing 'WriteOptions' required for Drasil (the
-- main Makefile will not play well otherwise).
--
-- Options:
--
-- 1. 'basePath': Always generate to local path.
-- 2. 'overwritePolicy': Always allow file-overwriting.
-- 3. 'dirName': The example's abbreviation.
-- 4. 'textEncoding': UTF-8.
drasilMakefileReqOpts :: WriteOptions
drasilMakefileReqOpts = WO localPath OverwriteAllowed dirName utf8
  where
    dirName sys = let ab = abrv $ sys ^. systemMeta . sysName
                   in [ps|{ab}|]

-- FIXME: Both `abrv` usage and `sysName` usage above is dubious. We need to
-- replace this field with something better, such as project name and project
-- shortname (repo name).

-- | Concretize a system into a concrete set of files and write them to disk.
--
-- Note: Sets system locale to utf8 for cross-platform consistency.
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
  writeFiles overwritePolicy basePath $ directory (localDirName sys) $ toFiles sys concOpts
