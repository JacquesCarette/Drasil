{-# LANGUAGE QuasiQuotes #-}
module Drasil.Generator.RenderSystem (
  writeSystemRepoDir
) where

import Control.Lens ((^.))
import System.OsPath (OsPath)

import Language.Drasil (CommonIdea(abrv))

import Drasil.FileHandling (OverwritePolicy, writeFiles, directory, ps)
import Drasil.System (HasSystemMeta(..), ToFiles(..))

-- | 'toFiles', but we also write the files to disk (using the system's
-- abbreviation as the assumed repository name).
writeSystemRepoDir :: ToFiles sys opts =>
  -- | The parent path.
  OsPath ->
  -- | File overwrite policy.
  OverwritePolicy ->
  -- | The system.
  sys ->
  -- | The generation options.
  opts ->
  -- | The software artifacts will be rendered about the 'OsPath'.
  IO ()
writeSystemRepoDir basePath pol sys =
  writeFiles pol basePath . directory [ps|{dirName}|] . toFiles sys
  where
    dirName = abrv $ sys ^. systemMeta . sysName
    -- FIXME: Both `abrv` usage and `sysName` usage here is dubious. We need to
    -- replace this field with something better, such as project name and
    -- project shortname (repo name).
    --
    -- In some sense, I want to rename `System` to `Project`. Hence,
    -- `SystemMeta` becomes `ProjectMeta`. This is nice because `SystemRepo`
    -- then also becomes `ProjectRepo` (which is more commonly understood repo
    -- and vague in a positive way).
