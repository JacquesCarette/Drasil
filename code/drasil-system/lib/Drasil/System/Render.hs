{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}
module Drasil.System.Render (
  Render(..),
  renderSystemRepo, renderSystemRepo'
) where

import Control.Lens ((^.))
import System.OsPath (OsPath)

import Drasil.FileHandling (FileLayout, directory, ps, writeFiles, OverwritePolicy(..))
import Language.Drasil (CommonIdea(abrv))

import Drasil.System.Core (HasSystemMeta(..))

-- | A "system" can be rendered into a set of (human-readable) software
-- artifacts. An instance of `Render` is approximately what it means to define a
-- off-the-shelf software generator.
class HasSystemMeta sys => Render sys opts | opts -> sys where
  render ::
    -- | The system.
    sys ->
    -- | The rendering options.
    opts ->
    -- | The final, rendered software artifacts.
    [FileLayout]

-- | Render a "system" as a directory of (human-readable) software artifacts,
-- where the directory name is the assigned project shortname.
renderSystemRepo :: Render sys opts =>
  -- | The system.
  sys ->
  -- | The rendering options.
  opts ->
  -- | The final, rendered software artifacts packaged into a single directory.
  FileLayout
renderSystemRepo sys opts = directory [ps|{x}|] $ render sys opts
  where
    x = abrv $ sys ^. systemMeta . sysName
    -- FIXME: Both `abrv` usage and `sysName` usage here is dubious. We need to
    -- replace this field with something better, such as project name and
    -- project shortname (repo name).
    --
    -- In some sense, I want to rename `System` to `Project`. Hence,
    -- `SystemMeta` becomes `ProjectMeta`. This is nice because `SystemRepo`
    -- then also becomes `ProjectRepo` (which is more commonly understood repo
    -- and vague in a positive way).

-- | 'renderSystemRepo', but it also writes the directory to disk (relative to a
-- parent path).
renderSystemRepo' :: Render sys opts =>
  -- | The parent path.
  OsPath ->
  -- | The system.
  sys ->
  -- | The rendering options.
  opts ->
  -- | The software artifacts will be rendered about the 'OsPath'.
  IO ()
renderSystemRepo' basePath sys =
  writeFiles OverwriteAllowed basePath . renderSystemRepo sys
