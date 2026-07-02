module Drasil.Generator.RenderSystem (
  writeSystemRepoDir
) where

import System.OsPath (OsPath)

import Drasil.FileHandling
import Drasil.System

-- | 'renderRepoDir', but it also writes the directory to disk (relative to a
-- parent path).
writeSystemRepoDir :: Render sys opts =>
  -- | The parent path.
  OsPath ->
  -- | File overwrite policy.
  OverwritePolicy ->
  -- | The system.
  sys ->
  -- | The rendering options.
  opts ->
  -- | The software artifacts will be rendered about the 'OsPath'.
  IO ()
writeSystemRepoDir basePath pol sys =
  writeFiles pol basePath . renderRepoDir sys
