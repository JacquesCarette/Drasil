{-# LANGUAGE TemplateHaskell #-}

module Drasil.Build.Artifacts.FileLayout
  ( -- * File Layout
    FileLayout,
    name,

    -- ** Constructors
    file,
    directory,

    -- ** Writing
    OverwritePolicy(..),
    writeFiles,
  )
where

import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Drasil.Build.Artifacts.FilePath (PathSegment, toPath, (</>))
import Drasil.Build.Artifacts.Render (Renderable (..))
import System.Directory.OsPath (createDirectoryIfMissing, doesPathExist)
import System.OsPath (OsPath, decodeUtf)

-- | Container for laying out files in a single container for writing to disk.
-- Notes:
--
--     1. Polymorphic over the representation of the file contents. For
--        rendering, writeFiles requires the file content representation
--        satisfy 'Renderable'.
--     2. Only permits writing files/directories relative to a base path
--        provided. Does not permit `..`, `.`, nor `~` as directory names.
--     3. System-local path separator is forbidden from use in directory names.
--     4. Assumes host file system is case-sensitive (i.e., recognizes `A.txt`
--        and `a.txt` as different paths).
data FileLayout doc = FileLayout
  { -- | The /name/ of the file or directory.
    pathSeg :: PathSegment,
    -- | The /contents/ of the file or directory.
    fileTree :: FileTree doc
  }
  deriving (Functor)

-- | Get the top-level name (a 'PathSegment') of a 'FileLayout'.
name :: FileLayout doc -> PathSegment
name = pathSeg
{-# INLINE name #-}

-- | Internal: File layout tree.
data FileTree doc
  = -- | A directory with optionally many nested artifacts.
    Directory (M.Map PathSegment (FileTree doc))
  | -- | A file with content (of an unspecific type).
    File doc
  deriving (Functor)

-- | Create a file 'FileLayout'.
file :: PathSegment -> doc -> FileLayout doc
file fp = FileLayout fp . File
{-# INLINE file #-}

-- | Create a directory 'FileLayout', optionally containing any number of nested
-- files.
directory :: (Foldable f) => PathSegment -> f (FileLayout doc) -> FileLayout doc
directory fp = FileLayout fp . Directory . F.foldr' insert mempty
-- NOTE: Inlining here and in 'directory' allows for the held @f (FileLayout
-- doc)@s to never actually build 'FileLayout's!
{-# INLINE directory #-}

-- | Internal: Insert a 'FileLayout' into a 'Directory''s map.
insert :: FileLayout d -> M.Map PathSegment (FileTree d) -> M.Map PathSegment (FileTree d)
insert v =
  M.insertWithKey
    (\dup _ -> error $ "duplicate path: " ++ fromMaybe "cannot decode" $([|decodeUtf $ toPath dup :: Maybe String|]))
    (pathSeg v)
    (fileTree v)
{-# INLINE insert #-}

-- | When writing files or creating directories, is overwriting allowed?
data OverwritePolicy = OverwriteAllowed | NeverOverwrite

-- | Write a 'FileLayout' to disk about a base path.
--
-- If the given 'OverwritePolicy' is 'NeverOverwrite', this will fail if the
-- /top-level/ target path already exists. If it is 'OverwriteAllowed', it will
-- overwrite existing files.
--
-- Disclaimer: For case-insensitive file systems where different paths in the
-- layout might reference the same on-disk path, this code will fail.
writeFiles :: (Renderable doc) => OverwritePolicy -> OsPath -> FileLayout doc -> IO ()
writeFiles OverwriteAllowed basePath layout = writeFiles0 basePath layout
writeFiles NeverOverwrite basePath layout = do
  exists <- doesPathExist targetPath
  if exists
    then error $ "Path already exists: " ++ show targetPath
    else writeFiles0 basePath layout
  where
    targetPath = basePath </> pathSeg layout

-- Internal: `writeFiles` internal. It will create directories as needed and
-- blindly overwrite any existing files as designated in the layout.
writeFiles0 :: (Renderable doc) => OsPath -> FileLayout doc -> IO ()
writeFiles0 currentPath (FileLayout fname (File content)) =
  renderToFile (currentPath </> fname) content
writeFiles0 currentPath (FileLayout dname (Directory children)) = do
  let nextPath = currentPath </> dname
  createDirectoryIfMissing False nextPath
  F.traverse_ (\(n, c) -> writeFiles0 nextPath (FileLayout n c)) (M.toList children)
{-# INLINE writeFiles0 #-}
