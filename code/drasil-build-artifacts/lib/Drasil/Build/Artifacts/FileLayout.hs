{-# LANGUAGE TemplateHaskell #-}

module Drasil.Build.Artifacts.FileLayout
  ( -- * File Layout
    FileLayout,

    -- ** Constructors
    file,
    directory,

    -- ** Writing
    writeFiles,
  )
where

import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Drasil.Build.Artifacts.FilePath (PathSegment, toPath, (</>))
import Drasil.Build.Artifacts.Render (Renderable (..))
import System.Directory.OsPath (createDirectory, doesPathExist)
import System.OsPath (OsPath, decodeUtf)

-- | Container for laying out files, and safely writing to disk.
--
-- Polymorphic over the representation of the file contents. For rendering,
-- writeFiles requires the file content representation satisfy 'Renderable'.
data FileLayout doc = FileLayout
  { -- | The /name/ of the file or directory.
    pathSeg :: PathSegment,
    -- | The /contents/ of the file or directory.
    fileTree :: FileTree doc
  }
  deriving (Functor, Foldable, Traversable)

-- | Internal: File layout tree.
data FileTree doc
  = -- | A directory with optionally many nested artifacts.
    Directory (M.Map PathSegment (FileTree doc))
  | -- | A file with content (of an unspecific type).
    File doc
  deriving (Functor, Foldable, Traversable)

-- | Create a file 'FileLayout'.
file :: PathSegment -> doc -> FileLayout doc
file fp = FileLayout fp . File
{-# INLINE file #-}

-- | Create a directory 'FileLayout', optionally containing any number of nested
-- files.
directory :: (Foldable f) => PathSegment -> f (FileLayout doc) -> FileLayout doc
directory fp = FileLayout fp . Directory . F.foldr' insert mempty -- FIXME: Need to deal with duplicate PathSegs due to insensitivity
{-# INLINE directory #-} -- NOTE: Inlining here and in 'directory' allows for the held @f (FileLayout doc)@s to never actually build 'FileLayout's!

-- | Internal: Insert a 'FileLayout' into a 'Directory''s map.
insert :: FileLayout d -> M.Map PathSegment (FileTree d) -> M.Map PathSegment (FileTree d)
insert v = M.insertWithKey (\dup _ -> error $ "duplicate path: " ++ fromMaybe "cannot decode" $([|decodeUtf $ toPath dup :: Maybe String|])) (pathSeg v) (fileTree v)
{-# INLINE insert #-}

-- | Write a 'FileLayout' to disk about a base path.
writeFiles :: (Renderable doc) => OsPath -> FileLayout doc -> IO ()
writeFiles basePath layout = do
  let targetPath = basePath </> pathSeg layout
  exists <- doesPathExist targetPath
  if exists
    then error $ "Path already exists: " ++ show targetPath
    else go basePath layout
  where
    go currentPath (FileLayout fname (File content)) =
      renderToFile (currentPath </> fname) content
    go currentPath (FileLayout dname (Directory children)) = do
      let nextPath = currentPath </> dname
      createDirectory nextPath
      F.traverse_ (\(n, c) -> go nextPath (FileLayout n c)) (M.toList children)
