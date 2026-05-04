module Drasil.Build.Artifacts.FileLayout
  ( FileLayout,
    file,
    directory,
    writeFiles,
  )
where

import Data.Foldable qualified as F
import Data.Function (on)
import Data.Map.Strict qualified as M
import Drasil.Build.Artifacts.Render (Renderable (..))
import Drasil.Build.Artifacts.FilePath
import System.Directory (createDirectory, doesPathExist)
import System.FilePath ((</>))

-- | Internal: File nodes for layout, omitting the names of things.
data FileNode d
  = -- | A directory with optionally many nested artifacts.
    Directory (M.Map PathComponent (FileNode d))
  | -- | A file with content (of an unspecific type).
    File d
  deriving (Show, Functor, Foldable, Traversable)

-- | A software artifact may contain other software artifacts (i.e., be a
-- directory of other artifacts), or just be a single file.
--
-- Polymorphic over the internal representation of the contents of the
-- artifacts. For rendering, 'writeArtifact' requires the file content
-- representation satisfy 'Renderable'.
data FileLayout d = FileLayout PathComponent (FileNode d)
  deriving (Show, Functor, Foldable, Traversable)

-- | Get the name of an 'FileLayout'.
name :: FileLayout d -> PathComponent
name (FileLayout n _) = n

node :: FileLayout d -> FileNode d
node (FileLayout _ n) = n

-- | Equality on file layout is dependent solely on the _names_. Note that this
-- implies a file and directory of the same name cannot exist within the same
-- directory. This is a restriction partially imposed by macOS.
instance Eq (FileLayout d) where
  (==) = (==) `on` name -- FIXME: Need to be extra cautious about normalizing names.

-- | Create a file 'FileLayout'.
file :: FilePath -> d -> FileLayout d
file fp content = FileLayout (checkValid fp) (File content)
{-# INLINE file #-}

-- | Create a directory 'FileLayout', optionally containing any number of other
-- 'FileLayout's.
directory :: (Foldable f) => FilePath -> f (FileLayout d) -> FileLayout d
directory fp children = FileLayout (checkValid fp) (Directory $ F.foldr' ins mempty children)
  where
    ins :: FileLayout d -> M.Map PathComponent (FileNode d) -> M.Map PathComponent (FileNode d)
    ins v = M.insertWith (error $ "attempting to overwrite: " ++ unPC (name v)) (name v) (node v)
{-# INLINE directory #-}

-- | Write a 'FileLayout' to disk, about a base path.
writeFiles :: (Renderable d) => FilePath -> FileLayout d -> IO ()
writeFiles basePath layout = do
  let targetPath = basePath </> unPC (name layout)
  exists <- doesPathExist targetPath
  if exists
    then error $ "Path already exists: " ++ targetPath
    else go basePath layout
  where
    go currentPath (FileLayout fname (File content)) =
      renderToFile (currentPath </> unPC fname) content
    go currentPath (FileLayout dname (Directory children)) = do
      let nextPath = currentPath </> unPC dname
      createDirectory nextPath
      F.traverse_ (\(n, c) -> go nextPath (FileLayout n c)) (M.toList children)
