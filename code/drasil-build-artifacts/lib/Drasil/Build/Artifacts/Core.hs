module Drasil.Build.Artifacts.Core
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
import System.Directory (createDirectory, doesPathExist)
import System.FilePath (pathSeparator, (</>))

-- | Represents a valid component of a path (e.g., a file or directory name).
newtype PathComponent = PC { unPC :: String }
  deriving (Eq, Ord, Show)

-- | A software artifact may contain other software artifacts (i.e., be a
-- directory of other artifacts), or just be a single file.
--
-- Polymorphic over the internal representation of the contents of the
-- artifacts. For rendering, 'writeArtifact' requires the file content
-- representation satisfy 'Renderable'.
data FileLayout d
  = -- | A directory with optionally many nested artifacts.
    Directory PathComponent (M.Map PathComponent (FileLayout d))
  | -- | A file with content (of an unspecific type).
    File PathComponent d
  deriving (Show, Functor, Foldable, Traversable)

-- | Get the name of an 'FileLayout'.
name :: FileLayout d -> PathComponent
name (Directory dn _) = dn
name (File fn _) = fn

-- | Equality on file layout is dependent solely on the _names_. Note that this
-- implies a file and directory of the same name cannot exist within the same
-- directory. This is a restriction partially imposed by macOS.
instance Eq (FileLayout d) where
  (==) = (==) `on` name

-- | Internal: Check if a path component (i.e., text before/between/after path
-- separators) is a valid component. Here, validity being defined by not being
-- any of: ., .., ~, or the system-local path separator.
checkValid :: FilePath -> PathComponent
checkValid s
  | s `elem` [".", "..", "~"] = error $ "invalid path component: " ++ show s ++ "."
  | pathSeparator `elem` s =
      error $
        "cannot create artifact with " ++ show pathSeparator ++ " in the name."
  | otherwise = PC s

-- | Create a file 'FileLayout'.
file :: FilePath -> d -> FileLayout d
file = File . checkValid

-- | Create a directory 'FileLayout', optionally containing any number of other
-- 'FileLayout's.
directory :: (Foldable f) => FilePath -> f (FileLayout d) -> FileLayout d
directory fp = Directory (checkValid fp) . F.foldr' ins mempty
  where
    ins :: FileLayout d -> M.Map PathComponent (FileLayout d) -> M.Map PathComponent (FileLayout d)
    ins v = M.insertWith (error $ "attempting to overwrite: " ++ unPC (name v)) (name v) v

-- | Write a 'FileLayout' to disk, about a base path.
writeFiles :: (Renderable d) => FilePath -> FileLayout d -> IO ()
writeFiles basePath layout = do
  let targetPath = basePath </> unPC (name layout)
  exists <- doesPathExist targetPath
  if exists
    then error $ "Path already exists: " ++ targetPath
    else go basePath layout
  where
    go currentPath (File fname content) =
      renderToFile (currentPath </> unPC fname) content
    go currentPath (Directory dname children) = do
      let nextPath = currentPath </> unPC dname
      createDirectory nextPath
      F.traverse_ (go nextPath) children
