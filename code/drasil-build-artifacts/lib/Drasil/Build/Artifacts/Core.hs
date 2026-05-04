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

-- | A software artifact may contain other software artifacts (i.e., be a
-- directory of other artifacts), or just be a single file.
--
-- Polymorphic over the internal representation of the contents of the
-- artifacts. For rendering, 'writeArtifact' requires the file content
-- representation satisfy 'Renderable'.
data FileLayout d
  = -- | A directory with optionally many nested artifacts.
    Directory String (M.Map String (FileLayout d))
  | -- | A file with content (of an unspecific type).
    File String d
  deriving (Show, Functor, Foldable, Traversable)

-- | Get the name of an 'FileLayout'.
name :: FileLayout d -> String
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
checkValid :: String -> String
checkValid "." = error "invalid path component: \".\"."
checkValid ".." = error "invalid path component: \"..\"."
checkValid "~" = error "invalid path component: \"~\"."
checkValid s
  | pathSeparator `elem` s =
      error $
        "cannot create artifact with \"" ++ pathSeparator : "\" in the name."
  | otherwise = s

-- | Create a file 'FileLayout'.
file :: String -> d -> FileLayout d
file = File . checkValid

-- | Create a directory 'FileLayout', optionally containing any number of other
-- 'FileLayout's.
directory :: (Foldable f) => String -> f (FileLayout d) -> FileLayout d
directory fp = Directory (checkValid fp) . F.foldr' ins mempty
  where
    ins :: FileLayout d -> M.Map String (FileLayout d) -> M.Map String (FileLayout d)
    ins v = M.insertWith (error $ "attempting to overwrite: " ++ name v) (name v) v

-- | Write a 'FileLayout' to disk, about a base path.
writeFiles :: (Renderable d) => FilePath -> FileLayout d -> IO ()
writeFiles basePath layout = do
  let targetPath = basePath </> name layout
  exists <- doesPathExist targetPath
  if exists
    then error $ "Path already exists: " ++ targetPath
    else go basePath layout
  where
    go currentPath (File fname content) =
      renderToFile (currentPath </> fname) content
    go currentPath (Directory dname children) = do
      let nextPath = currentPath </> dname
      createDirectory nextPath
      F.traverse_ (go nextPath) children
