module Drasil.Build.Artifacts.Core
  ( Artifact,
    file,
    directory,
    writeArtifact,
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
data Artifact d
  = -- | A directory with optionally many nested artifacts.
    Directory String (M.Map String (Artifact d))
  | -- | A file with content (of an unspecific type).
    File String d
  deriving (Show, Functor, Foldable, Traversable)

-- | Get the name of an 'Artifact'.
name :: Artifact d -> String
name (Directory dn _) = dn
name (File fn _) = fn

-- | Equality on artifacts is dependent solely on the _names_. Note that this
-- implies a file and directory of the same name cannot exist within the same
-- directory. This is a restriction partially imposed by macOS.
instance Eq (Artifact d) where
  (==) = (==) `on` name

-- | Internal: Check if a path component (i.e., text before/between/after path
-- separators) is a valid component. Here, validity being defined by not being
-- any of: ., .., ~, or the system-local path separator.
checkValid :: String -> String
checkValid "." = error "cannot create artifact with name \".\"."
checkValid ".." = error "cannot create artifact with name \"..\"."
checkValid "~" = error "cannot create artifact with name \"~\"."
checkValid s
  | pathSeparator `elem` s =
      error $
        "cannot create artifact with \"" ++ pathSeparator : "\" in the name."
  | otherwise = s

-- | Create a file 'Artifact'.
file :: String -> d -> Artifact d
file = File . checkValid

-- | Create a directory 'Artifact', optionally containing any number of other
-- artifacts.
directory :: (Foldable f) => String -> f (Artifact d) -> Artifact d
directory fp = Directory (checkValid fp) . F.foldr' ins mempty
  where
    ins :: Artifact d -> M.Map String (Artifact d) -> M.Map String (Artifact d)
    ins v = M.insertWith (error $ "attempting to overwrite: " ++ name v) (name v) v

-- | Write 'Artifact' an artifact to disk, about a base path.
writeArtifact :: (Renderable d) => FilePath -> Artifact d -> IO ()
writeArtifact basePath artifact = do
  let targetPath = basePath </> name artifact
  exists <- doesPathExist targetPath
  if exists
    then error $ "Path already exists: " ++ targetPath
    else go basePath artifact
  where
    go currentPath (File fname content) =
      renderToFile (currentPath </> fname) content
    go currentPath (Directory dname children) = do
      let nextPath = currentPath </> dname
      createDirectory nextPath
      F.traverse_ (go nextPath) children
