module Drasil.Build.Artifacts.Core
  ( Artifact (..),
    file,
    directory,
    writeArtifact,
  )
where

import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Drasil.Build.Artifacts.Render (Renderable (..))
import System.Directory (createDirectory, doesPathExist)
import System.FilePath (pathSeparator, (</>))

-- | A software artifact may contain other software artifacts (i.e., be a
-- directory of other artifacts), or just be a single file.
data Artifact d
  = Directory String (M.Map String (Artifact d))
  | File String d
  deriving (Show, Functor, Foldable, Traversable)

name :: Artifact d -> String
name (Directory s _) = s
name (File s _) = s

-- | Equality on artifacts is purely dependent on the _names_. Note that this
-- implies a file and directory of the same name cannot exist within the same
-- directory. This is a restriction partially imposed by macOS.
instance Eq (Artifact d) where
  a1 == a2 = name a1 == name a2

checkValid :: String -> String
checkValid "." = error "cannot create artifact with name \".\"."
checkValid ".." = error "cannot create artifact with name \"..\"."
checkValid "~" = error "cannot create artifact with name \"~\"."
checkValid s
  | pathSeparator `elem` s = error $ "cannot create artifact with \"" ++ pathSeparator ++ "\" in the name."
  | otherwise = s

file :: String -> d -> Artifact d
file = File . checkValid

directory :: (Foldable f) => String -> f (Artifact d) -> Artifact d
directory fp = Directory (checkValid fp) . F.foldr' ins mempty
  where
    ins :: Artifact d -> M.Map String (Artifact d) -> M.Map String (Artifact d)
    ins v = M.insertWith (error $ "attempting to overwrite: " ++ name v) (name v) v

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
      -- createDirectoryIfMissing True nextPath -- FIXME: Create parent or not?
      F.traverse_ (go nextPath) children
