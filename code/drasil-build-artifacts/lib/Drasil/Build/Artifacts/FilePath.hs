module Drasil.Build.Artifacts.FilePath where

import System.FilePath (pathSeparator)

-- | Represents a valid component of a path (e.g., a file or directory /name/).
newtype PathComponent = PC { unPC :: String }
  deriving (Eq, Ord, Show)

-- | Internal: Check if a path component (i.e., text before/between/after path
-- separators) is a valid component. Here, validity being defined by not being
-- any of: ., .., ~, or the system-local path separator.
checkValid :: FilePath -> PathComponent
checkValid s
  | s `elem` [".", "..", "~"] = error $ "invalid path component: " ++ show s ++ "."
  | pathSeparator `elem` s =
      error $
        "cannot create path component with " ++ show pathSeparator ++ " in the name."
  | otherwise = PC s
