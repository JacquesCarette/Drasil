module Utils.Drasil.FilePath (
  RelativeFile, relativeFile, relFileToStr
) where

import Data.List (foldl')
import System.FilePath (isAbsolute, isValid, hasExtension, splitDirectories)

-- | A valid, relative file path with an extension in canonical form.
newtype RelativeFile = RF { relFileToStr :: String }
  deriving Eq

-- | Create a 'RelativeFile' given a 'String' that must be in canonical form, be
-- a valid file path, contain a file extension, and be relative (not absolute);
-- otherwise, an error is raised.
relativeFile :: String -> RelativeFile
relativeFile fp
  | not $ isCanonical fp = error $ "`" ++ fp ++ "` is not in canonical form."
  | not $ isValid fp = error $ "`" ++ fp ++ "` is not a valid file path."
  | not $ hasExtension fp = error $ "`" ++ fp ++ "` does not contain a file extension."
  | isAbsolute fp = error $ "`" ++ fp ++ "` is an absolute file path, but a relative file path was expected."
  | otherwise = RF fp

isCanonical :: String -> Bool
isCanonical fp = foldl' step True (splitDirectories fp)
  where
    step _   "."  = False -- Should not include current dir
    step _   ".." = False -- Should not "go up" directories
    step acc _    = acc
