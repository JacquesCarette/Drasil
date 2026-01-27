module Utils.Drasil.FilePath (
  RelativeFile, relativeFile, relFileToStr
) where

import System.FilePath (isAbsolute, isValid, hasExtension)

-- | A valid, relative file path with an extension.
newtype RelativeFile = RF { relFileToStr :: String }
  deriving Eq

-- | Create a 'RelativeFile' given a raw 'String'.
relativeFile :: String -> RelativeFile
relativeFile fp
  | not $ isValid fp = error $ "`" ++ fp ++ "` is not a valid file path."
  | not $ hasExtension fp = error $ "`" ++ fp ++ "` does not contain a file extension."
  | isAbsolute fp = error $ "`" ++ fp ++ "` is an absolute file path, but a relative file path was expected."
  | otherwise = RF fp
