module Utils.Drasil.Directory (createDirIfMissing) where

import System.Directory (createDirectoryIfMissing, doesPathExist)

-- | Creates a directory if it does not already exist (optionally with all
-- missing parent directories).
--
-- Implementation uses doesPathExist to check if the directory exists rather
-- than createDirectoryIfMissing True, which would create the directory
-- regardless of whether it exists or not, potentially leading to an error that
-- appears in `make debug` logs.
createDirIfMissing :: Bool -> FilePath -> IO ()
createDirIfMissing withParents path = do
  exists <- doesPathExist path
  if exists
    then pure ()
    else createDirectoryIfMissing withParents path
