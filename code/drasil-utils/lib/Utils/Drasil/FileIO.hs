module Utils.Drasil.FileIO (createFile) where

import Utils.Drasil.Directory (createDirIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Write a String to a given 'FilePath'.
createFile :: FilePath -> String -> IO ()
createFile path contents = do
  createDirIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hPutStrLn h contents
  hClose h
