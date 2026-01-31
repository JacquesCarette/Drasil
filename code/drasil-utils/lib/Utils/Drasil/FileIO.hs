module Utils.Drasil.FileIO (createFile) where

import Utils.Drasil.Directory (createDirIfMissing)
import Text.PrettyPrint.HughesPJ (Doc, render)
import System.FilePath.Posix (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Helper to convert a FileAndContents into a real file with the given document
-- at the given FilePath
createFile :: FilePath -> Doc -> IO ()
createFile path contents = do
  createDirIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hPutStrLn h (render contents)
  hClose h
