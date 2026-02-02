module Utils.Drasil.FileIO (createFile) where

import Utils.Drasil.Directory (createDirIfMissing)
import Text.PrettyPrint.HughesPJ (Doc, render)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Write a 'Doc' with default rendering options to a given 'FilePath'.
createFile :: FilePath -> Doc -> IO ()
createFile path contents = do
  createDirIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hPutStrLn h (render contents)
  hClose h
