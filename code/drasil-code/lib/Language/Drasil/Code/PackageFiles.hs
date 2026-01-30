-- | Contains the high-level functionality to consolidate and then produce the
-- actual generated package files.
module Language.Drasil.Code.PackageFiles (
    createPackageFile,
) where

import Text.PrettyPrint.HughesPJ (render)

import Utils.Drasil (createDirIfMissing)

import Language.Drasil.Code.FileData (FileAndContents(filePath, fileDoc))
import System.FilePath.Posix (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Helper to convert a FileAndContents into a real file with the given document
-- at the given FilePath
createPackageFile :: FileAndContents -> IO ()
createPackageFile file = do
  let path     = filePath file
      contents = fileDoc file
  createDirIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hPutStrLn h (render contents)
  hClose h
