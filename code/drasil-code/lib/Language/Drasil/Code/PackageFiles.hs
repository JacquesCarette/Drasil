-- | Contains the high-level functionality to consolidate and then produce the
-- actual generated package files.
module Language.Drasil.Code.PackageFiles (
    consolidatePackageFiles,
    createPackageFiles,
) where

import Text.PrettyPrint.HughesPJ (render)

import qualified Drasil.GOOL as G (FileData(..), ModData(modDoc))
import Utils.Drasil (createDirIfMissing)

import Language.Drasil.Code.FileData (FileAndContents(filePath, fileDoc), fileAndContents)
import System.FilePath.Posix (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Converts 'FileData' ('FilePath's with module data) and 'FileAndContents'
-- ('FilePath's with auxiliary document information) into a big list of
-- 'FileAndContents'
consolidatePackageFiles :: [G.FileData] -> [FileAndContents] -> [FileAndContents]
consolidatePackageFiles files aux = aux ++ map (\file -> fileAndContents (G.filePath file) ((G.modDoc . G.fileMod) file)) files

-- | Outputs the requested 'FileAndContents's into system files.
createPackageFiles :: [FileAndContents] -> IO ()
createPackageFiles = mapM_ createPackageFile

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
