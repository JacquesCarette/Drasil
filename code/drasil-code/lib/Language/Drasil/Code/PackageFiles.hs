-- | Contains the high-level functionality to create 'PackageFiles' and then produce the
-- actual generated package files.
module Language.Drasil.Code.PackageFiles (
    consolidatePackageFiles,
    createPackageFiles,
) where

import Text.PrettyPrint.HughesPJ ( Doc, Doc, render )

import Drasil.GOOL (FileData(..), ModData(modDoc))
import Utils.Drasil (createDirIfMissing)

import Language.Drasil.Code.FileData (FileAndContents(fileDoc))
import qualified Language.Drasil.Code.FileData as D (
  filePath)
import System.FilePath.Posix (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Represents the generated files of a package as a unified
--   list of pairs of file names and rendered contents.
newtype PackageFiles = PackageFiles [(FilePath, Doc)]

-- | Converts 'FileData' ('FilePath's with module data) and 'FileAndContents'
-- ('FilePath's with auxiliary document information) into 'PackageFiles'
-- (a unified format for all file types)
consolidatePackageFiles :: [FileData] -> [FileAndContents] -> PackageFiles
consolidatePackageFiles files aux = PackageFiles $ zip (map filePath files ++ map D.filePath aux)
  (map (modDoc . fileMod) files ++ map fileDoc aux)

-- | Outputs the requested 'Package Files' into system files.
createPackageFiles :: PackageFiles -> IO ()
createPackageFiles (PackageFiles cs) = mapM_ createPackageFile cs

-- | Helper that uses pairs of 'PackageFiles' to create a file written with the given
-- document at the given 'FilePath'.
createPackageFile :: (FilePath, Doc) -> IO ()
createPackageFile (path, contents) = do
  createDirIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hPutStrLn h (render contents)
  hClose h
