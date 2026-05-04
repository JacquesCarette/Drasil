module Drasil.Build.Artifacts.FilePath
  ( PathComponent (..),
    checkValid,
    (</>),
    createDirectory,
    doesPathExist,
    writeFile,
    writeFile'
  )
where

import System.Directory qualified as Dir (createDirectory, doesPathExist)
import System.FilePath qualified as FP (pathSeparator, (</>))
import System.IO qualified as IO (Handle, IOMode(WriteMode), withFile)
import Prelude hiding (writeFile)
import Prelude qualified (writeFile)

-- | Represents a valid component of a path (e.g., a file or directory /name/).
newtype PathComponent = PC {unPC :: String}
  deriving (Eq, Ord, Show)

-- | Internal: Check if a path component (i.e., text before/between/after path
-- separators) is a valid component. Here, validity being defined by not being
-- any of: ., .., ~, or the system-local path separator.
checkValid :: FilePath -> PathComponent
checkValid s
  | s `elem` [".", "..", "~"] = error $ "invalid path component: " ++ show s ++ "."
  | FP.pathSeparator `elem` s =
      error $
        "cannot create path component with " ++ show FP.pathSeparator ++ " in the name."
  | otherwise = PC s

(</>) :: PathComponent -> PathComponent -> PathComponent
(PC a) </> (PC b) = PC $ a FP.</> b

doesPathExist :: PathComponent -> IO Bool
doesPathExist = Dir.doesPathExist . unPC

createDirectory :: PathComponent -> IO ()
createDirectory = Dir.createDirectory . unPC

writeFile :: PathComponent -> String -> IO ()
writeFile (PC pc) = Prelude.writeFile pc

writeFile' :: PathComponent -> (IO.Handle -> IO r) -> IO r
writeFile' (PC pc) = IO.withFile pc IO.WriteMode
