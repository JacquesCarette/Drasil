-- | Contains the high-level functionality to create 'Code' and then produce the actual generated code files.
module Language.Drasil.Code.CodeGeneration (
  -- * Preparing the code files
  makeCode,
  -- * Creating the code files
  createCodeFiles
) where

import System.FilePath.Posix (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc,render)

import Drasil.GOOL (FileData(..), ModData(modDoc))
import Utils.Drasil (createDirIfMissing)

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..))

-- | Makes code from 'FileData' ('FilePath's with module data) and 'AuxData' ('FilePath's with auxiliary document information).
makeCode :: [FileData] -> [AuxData] -> Code
makeCode files aux = Code $ zip (map filePath files ++ map auxFilePath aux)
  (map (modDoc . fileMod) files ++ map auxDoc aux)

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files.
createCodeFiles :: Code -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles (Code cs) = mapM_ createCodeFile cs

-- | Helper that uses pairs of 'Code' to create a file written with the given document at the given 'FilePath'.
createCodeFile :: (FilePath, Doc) -> IO ()
createCodeFile (path, code) = do
  createDirIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hPutStrLn h (render code)
  hClose h
