-- | Contains the high-level functionality to create 'Code' and then produce the actual generated code files
module Language.Drasil.Code.CodeGeneration (
  -- * Preparing the code files
  makeCode,
  -- * Creating the code files
  createCodeFiles
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), FileData(..), 
  ModData(modDoc))

import Text.PrettyPrint.HughesPJ (Doc,render)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Takes code
makeCode :: [FileData] -> [AuxData] -> Code
makeCode files aux = Code $ zip (map filePath files ++ map auxFilePath aux)
  (map (modDoc . fileMod) files ++ map auxDoc aux)

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files
createCodeFiles :: Code -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles (Code cs) = mapM_ createCodeFile cs


createCodeFile :: (FilePath, Doc) -> IO ()
createCodeFile (path, code) = do
  createDirectoryIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hPutStrLn h (render code)
  hClose h
