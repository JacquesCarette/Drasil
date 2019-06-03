-- | Contains the high-level functionality to create 'Code' and then produce the actual generated code files
module Language.Drasil.Code.CodeGeneration (
  -- * Preparing the code files
  makeCode,
  -- * Creating the code files
  createCodeFiles
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New
import Language.Drasil.Code.Imperative.Helpers (ModData(..))

import Text.PrettyPrint.HughesPJ (Doc,render)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Takes code and extensions
makeCode :: [[ModData]] -> [Label] -> Code
makeCode files exts = Code
  [(nm, contents) | (MD nm _ contents) <- concat [map (applyExt ext) files' | (files', ext) <- zip files exts]]

applyExt :: Label -> ModData -> ModData
applyExt ext (MD n b d) = MD (n ++ ext) b d

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files
createCodeFiles :: Code -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles (Code cs) = mapM_ createCodeFile cs


createCodeFile :: (FilePath, Doc) -> IO ()
createCodeFile (path, code) = do
  h <- openFile path WriteMode
  hPutStrLn h (render code)
  hClose h
