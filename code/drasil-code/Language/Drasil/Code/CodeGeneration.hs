-- | Contains the high-level functionality to create 'Code' and then produce the actual generated code files
module Language.Drasil.Code.CodeGeneration (
    -- * Preparing the code files
    makeCode,
    -- * Creating the code files
    createCodeFiles
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New

import Text.PrettyPrint.HughesPJ (Doc,render)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Takes code and extensions
makeCode :: [(Doc, Label, Bool)] -> Label -> Code
makeCode files ext = Code
    [(name ++ ext, file) | (file, name, _) <- files]

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
