-- | Contains the high-level functionality to create 'Code' and then produce the actual generated code files
module Language.Drasil.Code.CodeGeneration (
    -- * Preparing the code files
    -- makeCode,
    
    -- * Creating the code files
    -- createCodeFiles
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New
import Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer (csharpConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer (cppConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.GOOLRenderer (goolConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer (javaConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.NewJavaRenderer (JavaCode(unJC))
import Language.Drasil.Code.Imperative.LanguageRenderer.ObjectiveCRenderer (objcConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.PythonRenderer (pythonConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.LuaRenderer (luaConfig)
import Language.Drasil.Code.Imperative.Parsers.ConfigParser (cSharpLabel,cppLabel,goolLabel,javaLabel,objectiveCLabel,pythonLabel,luaLabel)
import Language.Drasil.Code.Imperative.LanguageRenderer (Options)

import Data.List (intercalate)
import qualified Data.Map as Map (fromList,keys,lookup,Map)
import Text.PrettyPrint.HughesPJ (Doc,render)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))
import Data.Function (fix)

-- -- | Takes a language parameter, a set of optional parameters, and a list of module names, and passes an 'AbstractCode' to the required rendering function, which produces a 'Code'
-- makeCode :: [a] -> [Label] -> [Label] -> [(FilePath, a)]
-- makeCode files names exts = 
--     [(name ++ ext, file) | (name, (file, ext)) <- zip (duplicateListElems names) (zip files (cycle exts))]

-- duplicateListElems :: [a] -> [a]
-- duplicateListElems [] = []
-- duplicateListElems x:xs = x:x:duplicateListElems xs

------------------
-- IO Functions --
------------------

-- -- | Creates the requested 'Code' by producing files
-- createCodeFiles :: Code -> IO () -- [(FilePath, Doc)] -> IO ()
-- createCodeFiles (Code cs) = mapM_ createCodeFile cs

-- createCodeFile :: (FilePath, Doc) -> IO ()
-- createCodeFile (path, code) = do
--     h <- openFile path WriteMode
--     hPutStrLn h (render code)
--     hClose h
