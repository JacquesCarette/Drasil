-- | Contains the high-level functionality to create 'Code' and then produce the actual generated code files
module Language.Drasil.Code.CodeGeneration (
    -- * Preparing the code files
    makeCode,
    makeLangConfig,
    
    -- * Creating the code files
    createCodeFiles
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST (AbstractCode)
import Language.Drasil.Code.Imperative.LanguageRenderer (Config(renderCode), Options)
import Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer (csharpConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer (cppConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.GOOLRenderer (goolConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer (javaConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.ObjectiveCRenderer (objcConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.PythonRenderer (pythonConfig)
import Language.Drasil.Code.Imperative.LanguageRenderer.LuaRenderer (luaConfig)
import Language.Drasil.Code.Imperative.Parsers.ConfigParser (cSharpLabel, cppLabel,
    goolLabel, javaLabel, objectiveCLabel, pythonLabel, luaLabel)

import Data.Function (fix)
import Data.List (intercalate)
import qualified Data.Map as Map (fromList,keys,lookup,Map)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, render)


-- | Map of (label,config) pairs for all supported languages.
langs :: Map.Map String (Options -> Config -> Config)
langs = Map.fromList[
    (cSharpLabel, csharpConfig),
    (cppLabel, cppConfig),
    (goolLabel, goolConfig),
    (javaLabel, javaConfig),
    (objectiveCLabel, objcConfig),
    (pythonLabel, pythonConfig),
    (luaLabel, luaConfig)
  ]

-- | Translates an AbstractCode to Code using the language of the passed Config
makeCode :: Config -> AbstractCode -> Code
makeCode = renderCode

makeLangConfig :: String -> Options -> Config
makeLangConfig l options =
    case Map.lookup l langs of
        Just c  -> fix $ c options
        Nothing -> error errStr
          where errStr = "GOOL.CodeGeneration.makeLangConfig: must supply "
                         ++ listLabels (Map.keys langs)
                         ++ " for the \"Generation Language\" option in the configuration file"

listLabels :: [String] -> String
listLabels ns = intercalate ", " (init ns) ++ ", or " ++ last ns

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files
createCodeFiles :: Code -> IO ()
createCodeFiles (Code cs) = mapM_ createCodeFile cs


createCodeFile :: (FilePath, Doc) -> IO ()
createCodeFile (path, code) = do
    h <- openFile path WriteMode
    hPutStrLn h (render code)
    hClose h
