-- | Contains the high-level functionality to create 'Code' and then produce the actual generated code files
module GOOL.CodeGeneration (
    -- * Preparing the code files
    makeCode,
    
    -- * Creating the code files
    createCodeFiles
) where

import GOOL.Code (Code(..))
import GOOL.CodeGeneration.AbstractCode (AbstractCode(..),Package(..),Payload(..))
import GOOL.CodeGeneration.LanguageRenderer (Config(renderCode,getEnv))
import GOOL.CodeGeneration.LanguageRenderer.CSharpRenderer (csharpConfig)
import GOOL.CodeGeneration.LanguageRenderer.CppRenderer (cppConfig)
import GOOL.CodeGeneration.LanguageRenderer.GOOLRenderer (goolConfig)
import GOOL.CodeGeneration.LanguageRenderer.JavaRenderer (javaConfig)
import GOOL.CodeGeneration.LanguageRenderer.ObjectiveCRenderer (objcConfig)
import GOOL.CodeGeneration.LanguageRenderer.PythonRenderer (pythonConfig)
import GOOL.CodeGeneration.LanguageRenderer.LuaRenderer (luaConfig)
import GOOL.Parsers.ConfigParser (cSharpLabel,cppLabel,goolLabel,javaLabel,objectiveCLabel,pythonLabel,luaLabel)
import GOOL.Auxil.DataTypes (Options(..))

import System.Random (mkStdGen, randomRs)
import Data.List (intercalate)
import qualified Data.Map as Map (fromList,keys,lookup,Map)
import Data.Map ((!))
import Text.PrettyPrint.HughesPJ (Doc,render,int)
import System.IO
import Data.Function (fix)

-- | Map of (label,config) pairs for all supported languages.
langs :: Map.Map String (Options -> Config -> Config)
langs = Map.fromList [
    (cSharpLabel, csharpConfig),
    (cppLabel, cppConfig),
    (goolLabel, goolConfig),
    (javaLabel, javaConfig),
    (objectiveCLabel, objcConfig),
    (pythonLabel, pythonConfig),
    (luaLabel, luaConfig)
    ]

-- | Takes a language parameter, a set of optional parameters, and a list of module names, and passes an 'AbstractCode' to the required rendering function, which produces a 'Code'
makeCode :: String -> Options -> [String] -> AbstractCode -> Code
makeCode l options lbls code@(AbsCode (Pack _ _ (Payload vl)))  = 
    -- First, if we have gen-time globals, instantiate them
    case Map.lookup l langs of 
        Just c  -> renderCode config lbls code
          where
            config = case vl of
                       [] -> fix $ c options
                       _  -> updateConfig options (c options) vl
        Nothing -> error errStr
          where errStr = "GOOL.CodeGeneration.makeCode: must supply " 
                         ++ (listLabels $ Map.keys langs) 
                         ++ " for the \"Generation Language\" option in the configuration file"

listLabels :: [String] -> String
listLabels ns = intercalate ", " (init ns) ++ ", or " ++ last ns

updateConfig :: Options -> (Config -> Config) -> [String] -> Config
updateConfig o c vl = 
   case studentid o of
       Just num -> 
           let i = length vl in
           let vals = map int . take i . randomRs (1,99) . mkStdGen . fromInteger $ num in
           let envMap = Map.fromList $ zip vl vals in
           let newc = \cfg -> (c cfg) { getEnv = (envMap !) } in
           (fix newc) { getEnv = \_ -> error "do we get here 2?" }
           
       Nothing  -> error "must supply a student number for Assign2"

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
