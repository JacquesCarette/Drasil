module Test.Main (main) where

import Language.Drasil.Code.Imperative.Symantics (Label, PackageSym(..))
import Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer (JavaCode(..))
import Language.Drasil.Code.Imperative.LanguageRenderer.PythonRenderer (PythonCode(..))
import Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer (CSharpCode(..))
import Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer (unCPPC)
import Language.Drasil.Code.Imperative.Data (FileData(..), ModData(..), 
  PackData(..), ProgData(..))
import Text.PrettyPrint.HughesPJ (Doc, render)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath.Posix (takeDirectory)
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Test.HelloWorld (helloWorld)
import Test.PatternTest (patternTest)
import Test.FileTests (fileTests)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode (classes unJC)
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "python"
  setCurrentDirectory "python"
  genCode (classes unPC)
  setCurrentDirectory workingDir 
  createDirectoryIfMissing False "csharp"
  setCurrentDirectory "csharp"
  genCode (classes unCSC)
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "cpp"
  setCurrentDirectory "cpp"
  genCode (classes unCPPC)
  setCurrentDirectory workingDir
    
genCode :: [PackData] -> IO()
genCode files = createCodeFiles (concatMap (\p -> replicate (length $ progMods $
  packProg p) (progName $ packProg p)) files) $ makeCode (concatMap (progMods . packProg) 
  files)

classes :: (PackageSym repr) => (repr (Package repr) -> PackData) -> [PackData]
classes unRepr = map unRepr [helloWorld, patternTest, fileTests]

-- | Takes code
makeCode :: [FileData] -> [(FilePath, Doc)]
makeCode files = zip (map filePath files)
  (map (modDoc . fileMod) files)

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files
createCodeFiles :: [Label] -> [(FilePath, Doc)] -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles ns cs = mapM_ createCodeFile (zip ns cs)

createCodeFile :: (Label, (FilePath, Doc)) -> IO ()
createCodeFile (n, (path, code)) = do
    createDirectoryIfMissing False n
    setCurrentDirectory n
    createDirectoryIfMissing True (takeDirectory path)
    h <- openFile path WriteMode
    hPutStrLn h (render code)
    hClose h
    setCurrentDirectory ".."
