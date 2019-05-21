module Example.Main (main) where

import New (Label, PackageSym(..))
import NewLanguageRenderer (makeCode, createCodeFiles)
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import LanguageRenderer.NewPythonRenderer (PythonCode(..))
import LanguageRenderer.NewCSharpRenderer (CSharpCode(..))
import LanguageRenderer.NewCppRenderer (CppSrcCode(..), CppHdrCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Example.HelloWorld (helloWorld)
import Example.Helper (helper)
import Example.PatternTest (patternTest)
import Example.FileTests (fileTests)
import Example.Observer (observer)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode (classes unJC) [".java"]
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "python"
  setCurrentDirectory "python"
  genCode (classes unPC) [".py"]
  setCurrentDirectory workingDir 
  createDirectoryIfMissing False "csharp"
  setCurrentDirectory "csharp"
  genCode (classes unCSC) [".cs"]
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "cpp"
  setCurrentDirectory "cpp"
  genCode (classes unCPPSC) [".cpp"]
  genCode (classes unCPPHC) [".hpp"]
  setCurrentDirectory workingDir
    
genCode :: [([(Doc, Label, Bool)], Label)] -> [Label] -> IO()
genCode files exts = createCodeFiles $ makeCode (concatMap fst files) exts

classes :: (PackageSym repr) => (repr (Package repr) -> ([(Doc, Label, Bool)], Label)) -> [([(Doc, Label, Bool)], Label)]
classes unRepr = map unRepr [helloWorld, helper, patternTest, fileTests, observer]
