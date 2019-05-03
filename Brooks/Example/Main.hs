module Example.Main (main) where

import New (Label, RenderSym(..))
import NewLanguageRenderer (makeCode, createCodeFiles)
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import LanguageRenderer.NewPythonRenderer (PythonCode(..))
import LanguageRenderer.NewCSharpRenderer (CSharpCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Example.HelloWorld (helloWorld)
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
    
genCode :: [(Doc, Label)] -> [Label] -> IO()
genCode files exts = createCodeFiles $ makeCode files exts

classes :: (RenderSym repr) => (repr (RenderFile repr) -> (Doc, Label)) -> [(Doc, Label)]
classes unRepr = map unRepr [helloWorld, patternTest, fileTests, observer]
