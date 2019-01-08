module Example.Main (main) where

import New (Label, RenderSym(..))
import NewLanguageRenderer (makeCode, createCodeFiles)
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import LanguageRenderer.NewPythonRenderer (PythonCode(..))
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
  genCode (classes unJC) filenames [".java"]
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "python"
  setCurrentDirectory "python"
  genCode (classes unPC) filenames [".py"]
  setCurrentDirectory workingDir
    
genCode :: [Doc] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts

classes :: (RenderSym repr) => (repr (RenderFile repr) -> Doc) -> [Doc]
classes unRepr = map unRepr [helloWorld, patternTest, fileTests, observer]

filenames :: [Label]
filenames = ["HelloWorld", "PatternTest", "FileTests", "Observer"]
