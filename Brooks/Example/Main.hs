module Example.Main (main) where

import New (Label, Library,
  RenderSym(..), KeywordSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..),
  FunctionSym(..), SelectorFunction(..), ScopeSym(..), MethodTypeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import NewLanguageRenderer (makeCode, createCodeFiles)
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import LanguageRenderer.NewPythonRenderer (PythonCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Example.HelloWorld (helloWorld)
import Example.PatternTest (patternTest)
import Example.FileTests (fileTests)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode (map unJC [helloWorld, patternTest, fileTests]) ["HelloWorld", "PatternTest", "FileTests"] [".java"]
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "python"
  setCurrentDirectory "python"
  genCode (map unPC [helloWorld, patternTest, fileTests]) ["HelloWorld", "PatternTest", "FileTests"] [".py"]
  setCurrentDirectory workingDir
    
genCode :: [Doc] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts