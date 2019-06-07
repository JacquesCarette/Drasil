module Main where

import System.Directory

import Language.Drasil.Code.Imperative.New (Label)
import Language.Drasil.Code.Imperative.NewLanguageRenderer (makeCode, createCodeFiles)
import Language.Drasil.Code.Imperative.LanguageRenderer.NewJavaRenderer (JavaCode(..))
import Language.Drasil.Code.Imperative.LanguageRenderer.NewPythonRenderer (PythonCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

import Modules.InputParameters
import Modules.InputFormat
import Modules.DerivedValues
import Modules.InputConstraints
import Modules.Interpolation
import Modules.Calculations
import Modules.OutputFormat
import Modules.Control
import Modules.ReadTable

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "python"
  setCurrentDirectory "python"
  genCode (classes unPC) filenames [".py"]
  setCurrentDirectory workingDir
  -- createDirectoryIfMissing False "cpp"
  -- setCurrentDirectory "cpp"
  -- genCode cppLabel
  -- setCurrentDirectory workingDir
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode (classes unJC) filenames [".java"]
  setCurrentDirectory workingDir
  -- createDirectoryIfMissing False "csharp"
  -- setCurrentDirectory "csharp"
  -- genCode cSharpLabel
  -- setCurrentDirectory workingDir

genCode :: [Doc] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts

classes :: (RenderSym repr) => (repr (RenderFile repr) -> Doc) -> [Doc]
classes unRepr = map unRepr [inputParameters, derivedValues, inputFormat, inputConstraints, interpolation, calculations, outputFormat, control, readTable]

filenames :: [Label]
filenames = ["inputParameters", "derivedValues", "inputFormat", "inputConstraints", "interpolation", "calculations", "outputFormat", "control", "readTable"]]
