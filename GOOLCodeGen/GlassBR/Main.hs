module Main where

import System.Directory

import Language.Drasil.Code
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
  genCode pythonLabel
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "cpp"
  setCurrentDirectory "cpp"
  genCode cppLabel
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode javaLabel
  setCurrentDirectory workingDir
  createDirectoryIfMissing False "csharp"
  setCurrentDirectory "csharp"
  genCode cSharpLabel
  setCurrentDirectory workingDir
  
genCode :: String -> IO()
genCode lang = createCodeFiles $ makeCode 
  lang 
  (Options Nothing Nothing Nothing (Just "Code"))  
  (toAbsCode "GlassBR" classes)

classes :: [Module]
classes = [inputParameters, derivedValues, inputFormat, inputConstraints, interpolation,   calculations, outputFormat, control, readTable]

