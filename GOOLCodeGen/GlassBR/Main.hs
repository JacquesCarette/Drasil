module Main where

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
  genCode cppLabel

genCode :: String -> IO()
genCode lang = createCodeFiles $ makeCode 
  lang 
  (Options Nothing Nothing Nothing (Just "Code"))  
  (toAbsCode "GlassBR" classes)

classes :: [Module]
classes = [inputParameters, derivedValues, inputFormat, inputConstraints, interpolation,   calculations, outputFormat, control, readTable]

