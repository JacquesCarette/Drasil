module Main where

import Language.Drasil.Code
import Modules.InputParameters
import Modules.InputFormat
import Modules.DerivedValues
import Modules.InputConstraints
import Modules.Interpolation
import Modules.Calculations

main :: IO()
main = do
  genCode pythonLabel
  --genCode goolLabel

genCode :: String -> IO()
genCode lang = createCodeFiles $ makeCode 
  lang 
  (Options Nothing Nothing Nothing (Just "Code")) 
  (map moduleName classes) 
  (toAbsCode "GlassBR" classes)

classes :: [Module]
classes = [inputParameters, derivedValues, inputFormat, inputConstraints, interpolation,   calculations]

