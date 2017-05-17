module Main where

import Language.Drasil.Code
import Modules.InputParameters
import Modules.InputConstraints

main :: IO()
main = do
  genCode pythonLabel
  genCode goolLabel

genCode :: String -> IO()
genCode lang = createCodeFiles $ makeCode 
  lang 
  (Options Nothing Nothing Nothing (Just "Code")) 
  (map getClassName classes) 
  (toAbsCode "GlassBR" classes)

classes :: [Class]
classes = [inputParameters, inputConstraints]

