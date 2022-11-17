module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, typeCheckSI, genCode, genDot, genLog, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.NoPCM.Body (srs, printSetting, fullSI)
import Drasil.NoPCM.Choices (choices, code)
       
main :: IO ()            
main = do
  setLocaleEncoding utf8
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX]) "NoPCM_SRS") srs printSetting
  genCode choices code
  genDot fullSI
  genLog fullSI printSetting
