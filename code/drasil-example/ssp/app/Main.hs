module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, typeCheckSIQDs, genDot, genLog, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.SSP.Body (srs, printSetting, fullSI)
-- import Drasil.SSP.Choices
       
main :: IO ()            
main = do
  setLocaleEncoding utf8
  typeCheckSIQDs fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX]) "SSP_SRS") srs printSetting
  genDot fullSI
  genLog fullSI printSetting
  -- for when we can generate code again, uncomment this file and Choices.hs
  --genCode choices code
