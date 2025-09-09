module Main (main) where

import GHC.IO.Encoding
import Drasil.Generator (gen, typeCheckSI, genDot, 
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything)
import Drasil.SSP.Body (srs, printSetting, fullSI)
-- import Drasil.SSP.Choices
       
main :: IO ()            
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) "SSP_SRS") srs printSetting
  genDot fullSI
  -- for when we can generate code again, uncomment this file and Choices.hs
  --genCode choices code
