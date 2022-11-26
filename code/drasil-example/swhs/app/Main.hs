module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, typeCheckSI, genDot, genLog, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.SWHS.Body (srs, printSetting, fullSI)
-- import Drasil.SWHS.Choices (code, choices)

main :: IO ()
main = 
  do
    setLocaleEncoding utf8
    typeCheckSI fullSI
    gen (DocSpec (docChoices SRS [HTML, TeX]) "SWHS_SRS") srs printSetting
    genDot fullSI
    genLog fullSI printSetting
    -- When ready to generate code from SWHS, uncomment this file and Choices
    -- genCode choices code
       
