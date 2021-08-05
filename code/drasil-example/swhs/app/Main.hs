module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genDot, DocSpec(DocSpec), DocType(SRS), Format(..), DebugOption(..), docChoices)
import Drasil.SWHS.Body (srs, printSetting, fullSI)
-- import Drasil.SWHS.Choices (code, choices)

main :: IO ()
main = 
  do
    setLocaleEncoding utf8
    gen (DocSpec (docChoices SRS [HTML, TeX] NoDebug) "SWHS_SRS") srs printSetting
    genDot fullSI
    -- When ready to generate code from SWHS, uncomment this file and Choices
    -- genCode choices code
       
