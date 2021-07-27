module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genDot, DocType(SRS, Website), DocSpec(DocSpec))
import Drasil.SWHS.Body (srs, printSetting, fullSI)
-- import Drasil.SWHS.Choices (code, choices)

main :: IO ()
main = 
  do
    setLocaleEncoding utf8
    gen (DocSpec SRS "SWHS_SRS")     srs printSetting
    gen (DocSpec Website "SWHS_SRS") srs printSetting
    genDot fullSI
    -- When ready to generate code from SWHS, uncomment this file and Choices
    -- genCode choices code
       
