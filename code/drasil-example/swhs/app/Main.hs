module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, typeCheckSI, genDot, 
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything,
  MDFlavour(GitHub))
import Drasil.SWHS.Body (srs, printSetting, fullSI)
-- import Drasil.SWHS.Choices (code, choices)

main :: IO ()
main = 
  do
    setLocaleEncoding utf8
    dumpEverything fullSI printSetting ".drasil/"
    typeCheckSI fullSI
    gen (DocSpec (docChoices SRS [HTML, TeX, Jupyter, Markdown GitHub, MDBook]) "SWHS_SRS") srs printSetting
    genDot fullSI
    -- When ready to generate code from SWHS, uncomment this file and Choices
    -- genCode choices code
       
