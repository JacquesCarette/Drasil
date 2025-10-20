module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (gen, typeCheckSI, genCode, genDot,
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything)
import Drasil.DblPend.Body (srs, printSetting, fullSI)
import Drasil.DblPend.Choices (choices, code)

main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) "DblPend_SRS") srs printSetting
  genCode choices code
  genDot fullSI