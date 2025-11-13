module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (genDoc, typeCheckSI, genDot,
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything)
import Drasil.SglPend.Body (srs, printSetting, fullSI)


main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  genDoc (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) "SglPend_SRS") srs printSetting
  genDot fullSI
