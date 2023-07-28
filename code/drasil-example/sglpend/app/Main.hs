module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, typeCheckSI, genDot, genLog, 
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything)
import Drasil.SglPend.Body (srs, printSetting, fullSI)


main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON]) "SglPend_SRS") srs printSetting
  genDot fullSI
  genLog fullSI printSetting
