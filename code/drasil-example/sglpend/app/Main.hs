module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, typeCheckSI, genDot, 
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything,
  MDFlavour(GitHub))
import Drasil.SglPend.Body (srs, printSetting, fullSI)


main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, Jupyter, Markdown GitHub, MDBook]) "SglPend_SRS") srs printSetting
  genDot fullSI
