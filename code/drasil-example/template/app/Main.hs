module Main (main) where

import GHC.IO.Encoding
import Drasil.Generator (gen, DocSpec(DocSpec), DocType(SRS), 
  Format(..), docChoices, dumpEverything)
import Drasil.Template.Body (fullSI, srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  gen (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) "Template_SRS") srs printSetting
