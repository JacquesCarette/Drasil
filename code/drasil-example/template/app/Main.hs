module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(SRS),
  Format(..), docChoices, dumpEverything, typeCheckSI)
import Drasil.Template.Body (fullSI, srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) "Template_SRS") srs printSetting
