module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(SRS), 
  Format(..), docChoices, dumpEverything)
import Drasil.Template.Body (fullSI, srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI ".drasil/"
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON]) "Template_SRS") srs printSetting
