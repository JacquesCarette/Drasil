module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genLog, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.Template.Body (srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX]) "Template_SRS") srs printSetting
