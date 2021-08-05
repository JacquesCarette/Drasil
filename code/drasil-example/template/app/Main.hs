module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(SRS), Format(..), DebugOption(..), docChoices)
import Drasil.Template.Body (srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX] NoDebug) "Template_SRS") srs printSetting
