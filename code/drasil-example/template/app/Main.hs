module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(SRS), Format(..))
import Drasil.Template.Body (srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (SRS [HTML, TeX]) "Template_SRS") srs printSetting
