module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.WebsiteLayout.Body (srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX]) "Website_SRS") srs printSetting
