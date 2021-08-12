module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genCode, genDot, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.GlassBR.Body (srs, printSetting, fullSI)
import Drasil.GlassBR.Choices (choices, code)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX]) "GlassBR_SRS") srs printSetting
  genCode choices code
  genDot fullSI
