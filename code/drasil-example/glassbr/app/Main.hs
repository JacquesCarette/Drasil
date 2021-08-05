module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genCode, genDot, DocSpec(DocSpec), DocType(SRS), Format(..), DebugOption(..), docChoices)
import Drasil.GlassBR.Body (srs, printSetting, fullSI)
import Drasil.GlassBR.Choices (choices, code)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX] NoDebug) "GlassBR_SRS") srs printSetting
  genCode choices code
  genDot fullSI
