module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, typeCheckSI, genCode, genDot, genLog,
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything)
import Drasil.DblPendulum.Body (srs, printSetting, fullSI)
import Drasil.DblPendulum.Choices (choices, code)

main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON]) "DblPendulum_SRS") srs printSetting
  genCode choices code
  genDot fullSI
  genLog fullSI printSetting
