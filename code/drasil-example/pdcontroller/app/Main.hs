module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Drasil.PDController.Body (printSetting, srs, fullSI)
import Drasil.PDController.Choices (codeChoices, codeSpecs)
import Drasil.Generator (gen, typeCheckSI, genCode, genDot,
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything)


main :: IO ()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) "PDController_SRS") srs printSetting
  genCode codeChoices codeSpecs
  genDot fullSI
