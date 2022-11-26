module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Drasil.PDController.Body (printSetting, srs, fullSI)
import Drasil.PDController.Choices (codeChoices, codeSpecs)
import Language.Drasil.Generate (gen, typeCheckSI, genCode, genDot, genLog, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)


main :: IO ()
main = do
  setLocaleEncoding utf8
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX]) "PDController_SRS") srs printSetting
  genCode codeChoices codeSpecs
  genDot fullSI
  genLog fullSI printSetting
