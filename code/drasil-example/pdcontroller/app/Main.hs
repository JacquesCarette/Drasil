module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Drasil.PDController.Body (printSetting, srs, fullSI)
import Drasil.PDController.Choices (codeChoices, codeSpecs)
import Language.Drasil.Generate (gen, typeCheckSI, genCode, genDot, genLog, 
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything)


main :: IO ()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON]) "PDController_SRS") srs printSetting
  genCode codeChoices codeSpecs
  genDot fullSI
  genLog fullSI printSetting
