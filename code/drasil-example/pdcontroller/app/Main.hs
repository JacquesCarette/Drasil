module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Drasil.PDController.Body (printSetting, srs, fullSI)
import Drasil.PDController.Choices (codeChoices, codeSpecs)
import Language.Drasil.Generate (gen, genCode, genDot, DocSpec(DocSpec), DocType(SRS))


main :: IO ()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS "PDController_SRS") srs printSetting
  genCode codeChoices codeSpecs
  genDot fullSI
