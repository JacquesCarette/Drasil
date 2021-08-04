module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genCode, genDot, DocType(SRS), DocSpec(DocSpec))
import Drasil.NoPCM.Body (srs, printSetting, fullSI)
import Drasil.NoPCM.Choices (choices, code)
       
main :: IO ()            
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS "NoPCM_SRS") srs printSetting
  genCode choices code
  genDot fullSI