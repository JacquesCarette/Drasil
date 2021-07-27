module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocType(SRS, Website), DocSpec(DocSpec))
import Drasil.HGHC.Body (srs, printSetting) --fullSI
-- import Drasil.HGHC.Choices (thisChoices, thisCode)
  
main :: IO ()            
main = do
  setLocaleEncoding utf8
  gen (DocSpec Website "HGHC_SRS") srs printSetting
  gen (DocSpec SRS "HGHC_SRS")     srs printSetting
  -- When ready to generate code, uncomment this file and Choices.hs
  --genCode thisChoices thisCode
  -- When ready to generate traceability graphs, uncomment this and import genDot and fullSI:
  -- genDot fullSI
