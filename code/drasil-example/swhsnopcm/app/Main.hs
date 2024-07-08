module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, typeCheckSI, genCode, genDot,
  DocSpec(DocSpec), DocType(SRS), Format(..), docChoices, dumpEverything,
  MDFlavour(GitHub))
import Drasil.SWHSNoPCM.Body (srs, printSetting, fullSI)
import Drasil.SWHSNoPCM.Choices (choices, code)
       
main :: IO ()            
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, Jupyter, Markdown GitHub, MDBook]) "SWHSNoPCM_SRS") srs printSetting
  genCode choices code
  genDot fullSI
