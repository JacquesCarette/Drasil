  
module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genDot, genLog, DocSpec(DocSpec), DocType(SRS,Jupyter), Format(..), docChoices)
import Drasil.Projectile.Body (printSetting, srs, fullSI)
import Drasil.Projectile.Choices (choiceCombos, genCodeWithChoices)

import qualified Drasil.Projectile.Lesson.Body as PL (nb, printSetting)


main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX]) "Projectile_SRS") srs printSetting
  gen (DocSpec (docChoices Jupyter [])      "Projectile_SRS") srs printSetting
  gen (DocSpec (docChoices Jupyter [])      "Projectile Motion Lesson") PL.nb PL.printSetting
  genCodeWithChoices choiceCombos
  genDot fullSI
  genLog fullSI printSetting
  
  