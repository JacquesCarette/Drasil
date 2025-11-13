module Main (main) where

import GHC.IO.Encoding
import Drasil.Generator (genDoc, typeCheckSI, genDot,
  DocSpec(DocSpec), DocType(SRS, Lesson), Format(..), docChoices,
  dumpEverything)
import Drasil.Projectile.Body (printSetting, srs, fullSI)
import Drasil.Projectile.Choices (choiceCombos, genCodeWithChoices)

import qualified Drasil.Projectile.Lesson.Body as PL (nb, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  dumpEverything fullSI printSetting ".drasil/"
  typeCheckSI fullSI
  genDoc (DocSpec (docChoices SRS [HTML, TeX, Jupyter, MDBook]) "Projectile_SRS") srs printSetting
  genDoc (DocSpec (docChoices Lesson []) "Projectile_Lesson") PL.nb PL.printSetting
  genCodeWithChoices choiceCombos
  genDot fullSI
