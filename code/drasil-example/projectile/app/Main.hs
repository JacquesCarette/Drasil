  
module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genDot, DocSpec(DocSpec), DocType(SRS,Jupyter), Format(..), DebugOption(..), docChoices)
import Drasil.Projectile.Body (printSetting, srs, fullSI)
import Drasil.Projectile.Choices (choiceCombos, genCodeWithChoices)

import qualified Drasil.Projectile.Lesson.Body as PL (nb, printSetting)


main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices SRS [HTML, TeX] NoDebug)     "Projectile_SRS") srs printSetting
  gen (DocSpec (docChoices Jupyter [] NoDebug) "Projectile_SRS") srs printSetting
  gen (DocSpec (docChoices Jupyter [] NoDebug) "Projectile Motion Lesson") PL.nb PL.printSetting
  genCodeWithChoices choiceCombos
  genDot fullSI
  
  