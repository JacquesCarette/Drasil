module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genDot, DocSpec(DocSpec), DocType(SRS), Format(..))
import Drasil.Projectile.Body (printSetting, srs, fullSI)
import Drasil.Projectile.Choices (choiceCombos, genCodeWithChoices)


main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (SRS [HTML, TeX]) "Projectile_SRS") srs printSetting
  genCodeWithChoices choiceCombos
  genDot fullSI