module Main (main) where

import GHC.IO.Encoding
import Drasil.Generator (docChoices, genDoc, DocSpec(DocSpec),
  DocType(Lesson), exportSmithEtAlSrsWCodeZoo)
import Drasil.Projectile.Body (si, mkSRS)
import Drasil.Projectile.Choices (choiceCombos)

import qualified Drasil.Projectile.Lesson.Body as PL (nb, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  genDoc (DocSpec (docChoices Lesson []) "Projectile_Lesson") PL.nb PL.printSetting
  exportSmithEtAlSrsWCodeZoo si mkSRS "Projectile_SRS" choiceCombos
