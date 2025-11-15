module Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (exportSmithEtAlSrsWCodeZoo, exportLessonPlan)

import qualified Drasil.Projectile.Body as Proj (si, mkSRS)
import qualified Drasil.Projectile.Choices as Proj (choiceCombos)

import qualified Drasil.Projectile.Lesson.Body as ProjLP

main :: IO ()
main = do
  setLocaleEncoding utf8
  exportSmithEtAlSrsWCodeZoo Proj.si Proj.mkSRS "Projectile_SRS" Proj.choiceCombos
  exportLessonPlan ProjLP.si ProjLP.nbDecl "Projectile_Lesson"
