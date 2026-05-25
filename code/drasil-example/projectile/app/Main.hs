module Main (main) where

import Drasil.Generator (caseStudyMainSRSWCodeZooWLsnPlan)

import qualified Drasil.Projectile.Body as Proj (si, mkSRS)
import qualified Drasil.Projectile.Choices as Proj (choiceCombos)

import qualified Drasil.Projectile.Lesson.Body as ProjLP

main :: IO ()
main = caseStudyMainSRSWCodeZooWLsnPlan Proj.si Proj.mkSRS "Projectile_SRS"
  Proj.choiceCombos ProjLP.si ProjLP.nbDecl "Projectile_Lesson"
