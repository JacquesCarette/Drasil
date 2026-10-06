module Drasil.Projectile.Lesson.MetaConcepts (projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

projName :: ProjectName
projName = mkCommonProjName (mkUid "projLsnPlanProjName") (nounPhraseSP "Projectile Motion Lesson") "Projectile Lesson"
