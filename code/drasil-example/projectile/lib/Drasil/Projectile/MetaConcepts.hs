module Drasil.Projectile.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

progName :: CI
progName = commonIdea (mkUid "projectileApp") (pn "Projectile") "Projectile" []

projName :: ProjectName
projName = mkCommonProjName (mkUid "projectileProjName") (nounPhraseSP "Projectile") "Projectile"
