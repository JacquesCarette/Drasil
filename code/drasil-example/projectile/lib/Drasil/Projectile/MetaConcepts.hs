module Drasil.Projectile.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil

progName :: CI
progName = commonIdeaWithDict (mkUid "projectileApp") (pn "Projectile") "Projectile" []
