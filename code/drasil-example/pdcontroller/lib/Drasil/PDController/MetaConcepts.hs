module Drasil.PDController.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil

progName :: CI
progName = commonIdeaWithDict (mkUid "pdControllerApp") (pn "PD Controller") "PD Controller" []
