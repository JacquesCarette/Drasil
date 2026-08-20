module Drasil.PDController.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil

progName :: CI
progName = commonIdea (mkUid "pdControllerApp")
  (pn "Proportional Differential Controller") "PD Controller" []
