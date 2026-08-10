module Drasil.PDController.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

progName :: CI
progName = commonIdea (mkUid "pdControllerApp") (pn "PD Controller") "PD Controller" []

projName :: ProjectName
projName = mkCommonProjName (mkUid "pdcontrollerProjName")
  (pn "PD Controller") "PDController"
