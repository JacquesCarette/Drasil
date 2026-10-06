module Drasil.PDController.MetaConcepts (projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

projName :: ProjectName
projName = mkCommonProjName (mkUid "pdcontrollerProjName")
  (pn "PD Controller") "PDController"
