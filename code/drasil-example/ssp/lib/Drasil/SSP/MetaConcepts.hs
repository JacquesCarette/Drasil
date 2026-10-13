module Drasil.SSP.MetaConcepts (projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

projName :: ProjectName
projName = mkCommonProjName (mkUid "sspProjName")
  (pn' "Slope Stability analysis Program") "SSP"
