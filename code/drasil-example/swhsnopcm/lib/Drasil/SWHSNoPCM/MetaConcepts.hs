module Drasil.SWHSNoPCM.MetaConcepts (projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

projName :: ProjectName
projName = mkCommonProjName (mkUid "swhsNoPCMProjName")
  (pn' "Solar Water Heating System with no Phase Change Material") "SWHSNoPCM"
