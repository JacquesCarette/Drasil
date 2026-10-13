module Drasil.HGHC.MetaConcepts (projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

projName :: ProjectName
projName = mkCommonProjName (mkUid "hghcProjName") (nounPhraseSP "HGHC") "HGHC"
