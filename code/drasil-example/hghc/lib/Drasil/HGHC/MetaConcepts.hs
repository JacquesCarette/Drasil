module Drasil.HGHC.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

-- hack... but will have to stay until a progName is not a CI
progName :: CI
progName = commonIdea (mkUid "hghc") (pn "HGHC") "HGHC" []

projName :: ProjectName
projName = mkCommonProjName (mkUid "hghcProjName") (nounPhraseSP "HGHC") "HGHC"
