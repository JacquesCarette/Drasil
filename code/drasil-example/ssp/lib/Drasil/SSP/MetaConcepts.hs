module Drasil.SSP.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)
import Data.Drasil.Domains (civilEng)

progName :: CI
progName = commonIdea (mkUid "ssp") (pn' "Slope Stability analysis Program") "SSP" [civilEng]

projName :: ProjectName
projName = mkCommonProjName (mkUid "sspProjName") (nounPhraseSP "Slope Stability analysis Program") "SSP"
