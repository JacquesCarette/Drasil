module Drasil.GlassBR.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)
import Drasil.GlassBR.Concepts (idglass)

progName :: CI
progName = commonIdea (mkUid "glassBR") (pn "GlassBR") "GlassBR"  [idglass]

projName :: ProjectName
projName = mkCommonProjName (mkUid "glassBRProjName") (nounPhraseSP "GlassBR") "GlassBR"
