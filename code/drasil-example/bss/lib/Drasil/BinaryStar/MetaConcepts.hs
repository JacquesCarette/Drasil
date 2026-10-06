module Drasil.BinaryStar.MetaConcepts (projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

projName :: ProjectName
projName = mkCommonProjName (mkUid "bssProjName")
  (pn "Binary Star System Simulator") "BSS"
