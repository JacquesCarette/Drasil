module Drasil.BinaryStar.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdea (mkUid "bss")
  (pn "Binary Star System Simulator") "BSS" [physics]

projName :: ProjectName
projName = mkCommonProjName (mkUid "bssProjName") (nounPhraseSP "BSS") "BSS"
