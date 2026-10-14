module Drasil.SglPend.MetaConcepts (projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

projName :: ProjectName
projName = mkCommonProjName (mkUid "sglpendProjName")
  (pn "Single Pendulum") "SglPend"
