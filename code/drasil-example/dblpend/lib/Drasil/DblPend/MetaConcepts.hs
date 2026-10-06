module Drasil.DblPend.MetaConcepts (projName) where
import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

projName :: ProjectName
projName = mkCommonProjName (mkUid "dblpendProjName")
  (pn "Double Pendulum") "DblPend"
