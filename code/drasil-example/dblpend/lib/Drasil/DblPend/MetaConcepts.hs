module Drasil.DblPend.MetaConcepts (progName, projName) where
import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)
import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdea (mkUid "dblPendulum") (pn "Double Pendulum") "DblPend" [physics]

projName :: ProjectName
projName = mkCommonProjName (mkUid "dblpendProjName")
  (pn "Double Pendulum") "DblPend"
