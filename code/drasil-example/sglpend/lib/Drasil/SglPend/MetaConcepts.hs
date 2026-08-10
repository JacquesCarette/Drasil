module Drasil.SglPend.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)
import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdea (mkUid "sglPendulum") (pn "Single Pendulum") "SglPend" [physics]

projName :: ProjectName
projName = mkCommonProjName (mkUid "sglpendProjName")
  (pn "Single Pendulum") "SglPend"
