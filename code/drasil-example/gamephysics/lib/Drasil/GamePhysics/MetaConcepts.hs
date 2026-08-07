module Drasil.GamePhysics.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)
import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdea (mkUid "gamePhysics") (pn "GamePhysics") "GamePhysics" [physics]

projName :: ProjectName
projName = mkCommonProjName (mkUid "gamephysicsProjName") (nounPhraseSP "GamePhysics") "GamePhysics"
