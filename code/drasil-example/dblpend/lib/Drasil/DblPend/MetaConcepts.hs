module Drasil.DblPend.MetaConcepts (progName) where
import Drasil.Database (mkUid)
import Language.Drasil
import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdea (mkUid "dblPendulum") (pn "Double Pendulum") "DblPend" [physics]
