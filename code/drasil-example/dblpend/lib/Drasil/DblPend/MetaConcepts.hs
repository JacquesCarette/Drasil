module Drasil.DblPend.MetaConcepts (progName) where
import Language.Drasil
import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdeaWithDict "dblPendulum" (pn "Double Pendulum") "DblPend" [physics]