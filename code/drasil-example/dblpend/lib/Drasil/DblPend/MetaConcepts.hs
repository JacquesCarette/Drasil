module Drasil.DblPend.MetaConcepts (progName) where
import Language.Drasil
import Drasil.Metadata.Domains (physics)

progName :: CI
progName = commonIdeaWithDict "dblPendulum" (pn "Double Pendulum") "DblPend" [physics]
