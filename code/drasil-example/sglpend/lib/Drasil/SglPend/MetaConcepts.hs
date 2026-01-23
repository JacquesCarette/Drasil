module Drasil.SglPend.MetaConcepts (progName) where

import Language.Drasil
import Drasil.Metadata.Domains (physics)

progName :: CI
progName = commonIdeaWithDict "sglPendulum" (pn "Single Pendulum") "SglPend" [physics]
