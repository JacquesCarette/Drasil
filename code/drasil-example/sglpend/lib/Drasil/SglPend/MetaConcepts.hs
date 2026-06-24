module Drasil.SglPend.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdea (mkUid "sglPendulum") (pn "Single Pendulum") "SglPend" [physics]
