module Drasil.GamePhysics.MetaConcepts (progName) where

import Language.Drasil
import Drasil.Metadata (physics)

progName :: CI
progName = commonIdeaWithDict "gamePhysics" (pn "GamePhysics") "GamePhysics" [physics]