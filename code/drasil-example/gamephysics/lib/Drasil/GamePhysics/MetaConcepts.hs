module Drasil.GamePhysics.MetaConcepts (progName) where

import Language.Drasil
import Drasil.Metadata.Domains (physics)

progName :: CI
progName = commonIdeaWithDict "gamePhysics" (pn "GamePhysics") "GamePhysics" [physics]
