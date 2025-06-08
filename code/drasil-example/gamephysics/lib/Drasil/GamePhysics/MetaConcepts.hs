module Drasil.GamePhysics.MetaConcepts (progName) where

import Language.Drasil
import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdeaWithDict "gamePhysics" (pn "GamePhysics") "GamePhysics" [physics]