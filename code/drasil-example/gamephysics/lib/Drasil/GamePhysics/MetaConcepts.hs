module Drasil.GamePhysics.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdeaWithDict (mkUid "gamePhysics") (pn "GamePhysics") "GamePhysics" [physics]
