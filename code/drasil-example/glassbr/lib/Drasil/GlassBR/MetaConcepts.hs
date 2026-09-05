module Drasil.GlassBR.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.GlassBR.Concepts (glass)

progName :: CI
progName = commonIdea (mkUid "glassBR") (pn "GlassBR") "GlassBR"  [glass]
