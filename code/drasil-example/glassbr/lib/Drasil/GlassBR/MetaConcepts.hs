module Drasil.GlassBR.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.GlassBR.Concepts (idglass)

progName :: CI
progName = commonIdeaWithDict (mkUid "glassBR") (pn "GlassBR") "GlassBR"  [idglass]
