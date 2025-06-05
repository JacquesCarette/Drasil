module Drasil.GlassBR.MetaConcepts (progName) where

import Language.Drasil
import Drasil.GlassBR.Concepts (idglass)

progName :: CI
progName = commonIdeaWithDict "glassBR" (pn "GlassBR") "GlassBR"  [idglass]
