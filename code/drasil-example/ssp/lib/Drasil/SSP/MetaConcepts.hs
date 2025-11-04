module Drasil.SSP.MetaConcepts (progName) where

import Language.Drasil
import Drasil.Metadata (civilEng)

progName :: CI
progName = commonIdeaWithDict "ssp" (pn' "Slope Stability analysis Program") "SSP" [civilEng]
