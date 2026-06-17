module Drasil.SSP.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Data.Drasil.Domains (civilEng)

progName :: CI
progName = commonIdeaWithDict (mkUid "ssp") (pn' "Slope Stability analysis Program") "SSP" [civilEng]
