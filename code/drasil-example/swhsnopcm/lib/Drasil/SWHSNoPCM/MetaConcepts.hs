module Drasil.SWHSNoPCM.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators as NP
import Data.Drasil.Domains (materialEng)

progName :: CI
progName = commonIdeaWithDict (mkUid "swhsNoPCM")
  (nounPhrase' "solar water heating system with no phase change material"
  "solar water heating systems with no phase change material" $ Replace $
  NP.S "Solar Water Heating System with no Phase Change Material") "SWHSNoPCM" [materialEng]
