module Drasil.SWHSNoPCM.MetaConcepts (progName) where

import Language.Drasil
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators as NP
import Drasil.Metadata (materialEng)

progName :: CI
progName = commonIdeaWithDict "swhsNoPCM"
  (nounPhrase' "solar water heating system with no phase change material"
  "solar water heating systems with no phase change material" $ Replace $
  NP.S "Solar Water Heating System with no Phase Change Material") "SWHSNoPCM" [materialEng]
