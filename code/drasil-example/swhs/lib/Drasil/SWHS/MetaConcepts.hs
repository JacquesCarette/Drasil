module Drasil.SWHS.MetaConcepts (progName, progName') where

import Drasil.Database (mkUid)
import Language.Drasil
import Data.Drasil.Domains (materialEng)

progName :: CI
progName = commonIdea (mkUid "swhsName") (nounPhrase "solar water heating system"
  "solar water heating systems") "SWHS" [materialEng]

-- HACK: should re-decompose this noun phrase back into components!
progName' :: CI
progName' = commonIdea (mkUid "swhsPCM") (nounPhraseSP "solar water heating systems incorporating PCM")
  "SWHS"
  []
