module Drasil.SWHS.MetaConcepts where

import Language.Drasil
import Drasil.Metadata (materialEng)

progName :: CI
progName = commonIdeaWithDict "swhsName"   (nounPhrase "solar water heating system"
  "solar water heating systems") "SWHS" [materialEng]

-- HACK: should re-decompose this noun phrase back into components!
progName' :: CI
progName' = commonIdeaWithDict "swhsPCM" (nounPhraseSP "solar water heating systems incorporating PCM")
  "SWHS"
  []
