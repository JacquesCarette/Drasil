module Drasil.SWHS.MetaConcepts where

import Language.Drasil
import Drasil.Metadata (materialEng)
import Drasil.SWHS.Concepts (phsChgMtrl)

progName :: CI
progName = commonIdeaWithDict "swhsName"   (nounPhrase "solar water heating system"
  "solar water heating systems") "SWHS" [materialEng]

progName' :: CI
-- Nounphrase'' hack to get nounPhraseSP words to accept
-- nounPhrases instead of strings
-- Another capitalization hack.
progName' = commonIdea "swhsPCM" (nounPhrase''
  (S "solar water heating systems incorporating PCM")
  (S "solar water heating systems incorporating PCM")
  CapFirst CapWords)
  "SWHS"
  []
