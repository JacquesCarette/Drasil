module Drasil.SWHS.MetaConcepts (swhs, progName, progName', projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)
import Data.Drasil.Domains (materialEng)

swhs :: IdeaDict
swhs = idea (mkUid "swhsIdea")
  (nounPhrase "solar water heating system" "solar water heating systems")
  "SWHS"

progName :: CI
progName = commonIdea (mkUid "swhsName") (nounPhrase "solar water heating system"
  "solar water heating systems") "SWHS" [materialEng]

-- HACK: should re-decompose this noun phrase back into components!
progName' :: CI
progName' = commonIdea (mkUid "swhsPCM") (nounPhraseSP "solar water heating systems incorporating PCM")
  "SWHS"
  []

projName :: ProjectName
projName = mkCommonProjName (mkUid "swhsProjName")
  (pn "Solar Water Heating Systems Incorporating PCM") "SWHS"
