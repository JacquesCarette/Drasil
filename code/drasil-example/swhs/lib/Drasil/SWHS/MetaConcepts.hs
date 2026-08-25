module Drasil.SWHS.MetaConcepts (swhs, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

swhs :: IdeaDict
swhs = idea (mkUid "swhsIdea")
  (nounPhrase "solar water heating system" "solar water heating systems")
  "SWHS"

projName :: ProjectName
projName = mkCommonProjName (mkUid "swhsProjName")
  (pn "Solar Water Heating Systems incorporating PCM") "SWHS"
