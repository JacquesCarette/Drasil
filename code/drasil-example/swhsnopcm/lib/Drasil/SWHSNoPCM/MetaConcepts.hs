module Drasil.SWHSNoPCM.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators as NP
import Data.Drasil.Domains (materialEng)

progName :: CI
progName = commonIdea (mkUid "swhsNoPCM")
  (nounPhrase' "solar water heating system with no phase change material"
  "solar water heating systems with no phase change material" $ Replace $
  NP.S "Solar Water Heating System with no Phase Change Material") "SWHSNoPCM" [materialEng]

projName :: ProjectName
projName = mkCommonProjName (mkUid "swhsNoPCMProjName")
  (pn' "Solar Water Heating System With No Phase Change Material") "SWHSNoPCM"
