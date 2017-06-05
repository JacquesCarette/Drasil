module Drasil.GeneralSystDesc
  (genSysF, systCon) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.SentenceStructures
import qualified Drasil.SRS as SRS

-- wrapper for general system description
genSysF :: [Section] -> Contents -> [Contents] -> [Section] -> Section
genSysF sCntxt userIntro constraints systSubSec = SRS.genSysDes [genSysIntro]
  (sCntxt ++ [SRS.userChar [userIntro] [], systCon constraints systSubSec])

--generalized general system description introduction
genSysIntro :: Contents
genSysIntro = Paragraph $ foldlSent
              [S "This", phrase section_, S "provides general",
              phrase information, S "about the", phrase system `sC` S "identifies",
              S "the interfaces between the", phrase system, S "and its", phrase environment `sC`
              S "and describes the", plural userCharacteristic, S "and the", plural systemConstraint]

-- System Constraints
-- generalized if no constraints, but if there are, they can be passed through
systCon :: [Contents] -> [Section] -> Section
systCon [] subSec  = SRS.sysCon [systCon_none] subSec
            where systCon_none = Paragraph (S "There are no" +:+. plural systemConstraint)
systCon a subSec = SRS.sysCon a subSec  
