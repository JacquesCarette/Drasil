module Drasil.Sections.GeneralSystDesc
  where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (interface, system, environment,
  userCharacteristic, systemConstraint, information, section_)
import Data.Drasil.SentenceStructures (sAnd, foldlSP)
import qualified Drasil.DocLang.SRS as SRS (genSysDes, userChar, sysCon, sysCont)

-- wrapper for general system description
genSysF :: [Section] -> Contents -> [Contents] -> [Section] -> Section
genSysF sCntxt userIntro cnstrnts systSubSec = SRS.genSysDes [genSysIntro]
  (sCntxt ++ [usrCharsF [userIntro], systCon cnstrnts systSubSec])

--generalized general system description introduction
genSysIntro :: Contents
genSysIntro = foldlSP
              [S "This", phrase section_, S "provides general",
              phrase information, S "about the", phrase system, S "including identifying",
              S "the", plural interface, S "between the", phrase system `sAnd` S "its",
              phrase environment, S "(system context)" `sC` S "describing the", plural userCharacteristic 
              `sAnd` S "listing the", plural systemConstraint]

--User Characeristics
usrCharsF :: [Contents] -> Section
usrCharsF intro = SRS.userChar intro []

-- System Constraints
-- generalized if no constraints, but if there are, they can be passed through
systCon :: [Contents] -> [Section] -> Section
systCon [] subSec  = SRS.sysCon [systCon_none] subSec
            where systCon_none = Paragraph (S "There are no" +:+. plural systemConstraint)
systCon a subSec = SRS.sysCon a subSec

--System Context
sysContxt :: [Contents] -> Section
sysContxt cs = SRS.sysCont cs []
