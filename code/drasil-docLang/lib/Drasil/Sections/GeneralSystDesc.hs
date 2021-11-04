-- | Defines helper functions used to make the general system description section.
module Drasil.Sections.GeneralSystDesc where

import Language.Drasil
import Utils.Drasil
import Utils.Drasil.Sentence

import Data.Drasil.Concepts.Documentation (interface, system, environment,
  userCharacteristic, systemConstraint, information, section_)
import qualified Drasil.DocLang.SRS as SRS (sysCon, sysCont, userChar)

-- | Default General System Description introduction.
genSysIntro :: Contents
genSysIntro = foldlSP [S "This", phrase section_, S "provides general",
  phrase information, S "about the" +:+. phrase system, S "It identifies the",
  plural interface, S "between the", phrase system `andIts` phrase environment `sC`
  S "describes the", plural userCharacteristic `sC` S "and lists the", plural systemConstraint]

-- | User Characeristics section constructor. Does not contain any subsections.
usrCharsF :: [Contents] -> Section
usrCharsF intro = SRS.userChar 1 intro []

-- | System Constraints section constructor.
-- Generalized if no constraints, but if there are, they can be passed through.
systCon :: [Contents] -> [Section] -> Section
systCon [] subSec  = SRS.sysCon 1 [systCon_none] subSec
            where systCon_none = mkParagraph (S "There are no" +:+. plural systemConstraint)
systCon a subSec = SRS.sysCon 1 a subSec

-- | System Context section constructor. Does not contain any subsections.
sysContxt :: [Contents] -> Section
sysContxt cs = SRS.sysCont 1 cs []
