module Drasil.SWHSNoPCM.References (citations, swhsNoPCMOriginal) where

import Language.Drasil.Document (BibRef, Citation, author, cMisc,
  howPublishedU, title)

import Data.Drasil.Citations (parnasClements1986)
import Data.Drasil.People (spencerSmith, thulasi)

import Drasil.SWHS.References (incroperaEtAl2007)

citations :: BibRef
citations = [incroperaEtAl2007, parnasClements1986, swhsNoPCMOriginal]

-- | The original, manually created version of SWHSNoPCM.
swhsNoPCMOriginal :: Citation
swhsNoPCMOriginal = cMisc
  [ author [thulasi, spencerSmith]
  , title "Software Requirements Specification for Solar Water Heating Systems"
  , howPublishedU "https://github.com/smiths/caseStudies/blob/master/CaseStudies/noPCM"
  ]
  "swhsNoPCMOriginal"
