module Drasil.DblPend.References (citations, dblPendOriginal) where

import Language.Drasil.Document (BibRef, Citation, author, cMisc,
  howPublishedU, title)
import Data.Drasil.Citations (accelerationWiki, velocityWiki,
 parnasClements1986, hibbeler2004)
import Data.Drasil.People (zhiZhang)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004,
             parnasClements1986, dblPendOriginal]

-- | The original, manually created version of Double Pendulum.
dblPendOriginal :: Citation
dblPendOriginal = cMisc
  [ author [zhiZhang]
  , title "Software Requirements Specification for Double Pendulum"
  , howPublishedU "https://github.com/Zhang-Zhi-ZZ/CAS741Project"
  ]
  "dblPendOriginal"
