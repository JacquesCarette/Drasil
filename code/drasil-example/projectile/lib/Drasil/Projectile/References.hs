module Drasil.Projectile.References (citations, projectileOriginal) where

import Language.Drasil.Document (BibRef, Citation, author, cMisc,
  howPublishedU, title)
import Data.Drasil.Citations (accelerationWiki, velocityWiki,
 hibbeler2004, parnasClements1986)
import Data.Drasil.People (brooks, samCrawford, spencerSmith)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004,
             parnasClements1986, projectileOriginal]

-- | The original, manually created version of Projectile.
projectileOriginal :: Citation
projectileOriginal = cMisc
  [ author [samCrawford, brooks, spencerSmith]
  , title "Software Requirements Specification for Projectile"
  , howPublishedU "https://github.com/smiths/caseStudies/tree/master/CaseStudies/projectile"
  ]
  "projectileOriginal"
