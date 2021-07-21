module Drasil.DblPendulum.References (citations, koothoor2013,
  smithLai2005, citeRefs) where

import Language.Drasil
import Data.Drasil.Citations (cartesianWiki, accelerationWiki, velocityWiki, parnasClements1986, hibbeler2004, 
  koothoor2013, smithLai2005)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004, cartesianWiki, parnasClements1986,
             koothoor2013, smithLai2005]

-- References --
citeRefs :: [Reference]
citeRefs = map ref citations
