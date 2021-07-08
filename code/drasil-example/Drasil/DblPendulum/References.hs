module Drasil.DblPendulum.References where

import Language.Drasil
import Data.Drasil.Citations (cartesianWiki, accelerationWiki, velocityWiki, parnasClements1986)
import Data.Drasil.People (rcHibbeler)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004, cartesianWiki, parnasClements1986]

hibbeler2004 :: Citation
hibbeler2004 = cBookA [rcHibbeler] "Engineering Mechanics: Dynamics"
  "Pearson Prentice Hall" 2004 []
  "hibbeler2004"

-- References --
citeRefs :: [Reference]
citeRefs = map ref citations
