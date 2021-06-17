module Drasil.Projectile.References where

import Language.Drasil
import Data.Drasil.Citations (cartesianWiki, accelerationWiki, velocityWiki)
import Data.Drasil.People (rcHibbeler)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004, cartesianWiki]

hibbeler2004 :: Citation
hibbeler2004 = cBookA [rcHibbeler] "Engineering Mechanics: Dynamics"
  "Pearson Prentice Hall" 2004 []
  "hibbeler2004"

-- References --
citeRefs :: [Reference]
citeRefs = map rw citations
