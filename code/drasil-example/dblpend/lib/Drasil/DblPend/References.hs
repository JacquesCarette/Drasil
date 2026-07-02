module Drasil.DblPend.References (citations) where

import Language.Drasil (BibRef)
import Data.Drasil.Citations (accelerationWiki, velocityWiki,
 parnasClements1986, hibbeler2004)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004,
             parnasClements1986]
