module Drasil.BinaryStar.References (citations) where

import Language.Drasil (BibRef)
import Data.Drasil.Citations (parnasClements1986, hibbeler2004,
  velocityWiki, accelerationWiki)

citations :: BibRef
citations = [parnasClements1986, hibbeler2004, velocityWiki, accelerationWiki]
