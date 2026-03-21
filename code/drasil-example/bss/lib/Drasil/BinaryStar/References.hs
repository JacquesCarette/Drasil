module Drasil.BinaryStar.References (citations, koothoor2013,
  smithEtAl2007, smithLai2005, smithKoothoor2016) where

import Language.Drasil (BibRef)
import Data.Drasil.Citations (parnasClements1986, koothoor2013,
  smithEtAl2007, smithLai2005, smithKoothoor2016, hibbeler2004,
  velocityWiki, accelerationWiki)

citations :: BibRef
citations = [parnasClements1986, koothoor2013, smithEtAl2007, smithLai2005,
             smithKoothoor2016, hibbeler2004, velocityWiki, accelerationWiki]
