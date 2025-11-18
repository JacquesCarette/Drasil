module Drasil.DblPend.References (citations, koothoor2013,
  smithEtAl2007, smithLai2005, smithKoothoor2016) where

import Language.Drasil
import Data.Drasil.Citations (accelerationWiki, velocityWiki,
 parnasClements1986, hibbeler2004, koothoor2013, smithEtAl2007, smithLai2005,
 smithKoothoor2016)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004,
             parnasClements1986, koothoor2013, smithEtAl2007, smithLai2005,
             smithKoothoor2016]
