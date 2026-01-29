module Drasil.DblPend.References (citations, koothoor2013,
  smithEtAl2007, smithLai2005, smithKoothoor2016) where

import Language.Drasil (BibRef)
import Data.Drasil.Citations (cartesianWiki, accelerationWiki, velocityWiki,
 parnasClements1986, koothoor2013, smithEtAl2007, smithLai2005, smithKoothoor2016)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, cartesianWiki,
             parnasClements1986, koothoor2013, smithEtAl2007, smithLai2005,
             smithKoothoor2016]
