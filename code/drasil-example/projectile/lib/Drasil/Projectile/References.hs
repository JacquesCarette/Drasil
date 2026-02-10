module Drasil.Projectile.References where

import Language.Drasil
import Data.Drasil.Citations (accelerationWiki, velocityWiki,
 cartesianWiki, hibbeler2004, parnasClements1986, koothoor2013, smithKoothoor2016,
 smithEtAl2007, smithLai2005)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004,
             cartesianWiki,
             parnasClements1986, koothoor2013, smithKoothoor2016, smithEtAl2007,
             smithLai2005]
