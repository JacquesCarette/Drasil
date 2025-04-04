module Drasil.SWHSNoPCM.References (citations) where

import Language.Drasil

import Data.Drasil.Citations (koothoor2013, parnasClements1986, smithEtAl2007,
  smithKoothoor2016, smithLai2005)

import Drasil.SWHS.References (incroperaEtAl2007, lightstone2012)

citations :: BibRef
citations = [incroperaEtAl2007, koothoor2013, lightstone2012, parnasClements1986,
             smithEtAl2007, smithLai2005, smithKoothoor2016]
