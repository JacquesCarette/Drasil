module Drasil.SWHS.References (swhsCitations, bueche1986, incroperaEtAl2007, koothoor2013, lightstone2012, 
  parnasClements1986, parnas1972, parnasClements1984, smithLai2005) where

import Language.Drasil

import Data.Drasil.People (jBueche, fIncropera, dDewitt, tBergman, aLavine,
  mLightstone)

import Data.Drasil.Citations (koothoor2013, parnasClements1986, smithLai2005, parnas1972, parnasClements1984)

----------------------------
-- Section 9 : References --
----------------------------
swhsCitations :: BibRef
swhsCitations = [bueche1986, incroperaEtAl2007, koothoor2013, lightstone2012, parnasClements1986, 
  smithLai2005, parnas1972, parnasClements1984]

bueche1986, incroperaEtAl2007, lightstone2012 :: Citation

bueche1986 = cBookA "bueche1986" [jBueche]
  (S "Introduction to Physics for Scientists")
  (S "McGraw Hill") 1986
  [edition 4, address (S "New York City, New York")]
  (mkLabelSame "bueche1986" Cite)

incroperaEtAl2007 = cBookA "incroperaEtAl2007" [fIncropera, dDewitt, tBergman, aLavine]
  (S "Fundamentals of Heat and Mass Transfer")
  (S "John Wiley and Sons") 2007
  [edition 6, address (S "Hoboken, New Jersey")]
  (mkLabelSame "incroperaEtAl2007" Cite)

lightstone2012 = cMisc "lightstone2012" [
  author [mLightstone],
  title (S "Derivation of tank/pcm model"),
  year 2012,
  note (S "From Marilyn Lightstone's Personal Notes")]
  (mkLabelSame "lightstone2012" Cite)