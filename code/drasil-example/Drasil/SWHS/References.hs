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

bueche1986 = cBookA [jBueche]
  "Introduction to Physics for Scientists"
  "McGraw Hill" 1986
  [edition 4, address "New York City, New York"]
  "bueche1986"

incroperaEtAl2007 = cBookA [fIncropera, dDewitt, tBergman, aLavine]
  "Fundamentals of Heat and Mass Transfer"
  "John Wiley and Sons" 2007
  [edition 6, address "Hoboken, New Jersey"]
  "incroperaEtAl2007"

lightstone2012 = cMisc [
  author [mLightstone],
  title "Derivation of tank/pcm model",
  year 2012,
  note "From Marilyn Lightstone's Personal Notes"]
  "lightstone2012"
