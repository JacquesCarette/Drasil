module Drasil.SWHS.References where --all of this file is exported

import Language.Drasil

import Data.Drasil.People (jBueche, fIncropera, dDewitt, tBergman, aLavine,
  mLightstone)

import Data.Drasil.Citations (koothoor2013, parnasClements1986, smithLai2005)

----------------------------
-- Section 9 : References --
----------------------------
--s9_swhs_citations
ref_swhs_citations :: BibRef
ref_swhs_citations = [ref1, ref2, ref3, ref4, ref5, ref6]

ref1, ref2, ref3, ref4, ref5, ref6 :: Citation

ref1 = cBookA "bueche1986" [jBueche]
  (S "Introduction to Physics for Scientists")
  (S "McGraw Hill") 1986
  [edition 4, address (S "New York City, New York")]
  (mkLabelSame "bueche1986")

ref2 = cBookA "incroperaEtAl2007" [fIncropera, dDewitt, tBergman, aLavine]
  (S "Fundamentals of Heat and Mass Transfer")
  (S "John Wiley and Sons") 2007
  [edition 6, address (S "Hoboken, New Jersey")]
  (mkLabelSame "incroperaEtAl2007")

ref3 = koothoor2013

ref4 = cMisc "lightstone2012" [
  author [mLightstone],
  title (S "Derivation of tank/pcm model"),
  year 2012,
  note (S "From Marilyn Lightstone's Personal Notes")]
  (mkLabelSame "lightstone2012")

ref5 = parnasClements1986

ref6 = smithLai2005