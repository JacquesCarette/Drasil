module Drasil.SWHS.References where --all of this file is exported

import Language.Drasil

import Data.Drasil.People (jBueche, fIncropera, dDewitt, tBergman, aLavine,
  mLightstone)

import Data.Drasil.Citations (koothoor2013, parnas1986, smithLai2005)

----------------------------
-- Section 9 : References --
----------------------------
--s9_swhs_citations
ref_swhs_citations :: BibRef
ref_swhs_citations = [ref1, ref2, ref3, ref4, ref5, ref6]

ref1, ref2, ref3, ref4, ref5, ref6 :: Citation

ref1 = Article [
  Author [jBueche],
  Title (S "Introduction to Physics for Scientists"),
  Edition 4,
  Publisher (S "McGraw Hill"),
  Place (S "New York City", S "New York"),
  Year 1986]



ref2 = Article [
  Author [fIncropera, dDewitt, tBergman, aLavine],
  Title (S "Fundamentals of Heat and Mass Transfer"),
  Edition 6,
  Publisher (S "John Wiley and Sons"),
  Place (S "Hoboken", S "New Jersey"),
  Year 2007]

ref3 = koothoor2013

ref4 = Misc [
  Author [mLightstone],
  Title (S "Derivation of tank/pcm model"),
  Year 2012,
  Note (S "From Marilyn Lightstone's Personal Notes")]

ref5 = parnas1986

ref6 = smithLai2005