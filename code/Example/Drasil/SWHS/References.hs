module Drasil.SWHS.References where

import Language.Drasil

import Data.Drasil.People (jBueche, fIncropera, dDewitt, tBergman, aLavine,
  nKoothoor, mLightstone, dParnas, pcClements, spencerSmith, lLai,
  pjAgerfalk, nKraiem, jRalyte)

----------------------------
-- Section 9 : References --
----------------------------

s9_swhs_citations :: BibRef
s9_swhs_citations = [ref1, ref2, ref3, ref4, ref5, ref6]

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

ref3 = MThesis [
  Author [nKoothoor],
  Title (S "A document drive approach to certifying" +:+
  S "scientific computing software"),
  School (S "McMaster University"),
  Place (S "Hamilton", S "Canada"),
  Year 2013]

ref4 = Misc [
  Author [mLightstone],
  Title (S "Derivation of tank/pcm model"),
  Year 2012,
  Note (S "From Marilyn Lightstone's Personal Notes")]

ref5 = Article [
  Author [dParnas, pcClements],
  Title (S "A rational design process: How and why to fake it"),
  Journal (S "IEEE Transactions on Software Engineering"),
  Volume 12,
  Issue 2,
  Pages (251, 257),
  Place (S "Washington", S "USA"),
  Year 1986]

ref6 = Article [
  Author [spencerSmith, lLai],
  Title (S "A new requirements template for scientific computing"),
  Editor [pjAgerfalk, nKraiem, jRalyte],
  Journal (S "Proceedings of the First International Workshop on" +:+
  S "Situational Requirements Engineering Processes - Methods," +:+
  S "Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05"),
  Pages (107, 121),
  Place (S "Paris", S "France"),
  Year 2005,
  Note (S "In conjunction with 13th IEEE International Requirements" +:+
  S "Engineering Conference")]