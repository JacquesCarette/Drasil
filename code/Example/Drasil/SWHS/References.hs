module Drasil.SWHS.References where

import Language.Drasil

----------------------------
-- Section 9 : References --
----------------------------

s9_swhs_citations :: BibRef
s9_swhs_citations = [ref1, ref2]

ref1, ref2 :: Citation

ref3, ref4, ref5, ref6 :: [Sentence]

ref1 = Article [
  Author [personWM' "J." ["Frederick"] "Bueche"],
  Title (S "Introduction to Physics for Scientists"),
  Edition 4,
  Publisher (S "McGraw Hill"),
  Place (S "New York", S "USA"),
  Year 1986]



ref2 = Article [
  Author [personWM' "F." ["P."] "Incropera",
          personWM' "D." ["P."] "Dewitt",
          personWM' "T." ["L."] "Bergman",
          personWM' "A." ["S."] "Lavine"],
  Title (S "Fundamentals of Heat and Mass Transfer"),
  Edition 6,
  Publisher (S "John Wiley and Sons"),
  Place (S "Hoboken", S "New Jersey"),
  Year 2007]

ref3 = [S "Nirmitha Koothoor. A document drive approach to certifying" +:+
  S "scientific computing software. Master's thesis", S "McMaster University",
  S "Hamilton", S "Ontario", S "Canada", S "2013."]

ref4 = [S "Marilyn Lightstone. Derivation of tank/pcm model. Personal Notes",
  S "2012."]

ref5 = [S "David L. Parnas and P.C. Clements. A rational design process:" +:+
  S "How and why to fake it. IEEE Transactions on Software Engineering",
  S "12" :+: Quote (S "2") :+: S ":251-257", S "February 1986."]

ref6 = [S "W. Spencer Smith and Lei Lai. A new requirements template for" +:+
  S "scientific computing. In J. Ralyt" :+: (F Acute 'e'), S "P. Agerfalk",
  S "and N. Kraiem", S "editors", S "Proceedings of the First" +:+
  S "International Workshop on Situational Requirements Engineering" +:+
  S "Processes - Methods, Techniques and Tools to Support" +:+
  S "Situation-Specific Requirements Engineering Processes, SREP'05",
  S "pages 107-121", S "Paris", S "France", S "2005. In conjunction with" +:+
  S "13th IEEE International Requirements Engineering Conference."]