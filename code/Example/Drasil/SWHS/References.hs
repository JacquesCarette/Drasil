module Drasil.SWHS.References where

import Language.Drasil

----------------------------
-- Section 9 : References --
----------------------------

ref1 :: Citation

ref2, ref3, ref4, ref5, ref6 :: [Sentence]

ref1 = Article [
  Author [personWM' "J." ["Frederick"] "Bueche"],
  Title (S "Introduction to Physics for Scientists"),
  Publisher (S "McGraw Hill"),
  Place (S "York", S "USA"),
  Note (S "fourth edition"),
  Year 1986]



ref2 = [S "F. P. Incropera", S "D. P. Dewitt", S "T. L. Bergman",
  S "and A. S. Lavine. Fundamentals of Heat and Mass Transfer. John Wiley" +:+
  S "and Sons", S "United States", S "sixth edition edition", S "2007."]

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