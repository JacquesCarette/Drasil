module Drasil.SWHS.References where

import Language.Drasil (BibRef,
  (+:+), sC, person', personWM',
  Accent (Acute),
  Citation (Article, MThesis),
  CiteField (Author, Year, Note, Pages, Place, Journal, Title, Issue,
    School, Volume, Publisher, Edition),
  Sentence (S, (:+:), F))

----------------------------
-- Section 9 : References --
----------------------------

s9_swhs_citations :: BibRef
s9_swhs_citations = [ref1, ref2, ref3, ref4, ref5, ref6]

ref1, ref2, ref3, ref4, ref5, ref6 :: Citation

ref1 = Article [
  Author [personWM' "J" ["Frederick"] "Bueche"],
  Title (S "Introduction to Physics for Scientists"),
  Edition 4,
  Publisher (S "McGraw Hill"),
  Place (S "New York City", S "New York"),
  Year 1986]



ref2 = Article [
  Author [personWM' "F" ["P"] "Incropera",
          personWM' "D" ["P"] "Dewitt",
          personWM' "T" ["L"] "Bergman",
          personWM' "A" ["S"] "Lavine"],
  Title (S "Fundamentals of Heat and Mass Transfer"),
  Edition 6,
  Publisher (S "John Wiley and Sons"),
  Place (S "Hoboken", S "New Jersey"),
  Year 2007]

ref3 = MThesis [
  Author [person' "Nirmitha" "Koothoor"],
  Title (S "A document drive approach to certifying" +:+
  S "scientific computing software"),
  School (S "McMaster University"),
  Place (S "Hamilton", S "Canada"),
  Year 2013]

ref4 = Article [
  Author [person' "Marilyn" "Lightstone"],
  Title (S "Derivation of tank/pcm model"),
  Year 2012,
  Note (S "From Marilyn Lightstone's Personal Notes")]

ref5 = Article [
  Author [personWM' "David" ["L"] "Parnas",
          personWM' "P" ["C"] "Clements"],
  Title (S "A rational design process: How and why to fake it"),
  Journal (S "IEEE Transactions on Software Engineering"),
  Volume 12,
  Issue 2,
  Pages (251, 257),
  Place (S "Washington", S "USA"),
  Year 1986]

ref6 = Article [
  Author [personWM' "W." ["Spencer"] "Smith",
          person' "Lei" "Lai"],
  Title (S "A new requirements template for scientific computing"),
  Note (S "In J. Ralyt" :+: (F Acute 'e') `sC` S "P. Agerfalk" `sC`
  S "and N. Kraiem" `sC` S "editors"),
  --FIXME: need to add editor field
  --FIXME: person' takes strings but we need an "e" with an accent
  Journal (S "Proceedings of the First International Workshop on" +:+
  S "Situational Requirements Engineering Processes - Methods," +:+
  S "Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05"),
  Pages (107, 121),
  Place (S "Paris", S "France"),
  Year 2005,
  Note (S "In conjunction with 13th IEEE International Requirements" +:+
  S "Engineering Conference")]