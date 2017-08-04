module Drasil.SSP.References where

import Language.Drasil
import Drasil.SSP.Defs (ssa, crtSlpSrf, fs_concept)

import Data.Drasil.Concepts.Documentation (analysis)
import Data.Drasil.Software.Products (sciCompS)

sspCitations :: BibRef
sspCitations = [chen2005, parnas1986, koothoor2013, fredlund1977, smith2005, stolle2008, li2010]

chen2005, parnas1986, koothoor2013, fredlund1977, smith2005, stolle2008, li2010 :: Citation
--See Language.Drasil.People for all person constructors
chen2005 = Article [
  Author [personWM' "Q" ["H"] "Qian",
          personWM' "D" ["Y"] "Zhu",
          personWM' "C" ["F"] "Lee",
          personWM' "G" ["R"] "Chen"],
  Title $ S "A concise algorithm for computing the" +:+
          phrase fs_concept +:+ S "using the" +:+
          S "morgenstern price method",
  journalCGJ,
  Volume 42,
  Issue 1,
  Date 19 Feb 2005,
  Pages (272,278)]
  
parnas1986 = Article [
  Author [personWM "David" ["L"] "Parnas",
          personWM "Paul"  ["C"] "Clements"],
  Title $ S "A rational design process:" +:+
          S "How and why to fake it",
  Journal $ S "IEEE Transactions on Software Engineering",
  Volume 12,
  Issue 2,
  Pages (251,257),
  Year 1986, --February, but day unknown
  Place (S "Washington", S "USA")]

koothoor2013 = MThesis [
  Author [person "Nirmitha" "Koothoor"],
  Title $ S "A document drive approach to certifying"
          +:+ phrase sciCompS,
  School $ S "McMaster University",
  Place (S "Hamilton", S "Canada"),
  Year 2013]

fredlund1977 = Article [
  Author [personWM "D" ["G"] "Fredlund",
          person "J" "Krahn"],
  Title $ S "Comparison of slope stability methods of"
          +:+ phrase analysis,
  journalCGJ,
  Volume 14,
  Issue 3,
  Date 4 Apr 1977,
  Pages (429,439)]

smith2005 = Article [
  Author [personWM "W" ["Spencer"] "Smith",
          person "Lei" "Lai"],
  Title $ S "A new requirements template for" +:+
          S "scientific computing",
  Journal $ S "SREP",
  Pages (107,121),
  Year 2005,
  Place (S "Paris", S "France"),
  Note (S "In J. Ralyt" :+: (F Acute 'e') `sC` S "P. Agerfalk, and N. Kraiem" `sC`
        S "editors, Proceedings of the First International Workshopon" +:+
        S "Situational Requirements Engineering Processes - Methods," +:+
        S "Techniques and Tools to Support Situation-Specific Requirements" +:+
        S "Engineering Processes." +:+
        --FIXME: The above section is part of the citation but not the note
        S "In conjunction with 13th IEEE International" +:+.
        S "Requirements Engineering Conference")]

stolle2008 = Article [
  Author [person "Dieter" "Stolle",
          person "Peijun" "Guo"],
  Title $ S "Limit equilibrum" +:+ phrase ssa +:+
          S "using rigid finite elements",
  journalCGJ,
  Volume 45,
  Issue 5,
  Date 20 May 2008,
  Pages (653,662)]

li2010 = Article [
  Author [person' "Yu-Chao" "Li",
          person' "Yun-Min" "Chen",
          personWM "Tony" ["L","T"] "Zhan",
          person' "Sao-Sheng" "Ling",
          personWM "Peter" ["John"] "Cleall"],
  Title $ S "An efficient approach for locating the" +:+
          phrase crtSlpSrf +:+ S "in" +:+ plural ssa +:+
          S "using a real-coded genetic algorithm",
  journalCGJ,
  Volume 47,
  Issue 7,
  Date 25 Jun 2010,
  Pages (806,820)]


journalCGJ :: CiteField
journalCGJ = Journal $ S $ "Canadian Geotechnical Journal"