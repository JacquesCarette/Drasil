module Drasil.SSP.References where

import Language.Drasil
import Drasil.SSP.Defs (ssa, crtSlpSrf, fs_concept)

import Data.Drasil.Concepts.Documentation (analysis)
import Data.Drasil.Citations (smith2005, koothoor2013, parnas1986, journalCGJ)

sspCitations :: BibRef
sspCitations = [chen2005, parnas1986, koothoor2013,
  fredlund1977, smith2005, stolle2008, li2010]

chen2005, fredlund1977, stolle2008, li2010 :: Citation
--See Language.Drasil.People for all person constructors
chen2005 = Article $ [
  Author [personWM' "Q" ["H"] "Qian",
          personWM' "D" ["Y"] "Zhu",
          personWM' "C" ["F"] "Lee",
          personWM' "G" ["R"] "Chen"],
  Title $ S "A concise algorithm for computing the" +:+
          phrase fs_concept +:+ S "using the" +:+
          S "morgenstern price method",
  Date 19 Feb 2005,
  Pages (272,278)] ++
  journalCGJ 42 1

fredlund1977 = Article $ [
  Author [personWM "D" ["G"] "Fredlund",
          person "J" "Krahn"],
  Title $ S "Comparison of slope stability methods of"
          +:+ phrase analysis,
  Date 4 Apr 1977,
  Pages (429,439)] ++
  journalCGJ 14 3

stolle2008 = Article $ [
  Author [person "Dieter" "Stolle",
          person "Peijun" "Guo"],
  Title $ S "Limit equilibrum" +:+ phrase ssa +:+
          S "using rigid finite elements",
  Date 20 May 2008,
  Pages (653,662)] ++
  journalCGJ 45 5

li2010 = Article $ [
  Author [person' "Yu-Chao" "Li",
          person' "Yun-Min" "Chen",
          personWM "Tony" ["L","T"] "Zhan",
          person' "Sao-Sheng" "Ling",
          personWM "Peter" ["John"] "Cleall"],
  Title $ S "An efficient approach for locating the" +:+
          phrase crtSlpSrf +:+ S "in" +:+ plural ssa +:+
          S "using a real-coded genetic algorithm",
  Date 25 Jun 2010,
  Pages (806,820)] ++
  journalCGJ 47 7