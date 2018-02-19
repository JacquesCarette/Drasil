module Drasil.SSP.References (sspCitations) where

import Language.Drasil

import Drasil.SSP.Defs (ssa, crtSlpSrf, fs_concept)
import Data.Drasil.Concepts.Documentation (analysis)
import Data.Drasil.Citations (smithLai2005, koothoor2013, parnas1986,
  journalCGJ)
import Data.Drasil.People (qhQian, dyZhu, cfLee, grChen, dgFredlund,
  jKrahn, dStolle, yCLi, ymChen, tltZhan, ssLing, pjCleall, pGuo)

sspCitations :: BibRef
sspCitations = [chen2005, parnas1986, koothoor2013,
  fredlund1977, smithLai2005, stolle2008, li2010]

chen2005, fredlund1977, stolle2008, li2010 :: Citation
--See Language.Drasil.People for all person constructors
chen2005 = Citation Article $ [
  Author [qhQian, dyZhu, cfLee, grChen],
  Title $ S "A concise algorithm for computing the" +:+
          phrase fs_concept +:+ S "using the" +:+
          S "morgenstern price method",
  Date 19 Feb 2005,
  Pages (272,278)] ++
  journalCGJ 42 1

fredlund1977 = Citation Article $ [
  Author [dgFredlund, jKrahn],
  Title $ S "Comparison of slope stability methods of"
          +:+ phrase analysis,
  Date 4 Apr 1977,
  Pages (429,439)] ++
  journalCGJ 14 3

stolle2008 = Citation Article $ [
  Author [dStolle, pGuo],
  Title $ S "Limit equilibrum" +:+ phrase ssa +:+
          S "using rigid finite elements",
  Date 20 May 2008,
  Pages (653,662)] ++
  journalCGJ 45 5

li2010 = Citation Article $ [
  Author [yCLi, ymChen, tltZhan, ssLing, pjCleall],
  Title $ S "An efficient approach for locating the" +:+
          phrase crtSlpSrf +:+ S "in" +:+ plural ssa +:+
          S "using a real-coded genetic algorithm",
  Date 25 Jun 2010,
  Pages (806,820)] ++
  journalCGJ 47 7
