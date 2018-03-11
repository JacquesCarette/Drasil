module Drasil.SSP.References (sspCitations) where

import Language.Drasil

import Drasil.SSP.Defs (ssa, crtSlpSrf, fs_concept)
import Data.Drasil.Concepts.Documentation (analysis)
import Data.Drasil.Citations (smithLai2005, koothoor2013, parnas1986,
  jnlCGJ)
import Data.Drasil.People (qhQian, dyZhu, cfLee, grChen, dgFredlund,
  jKrahn, dStolle, yCLi, ymChen, tltZhan, ssLing, pjCleall, pGuo)

sspCitations :: BibRef
sspCitations = [chen2005, parnas1986, koothoor2013,
  fredlund1977, smithLai2005, stolle2008, li2010]

chen2005, fredlund1977, stolle2008, li2010 :: Citation
--See Language.Drasil.People for all person constructors
chen2005 = cArticle "chen2005" [qhQian, dyZhu, cfLee, grChen]
  (S "A concise algorithm for computing the" +:+
  phrase fs_concept +:+ S "using the" +:+ S "morgenstern price method")
  jnlCGJ 2005 [month Feb, volume 42, number 1, pages [272,278]]

fredlund1977 = cArticle "fredlund1977" [dgFredlund, jKrahn]
  (S "Comparison of slope stability methods of" +:+ phrase analysis)
  jnlCGJ 1977 [month Apr, pages [429, 439], volume 14, number 3]
  
stolle2008 = cArticle "stolle2008" [dStolle, pGuo]
  (S "Limit equilibrum" +:+ phrase ssa +:+ S "using rigid finite elements")
  jnlCGJ 2008 [month May, pages [653,662], volume 45, number 5]

li2010 = cArticle "li2010" [yCLi, ymChen, tltZhan, ssLing, pjCleall]
  (S "An efficient approach for locating the" +:+ phrase crtSlpSrf +:+ 
  S "in" +:+ plural ssa +:+ S "using a real-coded genetic algorithm")
  jnlCGJ 2010 [month Jun, pages [806,820], volume 47, number 7]
