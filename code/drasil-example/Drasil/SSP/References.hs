module Drasil.SSP.References where

import Language.Drasil

import Data.Drasil.Citations (jnlCGJ, koothoor2013, parnasClements1986, smithLai2005)
import Data.Drasil.People (bKarchewski, cfLee, dgFredlund, dStolle, dyZhu, grChen, 
  jKrahn, pGuo, pjCleall, qhQian, ssLing, tltZhan, yCLi, ymChen)

sspCitations :: BibRef
sspCitations = [chen2005, parnasClements1986, koothoor2013,
  fredlund1977, smithLai2005, li2010, karchewski2012]

chen2005, fredlund1977, li2010, karchewski2012 :: Citation
--See Language.Drasil.People for all person constructors
chen2005 = cArticle [qhQian, dyZhu, cfLee, grChen]
  "A concise algorithm for computing the factor of safety using the morgenstern price method"
  jnlCGJ 2005 [month Feb, volume 42, number 1, pages [272,278]]
  "chen2005"

fredlund1977 = cArticle [dgFredlund, jKrahn]
  "Comparison of slope stability methods of analysis"
  jnlCGJ 1977 [month Apr, pages [429, 439], volume 14, number 3]
  "fredlund1977"

li2010 = cArticle [yCLi, ymChen, tltZhan, ssLing, pjCleall]
  ("An efficient approach for locating the critical slip surface in" ++
  " slope stability analyses using a real-coded genetic algorithm")
  jnlCGJ 2010 [month Jun, pages [806,820], volume 47, number 7]
  "li2010"

karchewski2012 = cInProceedings [bKarchewski, pGuo, dStolle]
  ("Influence of inherent anisotropy of soil strength on limit equilibrium slope stability analysis") 
  "Proceedings of the 65th annual Canadian GeoTechnical Conference" 2012 
  [address "Winnipeg, MB, Canada", organization "Canadian Geotechnical Society"] 
  "karchewski2012"
