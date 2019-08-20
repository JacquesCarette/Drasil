module Data.Drasil.Citations where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Data.Drasil.People (dParnas, jRalyte, lLai, nKoothoor, nKraiem, 
  pcClements, pjAgerfalk, spencerSmith, mCampidelli, dmWiess, rodPierce)

---------------
-- CITATIONS --
---------------

campidelli, cartesianWiki, koothoor2013, parnas1972, parnasClements1984,
  parnasClements1986, smithLai2005, lineSource, pointSource :: Citation

campidelli = cBooklet
  "Glass-BR Software for the design and risk assessment of glass facades subjected to blast loading"
  [author [mCampidelli]] "campidelli"

cartesianWiki = cMisc
  [author [mononym "Wikipedia Contributors"], title "Cartesian coordinate system",
  howPublishedU "https://en.wikipedia.org/wiki/Cartesian_coordinate_system",
  month Jun, year 2019]
  "cartesianWiki"

koothoor2013 = 
  cMThesis [nKoothoor] 
  "A document drive approach to certifying scientific computing software"
  "McMaster University" 2013 [address "Hamilton, ON, Canada"]
  "koothoor2013"

parnas1972 = cArticle [dParnas]
  "On the Criteria To Be Used in Decomposing Systems into Modules"
  "Communications of the ACM" 1972
  [pages [1053..1058]] "dParnas1972"
  
parnasClements1984 = cInProceedings
  [dParnas, pcClements, dmWiess]
  "The Modular Structure of Complex Systems"
  "ICSE '84: Proceedings of the 7th international conference on Software engineering"
  1984 [pages [408..417]] "parnasClements1984"

parnasClements1986 = cArticle [dParnas, pcClements] 
  "A rational design process: How and why to fake it"
  "IEEE Transactions on Software Engineering" 1986
  [month Feb, volume 12, number 2, pages [251..257], address "Washington, USA"]
  "parnasClements1986"

smithLai2005 = cInProceedings [spencerSmith, lLai]
  "A new requirements template for scientific computing"
  ("Proceedings of the First International Workshop on " ++
  "Situational Requirements Engineering Processes - Methods, " ++
  "Techniques and Tools to Support Situation-Specific Requirements " ++
  "Engineering Processes, SREP'05") 2005
  [ editor [pjAgerfalk, nKraiem, jRalyte], address "Paris, France"
  , pages [107..121], 
  note "In conjunction with 13th IEEE International Requirements Engineering Conference,"] 
  "smithLai2005"

lineSource = cMisc
  [author [mononym "The Editors of Encyclopaedia Britannica"], title "Line",
  howPublishedU "https://www.britannica.com/science/line-mathematics",
  month Jun, year 2019]
  "lineSource"

pointSource = cMisc
  [author [rodPierce], title "Point",
  howPublishedU "https://www.mathsisfun.com/geometry/point.html",
  month May, year 2017]
  "pointSource"

------------------------
-- COMMON CITE-FIELDS --
------------------------

jnlCGJ :: String
jnlCGJ = "Canadian Geotechnical Journal"
