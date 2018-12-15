module Data.Drasil.Citations (campidelli, koothoor2013, parnas1972, parnasClements1986, 
  parnasClements1984, smithLai2005, jnlCGJ) where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Data.Drasil.People (dParnas, jRalyte, lLai, nKoothoor, nKraiem, 
  pcClements, pjAgerfalk, spencerSmith, mCampidelli, dmWiess)
import Data.Drasil.Software.Products (sciCompS)

---------------
-- CITATIONS --
---------------

campidelli, koothoor2013, parnas1972, parnasClements1984, parnasClements1986, smithLai2005 :: Citation

campidelli = cBooklet
  (S "Glass-BR Software for the design and risk assessment of glass facades subjected to blast loading")
  [author [mCampidelli]] "campidelli"

koothoor2013 = 
  cMThesis [nKoothoor] 
  (S "A document drive approach to certifying" +:+ phrase sciCompS)
  (S "McMaster University") 2013 [address (S "Hamilton, ON, Canada")]
  "koothoor2013"

parnas1972 = cArticle [dParnas]
  (S "On the Criteria To Be Used in Decomposing Systems into Modules")
  (S "Communications of the ACM") 1972
  [pages [1053, 1058]] "dParnas1972"
  
parnasClements1984 = cInProceedings
  [dParnas, pcClements, dmWiess]
  (S "The Modular Structure of Complex Systems")
  (S "ICSE '84: Proceedings of the 7th international conference on Software engineering")
  1984 [pages [408, 417]] "parnasClements1984"

parnasClements1986 = cArticle [dParnas, pcClements] 
  (S "A rational design process: How and why to fake it")
  (S "IEEE Transactions on Software Engineering") 1986
  [month Feb, volume 12, number 2, pages [251,257], address (S "Washington, USA")]
  "parnasClements1986"

smithLai2005 = cInProceedings [spencerSmith, lLai]
  (S "A new requirements template for scientific computing")
  (S "Proceedings of the First International Workshop on" +:+
  S "Situational Requirements Engineering Processes - Methods," +:+
  S "Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05") 2005
  [ editor [pjAgerfalk, nKraiem, jRalyte], address (S "Paris, France")
  , pages [107,121], 
  note (S "In conjunction with 13th IEEE International Requirements" +:+
  S "Engineering Conference,")] 
  "smithLai2005"

------------------------
-- COMMON CITE-FIELDS --
------------------------

jnlCGJ :: Sentence
jnlCGJ = S "Canadian Geotechnical Journal"
