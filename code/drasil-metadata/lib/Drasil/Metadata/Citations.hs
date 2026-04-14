-- | Defines citations used internally in Drasil.
module Drasil.Metadata.Citations where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Drasil.Metadata.People (dParnas, jRalyte, lLai, nKoothoor, nKraiem,
  pcClements, pjAgerfalk, spencerSmith, rKhedri)

-- * Citations

-- ** Papers

koothoor2013, parnasClements1986, smithEtAl2007, smithLai2005,
  smithKoothoor2016 :: Citation

koothoor2013 =
  cMThesis [nKoothoor]
  "A Document Driven Approach to Certifying Scientific Computing Software"
  "McMaster University" 2013 [address "Hamilton, ON, Canada"]
  "koothoor2013"

parnasClements1986 = cArticle [dParnas, pcClements]
  "A rational design process: How and why to fake it"
  "IEEE Transactions on Software Engineering" 1986
  [month Feb, volume 12, number 2, pages [251..257], address "Washington, USA"]
  "parnasClements1986"

smithEtAl2007 = cArticle [spencerSmith, lLai, rKhedri]
  ("Requirements Analysis for Engineering Computation: A Systematic Approach for" ++
    " Improving Software Reliability")
  "Reliable Computing, Special Issue on Reliable Engineering Computation" 2007
  [month Feb, volume 13, number 1, pages [83..107], howPublishedU "https://doi.org/10.1007/s11155-006-9020-7"]
  "smithEtAl2007"

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

smithKoothoor2016 = cArticle [spencerSmith, nKoothoor]
  ("A Document-Driven Method for Certifying Scientific Computing Software for Use" ++
    " in Nuclear Safety Analysis")
  "Nuclear Engineering and Technology" 2016
  [month Apr, volume 48, number 2, pages[404..418],
    howPublishedU "http://www.sciencedirect.com/science/article/pii/S1738573315002582"]
  "smithKoothoor2016"
