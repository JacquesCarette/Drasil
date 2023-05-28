-- | Defines citations used in Drasil.
module Data.Drasil.Citations where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Data.Drasil.People (dParnas, jRalyte, lLai, nKoothoor, nKraiem, 
  pcClements, pjAgerfalk, spencerSmith, mCampidelli, dmWiess, rodPierce, 
  wikiAuthors, rcHibbeler, rKhedri)

-- * Citations

-- ** Papers

campidelli, koothoor2013, parnas1972, parnasClements1984,
  parnasClements1986, smithLai2005, lineSource, pointSource,
  smithEtAl2007, smithKoothoor2016, hibbeler2004 :: Citation

campidelli = cBooklet
  "Glass-BR Software for the design and risk assessment of glass facades subjected to blast loading"
  [author [mCampidelli]] "campidelli"

koothoor2013 = 
  cMThesis [nKoothoor] 
  "A Document Driven Approach to Certifying Scientific Computing Software"
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

smithEtAl2007 = cArticle [spencerSmith, lLai, rKhedri]
  ("Requirements Analysis for Engineering Computation: A Systematic Approach for" ++ 
    " Improving Software Reliability")
  "Reliable Computing, Special Issue on Reliable Engineering Computation" 2007
  [month Feb, volume 13, number 1, pages [83..107], howPublishedU "https://doi.org/10.1007/s11155-006-9020-7"]
  "smithEtAl2007"

smithKoothoor2016 = cArticle [spencerSmith, nKoothoor]
  ("A Document-Driven Method for Certifying Scientific Computing Software for Use" ++ 
    " in Nuclear Safety Analysis")
  " Nuclear Engineering and Technology" 2016
  [month Apr, volume 48, number 2, pages[404..418], 
    howPublishedU "http://www.sciencedirect.com/science/article/pii/S1738573315002582"]
  "smithKoothoor2016"

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

hibbeler2004 = cBookA [rcHibbeler]
  "Engineering Mechanics: Dynamics"
  "Pearson Prentice Hall"
  2004 [] "hibbeler2004"

-- ** Wikipedia

dampingSource, accelerationWiki, velocityWiki, cartesianWiki, laplaceWiki, pidWiki :: Citation

dampingSource = cMisc
  [author [wikiAuthors], title "Damping",
  howPublishedU "https://en.wikipedia.org/wiki/Damping_ratio",
  month Jul, year 2019]
  "dampingSource"

accelerationWiki = cMisc [author [wikiAuthors],
  title "Acceleration", howPublishedU "https://en.wikipedia.org/wiki/Acceleration",
  month Jun, year 2019]
  "accelerationWiki"

velocityWiki = cMisc [author [wikiAuthors],
  title "Velocity", howPublishedU "https://en.wikipedia.org/wiki/Velocity",
  month Jun, year 2019]
  "velocityWiki"

cartesianWiki = cMisc
  [author [wikiAuthors], title "Cartesian coordinate system",
  howPublishedU "https://en.wikipedia.org/wiki/Cartesian_coordinate_system",
  month Jun, year 2019]
  "cartesianWiki"

laplaceWiki
  = cMisc
      [author [wikiAuthors], title "Laplace transform",
       howPublishedU "https://en.wikipedia.org/wiki/Laplace_transform",
       month Nov, year 2020]
      "laplaceWiki"

pidWiki
  = cMisc
      [author [wikiAuthors], title "PID controller",
       howPublishedU "https://en.wikipedia.org/wiki/PID_controller", month Oct,
       year 2020]
      "pidWiki"

-- * Common Cite Fields

jnlCGJ :: String
jnlCGJ = "Canadian Geotechnical Journal"
