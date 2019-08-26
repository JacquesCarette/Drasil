module Drasil.GamePhysics.References (chaslesWiki, citations, parnas1972,
  parnasClements1984) where

import Language.Drasil

import Data.Drasil.Citations (cartesianWiki, koothoor2013, parnasClements1986,
  parnas1972, parnasClements1984, smithLai2005, lineSource, pointSource)
import Data.Drasil.People (bWaugh, cTitus, dParnas, daAruliah, epWhite, gWilson,
  imMitchell, jBueche, kdHuff, mDavis, mdPlumblet, nChueHong, pWilson, rGuy, shdHaddock)

chaslesWiki, jfBeucheIntro, parnas1978, sciComp2013 :: Citation

citations :: BibRef
citations = [parnas1978, sciComp2013, parnas1972, parnasClements1984, chaslesWiki,
  parnasClements1986, koothoor2013, smithLai2005, jfBeucheIntro, cartesianWiki, lineSource, pointSource]

--FIXME: check for references made within document

chaslesWiki = cMisc [author [mononym "Wikipedia Contributors"],
  title "Chasles' theorem (kinematics)",
  howPublishedU "https://en.wikipedia.org/wiki/Chasles'_theorem_(kinematics)",
  month Nov, year 2018] "chaslesWiki"

jfBeucheIntro = cMisc
  [ author [jBueche]
  , title "Introduction to Physics for Scientists, Fourth Edition"
  , publisher "Mcgraw-Hill College" --FIXME: not sure if this is publisher of 4th edition
  , year 1986
  ] "jfBeucheIntro"

parnas1978 = cInProceedings [dParnas]
    "Designing Software for Ease of Extension and Contraction"
    "ICSE '78: Proceedings of the 3rd international conference on Software engineering" 
    1978 [pages [264..277]] "parnas1978"

sciComp2013 = cArticle
  [gWilson, daAruliah, cTitus, nChueHong, mDavis, rGuy, shdHaddock,
  kdHuff, imMitchell, mdPlumblet, bWaugh, epWhite, pWilson]
  "Best Practices for Scientific Computing, 2013"
  "PLoS Biol" 2013
  [volume 12, number 1] "sciComp2013"

