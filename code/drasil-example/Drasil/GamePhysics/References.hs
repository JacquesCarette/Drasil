module Drasil.GamePhysics.References (cpCitations, parnas1972, parnasClements1984) where

import Language.Drasil

import Data.Drasil.Citations (koothoor2013, parnasClements1986, parnas1972, parnasClements1984, 
  smithLai2005)
import Data.Drasil.People (bWaugh, cTitus, dParnas, daAruliah, epWhite,
  gWilson, imMitchell, jBueche, kdHuff, mDavis, mdPlumblet, nChueHong, pWilson, rGuy, shdHaddock)

parnas1978, sciComp2013, jfBeucheIntro :: Citation

cpCitations :: BibRef
cpCitations = [parnas1978, sciComp2013, parnas1972, parnasClements1984,
  parnasClements1986, koothoor2013, smithLai2005, jfBeucheIntro]

--FIXME: check for references made within document

parnas1978 = cInProceedings "parnas1978" [dParnas]
    (S "Designing Software for Ease of Extension and Contraction.")
    (S "ICSE '78: Proceedings of the 3rd international conference on" +:+ 
      S "Software engineering") 1978
    [pages [264,277]] (mkLabelSame "parnas1978Label" Cite)

sciComp2013 = cArticle "sciComp2013"
  [gWilson, daAruliah, cTitus, nChueHong, mDavis, rGuy, shdHaddock,
  kdHuff, imMitchell, mdPlumblet, bWaugh, epWhite, pWilson]
  (S "Best Practices for Scientific Computing, 2013")
  (S "PLoS Biol") 2013
  [volume 12, number 1] (mkLabelSame "sciComp2013Label" Cite)

jfBeucheIntro = cMisc "jfBeucheIntro"
  [ author [jBueche]
  , title (S "Introduction to Physics for Scientists, Fourth Edition")
  , publisher (S "Mcgraw-Hill College") --FIXME: not sure if this is publisher of 4th edition
  , year 1986
  ] (mkLabelSame "jfBeucheIntroLabel" Cite)
