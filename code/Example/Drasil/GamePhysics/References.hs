module Drasil.GamePhysics.References (cpCitations) where

import Language.Drasil

import Data.Drasil.People (dParnas, gWilson, daAruliah, cTitus, nChueHong, 
  mDavis, rGuy, shdHaddock, kdHuff, imMitchell, mdPlumblet, bWaugh, 
  epWhite, pWilson, pcClements, dmWiess, jBueche)
  
import Data.Drasil.Citations (koothoor2013, parnas1986, smithLai2005)

parnas1978, sciComp2013, dParnas1972, dParnasPcClements1984, jfBeucheIntro :: Citation

cpCitations :: BibRef
cpCitations = [parnas1978, sciComp2013, dParnas1972, dParnasPcClements1984, 
  parnas1986, koothoor2013, smithLai2005, jfBeucheIntro]

--FIXME: check for references made within document

parnas1978 = cInProceedings "parnas1978" [dParnas]
    (S "Designing Software for Ease of Extension and Contraction.")
    (S "ICSE '78: Proceedings of the 3rd international conference on" +:+ 
      S "Software engineering") 1978
    [pages [264,277]]

sciComp2013 = cArticle "sciComp2013"
  [gWilson, daAruliah, cTitus, nChueHong, mDavis, rGuy, shdHaddock,
  kdHuff, imMitchell, mdPlumblet, bWaugh, epWhite, pWilson]
  (S "Best Practices for Scientific Computing, 2013")
  (S "PLoS Biol") 2013
  [volume 12, number 1]

dParnas1972 = cArticle "dParnas1972" [dParnas]
  (S "On the Criteria To Be Used in Decomposing Systems into Modules")
  (S "Communications of the ACM") 1972
  [pages [1053, 1058]]

dParnasPcClements1984 = cInProceedings "dParnasPcClements1984" 
  [dParnas, pcClements, dmWiess]
  (S "The Modular Structure of Complex Systems")
  (S "ICSE '84: Proceedings of the 7th international conference on Software engineering")
  1984 [pages [408, 417]]

jfBeucheIntro = cMisc "jfBeucheIntro"
  [ author [jBueche]
  , title (S "Introduction to Physics for Scientists, Fourth Edition")
  , publisher (S "Mcgraw-Hill College") --FIXME: not sure if this is publisher of 4th edition
  , year 1986
  ]
