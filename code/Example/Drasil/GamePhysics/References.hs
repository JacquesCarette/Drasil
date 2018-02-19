module Drasil.GamePhysics.References (cpCitations) where

import Language.Drasil

import Data.Drasil.People (dParnas, gWilson, daAruliah, cTitus, nChueHong, 
  mDavis, rGuy, shdHaddock, kdHuff, imMitchell, mdPlumblet, bWaugh, 
  epWhite, pWilson, pcClements, dmWiess, nKoothoor, jBueche, lLai, 
  pjAgerfalk, nKraiem, jRalyte, spencerSmith)


parnas1978, sciComp2013, dParnas1972, dParnasPcClements1984,
  dParnasPcClements1986, koothoor2013, smithLai2005, jfBeucheIntro :: Citation

cpCitations :: BibRef
cpCitations = [parnas1978, sciComp2013, dParnas1972, dParnasPcClements1984, 
  dParnasPcClements1986, koothoor2013, smithLai2005, jfBeucheIntro]

--FIXME: check for references made within document

parnas1978 = Citation Article [
    Author [dParnas],
    Title (S "Designing Software for Ease of Extension and Contraction."),
    Journal (S "ICSE '78: Proceedings of the 3rd international conference on" +:+ 
      S "Software engineering"),
    Pages (264, 277),
    Year (1978)
  ]

sciComp2013 = Citation Article [
    Author [gWilson, daAruliah, cTitus, nChueHong, mDavis, rGuy, shdHaddock,
      kdHuff, imMitchell, mdPlumblet, bWaugh, epWhite, pWilson],
    Title (S "Best Practices for Scientific Computing, 2013"),
    Year (2013)
  ]

dParnas1972 = Citation Article [
    Author [dParnas],
    Title (S "On the Criteria To Be Used in Decomposing Systems into Modules"),
    Journal (S "Communications of the ACM"),
    Pages (1053, 1058),
    Year (1972)
  ]

dParnasPcClements1984 = Citation Article [
    Author [dParnas, pcClements, dmWiess],
    Title (S "The Modular Structure of Complex Systems"),
    Journal (S "ICSE '84: Proceedings of the 7th international conference" +:+ 
      S "on Software engineering"),
    Pages (408, 417),
    Year (1984)
  ]

dParnasPcClements1986 = Citation Article [
    Author [dParnas, pcClements],
    Title (S "A Rational Design Process: How and Why to Fake it"),
    Journal (S "IEEE Transactions on Software Engineering"),
    Pages (251, 257),
    Year (1986)
  ]

koothoor2013 = Citation MThesis [
    Author [nKoothoor],
    Title (S "A document drive approach to certifying scientific computing software"),
    School (S "McMaster University"),
    Place (S "Hamilton", S "Canada"),
    Year (2013)
  ]

smithLai2005 = Citation Article [
    Author [spencerSmith, lLai],
    Title (S "A new requirements template for scientific computing"),
    Journal (S "Proceedings of the First International Workshop on" +:+
    S "Situational Requirements Engineering Processes - Methods," +:+
    S "Techniques and Tools to Support Situation-Specific Requirements" +:+
    S "Engineering Processes, SREP'05"),
    Editor [pjAgerfalk, nKraiem, jRalyte],
    Place (S "Paris", S "France"),
    Pages (107, 121),
    Note (S "In conjunction with 13th IEEE International Requirements" +:+
    S "Engineering Conference,"),
    Year 2005
  ]

jfBeucheIntro = Citation Misc [ 
    Author [jBueche],
    Title (S "Introduction to Physics for Scientists, Fourth Edition"),
    Publisher (S "Mcgraw-Hill College"), --FIXME: not sure if this is publisher of 4th edition
    Year (1986)
  ]
