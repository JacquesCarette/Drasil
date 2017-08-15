module Drasil.GlassBR.References (gbCitations) where

import Language.Drasil

import Data.Drasil.People (nKoothoor, jmBracci, tlKohutek, wlBeason, spencerSmith,
  lLai, jRobertson, sRobertson, pjAgerfalk, nKraiem, jRalyte)

{--}

koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, astm_C1036, 
  astm_C1048, glThick1998 :: Citation

gbCitations :: BibRef
gbCitations = [koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, astm_C1036,
  astm_C1048, glThick1998]

--FIXME: check for references made within document

---

--ref1
koothoor2013 = MThesis 
  [
  Author [nKoothoor],
  Title (S "A document drive approach to certifying scientific computing software"),
  School (S "McMaster University"),
  Place (S "Hamilton", S "Canada"),
  Year (2013)
  ]

--FIXME: Place isn't displayed in TeX?

---

--ref2
smithLai2005 = Article 
  [
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

---

--ref3
rbrtsn2012 = Article 
  [
  Author [jRobertson, sRobertson],
  Title (S "Volere requirements specification template edition 16"),
  URL (S "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e5" :+: 
    S "87e09880561dbe22.pdf"),
  Year (2012)
  ]

---

--ref4
astm_LR2009 = Misc 
  [
  Author [mononym "ASTM E1300-09"],
  Title (S "Standard Practice for Determining Load Resistance of Glass in Buildings"),
  Publisher (S "ASTM International"),
  Collection (S "Standard E1300-09a"),
  Year (2009),
  URL (S "www.astm.org")
  ]

---

--ref5
astm_C1036 = Misc 
  [
  Author [mononym "ASTM C1036-16"],
  Title (S "Standard specification for Flat Glass"),
  Publisher (S "ASTM International"),  
  Place (S "West Conshohocken", S "PA"),
  Year (2016),
  URL (S "www.astm.org")
  ]

---

--ref6
astm_C1048 = Misc 
  [
  Author [mononym "ASTM C1048-04"],
  Title (S "Specification for Heat-Treated Flat Glass-Kind" +:+
    S "HS, Kind FT Coated and Uncoated Glass"),
  Publisher (S "ASTM International"),
  Place (S "West Conshohocken", S "PA"),
  Year (2009),
  URL (S "www.astm.org")
  ]

---

--ref7
glThick1998 = Misc
  [
  Author [wlBeason, tlKohutek, jmBracci],
  Title (S "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts"),
  Collection (S "ASCE Library"),
  Date 1 Feb 1998,
  URL (S "doi.org/10.1061/(ASCE)0733-9445(1998)124:2(215)")
  ]