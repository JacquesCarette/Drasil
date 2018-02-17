module Drasil.GlassBR.References (gbCitations) where

import Language.Drasil

import Data.Drasil.People (jmBracci, tlKohutek, wlBeason, jRobertson, sRobertson)
import Data.Drasil.Citations (koothoor2013, smithLai2005)

rbrtsn2012, astm_LR2009, astm_C1036, astm_C1048, glThick1998 :: Citation

gbCitations :: BibRef
gbCitations = [koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, astm_C1036,
  astm_C1048, glThick1998]

rbrtsn2012 = Citation Article 
  [
  Author [jRobertson, sRobertson],
  Title (S "Volere requirements specification template edition 16"),
  URL (S "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e5" :+: 
    S "87e09880561dbe22.pdf"),
  Year (2012)
  ]

astm_LR2009 = Citation Misc 
  [
  Author [mononym "ASTM E1300-09"],
  Title (S "Standard Practice for Determining Load Resistance of Glass in Buildings"),
  Publisher (S "ASTM International"),
  Collection (S "Standard E1300-09a"),
  Year (2009),
  URL (S "www.astm.org")
  ]

astm_C1036 = Citation Misc 
  [
  Author [mononym "ASTM C1036-16"],
  Title (S "Standard specification for Flat Glass"),
  Publisher (S "ASTM International"),  
  Place (S "West Conshohocken", S "PA"),
  Year (2016),
  URL (S "www.astm.org")
  ]

astm_C1048 = Citation Misc 
  [
  Author [mononym "ASTM C1048-04"],
  Title (S "Specification for Heat-Treated Flat Glass-Kind" +:+
    S "HS, Kind FT Coated and Uncoated Glass"),
  Publisher (S "ASTM International"),
  Place (S "West Conshohocken", S "PA"),
  Year (2009),
  URL (S "www.astm.org")
  ]

glThick1998 = Citation Misc
  [
  Author [wlBeason, tlKohutek, jmBracci],
  Title (S "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts"),
  Collection (S "ASCE Library"),
  Date 1 Feb 1998,
  URL (S "doi.org/10.1061/(ASCE)0733-9445(1998)124:2(215)")
  ]
