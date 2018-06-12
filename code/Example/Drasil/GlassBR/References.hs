module Drasil.GlassBR.References where

import Language.Drasil

import Data.Drasil.Citations (koothoor2013, smithLai2005)
import Data.Drasil.People (jRobertson, jmBracci, sRobertson, tlKohutek, wlBeason)

rbrtsn2012, astm_LR2009, astm_C1036, astm_C1048, glThick1998 :: Citation

gbCitations :: BibRef
gbCitations = [koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, astm_C1036,
  astm_C1048, glThick1998]

rbrtsn2012 = cMisc "rbrtsn2012" [author [jRobertson, sRobertson], title
  (S "Volere requirements specification template edition 16"),
  howPublishedU (S "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e5" :+: 
    S "87e09880561dbe22.pdf"), year 2012]

astm_LR2009 = cMisc "astm_LR2009" [author [mononym "ASTM E1300-09"],
  title (S "Standard Practice for Determining Load Resistance of Glass in Buildings"),
  publisher (S "ASTM International"),
  bookTitle (S "Standard E1300-09a"),
  year (2009),
  howPublishedU (S "www.astm.org")]

astm_C1036 = cMisc "astm_C1036"
  [ author [mononym "ASTM C1036-16"],
  title (S "Standard specification for Flat Glass"),
  publisher (S "ASTM International"),  
  address (S "West Conshohocken, PA"),
  year 2016, howPublishedU (S "www.astm.org")
  ]

astm_C1048 = cMisc "astm_C1048"
  [
  author [mononym "ASTM C1048-04"],
  title (S "Specification for Heat-Treated Flat Glass-Kind" +:+
    S "HS, Kind FT Coated and Uncoated Glass"),
  publisher (S "ASTM International"),
  address (S "West Conshohocken, PA"),
  year 2009,
  howPublishedU (S "www.astm.org")
  ]

glThick1998 = cMisc "glThick1998"
  [
  author [wlBeason, tlKohutek, jmBracci],
  title (S "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts"),
  bookTitle (S "ASCE Library"),
  month Feb, year 1998,
  howPublishedU (S "doi.org/10.1061/(ASCE)0733-9445(1998)124:2(215)")
  ]
