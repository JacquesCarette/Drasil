module Drasil.GlassBR.References where

import Language.Drasil

import Data.Drasil.Citations (koothoor2013, smithLai2005, parnasClements1986)
import Data.Drasil.People (jRobertson, jmBracci, sRobertson, tlKohutek, wlBeason)

rbrtsn2012, astm2009, astm2016, astm2012, beasonEtAl1998 :: Citation

gbCitations :: BibRef
gbCitations = [koothoor2013, smithLai2005, rbrtsn2012, astm2009, astm2016,
  astm2012, beasonEtAl1998, parnasClements1986]

rbrtsn2012 = cMisc "rbrtsn2012" [author [jRobertson, sRobertson], title
  (S "Volere requirements specification template edition 16"),
  howPublishedU (S "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e5" :+: 
    S "87e09880561dbe22.pdf"), year 2012]

astm2009 = cMisc "astm2009" [author [mononym "ASTM"],
  title (S "Standard Practice for Determining Load Resistance of Glass in Buildings"),
  publisher (S "ASTM International"),
  bookTitle (S "Standard E1300-09a"),
  year (2009), howPublishedU (S "www.astm.org")]

astm2016 = cMisc "astm2016"
  [ author [mononym "ASTM"],
  title (S "Standard specification for Flat Glass"),
  publisher (S "ASTM International"),  
  address (S "West Conshohocken, PA"),
  year 2016, howPublishedU (S "https://doi.org/10.1520/C1036-16")]

astm2012 = cMisc "astm2012"
  [ author [mononym "ASTM"],
  title (S "Standard Specification for Heat-Strengthened and Fully Tempered" +:+
    S "Flat Glass"),
  publisher (S "ASTM International"),
  address (S "West Conshohocken, PA"),
  year 2012, howPublishedU (S "https://doi.org/10.1520/C1048-12E01")]

beasonEtAl1998 = cMisc "beasonEtAl1998"
  [ author [wlBeason, tlKohutek, jmBracci],
  title (S "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts"),
  bookTitle (S "ASCE Library"),
  month Feb, year 1998,
  howPublishedU (S "doi.org/10.1061/(ASCE)0733-9445(1998)124:2(215)")]