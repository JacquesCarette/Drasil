module Drasil.GlassBR.References where

import Language.Drasil

import Data.Drasil.Citations (campidelli, koothoor2013, smithLai2005, parnasClements1986)
import Data.Drasil.People (jRobertson, jmBracci, sRobertson, tlKohutek, wlBeason,
  carloGhezzi, mJazayeri, dMandrioli, hoffman, strooper)

rbrtsn2012, astm2009, astm2016, astm2012, beasonEtAl1998, 
  hoffmanAndStrooper1995, ghezziEtAl2003 :: Citation

srsCitations :: BibRef
srsCitations = [campidelli, koothoor2013, smithLai2005, rbrtsn2012, astm2009, astm2016,
  astm2012, beasonEtAl1998, parnasClements1986]

misCitations :: BibRef
misCitations = [hoffmanAndStrooper1995, ghezziEtAl2003]

rbrtsn2012 = cMisc "rbrtsn2012" [author [jRobertson, sRobertson], title
  (S "Volere requirements specification template edition 16"),
  howPublishedU (S "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e5" :+: 
    S "87e09880561dbe22.pdf"), year 2012]
  rbrtsn2012Label
  where
    rbrtsn2012Label :: Label
    rbrtsn2012Label = mkLabelRA' "rbrtsn2012Label" "rbrtsn2012" Cite

astm2009 = cMisc "astm2009" [author [mononym "ASTM"],
  title (S "Standard Practice for Determining Load Resistance of Glass in Buildings"),
  publisher (S "ASTM International"),
  bookTitle (S "Standard E1300-09a"),
  year (2009), howPublishedU (S "www.astm.org")]
  astm2009Label
  where
    astm2009Label = mkLabelRA' "astm2009Label" "astm2009" Cite

astm2016 = cMisc "astm2016"
  [ author [mononym "ASTM"],
  title (S "Standard specification for Flat Glass"),
  publisher (S "ASTM International"),  
  address (S "West Conshohocken, PA"),
  year 2016, howPublishedU (S "https://doi.org/10.1520/C1036-16")]
  astm2016Label
  where
    astm2016Label = mkLabelRA' "astm2016Label" "astm2016" Cite

astm2012 = cMisc "astm2012"
  [ author [mononym "ASTM"],
  title (S "Standard Specification for Heat-Strengthened and Fully Tempered" +:+
    S "Flat Glass"),
  publisher (S "ASTM International"),
  address (S "West Conshohocken, PA"),
  year 2012, howPublishedU (S "https://doi.org/10.1520/C1048-12E01")]
  astm2012Label
  where
    astm2012Label = mkLabelRA' "astm2012Label" "astm2012" Cite

beasonEtAl1998 = cMisc "beasonEtAl1998"
  [ author [wlBeason, tlKohutek, jmBracci],
  title (S "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts"),
  bookTitle (S "ASCE Library"),
  month Feb, year 1998,
  howPublishedU (S "doi.org/10.1061/(ASCE)0733-9445(1998)124:2(215)")]
  beasonEtAl1998Label
  where
    beasonEtAl1998Label = mkLabelRA' "beasonEtAl1998Label" "beasonEtAl1998" Cite

ghezziEtAl2003 = cMisc "ghezziEtAl2003"
  [author [carloGhezzi, mJazayeri, dMandrioli],
  title (S "Fundamentals of Software Engineering"),
  year 2003,
  publisher (S "Prentice Hall"),
  address (S "Upper Saddle River, NJ, USA"),
  edition 2
  ]
  ghezziEtAl2003Label
  where
    ghezziEtAl2003Label = mkLabelRA' "ghezziEtAl2003Label" "ghezziEtAl2003" Cite

hoffmanAndStrooper1995 = cMisc "hoffmanAndStrooper1995"
  [author [hoffman, strooper],
  title (S 
  "Software Design, Automated Testing, and Maintenance: A Practical Approach"),
  publisher (S "International Thoomson Computer Press"),
  address (S "New York, NY, USA"),
  year 1995,
  howPublishedU (S "http://citeseer.ist.psu.edu/428727.html")
  ]
  hoffmanAndStrooper1995Label
  where 
    hoffmanAndStrooper1995Label = mkLabelRA' "hoffmanAndStrooper1995Label" 
      "hoffmanAndStrooper1995" Cite