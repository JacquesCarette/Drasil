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

rbrtsn2012 = cMisc [author [jRobertson, sRobertson], title
  "Volere requirements specification template edition 16",
  howPublishedU "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e587e09880561dbe22.pdf"
  , year 2012]
  "rbrtsn2012"

astm2009 = cMisc [author [mononym "ASTM"],
  title "Standard Practice for Determining Load Resistance of Glass in Buildings",
  publisher "ASTM International",
  bookTitle "Standard E1300-09a",
  year 2009, howPublishedU "www.astm.org"]
  "astm2009"

astm2016 = cMisc
  [ author [mononym "ASTM"],
  title "Standard specification for Flat Glass",
  publisher "ASTM International",
  address "West Conshohocken, PA",
  year 2016, howPublishedU "https://doi.org/10.1520/C1036-16"]
  "astm2016"

astm2012 = cMisc
  [ author [mononym "ASTM"],
  title "Standard Specification for Heat-Strengthened and Fully Tempered Flat Glass",
  publisher "ASTM International",
  address "West Conshohocken, PA",
  year 2012, howPublishedU "https://doi.org/10.1520/C1048-12E01"]
  "astm2012"

beasonEtAl1998 = cMisc
  [ author [wlBeason, tlKohutek, jmBracci],
  title "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts",
  bookTitle "ASCE Library",
  month Feb, year 1998,
  howPublishedU "doi.org/10.1061/(ASCE)0733-9445(1998)124:2(215)"]
  "beasonEtAl1998"

ghezziEtAl2003 = cMisc
  [author [carloGhezzi, mJazayeri, dMandrioli],
  title "Fundamentals of Software Engineering",
  year 2003,
  publisher "Prentice Hall",
  address "Upper Saddle River, NJ, USA",
  edition 2]
  "ghezziEtAl2003"

hoffmanAndStrooper1995 = cMisc
  [author [hoffman, strooper],
  title "Software Design, Automated Testing, and Maintenance: A Practical Approach",
  publisher "International Thoomson Computer Press",
  address "New York, NY, USA",
  year 1995,
  howPublishedU "http://citeseer.ist.psu.edu/428727.html"]
  "hoffmanAndStrooper1995"
