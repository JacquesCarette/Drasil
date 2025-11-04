module Drasil.GlassBR.References (
  citations,
  astm2009, astm2012, astm2016, beasonEtAl1998,
  cartesianWiki
) where

import Language.Drasil

import Data.Drasil.Citations (campidelli, cartesianWiki, koothoor2013,
  smithEtAl2007, smithLai2005, smithKoothoor2016, parnasClements1986)
import Data.Drasil.People (jmBracci, tlKohutek, wlBeason)

astm2009, astm2016, astm2012, beasonEtAl1998 :: Citation

citations :: BibRef
citations = [campidelli, koothoor2013, smithEtAl2007, smithLai2005,
             smithKoothoor2016, astm2009, astm2016, astm2012, beasonEtAl1998,
             parnasClements1986, cartesianWiki]

astm2009 = cMisc [author [mononym "ASTM"],
  title "Standard Practice for Determining Load Resistance of Glass in Buildings",
  publisher "ASTM International",
  bookTitle "Standard E1300-09a",
  year 2009, howPublishedU "https://www.astm.org"]
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
  howPublishedU "https://doi.org/10.1061/(ASCE)0733-9445(1998)124:2(215)"]
  "beasonEtAl1998"
