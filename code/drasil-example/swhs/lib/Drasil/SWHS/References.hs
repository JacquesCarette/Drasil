module Drasil.SWHS.References where

import Language.Drasil

import Data.Drasil.People (jBueche, fIncropera, dDewitt, tBergman, aLavine,
  mLightstone)

import Data.Drasil.Citations (koothoor2013, parnasClements1986,
  smithEtAl2007, smithLai2005, smithKoothoor2016)

----------------------------
-- Section 9 : References --
----------------------------
citations :: BibRef
citations = [bueche1986, incroperaEtAl2007, koothoor2013, lightstone2012,
             parnasClements1986, smithEtAl2007, smithLai2005,
             smithKoothoor2016]

bueche1986, incroperaEtAl2007, lightstone2012 :: Citation

bueche1986 = cBookA [jBueche]
  "Introduction to Physics for Scientists"
  "McGraw Hill" 1986
  [edition 4, address "New York City, New York"]
  "bueche1986"

incroperaEtAl2007 = cBookA [fIncropera, dDewitt, tBergman, aLavine]
  "Fundamentals of Heat and Mass Transfer"
  "John Wiley and Sons" 2007
  [edition 6, address "Hoboken, New Jersey"]
  "incroperaEtAl2007"

lightstone2012 = cMisc [
  author [mLightstone],
  title "Derivation of tank/pcm model",
  year 2012,
  note "From Marilyn Lightstone's Personal Notes"]
  "lightstone2012"

uriReferences :: [Reference]
uriReferences = [consThemESrc, latHtESrc, sensHtESrc]

consThemESrc :: Reference
consThemESrc = makeURI "consThemESrc"
  "http://www.efunda.com/formulae/heat_transfer/conduction/overview_cond.cfm" $
  shortname' $ S "Fourier Law of Heat Conduction and Heat Equation"

latHtESrc :: Reference
latHtESrc = makeURI "latHtESrc" "http://en.wikipedia.org/wiki/Latent_heat" $
  shortname' $ S "Definition of Latent Heat"

sensHtESrc :: Reference
sensHtESrc = makeURI "sensHtESrc"
  "http://en.wikipedia.org/wiki/Sensible_heat" $
  shortname' $ S "Definition of Sensible Heat"
