module Data.Drasil.People where

import Language.Drasil (Person, person, personWM)
  
spencerSmith, henryFrankis, nKoothoor, dParnas, daAruliah, gWilson,
  cTitus, kdHuff, cHong, mDavis, rGuy, shdHaddock, imMitchell, mdPlumblet,
  bWaugh, epWhite, pWilson, pcClements, luthfi, alex, nikitha, thulasi,
  brooks, mLightstone, lLai, pjAgerfalk, nKraiem, jRalyte, jBueche,
  fIncropera, dDewitt, tBergman, aLavine :: Person

pjAgerfalk   = person   "PJ"                        "Agerfalk"
daAruliah    = personWM "D"         ["A"]           "Aruliah"
tBergman     = personWM "T"         ["L"]           "Bergman"
jBueche      = personWM "J"         ["Frederick"]   "Bueche"
pcClements   = personWM "P"         ["C"]           "Clements" -- The Modular Structure of Complex Systems ICSE '84
mDavis       = person   "Matt"                      "Davis"
dDewitt      = personWM "D"         ["P"]           "Dewitt"
henryFrankis = person   "Henry"                     "Frankis"
rGuy         = personWM "Richard"   ["T"]           "Guy"
shdHaddock   = personWM "Steven"    ["H", "D"]      "Haddock"
alex         = person   "Alex"                      "Halliwushka"
cHong        = person   "Chue"                      "Hong"
kdHuff       = personWM "Kathryn"   ["D"]           "Huff"
fIncropera   = personWM "F"         ["P"]           "Incropera"
thulasi      = person   "Thulasi"                   "Jegatheesan"
nKoothoor    = person   "Nirmitha"                  "Koothoor"
nKraiem      = person   "N"                         "Kraiem"
nikitha      = person   "Nikitha"                   "Krithnan"
lLai         = person   "Lei"                       "Lai"
aLavine      = personWM "A"         ["S"]           "Lavine"
mLightstone  = person   "Marilyn"                   "Lightstone"
brooks       = person   "Brooks"                    "MacLachlan"
luthfi       = person   "Luthfi"                    "Mawarid"
imMitchell   = personWM "Ian"       ["M"]           "Mitchell"
dParnas      = personWM "David"     ["L"]           "Parnas"
mdPlumblet   = personWM "Mark"      ["D"]           "Plumblet"
jRalyte      = person   "J"                         "Ralyte"
  --FIXME: person takes strings but we need an "e" with an accent
  -- S "J. Ralyt" :+: (F Acute 'e')
spencerSmith = personWM "W"         ["Spencer"]     "Smith"
cTitus       = person   "C"                         "Titus"
bWaugh       = person   "Ben"                       "Waugh" -- Best Practices for Scientific Computing 2013
epWhite      = personWM "Ethan"     ["P"]           "White" -- Best Practices for Scientific Computing 2013
gWilson      = person   "Greg"                      "Wilson"
pWilson      = person   "Paul"                      "Wilson" -- Best Practices for Scientific Computing 2013