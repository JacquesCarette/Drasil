module Data.Drasil.People where

import Language.Drasil (Person, person, personWM)
  
spencerSmith, henryFrankis, nKoothoor, dParnas, daAruliah, gWilson,
  cTitus, kdHuff, cHong, mDavis, rGuy, shdHaddock, imMitchell, mdPlumblet,
  bWaugh, epWhite, pWilson, pcClements, dDewitt, tBergman, aLavine,
  jBueche, fIncropera :: Person
aLavine      = personWM "A"         ["S"]           "Lavine"
bWaugh       = person   "Ben"                       "Waugh" -- Best Practices for Scientific Computing 2013
cHong        = person   "Chue"                      "Hong"
cTitus       = person   "C"                         "Titus"
daAruliah    = personWM "D"         ["A"]           "Aruliah"
dDewitt      = personWM "D"         ["P"]           "Dewitt"
dParnas      = personWM "David"     ["L"]           "Parnas"
epWhite      = personWM "Ethan"     ["P"]           "White" -- Best Practices for Scientific Computing 2013
fIncropera   = personWM "F"         ["P"]           "Incropera"
gWilson      = person   "Greg"                      "Wilson"
henryFrankis = person   "Henry"                     "Frankis"
imMitchell   = personWM "Ian"       ["M"]           "Mitchell"
jBueche      = personWM "J"         ["Frederick"]   "Bueche"
kdHuff       = personWM "Kathryn"   ["D"]           "Huff"
mDavis       = person   "Matt"                      "Davis"
mdPlumblet   = personWM "Mark"      ["D"]           "Plumblet"
nKoothoor    = person   "Nirmitha"                  "Koothoor"
pcClements   = personWM "P"         ["C"]           "Clements" -- The Modular Structure of Complex Systems ICSE '84
pWilson      = person   "Paul"                      "Wilson" -- Best Practices for Scientific Computing 2013
rGuy         = personWM "Richard"   ["T"]           "Guy"
shdHaddock   = personWM "Steven"    ["H","D"]       "Haddock"
tBergman     = personWM "T"         ["L"]           "Bergman"
spencerSmith = personWM "W"         ["Spencer"]     "Smith"

-- short versions for now
luthfi, alex, nikitha, thulasi, brooks, mLightstone, lLai,
  pAgerfalk, nKraiem, jRalyte :: Person
luthfi      = person "Luthfi"     "Mawarid"
alex        = person "Alex"       "Halliwushka"
nikitha     = person "Nikitha"    "Krithnan"
thulasi     = person "Thulasi"    "Jegatheesan"
brooks      = person "Brooks"     "MacLachlan"
mLightstone = person "Marilyn"    "Lightstone"
lLai        = person "Lei"        "Lai"
pAgerfalk   = person "PJ"         "Agerfalk"
nKraiem     = person "N"          "Kraiem"
jRalyte     = person "J"          "Ralyte"
  --FIXME: person' takes strings but we need an "e" with an accent
  -- J. Ralyt" :+: (F Acute 'e')