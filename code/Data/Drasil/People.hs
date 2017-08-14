module Data.Drasil.People where

import Language.Drasil (Person, person, personWM)
  
spencerSmith, henryFrankis, nKoothoor, dParnas, daAruliah, gWilson,
  cTitus, kdHuff, cHong, mDavis, rGuy, shdHaddock, imMitchell, mdPlumblet,
  bWaugh, epWhite, pWilson, pcClements, dDewitt, tBergman, aLavine,
  jBueche, fIncropera :: Person
spencerSmith = personWM "W"         ["Spencer"]     "Smith"
dParnas      = personWM "David"     ["L"]           "Parnas"
daAruliah    = personWM "D"         ["A"]           "Aruliah"
henryFrankis = person   "Henry"                     "Frankis"
nKoothoor    = person   "Nirmitha"                  "Koothoor"
gWilson      = person   "Greg"                      "Wilson"
cTitus       = person   "C"                         "Titus"
kdHuff       = personWM "Kathryn"   ["D"]           "Huff"
cHong        = person   "Chue"                      "Hong"
mDavis       = person   "Matt"                      "Davis"
rGuy         = personWM "Richard"   ["T"]           "Guy"
shdHaddock   = personWM "Steven"    ["H","D"]       "Haddock"
imMitchell   = personWM "Ian"       ["M"]           "Mitchell"
mdPlumblet   = personWM "Mark"      ["D"]           "Plumblet"
bWaugh       = person   "Ben"                       "Waugh" -- Best Practices for Scientific Computing 2013
epWhite      = personWM "Ethan"     ["P"]           "White" -- Best Practices for Scientific Computing 2013
pWilson      = person   "Paul"                      "Wilson" -- Best Practices for Scientific Computing 2013
pcClements   = personWM "P"         ["C"]           "Clements" -- The Modular Structure of Complex Systems ICSE '84
aLavine      = personWM "A"         ["S"]           "Lavine"
tBergman     = personWM "T"         ["L"]           "Bergman"
dDewitt      = personWM "D"         ["P"]           "Dewitt"
fIncropera   = personWM "F"         ["P"]           "Incropera"
jBueche      = personWM "J"         ["Frederick"]   "Bueche"

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