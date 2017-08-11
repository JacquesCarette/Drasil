module Data.Drasil.Authors where

import Language.Drasil (Person, person, personWM)

spencerSmith, henryFrankis, nKoothoor, dParnas, jBueche, fIncropera,
  dDewitt, tBergman, aLavine, pClements :: Person

spencerSmith = personWM "W" ["Spencer"] "Smith"
henryFrankis = person "Henry"   "Frankis"
nKoothoor    = person "Nirmitha" "Koothoor"
dParnas      = personWM "David" ["L"] "Parnas"
jBueche      = personWM "J" ["Frederick"] "Bueche"
fIncropera   = personWM "F" ["P"] "Incropera"
dDewitt      = personWM "D" ["P"] "Dewitt"
tBergman     = personWM "T" ["L"] "Bergman"
aLavine      = personWM "A" ["S"] "Lavine"
pClements    = personWM "P" ["C"] "Clements"

-- short versions for now
luthfi, alex, nikitha, thulasi, brooks, mLightstone, lLai,
  pAgerfalk, nKraiem, jRalyte :: Person
luthfi      = person "Luthfi"  "Mawarid"
alex        = person "Alex"    "Halliwushka"
nikitha     = person "Nikitha"  "Krithnan"
thulasi     = person "Thulasi" "Jegatheesan"
brooks      = person "Brooks"  "MacLachlan"
mLightstone = person "Marilyn" "Lightstone"
lLai        = person "Lei" "Lai"
pAgerfalk   = person "PJ" "Agerfalk"
nKraiem     = person "N" "Kraiem"
jRalyte     = person "J" "Ralyte"
  --FIXME: person' takes strings but we need an "e" with an accent
  -- J. Ralyt" :+: (F Acute 'e')
