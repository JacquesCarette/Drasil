module Data.Drasil.Authors where

import Language.Drasil (Person, person, personWM)

spencerSmith, henryFrankis, nKoothoor, dParnas :: Person
spencerSmith = personWM "W" ["Spencer"] "Smith"
henryFrankis = person "Henry"   "Frankis"
nKoothoor    = person "Nirmitha" "Koothoor"
dParnas      = personWM "David" ["L"] "Parnas"

-- short versions for now
luthfi, alex, nikitha, thulasi, brooks :: Person
luthfi   = person "Luthfi"  "Mawarid"
alex     = person "Alex"    "Halliwushka"
nikitha  = person "Nikitha"  "Krithnan"
thulasi  = person "Thulasi" "Jegatheesan"
brooks   = person "Brooks"  "MacLachlan"
