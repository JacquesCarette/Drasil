-- | Define people for use in Drasil. Used often in authors and citations.
module Data.Drasil.People
  ( module Data.Drasil.People
  , module Drasil.Metadata.People
  ) where

import Language.Drasil (Person, person, person', personWM, personWM', mononym)
import Drasil.Metadata.People

henryFrankis, daAruliah, gWilson,
  cTitus, kdHuff, nChueHong, mDavis, rGuy, shdHaddock, imMitchell, mdPlumblet,
  bWaugh, epWhite, pWilson, luthfi, alex, nikitha, thulasi,
  brooks, mLightstone, jBueche,
  fIncropera, dDewitt, tBergman, aLavine, jRobertson, sRobertson,
  wlBeason, tlKohutek, jmBracci, qhQian, dyZhu, cfLee, grChen, dgFredlund,
  jKrahn, dStolle, yCLi, ymChen, tltZhan, ssLing, pjCleall, pGuo,
  mCampidelli, dmWiess, sPalmer, scottSmith, bKarchewski, rHuston,
  hJosephs, nrMorgenstern, vePrice, samCrawford, rcHibbeler, olu, rodPierce,
  dong :: Person

daAruliah     = personWM  "D"         ["A"]           "Aruliah"
wlBeason      = personWM  "W"         ["Lynn"]        "Beason"
tBergman      = personWM  "T"         ["L"]           "Bergman"
jmBracci      = personWM  "Joseph"    ["M"]           "Bracci"
jBueche       = personWM  "J"         ["Frederick"]   "Bueche"
mCampidelli   = person    "Manuel"                    "Campidelli"
dong          = person    "Dong"                      "Chen"
grChen        = personWM' "G"         ["R"]           "Chen"
ymChen        = person'   "Yun-Min"                   "Chen"
pjCleall      = personWM  "Peter"     ["John"]        "Cleall"
samCrawford   = personWM  "Samuel"    ["J"]           "Crawford"
mDavis        = person    "Matt"                      "Davis"
dDewitt       = personWM  "D"         ["P"]           "Dewitt"
henryFrankis  = person    "Henry"                     "Frankis"
dgFredlund    = personWM  "D"         ["G"]           "Fredlund"
rGuy          = personWM  "Richard"   ["T"]           "Guy"
pGuo          = person    "Peijun"                    "Guo"
shdHaddock    = personWM  "Steven"    ["H", "D"]      "Haddock"
alex          = person    "Alex"                      "Halliwushka"
rcHibbeler    = personWM' "R"         ["C"]           "Hibbeler"
rHuston       = person    "Ronald"                    "Huston"
nChueHong     = personWM  "Neil"      ["P"]           "Chue Hong"
kdHuff        = personWM  "Kathryn"   ["D"]           "Huff"
fIncropera    = personWM  "F"         ["P"]           "Incropera"
thulasi       = person    "Thulasi"                   "Jegatheesan"
hJosephs      = person    "Harold"                    "Josephs"
bKarchewski   = person    "Brandon"                   "Karchewski"
tlKohutek     = personWM  "Terry"     ["L"]           "Kohutek"
jKrahn        = person    "J"                         "Krahn"
nikitha       = person    "Nikitha"                   "Krithnan"
aLavine       = personWM  "A"         ["S"]           "Lavine"
cfLee         = personWM' "C"         ["F"]           "Lee"
mLightstone   = person    "Marilyn"                   "Lightstone"
yCLi          = person'   "Yu-Chao"                   "Li"
ssLing        = person'   "Sao-Sheng"                 "Ling"
brooks        = person    "Brooks"                    "MacLachlan"
luthfi        = person    "Luthfi"                    "Mawarid"
imMitchell    = personWM  "Ian"       ["M"]           "Mitchell"
nrMorgenstern = personWM  "N"         ["R"]           "Morgenstern"
sPalmer       = person    "Steven"                    "Palmer"
rodPierce     = person    "Rod"                       "Pierce"
mdPlumblet    = personWM  "Mark"      ["D"]           "Plumblet"
vePrice       = personWM  "P"         ["E"]           "Price"
qhQian        = personWM' "Q"         ["H"]           "Qian"
olu           = person    "Olu"                       "Owojaiye"
  --FIXME: person takes strings but we need an "e" with an accent
  -- S "J. Ralyt" :+: (F Acute 'e')
jRobertson    = person    "James"                     "Robertson"
sRobertson    = person    "Suzanne"                   "Robertson"
scottSmith    = person    "Scott"                     "Smith"
dStolle       = person    "Dieter"                    "Stolle"
cTitus        = person    "C"                         "Titus"
bWaugh        = person    "Ben"                       "Waugh" -- Best Practices for Scientific Computing 2013
epWhite       = personWM  "Ethan"     ["P"]           "White" -- Best Practices for Scientific Computing 2013
dmWiess       = personWM  ""          []              "Wiess"
gWilson       = person    "Greg"                      "Wilson"
pWilson       = person    "Paul"                      "Wilson" -- Best Practices for Scientific Computing 2013
tltZhan       = personWM  "Tony"      ["L","T"]       "Zhan"
dyZhu         = personWM' "D"         ["Y"]           "Zhu"

-- Right now, we have to say this is a 'Person', even though it clearly isn't
wikiAuthors :: Person
wikiAuthors   = mononym "Wikipedia Contributors"
