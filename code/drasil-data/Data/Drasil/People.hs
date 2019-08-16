module Data.Drasil.People where

import Language.Drasil (Person, person, person', personWM, personWM')
  
spencerSmith, henryFrankis, nKoothoor, dParnas, daAruliah, gWilson,
  cTitus, kdHuff, nChueHong, mDavis, rGuy, shdHaddock, imMitchell, mdPlumblet,
  bWaugh, epWhite, pWilson, pcClements, luthfi, alex, nikitha, thulasi,
  brooks, mLightstone, lLai, pjAgerfalk, nKraiem, jRalyte, jBueche,
  fIncropera, dDewitt, tBergman, aLavine, jRobertson, sRobertson, 
  wlBeason, tlKohutek, jmBracci, qhQian, dyZhu, cfLee, grChen, dgFredlund,
  jKrahn, dStolle, yCLi, ymChen, tltZhan, ssLing, pjCleall, pGuo,
  mCampidelli,   dmWiess, sPalmer, scottSmith, bKarchewski, rHuston, 
  hJosephs, nrMorgenstern, vePrice, samCrawford, rcHibbeler, olu, rodPierce :: Person
  
pjAgerfalk    = person    "PJ"                        "Agerfalk"
daAruliah     = personWM  "D"         ["A"]           "Aruliah"
wlBeason      = personWM  "W"         ["Lynn"]        "Beason"
tBergman      = personWM  "T"         ["L"]           "Bergman"
jmBracci      = personWM  "Joseph"    ["M"]           "Bracci"
jBueche       = personWM  "J"         ["Frederick"]   "Bueche"
mCampidelli   = person    "Manuel"                    "Campidelli"
grChen        = personWM' "G"         ["R"]           "Chen"
ymChen        = person'   "Yun-Min"                   "Chen"
pjCleall      = personWM  "Peter"     ["John"]        "Cleall"
pcClements    = personWM  "P"         ["C"]           "Clements" -- The Modular Structure of Complex Systems ICSE '84
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
nKoothoor     = person    "Nirmitha"                  "Koothoor"
nKraiem       = person    "N"                         "Kraiem"
jKrahn        = person    "J"                         "Krahn"
nikitha       = person    "Nikitha"                   "Krithnan"
lLai          = person    "Lei"                       "Lai"
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
dParnas       = personWM  "David"     ["L"]           "Parnas"
mdPlumblet    = personWM  "Mark"      ["D"]           "Plumblet"
vePrice       = personWM  "P"         ["E"]           "Price"
qhQian        = personWM' "Q"         ["H"]           "Qian"
jRalyte       = person    "J"                         "Ralyte"
  --FIXME: person takes strings but we need an "e" with an accent
  -- S "J. Ralyt" :+: (F Acute 'e')
jRobertson    = person    "James"                     "Robertson"
sRobertson    = person    "Suzanne"                   "Robertson"
scottSmith    = person    "Scott"                     "Smith"
spencerSmith  = personWM  "W"         ["Spencer"]     "Smith"
dStolle       = person    "Dieter"                    "Stolle"
cTitus        = person    "C"                         "Titus"
bWaugh        = person    "Ben"                       "Waugh" -- Best Practices for Scientific Computing 2013
epWhite       = personWM  "Ethan"     ["P"]           "White" -- Best Practices for Scientific Computing 2013
gWilson       = person    "Greg"                      "Wilson"
pWilson       = person    "Paul"                      "Wilson" -- Best Practices for Scientific Computing 2013
tltZhan       = personWM  "Tony"      ["L","T"]       "Zhan"
dyZhu         = personWM' "D"         ["Y"]           "Zhu"
dmWiess       = personWM  ""          []              "Wiess"
olu           = person    "Olu"                       "Owojaiye"
rodPierce     = person    "Rod"                       "Pierce"