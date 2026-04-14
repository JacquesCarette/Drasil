-- | Define people for use internally to Drasil. Used in authors and citations.
module Drasil.Metadata.People where

import Language.Drasil (Person, person, personWM)

spencerSmith, nKoothoor, dParnas, pjAgerfalk, pcClements, lLai, nKraiem,
  jRalyte, rKhedri :: Person

pjAgerfalk    = person    "PJ"                        "Agerfalk"
pcClements    = personWM  "P"         ["C"]           "Clements" -- The Modular Structure of Complex Systems ICSE '84
nKoothoor     = person    "Nirmitha"                  "Koothoor"
nKraiem       = person    "N"                         "Kraiem"
lLai          = person    "Lei"                       "Lai"
dParnas       = personWM  "David"     ["L"]           "Parnas"
jRalyte       = person    "J"                         "Ralyte"
spencerSmith  = personWM  "W"         ["Spencer"]     "Smith"
rKhedri       = person    "Ridha"                     "Khedri"

