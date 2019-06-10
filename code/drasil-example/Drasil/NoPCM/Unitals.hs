module Drasil.NoPCM.Unitals where

import Language.Drasil

import Data.Drasil.SI_Units (centigrade)
import Data.Drasil.Quantities.Thermodynamics (temp)

tempInit :: UncertQ
tempInit = uqc "tempInit" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation"
  (sub (eqSymb temp)(Atomic "init")) centigrade Real
  [physc $ Bounded (Exc,0) (Exc,100)] (dbl 40) defaultUncrt
