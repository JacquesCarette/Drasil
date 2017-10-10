module Drasil.NoPCM.Unitals where

import Language.Drasil

import Data.Drasil.SI_Units
import Data.Drasil.Quantities.Thermodynamics

temp_init :: UncertQ
temp_init = uqc "temp_init" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation"
  (sub (symbol temp)(Atomic "init")) centigrade Real
  [physc $ \c -> 0 :< c :< 100] (Dbl 40) 0.1