module Drasil.NoPCM.Unitals where

import Language.Drasil

import Data.Drasil.SI_Units (centigrade)
import Data.Drasil.Quantities.Thermodynamics (temp)

import Drasil.SWHS.Unitals (tank_length_min, tank_length_max,
  w_density_min, w_density_max, coil_SA_max, htCap_W_min, htCap_W_max, 
  coil_HTC_min, coil_HTC_max, time_final_max)

temp_init :: UncertQ
temp_init = uqc "temp_init" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation"
  (sub (eqSymb temp)(Atomic "init")) centigrade Real
  [physc $ Bounded (Exc,0) (Exc,100)] (dbl 40) defaultUncrt

specParamValList :: [QDefinition]
specParamValList = [tank_length_min, tank_length_max,
  w_density_min, w_density_max, coil_SA_max, htCap_W_min, htCap_W_max, 
  coil_HTC_min, coil_HTC_max, time_final_max]
