module Drasil.NoPCM.Unitals where

import Language.Drasil

import Data.Drasil.SI_Units (centigrade)
import Data.Drasil.Quantities.Thermodynamics (temp)
import Drasil.SWHS.Unitals (coil_HTC, coil_SA, diam, htCap_W, tank_length, 
  temp_C, time_final, timeStep, w_density, abs_tol, rel_tol, cons_tol)

inputs :: [QuantityDict]
inputs = map qw constrained ++ map qw [abs_tol, rel_tol, cons_tol]

constrained :: [UncertQ]
constrained =  [coil_SA, htCap_W, coil_HTC, temp_init,
  time_final, tank_length, temp_C, timeStep, w_density, diam]
  -- w_E, temp_W

temp_init :: UncertQ
temp_init = uqc "temp_init" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation"
  (sub (eqSymb temp)(Atomic "init")) centigrade Real
  [physc $ Bounded (Exc,0) (Exc,100)] (dbl 40) defaultUncrt
