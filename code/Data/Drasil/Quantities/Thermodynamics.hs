module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Data.Drasil.Concepts.Thermodynamics as CT
import Data.Drasil.Units.Thermodynamics as UT
import Data.Drasil.SI_Units

temp, heat_cap_spec, ht_flux :: UnitalChunk

temp          = uc CT.temp cT centigrade
heat_cap_spec = uc CT.heat_cap_spec cC (UT.heat_cap_spec)
ht_flux       = uc CT.ht_flux lQ U.heat_transfer_coef