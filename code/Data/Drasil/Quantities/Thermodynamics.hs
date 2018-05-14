module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Data.Drasil.Concepts.Thermodynamics as CT
import Data.Drasil.Units.Thermodynamics as UT
import Data.Drasil.SI_Units

boil_pt, temp, heat_cap_spec, ht_flux, latent_heat, melt_pt, sens_heat :: UnitalChunk

boil_pt       = ucEL CT.boil_pt (sub cT (Atomic "boil")) centigrade
temp          = ucEL CT.temp cT centigrade
heat_cap_spec = ucEL CT.heat_cap_spec cC (UT.heat_cap_spec)
ht_flux       = ucEL CT.ht_flux lQ UT.thermal_flux
latent_heat   = ucEL CT.latent_heat cQ joule
melt_pt       = ucEL CT.melt_pt (sub cT (Atomic "melt")) centigrade
sens_heat     = ucEL CT.sens_heat cE joule