module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Data.Drasil.Concepts.Thermodynamics as CT (boil_pt, heat_cap_spec,
    ht_flux, latent_heat, melt_pt, sens_heat, temp)
import Data.Drasil.SI_Units (centigrade, joule)
import Data.Drasil.Units.Thermodynamics as UT (heat_cap_spec, thermal_flux)

boil_pt, temp, heat_cap_spec, ht_flux, latent_heat, melt_pt, sens_heat :: UnitalChunk

boil_pt       = uc CT.boil_pt (sub cT (Atomic "boil")) centigrade
temp          = uc CT.temp cT centigrade
heat_cap_spec = uc CT.heat_cap_spec cC UT.heat_cap_spec
ht_flux       = uc CT.ht_flux lQ UT.thermal_flux
latent_heat   = uc CT.latent_heat cQ joule
melt_pt       = uc CT.melt_pt (sub cT (Atomic "melt")) centigrade
sens_heat     = uc CT.sens_heat cE joule