module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Language.Drasil.ShortHands (cT, cC, lQ, cQ, cE)

import Data.Drasil.Concepts.Thermodynamics as CT (boilPt, heatCapSpec,
    ht_flux, latent_heat, melt_pt, sens_heat, temp)
import Data.Drasil.SI_Units (centigrade, joule)
import Data.Drasil.Units.Thermodynamics as UT (heatCapSpec, thermal_flux)

boilPt, temp, heatCapSpec, ht_flux, latent_heat, melt_pt, sens_heat :: UnitalChunk

boilPt        = uc CT.boilPt (sub cT (Atomic "boil")) centigrade
temp          = uc CT.temp cT centigrade
heatCapSpec = uc CT.heatCapSpec cC UT.heatCapSpec
ht_flux       = uc CT.ht_flux lQ UT.thermal_flux
latent_heat   = uc CT.latent_heat cQ joule
melt_pt       = uc CT.melt_pt (sub cT (Atomic "melt")) centigrade
sens_heat     = uc CT.sens_heat cE joule
