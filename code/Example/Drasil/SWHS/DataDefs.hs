module Example.Drasil.SWHS.DataDefs where

import Example.Drasil.SWHS.Units
import Example.Drasil.SWHS.Unitals

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

dd1HtFluxC :: EqChunk
dd1HtFluxC = fromEqn "Heat flux out of coil" dd1descr (ht_flux_C ^. symbol) thermFluxU htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (C coil_HTC) * ((C temp_C) - FCall (C temp_W) [C time])

--Function calls for the left side of an EqChunk?

dd1descr :: Sentence
dd1descr = (S "heat flux out of the coil.")

-- dd1descr :: Sentence
-- dd1descr = (U (temp_C ^. symbol) :+: S " is the temperature of the coil. " :+:
           -- U (temp_W ^. symbol) :+: S " is the temperature of the water. " :+:
           -- S "The heat flux out of the coil, " :+: U (ht_flux_C ^. symbol) :+:
           -- S ", is found by assuming that Newton's Law of Cooling applies" :+:
           -- S " (A7). This law (GD1) is used on the surface of the coil, " :+:
           -- S "which has area " :+: U (coil_SA ^. symbol) :+: S " and heat" :+:
           -- S " transfer coefficient " :+: U (coil_HTC ^. symbol) :+: S "." :+:
           -- S " This equation assumes that the temperature of the coil is " :+:
           -- S "constant over time (A8) and that it does not vary along the " :+:
           -- S "length of the coil (A9).")

--Can't include info in description beyond definition of variables?

dd2HtFluxP :: EqChunk
dd2HtFluxP = fromEqn "Heat flux into PCM" dd2descr (ht_flux_P ^. symbol) thermFluxU htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (C pcm_HTC) * (FCall (C temp_W) [C time] - FCall (C temp_PCM) [C time])

dd2descr :: Sentence
dd2descr = (S "heat flux into the PCM.")

dd3HtFusion :: EqChunk
dd3HtFusion = fromEqn "Specific latent heat of fusion" dd3descr (htFusion ^. symbol) specificE htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (C latentE) / (C mass)

dd3descr :: Sentence
dd3descr = (S "amount of heat energy required to completely melt a unit " :+:
           S "mass of a substance.")

dd4MeltFrac :: EqChunk
dd4MeltFrac = fromEqn "Melt fraction" dd4descr (melt_frac ^. symbol) unitless melt_frac_eqn

melt_frac_eqn :: Expr
melt_frac_eqn = (C latentE_P) / ((C htFusion) * (C pcm_mass))

dd4descr :: Sentence
dd4descr = (S "fraction of the PCM that is liquid.")

--Need to add units to data definition descriptions