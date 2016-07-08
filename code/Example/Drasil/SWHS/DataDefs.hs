module Example.Drasil.SWHS.DataDefs where

import Data.Char (toLower)

import Example.Drasil.SWHS.Units
import Example.Drasil.SWHS.Unitals
import Example.Drasil.SWHS.Concepts

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

dd1HtFluxC :: EqChunk
dd1HtFluxC = fromEqn "q_C" dd1descr (ht_flux_C ^. symbol) thermFluxU htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (C coil_HTC) * ((C temp_C) - FCall (C temp_W) [C time])

--Function calls for the left side of an EqChunk?

dd1descr :: Sentence
dd1descr = (ht_flux_C ^. descr)

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
dd2HtFluxP = fromEqn "q_P" dd2descr (ht_flux_P ^. symbol) thermFluxU htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (C pcm_HTC) * (FCall (C temp_W) [C time] - FCall (C temp_PCM) [C time])

dd2descr :: Sentence
dd2descr = (ht_flux_P ^. descr)

dd3HtFusion :: EqChunk
dd3HtFusion = fromEqn "H_f" dd3descr (htFusion ^. symbol) specificE htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (C latentE) / (C mass)

dd3descr :: Sentence
dd3descr = (S "amount of " :+: (sMap (map toLower) (S (thermal_energy ^. name))) :+:
           S " required to completely melt a unit " :+:
           (mass ^. descr) :+: S " of a substance.")

dd4MeltFrac :: EqChunk
dd4MeltFrac = fromEqn "melt_fraction" dd4descr (melt_frac ^. symbol) unitless melt_frac_eqn

melt_frac_eqn :: Expr
melt_frac_eqn = (C latentE_P) / ((C htFusion) * (C pcm_mass))

dd4descr :: Sentence
dd4descr = (S "fraction of the " :+: S (phsChgMtrl ^. name) :+: S " that is " :+:
           (sMap (map toLower) (S (liquid ^. name))) :+: S ".")

--Need to add units to data definition descriptions

s4_2_4_DD1, s4_2_4_DD2, s4_2_4_DD3, s4_2_4_DD4 :: Contents
s4_2_4_DD1 = Definition (Data dd1HtFluxC)
s4_2_4_DD2 = Definition (Data dd2HtFluxP)
s4_2_4_DD3 = Definition (Data dd3HtFusion)
s4_2_4_DD4 = Definition (Data dd4MeltFrac)

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear