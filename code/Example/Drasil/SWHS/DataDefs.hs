module Drasil.SWHS.DataDefs where

import Data.Char (toLower)

import Drasil.SWHS.Units
import Drasil.SWHS.Unitals

import Language.Drasil
import Data.Drasil.Units.Thermodynamics (thermal_flux)

import Data.Drasil.Concepts.Thermodynamics

import Control.Lens ((^.))

dd1HtFluxC :: QDefinition
dd1HtFluxC = fromEqn "q_C" dd1descr (ht_flux_C ^. symbol) thermal_flux htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (C coil_HTC) * ((C temp_C) - FCall (C temp_W) [C time])

--Function calls for the left side of an QDefinition?

dd1descr :: Sentence
dd1descr = (ht_flux_C ^. term)

--Can't include info in description beyond definition of variables?

dd2HtFluxP :: QDefinition
dd2HtFluxP = fromEqn "q_P" dd2descr (ht_flux_P ^. symbol) thermal_flux htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (C pcm_HTC) * (FCall (C temp_W) [C time] - 
             FCall (C temp_PCM) [C time])

dd2descr :: Sentence
dd2descr = (ht_flux_P ^. term)

dd3HtFusion :: QDefinition
dd3HtFusion = fromEqn "H_f" dd3descr (htFusion ^. symbol) specificE htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (C latentE) / (C mass)

dd3descr :: Sentence
dd3descr = (S "amount of " :+: (sMap (map toLower) (thermal_energy ^. term))
           :+: S " required to completely melt a unit " :+: (mass ^. term) :+:
           S " of a substance.")

-- dd4MeltFrac :: QDefinition
--dd4MeltFrac = fromEqn "melt_fraction" dd4descr (melt_frac ^. symbol) unitless
              --melt_frac_eqn

-- melt_frac_eqn :: Expr
-- melt_frac_eqn = (C latentE_P) / ((C htFusion) * (C pcm_mass))

-- dd4descr :: Sentence
-- dd4descr = (S "fraction of the " :+: S (phsChgMtrl ^. name) :+: 
           -- S " that is " :+: (sMap (map toLower) (S (liquid ^. name))) :+: 
           -- S ".")

--Need to add units to data definition descriptions

s4_2_4_DD1, s4_2_4_DD2, s4_2_4_DD3 :: Contents
s4_2_4_DD1 = Definition (Data dd1HtFluxC)
s4_2_4_DD2 = Definition (Data dd2HtFluxP)
s4_2_4_DD3 = Definition (Data dd3HtFusion)
--s4_2_4_DD4 = Definition (Data dd4MeltFrac)

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
