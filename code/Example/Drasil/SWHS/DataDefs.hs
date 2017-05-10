module Drasil.SWHS.DataDefs where

import Drasil.SWHS.Unitals

import Language.Drasil
import Data.Drasil.SI_Units (specificE)
import Data.Drasil.Units.Thermodynamics (thermal_flux)

import Control.Lens ((^.))

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

dd1HtFluxC :: QDefinition
dd1HtFluxC = fromEqn "dd1HtFluxC" (ht_flux_C ^. term) (ht_flux_C ^. symbol) 
  thermal_flux htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (C coil_HTC) * ((C temp_C) - FCall (C temp_W) [C time])

--Can't include info in description beyond definition of variables?

dd2HtFluxP :: QDefinition
dd2HtFluxP = fromEqn "dd2HtFluxP" (ht_flux_P ^. term) (ht_flux_P ^. symbol) 
  thermal_flux htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (C pcm_HTC) * (FCall (C temp_W) [C time] - 
             FCall (C temp_PCM) [C time])

dd3HtFusion :: QDefinition
dd3HtFusion = fromEqn "dd3HtFusion" (nounPhraseSP 
  "amount of thermal energy required to completely melt a unit mass of a substance.")
  --FIXME: Should be (htFusion ^. defn)?
  (htFusion ^. symbol) specificE htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (C latentE) / (C mass)

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
