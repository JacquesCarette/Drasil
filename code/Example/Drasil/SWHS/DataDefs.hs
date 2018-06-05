module Drasil.SWHS.DataDefs where --exports all of it

import Language.Drasil
import Control.Lens ((^.))
import Drasil.DocumentLanguage.RefHelpers

import Drasil.SWHS.Unitals (melt_frac, latentE_P, htFusion, pcm_mass,
  temp_W, temp_PCM, ht_flux_P, pcm_HTC, coil_HTC, temp_C, ht_flux_C)

import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latent_heat)
import Data.Drasil.Utils (mkDataDef)

ddRef = refDD (ddRefDB swhsRefMDB)

swhsRefMDB :: ModelDB
swhsRefMDB = mdb [] [] swhsDataDefs []

swhsDataDefs :: [QDefinition]
swhsDataDefs = [dd1HtFluxC, dd2HtFluxP, dd3HtFusion, dd4MeltFrac]

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

dd1HtFluxC :: QDefinition
dd1HtFluxC = mkDataDef ht_flux_C htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (sy coil_HTC) * ((sy temp_C) - apply1 temp_W time)

--Can't include info in description beyond definition of variables?

dd2HtFluxP :: QDefinition
dd2HtFluxP = mkDataDef ht_flux_P htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (sy pcm_HTC) * (apply1 temp_W time - apply1 temp_PCM time)

dd3HtFusion :: QDefinition
dd3HtFusion = mkDataDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (sy latent_heat) / (sy mass)

dd4MeltFrac :: QDefinition
dd4MeltFrac = fromEqn' (melt_frac ^. uid) -- FIXME Should (^. id) be used
  (melt_frac ^. term) (S "fraction of the PCM that is liquid")
  (eqSymb melt_frac) melt_frac_eqn []
--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

melt_frac_eqn :: Expr
melt_frac_eqn = (sy latentE_P) / ((sy htFusion) * (sy pcm_mass))

--Need to add units to data definition descriptions

s4_2_4_swhsDataDefs :: [Contents]
s4_2_4_swhsDataDefs = [s4_2_4_DD1, s4_2_4_DD2, s4_2_4_DD3, s4_2_4_DD4] 

s4_2_4_DD1, s4_2_4_DD2, s4_2_4_DD3, s4_2_4_DD4 :: Contents
s4_2_4_DD1 = datadefn dd1HtFluxC
s4_2_4_DD2 = datadefn dd2HtFluxP
s4_2_4_DD3 = datadefn dd3HtFusion
s4_2_4_DD4 = datadefn dd4MeltFrac

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
