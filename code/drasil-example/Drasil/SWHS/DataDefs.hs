module Drasil.SWHS.DataDefs where --exports all of it

import Language.Drasil
import Control.Lens ((^.))
import Drasil.DocLang (ModelDB, mdb, refDD, ddRefDB)

import Drasil.SWHS.Unitals (melt_frac, latentE_P, htFusion, pcm_mass,
  temp_W, temp_PCM, ht_flux_P, pcm_HTC, coil_HTC, temp_C, ht_flux_C)

import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latent_heat)

ddRef :: QDefinition -> Sentence
ddRef = refDD (ddRefDB swhsRefMDB)

swhsRefMDB :: ModelDB
swhsRefMDB = mdb [] [] swhsDataDefs []

swhsDataDefs :: [QDefinition]
swhsDataDefs = [dd1HtFluxC, dd2HtFluxP, dd3HtFusion, dd4MeltFrac]

dataDefns :: [DataDefinition] 
dataDefns = [dd1HtFluxCDD, dd2HtFluxPDD, dd3HtFusionDD, dd4MeltFracDD]

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

dd1HtFluxC :: QDefinition
dd1HtFluxC = mkDataDef ht_flux_C htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (sy coil_HTC) * ((sy temp_C) - apply1 temp_W time)

dd1HtFluxCDD :: DataDefinition
dd1HtFluxCDD = mkDD dd1HtFluxC [] [] "" Nothing

--Can't include info in description beyond definition of variables?
----

dd2HtFluxP :: QDefinition
dd2HtFluxP = mkDataDef ht_flux_P htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (sy pcm_HTC) * (apply1 temp_W time - apply1 temp_PCM time)

dd2HtFluxPDD :: DataDefinition
dd2HtFluxPDD = mkDD dd2HtFluxP [] [] "" Nothing

----

dd3HtFusion :: QDefinition
dd3HtFusion = mkDataDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (sy latent_heat) / (sy mass)

dd3HtFusionDD :: DataDefinition
dd3HtFusionDD = mkDD dd3HtFusion [] [] "" Nothing

----

dd4MeltFrac :: QDefinition
dd4MeltFrac = fromEqn' (melt_frac ^. uid) -- FIXME Should (^. id) be used
  (melt_frac ^. term) (S "fraction of the PCM that is liquid")
  (eqSymb melt_frac) melt_frac_eqn [] "meltFrac"
--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

melt_frac_eqn :: Expr
melt_frac_eqn = (sy latentE_P) / ((sy htFusion) * (sy pcm_mass))

dd4MeltFracDD :: DataDefinition
dd4MeltFracDD = mkDD dd4MeltFrac [] [] "" Nothing

--Need to add units to data definition descriptions

--Fixme: should be removed with proper addition of labels
swhsDD1, swhsDD2, swhsDD3, swhsDD4 :: Contents
swhsDD1 = LlC $ datadefn dd1HtFluxC
swhsDD2 = LlC $ datadefn dd2HtFluxP
swhsDD3 = LlC $ datadefn dd3HtFusion
swhsDD4 = LlC $ datadefn dd4MeltFrac

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
