module Drasil.SWHS.DataDefs where --exports all of it

import Prelude hiding (id)

import Language.Drasil
import Control.Lens ((^.))

import Drasil.SWHS.Unitals (melt_frac, latentE_P, htFusion, pcm_mass,
  temp_W, temp_PCM, ht_flux_P, pcm_HTC, coil_HTC, temp_C, ht_flux_C)

import Data.Drasil.Concepts.Documentation (acroNumGen)

import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latent_heat)
import Data.Drasil.Utils (symbolMapFun, mkDataDef)

swhsDataDefs :: [QDefinition]
swhsDataDefs = [dd1HtFluxC, dd2HtFluxP, dd3HtFusion, dd4MeltFrac]

-- SYMBOL MAP HELPERS --
swhsSymbMapD :: QDefinition -> Contents
swhsSymbMapD = symbolMapFun Data

swhsSymbMapT :: RelationConcept -> Contents
swhsSymbMapT = symbolMapFun Theory

swhsSymbMapDRef :: QDefinition -> Sentence
swhsSymbMapDRef = makeRef . swhsSymbMapD

swhsSymbMapTRef :: RelationConcept -> Sentence
swhsSymbMapTRef = makeRef . swhsSymbMapT

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

dd1HtFluxC :: QDefinition
dd1HtFluxC = mkDataDef ht_flux_C htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (C coil_HTC) * ((C temp_C) - FCall (C temp_W) [C time])

--Can't include info in description beyond definition of variables?

dd2HtFluxP :: QDefinition
dd2HtFluxP = mkDataDef ht_flux_P htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (C pcm_HTC) * (FCall (C temp_W) [C time] -
             FCall (C temp_PCM) [C time])

dd3HtFusion :: QDefinition
dd3HtFusion = mkDataDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (C latent_heat) / (C mass)

dd4MeltFrac :: QDefinition
dd4MeltFrac = fromEqn' (melt_frac ^. id) -- FIXME Should (^. id) be used
  (melt_frac ^. term) (S "fraction of the PCM that is liquid")
  (eqSymb melt_frac) melt_frac_eqn
--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

melt_frac_eqn :: Expr
melt_frac_eqn = (C latentE_P) / ((C htFusion) * (C pcm_mass))

--Need to add units to data definition descriptions
--s4_2_4_
data_def_swhsDataDefs :: [Contents]
data_def_swhsDataDefs = acroNumGen (data_def_DD1 ++ data_def_DD2 ++
  data_def_DD3 ++ data_def_DD4) 1

data_def_DD1, data_def_DD2, data_def_DD3, data_def_DD4 :: [Contents]
data_def_DD1 = map swhsSymbMapD [dd1HtFluxC]
data_def_DD2 = map swhsSymbMapD [dd2HtFluxP]
data_def_DD3 = map swhsSymbMapD [dd3HtFusion]
data_def_DD4 = map swhsSymbMapD [dd4MeltFrac]

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
