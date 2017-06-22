module Drasil.SWHS.DataDefs where

import Drasil.SWHS.Unitals

import Language.Drasil
import Data.Drasil.SI_Units (specificE)
import Data.Drasil.Units.Thermodynamics (thermal_flux)
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latent_heat)
import Data.Drasil.Utils (symbolMapFun)
import Prelude hiding (id)

import Control.Lens ((^.))

dataDefns :: [QDefinition]
dataDefns = [dd1HtFluxC] ++ [dd2HtFluxP] ++ [dd3HtFusion] ++ [dd4MeltFrac]

-- SYMBOL MAP HELPERS --
swhsSymMap :: SymbolMap
swhsSymMap = symbolMap swhsSymbols

swhsSymbMapD :: QDefinition -> Contents
swhsSymbMapD = symbolMapFun swhsSymMap Data

swhsSymbMapT :: RelationConcept -> Contents
swhsSymbMapT = symbolMapFun swhsSymMap Theory

swhsSymbMapDRef :: QDefinition -> Sentence
swhsSymbMapDRef = makeRef . swhsSymbMapD

swhsSymbMapTRef :: RelationConcept -> Sentence
swhsSymbMapTRef = makeRef . swhsSymbMapT

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

dd1HtFluxC :: QDefinition
dd1HtFluxC = fromEqn (ht_flux_C ^. id) (ht_flux_C ^. term) (ht_flux_C ^. symbol)
  thermal_flux htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (C coil_HTC) * ((C temp_C) - FCall (C temp_W) [C time])

--Can't include info in description beyond definition of variables?

dd2HtFluxP :: QDefinition
dd2HtFluxP = fromEqn (ht_flux_P ^. id) (ht_flux_P ^. term) (ht_flux_P ^. symbol)
  thermal_flux htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (C pcm_HTC) * (FCall (C temp_W) [C time] -
             FCall (C temp_PCM) [C time])

dd3HtFusion :: QDefinition
dd3HtFusion = fromEqn (htFusion ^. id) (htFusion ^. term) (htFusion ^. symbol)
  specificE htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (C latent_heat) / (C mass)

dd4MeltFrac :: QDefinition
dd4MeltFrac = fromEqn' (melt_frac ^. id) (nounPhraseSP "fraction of the PCM that is liquid")
  (melt_frac ^. symbol) melt_frac_eqn
--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

melt_frac_eqn :: Expr
melt_frac_eqn = (C latentE_P) / ((C htFusion) * (C pcm_mass))

--Need to add units to data definition descriptions

s4_2_4_DD1, s4_2_4_DD2, s4_2_4_DD3, s4_2_4_DD4 :: [Contents]
s4_2_4_DD1 = map swhsSymbMapD [dd1HtFluxC]
s4_2_4_DD2 = map swhsSymbMapD [dd2HtFluxP]
s4_2_4_DD3 = map swhsSymbMapD [dd3HtFusion]
s4_2_4_DD4 = map swhsSymbMapD [dd4MeltFrac]

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear