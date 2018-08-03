module Drasil.SWHS.Assumptions where --all of this file is exported

import Drasil.SWHS.Labels (assump1Label)
import Language.Drasil
import Control.Lens ((^.))
import Drasil.SWHS.References (ref_swhs_citations)

import Data.Drasil.Concepts.Documentation (system, simulation, model, 
  problem)

import Drasil.SWHS.DataDefs (dd1HtFluxC, dd2HtFluxP)
import Drasil.SWHS.Concepts (coil, tank, phsChgMtrl, water, perfect_insul,
  charging, discharging)
import Drasil.SWHS.Unitals (w_vol, vol_ht_gen, temp_C, temp_init, temp_W,
  temp_PCM, htCap_L_P, htCap_W, htCap_S_P, w_density, pcm_density, pcm_vol)
import Drasil.SWHS.TMods (t1ConsThermE_new)
import Drasil.SWHS.Labels (assump1Label, assump14Label, assump18Label, 
  assump19Label, assump10Label)

import Data.Drasil.Quantities.PhysicalProperties (vol)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Quantities.Thermodynamics (temp, boil_pt, melt_pt)

import Data.Drasil.Concepts.Thermodynamics as CT (heat, melting,
  law_conv_cooling, heat_trans, thermal_energy)
import Data.Drasil.Concepts.PhysicalProperties (solid, liquid, gaseous)
import Data.Drasil.Concepts.Math (change)
import Data.Drasil.Concepts.Physics (mech_energy)

import Data.Drasil.SentenceStructures (acroGD, acroIM, foldlSent, ofThe,
  ofThe', sAnd, isThe)

-------------------------
-- 4.2.1 : Assumptions --
-------------------------
swhsRefDB :: ReferenceDB
swhsRefDB = rdb [] [] newAssumptions [] [] ref_swhs_citations []

newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10,
  newA11, newA12, newA13, newA14, newA15, newA16, newA17, newA18, newA19, newA20]

newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10,
  newA11, newA12, newA13, newA14, newA15, newA16, newA17, newA18, newA19, newA20 :: AssumpChunk

newA1 = assump "Thermal-Energy-Only" assumpS1 assump1Label
newA2 = assump "Heat-Transfer-Coeffs-Constant" assumpS2 (mkLabelRA'' "Heat-Transfer-Coeffs-Constant"  )
newA3 = assump "Constant-Water-Temp-Across-Tank" assumpS3 (mkLabelRA'' "Constant-Water-Temp-Across-Tank" )
newA4 = assump "Temp-PCM-Constant-Across-Volume" assumpS4 (mkLabelRA'' "Temp-PCM-Constant-Across-Volume") 
newA5 = assump "Density-Water-PCM-Constant-over-Volume" assumpS5 (mkLabelRA'' "Density-Water-PCM-Constant-over-Volume"  )
newA6 = assump "Specific-Heat-Energy-Constant-over-Volume" assumpS6 (mkLabelRA'' "Specific-Heat-Energy-Constant-over-Volume" )
newA7 = assump "Law-Convective-Cooling-Coil-Water" assumpS7 (mkLabelRA'' "Newton-Law-Convective-Cooling-Coil-Water" )
newA8 = assump "Temp-Heating-Coil-Constant-over-Time" assumpS8 (mkLabelRA'' "Temp-Heating-Coil-Constant-over-Time" )
newA9 = assump "Temp-Heating-Coil-Constant-over-Length" assumpS9 (mkLabelRA'' "Temp-Heating-Coil-Constant-over-Length" )
newA10 = assump "Law-Convective-Cooling-Water-PCM" assumpS10 assump10Label
newA11 = assump "Charging-Tank-No-Temp-Discharge" assumpS11 (mkLabelRA'' "Charging-Tank-No-Temp-Discharge" )
newA12 = assump "Same-Initial-Temp-Water-PCM" assumpS12 (mkLabelRA'' "Same-Initial-Temp-Water-PCM" )
newA13 = assump "PCM-Initially-Solid" assumpS13 (mkLabelRA'' "PCM-Initially-Solid") 
newA14 = assump "Water-Always-Liquid" assumpS14 assump14Label
newA15 = assump "Perfect-Insulation-Tank" assumpS15 (mkLabelRA'' "Perfect-Insulation-Tank" )
newA16 = assump "No-Internal-Heat-Generation-By-Water-PCM" assumpS16 (mkLabelRA'' "No-Internal-Heat-Generation-By-Water-PCM" )
newA17 = assump "Volume-Change-Melting-PCM-Negligible" assumpS17 (mkLabelRA'' "Volume-Change-Melting-PCM-Negligible" )
newA18 = assump "No-Gaseous-State-PCM" assumpS18 assump18Label
newA19 = assump "Atmospheric-Pressure-Tank" assumpS19 assump19Label
newA20 = assump "Volume-Coil-Negligible" assumpS20 (mkLabelRA'' "Volume-Coil-Negligible" )


swhsAssumptionsS:: [Sentence]
swhsAssumptionsS = [assumpS1, assumpS2, assumpS3, assumpS4, assumpS5,
  assumpS6, assumpS7, assumpS8, assumpS9, assumpS10, assumpS11, assumpS12, assumpS13, assumpS14,
  assumpS15, assumpS16, assumpS17, assumpS18, assumpS19, assumpS20]

assumpS1, assumpS2, assumpS3, assumpS4, assumpS5, assumpS6, assumpS7,
  assumpS8, assumpS9, assumpS10, assumpS11, assumpS12, assumpS13, assumpS14,
  assumpS15, assumpS16, assumpS17, assumpS18, assumpS19, assumpS20 :: Sentence

assumpS1 = foldlSent [
  S "The only form of", phrase energy, S "that is",
  S "relevant for this", phrase problem, S "is" +:+. 
  phrase CT.thermal_energy, S "All other forms of", phrase energy `sC`
  S "such as", phrase mech_energy `sC` S "are assumed to be negligible",
  sSqBr $ makeRef t1ConsThermE_new]
assumpS2 = foldlSent [
  S "All", phrase CT.heat_trans, S "coefficients are constant over",
  phrase time, sSqBr $ acroGD 1]
assumpS3 = foldlSent [
  S "The", phrase water, S "in the", phrase tank,
  S "is fully mixed, so the", phrase temp_W `isThe` 
  S "same throughout the entire", phrase tank,
  sSqBr $ acroGD 2 `sC` (makeRef dd2HtFluxP)]
assumpS4 = foldlSent [
  S "The", phrase temp_PCM `isThe` S "same throughout the", phrase pcm_vol,
  sSqBr $ acroGD 2 `sC` (makeRef dd2HtFluxP)]
  --FIXME `sC` makeRef likeChg1]
assumpS5 = foldlSent [
  S "The", phrase w_density `sAnd` phrase pcm_density,
  S "have no spatial variation; that is" `sC`
  S "they are each constant over their entire", phrase vol,
  sSqBr $ acroGD 2]
assumpS6 = foldlSent [
  S "The", phrase htCap_W `sC` phrase htCap_S_P `sC` S "and",
  phrase htCap_L_P, S "have no spatial variation; that",
  S "is" `sC` S "they are each constant over their entire",
  phrase vol, sSqBr $ acroGD 2]
assumpS7 = foldlSent [
  CT.law_conv_cooling ^. defn, S "applies between the",
  phrase coil `sAnd` S "the", phrase water,
  sSqBr $ makeRef dd1HtFluxC]
assumpS8 = foldlSent [
  S "The", phrase temp_C, S "is constant over", phrase time,
  sSqBr $ makeRef dd1HtFluxC]
  --FIXME `sC` makeRef likeChg2]
assumpS9 = foldlSent [
  S "The", phrase temp_C, S "does not vary along its length",
  sSqBr $ makeRef dd1HtFluxC]
  --FIXME `sC` makeRef likeChg3]
assumpS10 = foldlSent [
  CT.law_conv_cooling ^. defn, S "applies between the",
  phrase water `sAnd` S "the", short phsChgMtrl,
  sSqBr $ makeRef dd2HtFluxP]
assumpS11 = foldlSent [
  S "The", phrase model, S "only accounts for", (charging ^. defn) `sC`
  S "not" +:+. phrase discharging, S "The", phrase temp_W `sAnd`
  phrase temp_PCM, S "can only increase, or remain",
  S "constant; they do not decrease. This implies that the",
  phrase temp_init, sSqBr $ makeRef newA12, S "is less than (or equal)",
  S "to the", phrase temp_C, sSqBr $ acroIM 1]
  --FIXME `sC` makeRef likeChg4]
assumpS12 = foldlSent [
  phrase temp_init `ofThe'` phrase water `sAnd` S "the",
  short phsChgMtrl `isThe` S "same",
  sSqBr $ acroIM 1 `sC` acroIM 2]
  --FIXME `sC` makeRef likeChg5]
assumpS13 = foldlSent [
  S "The", phrase simulation, S "will start with the",
  short phsChgMtrl, S "in a", solid ^. defn,
  sSqBr $ acroIM 2 `sC` acroIM 4]
assumpS14 = foldlSent [
  (S "operating" +:+ phrase temp +:+ S "range") `ofThe'` phrase system,
  S "is such that the", phrase water,
  S "is always in" +:+. (liquid ^. defn), S "That is" `sC`
  S "the", phrase temp, S "will not drop below the",
  phrase melt_pt, S "of", phrase water `sC` S "or rise above its",
  phrase boil_pt, sSqBr $ acroIM 1 `sC` acroIM 3]
assumpS15 = foldlSent [
  S "The", phrase tank, S "is", phrase perfect_insul,
  S "so that there is no", phrase CT.heat, S "loss from the",
  phrase tank, sSqBr $ acroIM 1]
  --FIXME `sC` makeRef likeChg6]
assumpS16 = foldlSent [
  S "No internal", phrase CT.heat, S "is generated by either the",
  phrase water, S "or the", short phsChgMtrl `semiCol`
  S "therefore, the", phrase vol_ht_gen, S "is zero",
  sSqBr $ acroIM 1 `sC` acroIM 2]
assumpS17 = foldlSent [
  (phrase vol +:+ phrase change) `ofThe'` short phsChgMtrl,
  S "due to", phrase CT.melting, S "is negligible", sSqBr $ acroIM 2]
assumpS18 = foldlSent [
  S "The", short phsChgMtrl, S "is either in a", liquid ^. defn,
  S "or a", solid ^. defn, S "but not a", gaseous ^. defn,
  sSqBr $ acroIM 2 `sC` acroIM 4]
assumpS19 = foldlSent [
  S "The pressure in the", phrase tank, S "is atmospheric, so the",
  phrase melt_pt `sAnd` phrase boil_pt, S "are", S (show (0 :: Integer)) :+:
  Sy (unit_symb temp) `sAnd` S (show (100 :: Integer)) :+:
  Sy (unit_symb temp) `sC` S "respectively", sSqBr $ acroIM 1 `sC` acroIM 3]
assumpS20 = foldlSent [
  S "When considering the", phrase w_vol, S "in the",
  phrase tank `sC` (phrase vol `ofThe` phrase coil),
  S "is assumed to be negligible"]
  --FIXME , sSqBr $ makeRef req2]

--- Again, list structure is same between all examples.
-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.
