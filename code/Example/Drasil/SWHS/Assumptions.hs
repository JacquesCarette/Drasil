module Drasil.SWHS.Assumptions where --all of this file is exported

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.Concepts.Documentation (system, simulation, model, 
  problem, acroNumGen)
import Drasil.DocumentLanguage (mkAssump)

import Drasil.SWHS.DataDefs (dd1HtFluxC, dd2HtFluxP)
import Drasil.SWHS.Concepts (coil, tank, phsChgMtrl, water, perfect_insul,
  charging, discharging)
import Drasil.SWHS.Unitals (w_vol, vol_ht_gen, temp_C, temp_init, temp_W,
  temp_PCM, htCap_L_P, htCap_W, htCap_S_P, w_density, pcm_density, pcm_vol)
import Drasil.SWHS.TMods (t1ConsThermE)

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

s4_2_1_list :: [Contents]
s4_2_1_list = acroNumGen s4_2_1_assump_list 1

s4_2_1_assump_list :: [Contents]
s4_2_1_assump_list = [assump1, assump2, assump3, assump4, assump5, assump6,
  assump7, assump8, assump9, assump10, assump11, assump12, assump13, assump14,
  assump15, assump16, assump17, assump18, assump19, assump20]

assump1, assump2, assump3, assump4, assump5, assump6, assump7,
  assump8, assump9, assump10, assump11, assump12, assump13, assump14,
  assump15, assump16, assump17, assump18, assump19, assump20 :: Contents

assump1 = mkAssump "assump1" $ foldlSent [
  S "The only form of", phrase energy, S "that is",
  S "relevant for this", phrase problem, S "is" +:+. 
  phrase CT.thermal_energy, S "All other forms of", phrase energy `sC`
  S "such as", phrase mech_energy `sC` S "are assumed to be negligible",
  sSqBr $ makeRef $ reldefn t1ConsThermE]
--
assump2 = mkAssump "assump2" $ foldlSent [
  S "All", phrase CT.heat_trans, S "coefficients are constant over",
  phrase time, sSqBr $ acroGD 1]
--
assump3 = mkAssump "assump3" $ foldlSent [
  S "The", phrase water, S "in the", phrase tank,
  S "is fully mixed, so the", phrase temp_W `isThe` 
  S "same throughout the entire", phrase tank,
  sSqBr $ acroGD 2 `sC` (makeRef $ datadefn dd2HtFluxP)]
--
assump4 = mkAssump "assump4" $ foldlSent [
  S "The", phrase temp_PCM `isThe` S "same throughout the", phrase pcm_vol,
  sSqBr $ acroGD 2 `sC`
  (makeRef $ datadefn dd2HtFluxP)]--FIXME `sC` makeRef likeChg1]
--
assump5 = mkAssump "assump5" $ foldlSent [
  S "The", phrase w_density `sAnd` phrase pcm_density,
  S "have no spatial variation; that is" `sC`
  S "they are each constant over their entire", phrase vol,
  sSqBr $ acroGD 2]
--
assump6 = mkAssump "assump6" $ foldlSent [
  S "The", phrase htCap_W `sC` phrase htCap_S_P `sC` S "and",
  phrase htCap_L_P, S "have no spatial variation; that",
  S "is" `sC` S "they are each constant over their entire",
  phrase vol, sSqBr $ acroGD 2]
--
assump7 = mkAssump "assump7" $ foldlSent [
  CT.law_conv_cooling ^. defn, S "applies between the",
  phrase coil `sAnd` S "the", phrase water,
  sSqBr $ makeRef $ datadefn dd1HtFluxC]
--
assump8 = mkAssump "assump8" $ foldlSent [
  S "The", phrase temp_C, S "is constant over", phrase time,
  sSqBr $ makeRef $ datadefn dd1HtFluxC]--FIXME `sC` makeRef likeChg2]
--
assump9 = mkAssump "assump9" $ foldlSent [
  S "The", phrase temp_C, S "does not vary along its length",
  sSqBr $ makeRef $ datadefn dd1HtFluxC]--FIXME `sC` makeRef likeChg3]
--
assump10 = mkAssump "assump10" $ foldlSent [
  CT.law_conv_cooling ^. defn, S "applies between the",
  phrase water `sAnd` S "the", short phsChgMtrl,
  sSqBr $ makeRef $ datadefn dd2HtFluxP]
--
assump11 = mkAssump "assump11" $ foldlSent [
  S "The", phrase model, S "only accounts for", (charging ^. defn) `sC`
  S "not" +:+. phrase discharging, S "The", phrase temp_W `sAnd`
  phrase temp_PCM, S "can only increase, or remain",
  S "constant; they do not decrease. This implies that the",
  phrase temp_init, sSqBr $ makeRef assump12, S "is less than (or equal)",
  S "to the", phrase temp_C, sSqBr $ acroIM 1]--FIXME `sC` makeRef likeChg4]
--
assump12 = mkAssump "assump12" $ foldlSent [
  phrase temp_init `ofThe'` phrase water `sAnd` S "the",
  short phsChgMtrl `isThe` S "same",
  sSqBr $ acroIM 1 `sC` acroIM 2]--FIXME `sC` makeRef likeChg5]
--
assump13 = mkAssump "assump13" $ foldlSent [
  S "The", phrase simulation, S "will start with the",
  short phsChgMtrl, S "in a", solid ^. defn,
  sSqBr $ acroIM 2 `sC` acroIM 4]
--
assump14 = mkAssump "assump14" $ foldlSent [
  (S "operating" +:+ phrase temp +:+ S "range") `ofThe'` phrase system,
  S "is such that the", phrase water,
  S "is always in" +:+. (liquid ^. defn), S "That is" `sC`
  S "the", phrase temp, S "will not drop below the",
  phrase melt_pt, S "of", phrase water `sC` S "or rise above its",
  phrase boil_pt, sSqBr $ acroIM 1 `sC` acroIM 3]
--
assump15 = mkAssump "assump15" $ foldlSent [
  S "The", phrase tank, S "is", phrase perfect_insul,
  S "so that there is no", phrase CT.heat, S "loss from the",
  phrase tank, sSqBr $ acroIM 1]--FIXME `sC` makeRef likeChg6]
--
assump16 = mkAssump "assump16" $ foldlSent [
  S "No internal", phrase CT.heat, S "is generated by either the",
  phrase water, S "or the", short phsChgMtrl `semiCol`
  S "therefore, the", phrase vol_ht_gen, S "is zero",
  sSqBr $ acroIM 1 `sC` acroIM 2]
--
assump17 = mkAssump "assump17" $ foldlSent [
  (phrase vol +:+ phrase change) `ofThe'` short phsChgMtrl,
  S "due to", phrase CT.melting, S "is negligible", sSqBr $ acroIM 2]
--
assump18 = mkAssump "assump18" $ foldlSent [
  S "The", short phsChgMtrl, S "is either in a", liquid ^. defn,
  S "or a", solid ^. defn, S "but not a", gaseous ^. defn,
  sSqBr $ acroIM 2 `sC` acroIM 4]
--
assump19 = mkAssump "assump19" $ foldlSent [
  S "The pressure in the", phrase tank, S "is atmospheric, so the",
  phrase melt_pt `sAnd` phrase boil_pt, S "are", S (show (0 :: Integer)) :+:
  Sy (unit_symb temp) `sAnd` S (show (100 :: Integer)) :+:
  Sy (unit_symb temp) `sC` S "respectively", sSqBr $ acroIM 1 `sC` acroIM 3]
--
assump20 = mkAssump "assump20" $ foldlSent [
  S "When considering the", phrase w_vol, S "in the",
  phrase tank `sC` (phrase vol `ofThe` phrase coil),
  S "is assumed to be negligible"]--FIXME , sSqBr $ makeRef req2]

-- Again, list structure is same between all examples.

-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.
