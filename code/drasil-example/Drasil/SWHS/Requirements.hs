module Drasil.SWHS.Requirements where --all of this file is exported

import Language.Drasil

import Data.Drasil.Concepts.Documentation (output_, simulation, quantity, 
  input_, physical, constraint, condition, property)
import Data.Drasil.Utils (eqUnR')
import Drasil.DocLang (mkRequirement, nonFuncReqF)

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (time, energy)

import Data.Drasil.Concepts.Thermodynamics as CT (law_cons_energy,
  melting)
import Data.Drasil.Concepts.Software (correctness, verifiability,
  understandability, reusability, maintainability, performance)
import Data.Drasil.Concepts.Math (parameter)

import Drasil.SWHS.Unitals (t_final_melt, t_init_melt, pcm_E, w_E, temp_PCM,
  temp_W, tau_S_P, tau_L_P, eta, tau_W, w_density, pcm_mass, pcm_vol,
  pcm_density, diam, tank_length, tank_vol, w_vol, w_mass)
import Drasil.SWHS.Concepts (phsChgMtrl, tank)

import Data.Drasil.SentenceStructures (acroIM, acroR, foldlSent, sAnd, isThe,
  foldlSentCol)


------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------
-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

req1, req2, req3, req4,
  req5, req6, req7, req8, req9, req10, req11 :: LabelledContent

reqEqn1, reqEqn2 :: Contents --Fixme: rename labels

req1 = mkRequirement "req1" ( foldlSentCol [
  titleize input_, S "the following", plural quantity `sC`
  S "which define the", phrase tank, plural parameter `sC` S "material",
  plural property, S "and initial", plural condition]) "Input-Initial-Quantities"

req2 = mkRequirement "req2" ( foldlSentCol [
  S "Use the", plural input_, S "in", makeRef req1,
  S "to find the", phrase mass, S "needed for", acroIM 1, S "to",
  acroIM 4 `sC` S "as follows, where", ch w_vol `isThe` phrase w_vol,
  S "and", ch tank_vol `isThe` phrase tank_vol] ) "Use-Above-Find-Mass-IM1-IM4"

reqEqn1 = eqUnR' $ ((sy w_mass) $= (sy w_vol) * (sy w_density) $=
  ((sy tank_vol) - (sy pcm_vol)) * (sy w_density) $=
  (((sy diam) / 2) * (sy tank_length) - (sy pcm_vol)) * (sy w_density)) -- FIXME: Ref Hack

reqEqn2 = eqUnR' $ ((sy pcm_mass) $= (sy pcm_vol) * (sy pcm_density)) -- FIXME: Ref Hack

req3 = mkRequirement "req3" ( foldlSent [
  S "Verify that the", plural input_, S "satisfy the required",
  phrase physical, plural constraint {-, S "shown in"
  --FIXME , makeRef s7_table1-}] ) 
  "Check-Input-with-Physical_Constraints"
--
req4 = mkRequirement "req4" ( foldlSent [
  titleize output_, S "the", phrase input_, plural quantity `sAnd`
  S "derived", plural quantity +: S "in the following list",
  S "the", plural quantity, S "from", acroR 1 `sC` S "the",
  plural mass, S "from", acroR 2 `sC` ch tau_W,
  sParen (S "from" +:+ acroIM 1) `sC` ch eta,
  sParen (S "from" +:+ acroIM 1) `sC` ch tau_S_P,
  sParen (S "from" +:+ acroIM 2) `sAnd` ch tau_L_P,
  sParen (S "from" +:+ acroIM 2)] ) 
  "Output-Input-Derived-Quantities"
--
req5 = mkRequirement "req5" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase temp_W,
  sParen(ch temp_W :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 1)] ) "Calculate-Temperature-Water-OverTime"
--
req6 = mkRequirement "req6" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase temp_PCM,
  sParen (ch temp_PCM :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 2)] ) "Calculate-Temperature-PCM-Over-Time"
--
req7 = mkRequirement "req7" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase w_E,
  sParen (ch w_E :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 3)] ) "Calculate-Change-Heat_Energy-Water-Over-Time"
--
req8 = mkRequirement "req8" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase pcm_E,
  sParen (ch pcm_E :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 4)] ) "Calculate-Change-Heat_Energy-PCM-Over-Time"
--
req9 = mkRequirement "req9" ( foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (ch w_E :+: sParen (ch time) `sAnd` ch pcm_E :+:
  sParen (ch time)), S "follow the", phrase CT.law_cons_energy, {-`sC`
  S "as outlined in"
  --FIXME , makeRef s4_2_7 `sC` -} 
  S "with relative error no greater than 0.001%"] ) "Verify-Energy-Output-follow-Conservation-of-Energy"
--
req10 = mkRequirement "req10" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "begins to melt",
  ch t_init_melt, sParen (S "from" +:+ acroIM 2)] ) "Calculate-PCM-melt-begin-time"
--
req11 = mkRequirement "req11" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "stops", phrase CT.melting,
  ch t_final_melt, sParen (S "from" +:+ acroIM 2)] ) "Calculate-PCM-melt-end-time"

-- List structure same between all examples

--How to include pi?
--How to add exponents?

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------

nonFuncReqs :: Section
nonFuncReqs = nonFuncReqF [performance] [correctness, verifiability,
  understandability, reusability, maintainability]
  (S "This problem is small in size and relatively simple")
  (S "Any reasonable implementation will be very" +:+
  S "quick and use minimal storage.")

-- The second sentence of the above paragraph is repeated in all examples (not
-- exactly, but the general idea is). The first sentence is not always
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be
-- abstracted out.

-- FIXME: Related to #792 --
newReq9 :: ReqChunk
newReq9 = frc "req9" ( foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (ch w_E :+: sParen (ch time) `sAnd` ch pcm_E :+:
  sParen (ch time)), S "follow the", phrase CT.law_cons_energy, {-`sC`
  S "as outlined in"
  --FIXME , makeRef s4_2_7 `sC` -} 
  S "with relative error no greater than 0.001%"] ) 
  (mkLabelSame "Verify-Energy-Output-follow-Conservation-of-Energy_Label")