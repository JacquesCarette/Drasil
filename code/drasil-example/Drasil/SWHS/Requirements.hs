module Drasil.SWHS.Requirements where --all of this file is exported

import Language.Drasil

import Data.Drasil.Concepts.Documentation (output_, simulation, quantity, 
  input_, physical, constraint, condition, property)
import Data.Drasil.Utils (eqUnR')
import Drasil.DocLang (mkRequirement, mkRequirementL, nonFuncReqF)
import Drasil.SWHS.Labels (inputInitQuantsL, useAboveFindMassL, checkWithPhysConstsL, 
  outputInputDerivQuantsL, calcTempWtrOverTimeL, calcTempPCMOverTimeL,
  calcChgHeatEnergyWtrOverTimeL, calcChgHeatEnergyPCMOverTimeL,
  verifyEnergyOutputL, calcPCMMeltBeginL, calcPCMMeltEndL)

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (time, energy)

import Data.Drasil.Concepts.Thermodynamics as CT (law_cons_energy,
  melting)
import Data.Drasil.Concepts.Software (correctness, verifiability,
  understandability, reusability, maintainability, performance)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), foldlList, 
  foldlSent, foldlSentCol, isThe, sAnd)

import Drasil.SWHS.Concepts (phsChgMtrl, tank)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, 
  heatEInPCM, swhsIMods)
import Drasil.SWHS.Unitals (t_final_melt, t_init_melt, pcm_E, w_E, temp_PCM,
  temp_W, tau_S_P, tau_L_P, eta, tau_W, w_density, pcm_mass, pcm_vol,
  pcm_density, diam, tank_length, tank_vol, w_vol, w_mass)

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------
-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

inputInitQuants, useAboveFindMass, checkWithPhysConsts, outputInputDerivQuants, calcTempWtrOverTime, 
  calcTempPCMOverTime, calcChgHeatEnergyWtrOverTime, calcChgHeatEnergyPCMOverTime, verifyEnergyOutput, 
  calcPCMMeltBegin, calcPCMMeltEnd :: LabelledContent

inputInitQuantsEqn, useAboveFindMassEqn :: Contents --Fixme: rename labels

inputInitQuants = mkRequirementL "inputInitQuants" ( foldlSentCol [
  titleize input_, S "the following", plural quantity `sC`
  S "which define the", phrase tank, plural parameter `sC` S "material",
  plural property, S "and initial", plural condition]) inputInitQuantsL

inputInitQuantsEqn = eqUnR' $ ((sy w_mass) $= (sy w_vol) * (sy w_density) $=
  ((sy tank_vol) - (sy pcm_vol)) * (sy w_density) $=
  (((sy diam) / 2) * (sy tank_length) - (sy pcm_vol)) * (sy w_density)) -- FIXME: Ref Hack

useAboveFindMass = mkRequirementL "useAboveFindMass" ( foldlSentCol [
  S "Use the", plural input_, S "in", makeRef inputInitQuants,
  S "to find the", phrase mass, S "needed for", (foldlList Comma List $ map makeRef swhsIMods) `sC` 
  S "as follows, where", ch w_vol `isThe` phrase w_vol,
  S "and", ch tank_vol `isThe` phrase tank_vol] ) useAboveFindMassL

useAboveFindMassEqn = eqUnR' $ ((sy pcm_mass) $= (sy pcm_vol) * (sy pcm_density)) -- FIXME: Ref Hack

checkWithPhysConsts = mkRequirementL "checkWithPhysConsts" ( foldlSent [
  S "Verify that the", plural input_, S "satisfy the required",
  phrase physical, plural constraint {-, S "shown in"
  --FIXME , makeRef s7_table1-}] ) 
  checkWithPhysConstsL
--
outputInputDerivQuants = mkRequirementL "outputInputDerivQuants" ( foldlSent [
  titleize output_, S "the", phrase input_, plural quantity `sAnd`
  S "derived", plural quantity +: S "in the following list",
  S "the", plural quantity, S "from", makeRef inputInitQuants `sC` S "the",
  plural mass, S "from", makeRef useAboveFindMass `sC` ch tau_W,
  sParen (S "from" +:+ makeRef eBalanceOnWtr) `sC` ch eta,
  sParen (S "from" +:+ makeRef eBalanceOnWtr) `sC` ch tau_S_P,
  sParen (S "from" +:+ makeRef eBalanceOnPCM) `sAnd` ch tau_L_P,
  sParen (S "from" +:+ makeRef eBalanceOnPCM)] ) 
  outputInputDerivQuantsL
--
calcTempWtrOverTime = mkRequirementL "calcTempWtrOverTime" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase temp_W,
  sParen(ch temp_W :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef eBalanceOnWtr)] ) calcTempWtrOverTimeL
--
calcTempPCMOverTime = mkRequirementL "calcTempPCMOverTime" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase temp_PCM,
  sParen (ch temp_PCM :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef eBalanceOnPCM)] ) calcTempPCMOverTimeL
--
calcChgHeatEnergyWtrOverTime = mkRequirementL "calcChgHeatEnergyWtrOverTime" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase w_E,
  sParen (ch w_E :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef heatEInWtr)] ) calcChgHeatEnergyWtrOverTimeL
--
calcChgHeatEnergyPCMOverTime = mkRequirementL "calcChgHeatEnergyPCMOverTime" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase pcm_E,
  sParen (ch pcm_E :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef heatEInPCM)] ) calcChgHeatEnergyPCMOverTimeL
--
verifyEnergyOutput = mkRequirementL "verifyEnergyOutput" ( foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (ch w_E :+: sParen (ch time) `sAnd` ch pcm_E :+:
  sParen (ch time)), S "follow the", phrase CT.law_cons_energy, {-`sC`
  S "as outlined in"
  --FIXME , makeRef s4_2_7 `sC` -} 
  S "with relative error no greater than 0.001%"] ) verifyEnergyOutputL
--
calcPCMMeltBegin = mkRequirementL "calcPCMMeltBegin" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "begins to melt",
  ch t_init_melt, sParen (S "from" +:+ makeRef eBalanceOnPCM)] ) calcPCMMeltBeginL
--
calcPCMMeltEnd = mkRequirementL "calcPCMMeltEnd" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "stops", phrase CT.melting,
  ch t_final_melt, sParen (S "from" +:+ makeRef eBalanceOnPCM)] ) calcPCMMeltEndL

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

-- FIXME: Related to #792 -- Not sure if this is needed
newVerifyEnergyOutput :: ReqChunk
newVerifyEnergyOutput = frc "verifyEnergyOutput" ( foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (ch w_E :+: sParen (ch time) `sAnd` ch pcm_E :+:
  sParen (ch time)), S "follow the", phrase CT.law_cons_energy, {-`sC`
  S "as outlined in"
  --FIXME , makeRef s4_2_7 `sC` -} 
  S "with relative error no greater than 0.001%"] ) 
  (mkLabelSame "Verify-Energy-Output-follow-Conservation-of-Energy_Label" (Req FR))