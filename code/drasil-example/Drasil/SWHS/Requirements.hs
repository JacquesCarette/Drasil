module Drasil.SWHS.Requirements where --all of this file is exported

import Language.Drasil

import Data.Drasil.Concepts.Documentation (output_, simulation, quantity, 
  input_, physical, constraint, condition, property, funcReqDom)
import Drasil.DocLang (nonFuncReqF)

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (time, energy)

import Data.Drasil.Concepts.Thermodynamics as CT (law_cons_energy,
  melting)
import Data.Drasil.Concepts.Software (correctness, verifiability,
  understandability, reusability, maintainability, performance)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), foldlList, 
  foldlSent, isThe, sAnd)
import Drasil.SWHS.Assumptions (assumpVCN)
import Drasil.SWHS.Concepts (phsChgMtrl, tank)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, 
  heatEInPCM, swhsIMods)
import Drasil.SWHS.Tables (inputInitQuantsTblabled)
import Drasil.SWHS.Unitals (t_final_melt, t_init_melt, pcm_E, w_E, temp_PCM,
  temp_W, tau_S_P, tau_L_P, eta, tau_W, w_density, pcm_mass, pcm_vol,
  pcm_density, diam, tank_length, tank_vol, w_vol, w_mass)

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------
-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

funcReqs :: [ConceptInstance]
funcReqs = [inputInitQuants, findMass, checkWithPhysConsts, outputInputDerivQuants,
  calcTempWtrOverTime, calcTempPCMOverTime, calcChgHeatEnergyWtrOverTime,
  calcChgHeatEnergyPCMOverTime, verifyEnergyOutput, calcPCMMeltBegin, calcPCMMeltEnd]

inputInitQuants, findMass, checkWithPhysConsts, outputInputDerivQuants,
  calcTempWtrOverTime, calcTempPCMOverTime, calcChgHeatEnergyWtrOverTime,
  calcChgHeatEnergyPCMOverTime, verifyEnergyOutput, calcPCMMeltBegin,
  calcPCMMeltEnd :: ConceptInstance

inputInitQuantsEqn, findMassEqn :: Expr --Fixme: rename labels

inputInitQuants = cic "inputInitQuants" ( foldlSent [
  titleize input_, S "the following", plural quantity, S "described in",
  makeRef2S inputInitQuantsTblabled `sC` S "which define the", phrase tank,
  plural parameter `sC` S "material", plural property, S "and initial" +:+.
  plural condition, makeRef2S assumpVCN]) "Input-Initial-Quantities" funcReqDom
--
findMass = cic "findMass" ( foldlSent [
  S "Use the", plural input_, S "in", makeRef2S inputInitQuants,
  S "to find the", phrase mass, S "needed for",
  (foldlList Comma List $ map makeRef2S swhsIMods) `sC`
  S "using", E inputInitQuantsEqn, S "and", E findMassEqn `sC` S "where",
  ch w_vol `isThe` phrase w_vol, S "and", ch tank_vol `isThe` phrase tank_vol] )
  "Find-Mass" funcReqDom -- FIXME: Equations shouldn't be inline

inputInitQuantsEqn = (sy w_mass) $= (sy w_vol) * (sy w_density) $=
  ((sy tank_vol) - (sy pcm_vol)) * (sy w_density) $=
  (((sy diam) / 2) * (sy tank_length) - (sy pcm_vol)) * (sy w_density) -- FIXME: Ref Hack

findMassEqn = (sy pcm_mass) $= (sy pcm_vol) * (sy pcm_density) -- FIXME: Ref Hack
--
checkWithPhysConsts = cic "checkWithPhysConsts" ( foldlSent [
  S "Verify that the", plural input_, S "satisfy the required",
  phrase physical, plural constraint {-, S "shown in"
  --FIXME , makeRefS s7_table1-}] )
  "Check-Input-with-Physical_Constraints" funcReqDom
--
outputInputDerivQuants = cic "outputInputDerivQuants" ( foldlSent [
  titleize output_, S "the", phrase input_, plural quantity `sAnd`
  S "derived", plural quantity +: S "in the following list",
  S "the", plural quantity, S "from", makeRef2S inputInitQuants `sC` S "the",
  plural mass, S "from", makeRef2S findMass `sC` ch tau_W,
  sParen (S "from" +:+ makeRef2S eBalanceOnWtr) `sC` ch eta,
  sParen (S "from" +:+ makeRef2S eBalanceOnWtr) `sC` ch tau_S_P,
  sParen (S "from" +:+ makeRef2S eBalanceOnPCM) `sAnd` ch tau_L_P,
  sParen (S "from" +:+ makeRef2S eBalanceOnPCM)] )
  "Output-Input-Derived-Quantities" funcReqDom
--
calcTempWtrOverTime = cic "calcTempWtrOverTime" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase temp_W,
  sParen(ch temp_W :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S eBalanceOnWtr)] )
  "Calculate-Temperature-Water-Over-Time" funcReqDom
--
calcTempPCMOverTime = cic "calcTempPCMOverTime" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase temp_PCM,
  sParen (ch temp_PCM :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)] )
  "Calculate-Temperature-PCM-Over-Time" funcReqDom
--
calcChgHeatEnergyWtrOverTime = cic "calcChgHeatEnergyWtrOverTime" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase w_E,
  sParen (ch w_E :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S heatEInWtr)] )
  "Calculate-Change-Heat_Energy-Water-Over-Time" funcReqDom
--
calcChgHeatEnergyPCMOverTime = cic "calcChgHeatEnergyPCMOverTime" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase pcm_E,
  sParen (ch pcm_E :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S heatEInPCM)] )
  "Calculate-Change-Heat_Energy-PCM-Over-Time" funcReqDom
--
verifyEnergyOutput = cic "verifyEnergyOutput" ( foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (ch w_E :+: sParen (ch time) `sAnd` ch pcm_E :+:
  sParen (ch time)), S "follow the", phrase CT.law_cons_energy, {-`sC`
  S "as outlined in"
  --FIXME , makeRefS s4_2_7 `sC` -}
  S "with relative error no greater than 0.001%"] )
  "Verify-Energy-Output-Follow-Conservation-of-Energy" funcReqDom
--
calcPCMMeltBegin = cic "calcPCMMeltBegin" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "begins to melt",
  ch t_init_melt, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)] )
  "Calculate-PCM-Melt-Begin-Time" funcReqDom
--
calcPCMMeltEnd = cic "calcPCMMeltEnd" ( foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "stops", phrase CT.melting,
  ch t_final_melt, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)] )
  "Calculate-PCM-Melt-End-Time" funcReqDom

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
