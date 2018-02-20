module Drasil.SWHS.Requirements where --all of this file is exported

import Language.Drasil

import Data.Drasil.Concepts.Documentation (output_, simulation, quantity, 
  input_, physical, constraint, condition, property)
import Data.Drasil.Utils (getES)
import Drasil.DocumentLanguage (mkRequirement)
import Drasil.Sections.Requirements (nonFuncReqF)

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

--req1, req2, s5_1_2_Eqn1, s5_1_2_Eqn2, req3, req4,
--  req5, req6, req7, req8, req9, req10, req11
req1, req2, func_req_Eqn1, func_req_Eqn2, req3, req4,
  req5, req6, req7, req8, req9, req10, req11 :: Contents

req1 = mkRequirement "req1" $ foldlSentCol [
  titleize input_, S "the following", plural quantity `sC`
  S "which define the", phrase tank, plural parameter `sC` S "material",
  plural property, S "and initial", plural condition]

req2 = mkRequirement "req2" $ foldlSentCol [
  S "Use the", plural input_, S "in", makeRef req1,
  S "to find the", phrase mass, S "needed for", acroIM 1, S "to",
  acroIM 4 `sC` S "as follows, where", getES w_vol `isThe` phrase w_vol,
  S "and", getES tank_vol `isThe` phrase tank_vol]

func_req_Eqn1 = EqnBlock ((C w_mass) $= (C w_vol) * (C w_density) $=
  ((C tank_vol) - (C pcm_vol)) * (C w_density) $=
  (((C diam) / 2) * (C tank_length) - (C pcm_vol)) * (C w_density))

func_req_Eqn2 = EqnBlock ((C pcm_mass) $= (C pcm_vol) * (C pcm_density))

req3 = mkRequirement "req3" $ foldlSent [
  S "Verify that the", plural input_, S "satisfy the required",
  phrase physical, plural constraint {-, S "shown in"
  --FIXME , makeRef s7_table1-}]
--
req4 = mkRequirement "req4" $ foldlSent [
  titleize output_, S "the", phrase input_, plural quantity `sAnd`
  S "derived", plural quantity +: S "in the following list",
  S "the", plural quantity, S "from", acroR 1 `sC` S "the",
  plural mass, S "from", acroR 2 `sC` getES tau_W,
  sParen (S "from" +:+ acroIM 1) `sC` getES eta,
  sParen (S "from" +:+ acroIM 1) `sC` getES tau_S_P,
  sParen (S "from" +:+ acroIM 2) `sAnd` getES tau_L_P,
  sParen (S "from" +:+ acroIM 2)]
--
req5 = mkRequirement "req5" $ foldlSent [
  S "Calculate and", phrase output_, S "the", phrase temp_W,
  sParen(getES temp_W :+: sParen (getES time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 1)]
--
req6 = mkRequirement "req6" $ foldlSent [
  S "Calculate and", phrase output_, S "the", phrase temp_PCM,
  sParen (getES temp_PCM :+: sParen (getES time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 2)]
--
req7 = mkRequirement "req7" $ foldlSent [
  S "Calculate and", phrase output_, S "the", phrase w_E,
  sParen (getES w_E :+: sParen (getES time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 3)]
--
req8 = mkRequirement "req8" $ foldlSent [
  S "Calculate and", phrase output_, S "the", phrase pcm_E,
  sParen (getES pcm_E :+: sParen (getES time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 4)]
--
req9 = mkRequirement "req9" $ foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (getES w_E :+: sParen (getES time) `sAnd` getES pcm_E :+:
  sParen (getES time)), S "follow the", phrase CT.law_cons_energy, {-`sC`
  S "as outlined in"
  --FIXME , makeRef s4_2_7 `sC` -} 
  S "with relative error no greater than 0.001%"]
--
req10 = mkRequirement "req10" $ foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "begins to melt",
  getES t_init_melt, sParen (S "from" +:+ acroIM 2)]
--
req11 = mkRequirement "req11" $ foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "stops", phrase CT.melting,
  getES t_final_melt, sParen (S "from" +:+ acroIM 2)]

-- List structure same between all examples

--How to include pi?
--How to add exponents?

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------
--s5_2
non_func_req :: Section
non_func_req = nonFuncReqF [performance] [correctness, verifiability,
  understandability, reusability, maintainability]
  (S "This problem is small in size and relatively simple")
  (S "Any reasonable implementation will be very" +:+
  S "quick and use minimal storage.")

-- The second sentence of the above paragraph is repeated in all examples (not
-- exactly, but the general idea is). The first sentence is not always
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be
-- abstracted out.
