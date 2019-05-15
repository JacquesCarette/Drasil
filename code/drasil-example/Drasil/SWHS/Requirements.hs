module Drasil.SWHS.Requirements where --all of this file is exported

import Language.Drasil
import Theory.Drasil (DataDefinition)

import Drasil.DocLang (inDataConstTbl, mkEnumSimpleD)
import Drasil.DocLang.SRS (propCorSol) 

import Data.Drasil.Concepts.Documentation (assumption, code, condition, corSol,
  dataDefn, funcReqDom, genDefn, inModel, input_, likelyChg, mg, mis, module_,
  nonFuncReqDom, output_, physicalConstraint, property, quantity, requirement, 
  simulation, solution, srs, thModel, traceyMatrix, unlikelyChg, vavPlan)
import Data.Drasil.Concepts.Math (equation, parameter, surface)
import Data.Drasil.Concepts.Thermodynamics as CT (heatTrans, lawConsEnergy, melting)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (energy, time)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), foldlList, 
  foldlSent, foldlSent_, foldlSP, foldlSP_, foldlSPCol, isThe, ofThe', sAnd)
import Data.Drasil.Utils (eqUnR')

import Drasil.SWHS.Assumptions (assumpVCN)
import Drasil.SWHS.Concepts (coil, phsChgMtrl, progName, rightSide, tank, water)
import Drasil.SWHS.DataDefs (dd1HtFluxC, dd2HtFluxP)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM, swhsIMods)
import Drasil.SWHS.Tables (inputInitQuantsTblabled, inputInitQuantsTbl)
import Drasil.SWHS.Unitals (coil_HTC, coil_SA, diam, eta, htCap_L_P, htCap_S_P,
  htCap_W, htFusion, pcm_E, pcm_HTC, pcm_SA, pcm_density, pcm_mass, pcm_vol,
  sim_time, t_final_melt, t_init_melt, tank_length, tank_vol, tau_L_P, tau_S_P,
  tau_W, temp_C, temp_PCM, temp_W, temp_init, temp_melt_P, time_final, w_E,
  w_density, w_mass, w_vol)

------------------------------
-- Data Constraint: Table 1 --
------------------------------

-- FIXME: This probably shouldn't be here.
dataConTable1 :: LabelledContent
dataConTable1 = inDataConstTbl inputConstraints

inputConstraints :: [UncertQ]
inputConstraints = [tank_length, diam, pcm_vol, pcm_SA, pcm_density,
  temp_melt_P, htCap_S_P, htCap_L_P, htFusion, coil_SA,
  temp_C, w_density, htCap_W, coil_HTC, pcm_HTC, temp_init, time_final]

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------
-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

funcReqsList :: [Contents]
funcReqsList = reqs ++ [inputInitQuantsTbl]

reqs :: [Contents]
reqs = mkEnumSimpleD funcReqs

funcReqs :: [ConceptInstance]
funcReqs = [inputInitQuants, findMass, checkWithPhysConsts, outputInputDerivQuants,
  calcTempWtrOverTime, calcTempPCMOverTime, calcChgHeatEnergyWtrOverTime,
  calcChgHeatEnergyPCMOverTime, verifyEnergyOutput, calcPCMMeltBegin, calcPCMMeltEnd]

inputInitQuants, findMass, checkWithPhysConsts, outputInputDerivQuants,
  calcTempWtrOverTime, calcTempPCMOverTime, calcChgHeatEnergyWtrOverTime,
  calcChgHeatEnergyPCMOverTime, verifyEnergyOutput, calcPCMMeltBegin,
  calcPCMMeltEnd :: ConceptInstance

inputInitQuantsEqn, findMassEqn :: Expr --Fixme: rename labels

inputInitQuants = iIQConstruct inputInitQuantsTblabled

iIQConstruct :: (Referable l, HasShortName l) => l -> ConceptInstance
iIQConstruct x = cic "inputInitQuants" ( foldlSent [
  titleize input_, S "the following", plural quantity, S "described in",
  makeRef2S x `sC` S "which define the", phrase tank,
  plural parameter `sC` S "material", plural property, S "and initial",
  plural condition]) "Input-Initial-Quantities" funcReqDom
--
findMass = findMassConstruct inputInitQuants (plural mass)
            (foldlList Comma List $ map makeRef2S swhsIMods)
            (foldlList Comma List $ [E inputInitQuantsEqn, E findMassEqn, makeRef2S assumpVCN])
            (ch w_vol `isThe` phrase w_vol `sAnd` ch tank_vol `isThe` phrase tank_vol)

findMassConstruct :: (Referable l, HasShortName l) => l -> Sentence ->
                                   Sentence -> Sentence -> Sentence -> ConceptInstance
findMassConstruct fr m ims exprs defs = cic "findMass" ( foldlSent [
  S "Use the", plural input_, S "in", makeRef2S fr, S "to find the", 
  m, S "needed for", ims `sC` S "using", exprs `sC` S "where", defs])
  "Find-Mass" funcReqDom -- FIXME: Equations shouldn't be inline

inputInitQuantsEqn = (sy w_mass) $= (sy w_vol) * (sy w_density) $=
  ((sy tank_vol) - (sy pcm_vol)) * (sy w_density) $=
  ((sy pi_) * ((((sy diam) / 2) $^ 2)) * (sy tank_length) - (sy pcm_vol)) * (sy w_density) -- FIXME: Ref Hack

findMassEqn = (sy pcm_mass) $= (sy pcm_vol) * (sy pcm_density) -- FIXME: Ref Hack
--
checkWithPhysConsts = cic "checkWithPhysConsts" ( foldlSent [
  S "Verify that the", plural input_, S "satisfy the required",
  plural physicalConstraint , S "shown in", makeRef2S dataConTable1] )
  "Check-Input-with-Physical_Constraints" funcReqDom
--
outputInputDerivQuants = oIDQConstruct oIDQQuants

oIDQConstruct :: [Sentence] -> ConceptInstance
oIDQConstruct x = cic "outputInputDerivQuants" ( foldlSent_ [
  titleize output_, S "the", phrase input_, plural quantity `sAnd`
  S "derived", plural quantity +: S "in the following list"] +:+.
  (foldlList Comma List x)) "Output-Input-Derived-Quantities" funcReqDom

oIDQQuants :: [Sentence]
oIDQQuants = map foldlSent_ [
  [S "the", plural quantity, S "from", makeRef2S inputInitQuants],
  [S "the", plural mass, S "from", makeRef2S findMass],
  [ch tau_W, sParen (S "from" +:+ makeRef2S eBalanceOnWtr)],
  [ch eta, sParen (S "from" +:+ makeRef2S eBalanceOnWtr)],
  [ch tau_S_P, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)],
  [ch tau_L_P, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)]
  ]
  
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
  sParen (ch time)), S "follow the", phrase CT.lawConsEnergy, {-`sC`
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

swhsNFRequirements :: [ConceptInstance]
swhsNFRequirements = [correct, verifiable, understandable, reusable, maintainable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  plural output_ `ofThe'` phrase code, S "have the",
  plural property, S "described in", makeRef2S (propCorSol propsDeriv [])
  ]) "Correct" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  S "The", phrase code, S "is tested with complete",
  phrase vavPlan]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "The", phrase code, S "is modularized with complete",
  phrase mg `sAnd` phrase mis]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  S "The", phrase code, S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix, S "in the", getAcc srs `sAnd` phrase mg]) "Maintainable" nonFuncReqDom

{-Properties of a Correct Solution-}

propsDeriv :: [Contents]
propsDeriv =
  [propCorSolDeriv1 CT.lawConsEnergy w_E energy coil phsChgMtrl dd1HtFluxC
    dd2HtFluxP surface CT.heatTrans,
  propCorSolDeriv2,
  propCorSolDeriv3 pcm_E energy phsChgMtrl water,
  propCorSolDeriv4,
  propCorSolDeriv5 equation progName rightSide]

-- Remember to insert references in above derivation when available

propCorSolDeriv1 :: (NamedIdea b, NamedIdea h) => ConceptChunk -> b -> UnitalChunk -> ConceptChunk ->
  CI -> DataDefinition -> DataDefinition -> h -> ConceptChunk -> Contents
propCorSolDeriv1 lce ewat en co pcmat d1hfc d2hfp su ht  =
  foldlSPCol [S "A", phrase corSol, S "must exhibit the" +:+.
  phrase lce, S "This means that the", phrase ewat,
  S "should equal the difference between the total", phrase en,
  phrase input_, S "from the", phrase co `sAnd` S "the",
  phrase en, phrase output_, S "to the" +:+. short pcmat,
  S "This can be shown as an", phrase equation, S "by taking",
  (makeRef2S d1hfc) `sAnd` (makeRef2S d2hfp) `sC`
  S "multiplying each by their respective", phrase su,
  S "area of", phrase ht `sC` S "and integrating each",
  S "over the", phrase sim_time `sC` S "as follows"]

propCorSolDeriv2 :: Contents
propCorSolDeriv2 = eqUnR' $ 
  ((sy w_E) $= (defint (eqSymb time) 0 (sy time)
  ((sy coil_HTC) * (sy coil_SA) * ((sy temp_C) - apply1 temp_W time)))
  - (defint (eqSymb time) 0 (sy time)
  ((sy pcm_HTC) * (sy pcm_SA) * ((apply1 temp_W time) -
  (apply1 temp_PCM time)))))

propCorSolDeriv3 :: NamedIdea a => a -> UnitalChunk -> CI -> ConceptChunk -> Contents
propCorSolDeriv3 epcm en pcmat wa =
  foldlSP_ [S "In addition, the", phrase epcm, S "should equal the",
  phrase en, phrase input_, S "to the", short pcmat,
  S "from the" +:+. phrase wa, S "This can be expressed as"]

propCorSolDeriv4 :: Contents
propCorSolDeriv4 = eqUnR' $ 
  ((sy pcm_E) $= (defint (eqSymb time) 0 (sy time)
  ((sy pcm_HTC) * (sy pcm_SA) * ((apply1 temp_W time) - 
  (apply1 temp_PCM time)))))

propCorSolDeriv5 :: ConceptChunk -> CI -> CI -> Contents
propCorSolDeriv5 eq pro rs = foldlSP [titleize' eq, S "(FIXME: Equation 7)" 
  `sAnd` S "(FIXME: Equation 8) can be used as", Quote (S "sanity") +:+
  S "checks to gain confidence in any", phrase solution,
  S "computed by" +:+. short pro, S "The relative",
  S "error between the results computed by", short pro `sAnd`
  S "the results calculated from the", short rs, S "of these",
  plural eq, S "should be less than 0.001%", makeRef2S verifyEnergyOutput]

-- Above section only occurs in this example (although maybe it SHOULD be in
-- the others).

-- Remember to insert references in above derivation when available

{-NoPCM specific-}
-- Defined in this file since there isn't a NoPCM Requirements.hs file

noPCMNFRequirements :: [ConceptInstance]
noPCMNFRequirements = [correctNoPCM, verifiable, understandable, reusable, maintainable]

propsDerivNoPCM :: [Contents]
propsDerivNoPCM = [foldlSP [S "FIXME"]]

correctNoPCM :: ConceptInstance
correctNoPCM = cic "correct" (foldlSent [
  plural output_ `ofThe'` phrase code, S "have the",
  plural property, S "described in", makeRef2S (propCorSol propsDerivNoPCM [])
  ]) "Correct" nonFuncReqDom

-- The second sentence of the above paragraph is repeated in all examples (not
-- exactly, but the general idea is). The first sentence is not always
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be
-- abstracted out.
