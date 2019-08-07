module Drasil.SWHS.Requirements where --all of this file is exported

import Language.Drasil
import Utils.Drasil

import Drasil.DocLang (mkInputPropsTable)
import Drasil.DocLang.SRS (datCon, propCorSol) 

import Data.Drasil.Concepts.Documentation (assumption, code, condition,
  funcReqDom, input_, likelyChg, mg, mis, module_, nonFuncReqDom, output_,
  physicalConstraint, property, quantity, requirement, simulation, srs,
  traceyMatrix, unlikelyChg, vavPlan)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)
import Data.Drasil.Concepts.Thermodynamics as CT (lawConsEnergy, melting)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (energy, time)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, thModel)

import Drasil.SWHS.Assumptions (assumpVCN)
import Drasil.SWHS.DataDefs (balanceDecayRate, balanceDecayTime,
  balanceSolidPCM, balanceLiquidPCM)
import Drasil.SWHS.Concepts (phsChgMtrl, tank)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM, iMods)
import Drasil.SWHS.Unitals (inputs, consTol, diam, pcmE, pcmDensity, pcmMass,
  pcmVol, tFinalMelt, tInitMelt, tankLength, tankVol, tempPCM, tempW, watE,
  wDensity, wMass, wVol)

------------------------------
-- Data Constraint: Table 1 --
------------------------------

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


inputInitQuants = iIQConstruct inputInitQuantsTable

iIQConstruct :: (Referable l, HasShortName l) => l -> ConceptInstance
iIQConstruct req = cic "inputInitQuants" ( foldlSent [
  titleize input_, S "the following", plural quantity, S "described in",
  makeRef2S req `sC` S "which define the", phrase tank,
  plural parameter `sC` plural materialProprty, S "and initial",
  plural condition]) "Input-Initial-Quantities" funcReqDom

inputInitQuantsTable :: LabelledContent
inputInitQuantsTable = mkInputPropsTable inputs inputInitQuants

--
findMass = findMassConstruct inputInitQuants (plural mass)
            (foldlList Comma List $ map makeRef2S iMods)
            (foldlList Comma List [E inputInitQuantsEqn, E findMassEqn, makeRef2S assumpVCN])
            (ch wVol `isThe` phrase wVol `sAnd` ch tankVol `isThe` phrase tankVol)

findMassConstruct :: (Referable l, HasShortName l) => l -> Sentence ->
                                   Sentence -> Sentence -> Sentence -> ConceptInstance
findMassConstruct fr m ims exprs defs = cic "findMass" ( foldlSent [
  S "Use the", plural input_, S "in", makeRef2S fr, S "to find the", 
  m, S "needed for", ims `sC` S "using", exprs `sC` S "where", defs])
  "Find-Mass" funcReqDom -- FIXME: Equations shouldn't be inline

inputInitQuantsEqn, findMassEqn :: Expr --Fixme: rename labels

inputInitQuantsEqn = sy wMass $= sy wVol * sy wDensity $=
  (sy tankVol - sy pcmVol) * sy wDensity $=
  (sy pi_ * ((sy diam / 2) $^ 2) * sy tankLength - sy pcmVol) * sy wDensity -- FIXME: Ref Hack

findMassEqn = sy pcmMass $= sy pcmVol * sy pcmDensity -- FIXME: Ref Hack
--
checkWithPhysConsts = cic "checkWithPhysConsts" (foldlSent [
  S "Verify that the", plural input_, S "satisfy the required",
  plural physicalConstraint, S "shown in", makeRef2S (datCon ([]::[Contents]) ([]::[Section]))])
  "Check-Input-with-Physical_Constraints" funcReqDom
--
outputInputDerivQuants = oIDQConstruct oIDQQuants

oIDQConstruct :: [Sentence] -> ConceptInstance
oIDQConstruct x = cic "outputInputDerivQuants" (foldlSent_ [
  titleize output_, S "the", phrase input_, plural quantity `sAnd`
  S "derived", plural quantity +: S "in the following list"] +:+.
  foldlList Comma List x) "Output-Input-Derived-Quantities" funcReqDom

oIDQQuants :: [Sentence]
oIDQQuants = map foldlSent_ [
  [S "the", plural quantity, S "from", makeRef2S inputInitQuants],
  [S "the", plural mass, S "from", makeRef2S findMass],
  [ch balanceDecayRate, sParen (S "from" +:+ makeRef2S balanceDecayRate)],
  [ch balanceDecayTime, sParen (S "from" +:+ makeRef2S balanceDecayTime)],
  [ch balanceSolidPCM,  sParen (S "from" +:+ makeRef2S balanceSolidPCM)],
  [ch balanceLiquidPCM, sParen (S "from" +:+ makeRef2S balanceLiquidPCM)]
  ]
  
--
calcTempWtrOverTime = cic "calcTempWtrOverTime" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase tempW,
  sParen (ch tempW :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S eBalanceOnWtr)])
  "Calculate-Temperature-Water-Over-Time" funcReqDom
--
calcTempPCMOverTime = cic "calcTempPCMOverTime" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase tempPCM,
  sParen (ch tempPCM :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)])
  "Calculate-Temperature-PCM-Over-Time" funcReqDom
--
calcChgHeatEnergyWtrOverTime = cic "calcChgHeatEnergyWtrOverTime" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase watE,
  sParen (ch watE :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S heatEInWtr)])
  "Calculate-Change-Heat_Energy-Water-Over-Time" funcReqDom
--
calcChgHeatEnergyPCMOverTime = cic "calcChgHeatEnergyPCMOverTime" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase pcmE,
  sParen (ch pcmE :+: sParen (ch time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ makeRef2S heatEInPCM)])
  "Calculate-Change-Heat_Energy-PCM-Over-Time" funcReqDom
--
verifyEnergyOutput = cic "verifyEnergyOutput" (foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (ch watE :+: sParen (ch time) `sAnd` ch pcmE :+:
  sParen (ch time)), S "follow the", phrase CT.lawConsEnergy `sC`
  S "as outlined in", makeRef2S (propCorSol [] []) `sC`
  S "with relative error no greater than", ch consTol])
  "Verify-Energy-Output-Follow-Conservation-of-Energy" funcReqDom
--
calcPCMMeltBegin = cic "calcPCMMeltBegin" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "begins to melt",
  ch tInitMelt, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)])
  "Calculate-PCM-Melt-Begin-Time" funcReqDom
--
calcPCMMeltEnd = cic "calcPCMMeltEnd" (foldlSent [
  S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "stops", phrase CT.melting,
  ch tFinalMelt, sParen (S "from" +:+ makeRef2S eBalanceOnPCM)])
  "Calculate-PCM-Melt-End-Time" funcReqDom

-- List structure same between all examples

--How to include pi?
--How to add exponents?

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------

nfRequirements :: [ConceptInstance]
nfRequirements = [correct, verifiable, understandable, reusable, maintainable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  plural output_ `ofThe'` phrase code, S "have the",
  plural property, S "described in", makeRef2S (propCorSol [] [])
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

-- The second sentence of the above paragraph is repeated in all examples (not
-- exactly, but the general idea is). The first sentence is not always
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be
-- abstracted out.
