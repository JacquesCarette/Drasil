module Drasil.SWHS.Requirements where --all of this file is exported

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (InstanceModel, HasOutput(output))

import Drasil.DocLang (inReq)
import Drasil.DocLang.SRS (datCon, propCorSol) 

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (assumption, code, condition,
  funcReqDom, input_, likelyChg, mg, mis, module_, nonFuncReqDom, output_,
  physicalConstraint, property, propOfCorSol, requirement, srs, traceyMatrix,
  unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)
import Data.Drasil.Concepts.Thermodynamics as CT (lawConsEnergy, melting)

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (energy, time)

import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.SWHS.DataDefs (waterMass, waterVolume, tankVolume, 
  balanceDecayRate, balanceDecayTime, balanceSolidPCM, balanceLiquidPCM)
import Drasil.SWHS.Concepts (phsChgMtrl, tank)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM, 
  iMods)
import Drasil.SWHS.Unitals (consTol, pcmE, tFinalMelt, tInitMelt, watE)

import Control.Lens ((^.))

------------------------------
-- Data Constraint: Table 1 --
------------------------------

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------
-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

inReqDesc :: Sentence
inReqDesc = foldlList Comma List [pluralNP (NP.the (combineNINI tank parameter)),
  plural materialProprty, S "initial" +:+ plural condition]

funcReqs :: [ConceptInstance]
funcReqs = [findMass, checkWithPhysConsts, outputInputDerivVals,
  calcValues swhsOutputs, verifyEnergyOutput, calcPCMMeltBegin, calcPCMMeltEnd,
  outputValues swhsOutputs]

findMass, checkWithPhysConsts, outputInputDerivVals, verifyEnergyOutput,
  calcPCMMeltBegin, calcPCMMeltEnd :: ConceptInstance

calcValues, outputValues :: [InstanceModel] -> ConceptInstance

--
findMass = findMassConstruct (inReq EmptyS) (plural mass) iMods 
  [waterMass, waterVolume, tankVolume]

findMassConstruct :: (Referable r, HasShortName r, Referable s, HasShortName s,
  Referable t, HasShortName t) => r -> Sentence -> [s] -> [t] -> ConceptInstance
findMassConstruct fr m ims ddefs = cic "findMass" (foldlSent [
  S "Use the", plural input_ `S.in_` refS fr, S "to find the", 
  m, S "needed for", foldlList Comma List (map refS ims) `sC`
  S "using", foldlList Comma List (map refS ddefs)])
  "Find-Mass" funcReqDom
--
checkWithPhysConsts = cic "checkWithPhysConsts" (foldlSent [
  S "Verify that", pluralNP (the input_), S "satisfy the required",
  namedRef (datCon [] []) (plural physicalConstraint)])
  "Check-Input-with-Physical_Constraints" funcReqDom
--
outputInputDerivVals = oIDQConstruct oIDQVals

oIDQConstruct :: [Sentence] -> ConceptInstance
oIDQConstruct x = cic "outputInputDerivVals" (foldlSentCol [
  titleize output_, pluralNP (the inValue) `S.and_`
  S "derived", plural value `S.inThe` S "following list"] +:+.
  foldlList Comma List x) "Output-Input-Derived-Values" funcReqDom

oIDQVals :: [Sentence]
oIDQVals = map foldlSent_ [
  [pluralNP (the value), fromSource (inReq EmptyS)],
  [pluralNP (the mass), fromSource findMass],
  [ch (balanceDecayRate ^. defLhs), fromSource balanceDecayRate],
  [ch (balanceDecayTime ^. defLhs), fromSource balanceDecayTime],
  [ch (balanceSolidPCM ^. defLhs),  fromSource balanceSolidPCM],
  [ch (balanceLiquidPCM ^. defLhs), fromSource balanceLiquidPCM]
  ]

--
calcValues l = cic "calcValues" (S "Calculate the following" +: plural value +:+.
  outputList l) "Calculate-Values" funcReqDom
--
verifyEnergyOutput = cic "verifyEnergyOutput" (foldlSent [
  S "Verify that the", phrase energy, plural output_,
  sParen (ch watE :+: sParen (ch time) `S.and_` ch pcmE :+:
  sParen (ch time)), S "follow the", phrase CT.lawConsEnergy `sC`
  S "as outlined in", namedRef (propCorSol [] []) (titleize' propOfCorSol) `sC`
  S "with relative error no greater than", ch consTol])
  "Verify-Energy-Output-Follow-Conservation-of-Energy" funcReqDom
--
calcPCMMeltBegin = cic "calcPCMMeltBegin" (foldlSent [
  S "Calculate and", phrase output_, phraseNP (the time),
  S "at which the", short phsChgMtrl, S "begins to melt",
  ch tInitMelt, fromSource eBalanceOnPCM])
  "Calculate-PCM-Melt-Begin-Time" funcReqDom
--
calcPCMMeltEnd = cic "calcPCMMeltEnd" (foldlSent [
  S "Calculate and", phrase output_, phraseNP (the time),
  S "at which the", short phsChgMtrl, S "stops", phrase CT.melting,
  ch tFinalMelt, fromSource eBalanceOnPCM])
  "Calculate-PCM-Melt-End-Time" funcReqDom
--
outputValues l = cic "outputValues" (titleize output_ +:+. outputList l)
  "Output-Values" funcReqDom

outputList :: [InstanceModel] -> Sentence
outputList l = foldlList Comma List $
  map (\x -> ch (x ^. output) :+: sParen (ch time) +:+ fromSource x) l

swhsOutputs :: [InstanceModel]
swhsOutputs = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

-- List structure same between all examples

--How to include pi?
--How to add exponents?

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------

nfRequirements :: [ConceptInstance]
nfRequirements = [correct, verifiable, understandable, reusable, maintainable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [atStartNP'
  (output_ `the_ofThePS` code), S "have the",
  plural property, S "described in", namedRef (propCorSol [] []) (titleize' propOfCorSol)
  ]) "Correct" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  atStartNP (the code), S "is tested with complete",
  phrase vavPlan]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  atStartNP (the code), S "is modularized with complete",
  phrase mg `S.and_` phrase mis]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  atStartNP (the code), S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix, S "in the", getAcc srs `S.and_` phrase mg]) "Maintainable" nonFuncReqDom

-- The second sentence of the above paragraph is repeated in all examples (not
-- exactly, but the general idea is). The first sentence is not always
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be
-- abstracted out.
