module Drasil.SWHS.Requirements where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (InstanceModel, HasOutput(output))

import Drasil.DocLang (mkMaintainableNFR, mkCorrectNFR, mkVerifiableNFR,
  mkUnderstandableNFR, mkReusableNFR, inReqWTab)
import Drasil.DocLang.SRS (datCon, propCorSol)

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (condition, funcReqDom, input_, output_,
  physicalConstraint, propOfCorSol, value)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)
import Data.Drasil.Concepts.Thermodynamics as CT (lawConsEnergy, melting)

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (energy, time)

import Drasil.SWHS.DataDefs (waterMass, waterVolume, tankVolume,
  balanceDecayRate, balanceDecayTime, balanceSolidPCM, balanceLiquidPCM)
import Drasil.SWHS.Concepts (phsChgMtrl, tank)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM,
  iMods)
import Drasil.SWHS.Unitals (consTol, pcmE, tFinalMelt, tInitMelt, watE, inputs)

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

funcReqs :: [ConceptInstance]
funcReqs = [inputValues, findMass, checkWithPhysConsts, outputInputDerivVals,
  calcValues swhsOutputs, verifyEnergyOutput, calcPCMMeltBegin, calcPCMMeltEnd,
  outputValues swhsOutputs]

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable]

inReqDesc :: Sentence
inReqDesc = foldlList Comma List [D.toSent (pluralNP (NP.the (combineNINI tank parameter))),
  plural materialProprty, S "initial" +:+ plural condition]

inputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab (Just inReqDesc) inputs

findMass, checkWithPhysConsts, outputInputDerivVals, verifyEnergyOutput,
  calcPCMMeltBegin, calcPCMMeltEnd :: ConceptInstance

calcValues, outputValues :: [InstanceModel] -> ConceptInstance

--
findMass = findMassConstruct inputValues (plural mass) iMods
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
  S "Verify that", D.toSent (pluralNP (the input_)), S "satisfy the required",
  namedRef (datCon [] []) (plural physicalConstraint)])
  "Check-Input-with-Physical_Constraints" funcReqDom
--
outputInputDerivVals = oIDQConstruct oIDQVals

oIDQConstruct :: [Sentence] -> ConceptInstance
oIDQConstruct x = cic "outputInputDerivVals" (foldlSentCol [
  titleize output_, D.toSent (pluralNP (the inValue)) `S.and_`
  S "derived", plural value `S.inThe` S "following list"] +:+.
  foldlList Comma List x) "Output-Input-Derived-Values" funcReqDom

oIDQVals :: [Sentence]
oIDQVals = map foldlSent_ [
  [D.toSent (pluralNP (the value)), fromSource inputValues],
  [D.toSent (pluralNP (the mass)), fromSource findMass],
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
  S "Calculate and", phrase output_, D.toSent (phraseNP (the time)),
  S "at which the", short phsChgMtrl, S "begins to melt",
  ch tInitMelt, fromSource eBalanceOnPCM])
  "Calculate-PCM-Melt-Begin-Time" funcReqDom
--
calcPCMMeltEnd = cic "calcPCMMeltEnd" (foldlSent [
  S "Calculate and", phrase output_, D.toSent (phraseNP (the time)),
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
correct = mkCorrectNFR "correct" "Correctness"

verifiable :: ConceptInstance
verifiable = mkVerifiableNFR "verifiable" "Verifiability"

understandable :: ConceptInstance
understandable = mkUnderstandableNFR "understandable" "Understandability"

reusable :: ConceptInstance
reusable = mkReusableNFR "reusable" "Reusability"

maintainable :: ConceptInstance
maintainable = mkMaintainableNFR "maintainable" 10 "Maintainability"
