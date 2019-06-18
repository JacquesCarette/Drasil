module Drasil.Projectile.Requirements (funcReqs, inputParamsTable,
  nonfuncReqs, propsDeriv) where

import Language.Drasil
import Drasil.DocLang (mkInputPropsTable)
import Drasil.DocLang.SRS (datCon, propCorSol)
import Utils.Drasil

import Data.Drasil.Concepts.Computation (inParam)
import Data.Drasil.Concepts.Documentation (assumption, code, datumConstraint,
  environment, funcReqDom, input_, likelyChg, mg, mis, module_,
  nonFuncReqDom, output_, property, quantity, requirement, srs, traceyMatrix,
  unlikelyChg, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, thModel)

import Drasil.Projectile.IMods (distanceIM, messageIM, offsetIM, shortIM, timeIM)
import Drasil.Projectile.Unitals (flightDur, inputs, isShort, landPos,
  launAngle, launSpeed, message, offset, targPos)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [inputParams, verifyParams, calcValues, outputValues]

inputParams, verifyParams, calcValues, outputValues :: ConceptInstance

inputParams  = cic "inputParams"  inputParamsDesc  "Input-Parameters"  funcReqDom
verifyParams = cic "verifyParams" verifyParamsDesc "Verify-Parameters" funcReqDom
calcValues   = cic "calcValues"   calcValuesDesc   "Calculate-Values"  funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values"     funcReqDom

inputParamsTable :: LabelledContent
inputParamsTable = mkInputPropsTable inputs inputParams

inputParamsDesc, verifyParamsDesc, calcValuesDesc, outputValuesDesc :: Sentence
inputParamsDesc = foldlSent [atStart input_, S "the", plural quantity, S "from",
  makeRef2S inputParamsTable `sC` S "which define the" +:+
  foldlList Comma List (map phrase [launAngle, launSpeed, targPos])]
verifyParamsDesc = foldlSent [S "Check the entered", plural inParam,
  S "to ensure that they do not exceed the", plural datumConstraint,
  S "mentioned in" +:+. makeRef2S (datCon ([]::[Contents]) ([]::[Section])), 
  S "If any of the", plural inParam, S "are out of bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `andThe` plural calculation, S "stop"]
calcValuesDesc = foldlSent [S "Calculate the following" +: plural quantity,
  foldlList Comma List [
    ch flightDur +:+ sParen (S "from" +:+ makeRef2S timeIM),
    ch landPos   +:+ sParen (S "from" +:+ makeRef2S distanceIM),
    ch isShort   +:+ sParen (S "from" +:+ makeRef2S shortIM),
    ch offset    +:+ sParen (S "from" +:+ makeRef2S offsetIM),
    ch message   +:+ sParen (S "from" +:+ makeRef2S messageIM)
  ]]
outputValuesDesc = foldlSent [atStart output_, ch message,
  sParen (S "from" +:+ makeRef2S messageIM) `sAnd` ch offset,
  sParen (S "from" +:+ makeRef2S offsetIM)]

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

propsDeriv :: [Contents]
propsDeriv = [foldlSP [S "FIXME"]]

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

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  S "The", phrase code, S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom

