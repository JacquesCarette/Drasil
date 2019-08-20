module Drasil.GlassBR.Requirements (funcReqs, funcReqsTables, nonfuncReqs) where

import Control.Lens ((^.))

import Language.Drasil
import Drasil.DocLang (mkInputPropsTable, mkQRTuple, mkQRTupleRef, mkValsSourceTable)
import Drasil.DocLang.SRS (datCon, propCorSol)
import Theory.Drasil (DataDefinition)
import Utils.Drasil

import Data.Drasil.Concepts.Computation (inParam, inQty, inValue)
import Data.Drasil.Concepts.Documentation (assumption, characteristic, code,
  condition, datumConstraint, environment, failure, funcReqDom, input_,
  likelyChg, message, mg, mis, module_, nonFuncReqDom, output_, property,
  quantity, requirement, srs, system, traceyMatrix, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation, probability)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, thModel)

import Drasil.GlassBR.Assumptions (assumpSV, assumpGL, assumptionConstants)
import Drasil.GlassBR.Concepts (glass)
import Drasil.GlassBR.DataDefs (aspRat, dimLL, glaTyFac, hFromt, loadDF, nonFL, 
  risk, standOffDis, strDisFac, tolPre, tolStrDisFac)
import Drasil.GlassBR.IMods (iMods)
import Drasil.GlassBR.TMods (lrIsSafe, pbIsSafe)
import Drasil.GlassBR.Unitals (inputs, blast, glassTy, isSafeLR, isSafePb, 
  loadSF, notSafe, safeMessage)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [inputGlassProps, sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownQuants, checkGlassSafety, outputQuants]

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputGlassPropsTable, sysSetValsFollowingAssumpsTable, outputQuantsTable]

inputGlassProps, sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownQuants, checkGlassSafety, outputQuants :: ConceptInstance

inputGlassProps            = cic "inputGlassProps"            inputGlassPropsDesc            "Input-Glass-Props"                       funcReqDom
sysSetValsFollowingAssumps = cic "sysSetValsFollowingAssumps" sysSetValsFollowingAssumpsDesc "System-Set-Values-Following-Assumptions" funcReqDom
checkInputWithDataCons     = cic "checkInputWithDataCons"     checkInputWithDataConsDesc     "Check-Input-with-Data_Constraints"       funcReqDom
outputValsAndKnownQuants   = cic "outputValsAndKnownQuants"   outputValsAndKnownQuantsDesc   "Output-Values-and-Known-Quantities"      funcReqDom
checkGlassSafety           = cic "checkGlassSafety"           checkGlassSafetyDesc           "Check-Glass-Safety"                      funcReqDom
outputQuants               = cic "outputQuants"               outputQuantsDesc               "Output-Quantities"                       funcReqDom

inputGlassPropsDesc, checkInputWithDataConsDesc, outputValsAndKnownQuantsDesc, checkGlassSafetyDesc :: Sentence

inputGlassPropsDesc = foldlSent [atStart input_, S "the", plural quantity, S "from",
  makeRef2S inputGlassPropsTable `sC` S "which define the" +:+ foldlList Comma List
  [phrase glass +:+ plural dimension, glassTy ^. defn, S "tolerable" +:+
  phrase probability `sOf` phrase failure, plural characteristic `ofThe` 
  phrase blast]]

inputGlassPropsTable :: LabelledContent
inputGlassPropsTable = mkInputPropsTable inputs inputGlassProps

sysSetValsFollowingAssumpsDesc :: Sentence
sysSetValsFollowingAssumpsDesc = foldlSent [S "The", phrase system, S "shall set the known",
    plural value, S "as described in", makeRef2S sysSetValsFollowingAssumpsTable]

sysSetValsFollowingAssumpsTable :: LabelledContent
sysSetValsFollowingAssumpsTable = mkValsSourceTable (mkQRTupleRef r2AQs r2ARs ++ mkQRTuple r2DDs) "ReqAssignments"
                                  (S "Required Assignments" `follows` sysSetValsFollowingAssumps)
  where
    r2AQs = qw loadSF   : map qw (take 4 assumptionConstants)
    r2ARs = assumpGL : replicate 4 assumpSV
    r2DDs = [loadDF, hFromt, glaTyFac, standOffDis, aspRat]

--FIXME:should constants, LDF, and LSF have some sort of field that holds
-- the assumption(s) that're being followed? (Issue #349)

checkInputWithDataConsDesc = foldlSent [S "The", phrase system, S "shall check the entered",
  plural inValue, S "to ensure that they do not exceed the",
  plural datumConstraint, S "mentioned in" +:+. makeRef2S (datCon ([]::[Contents]) ([]::[Section])), 
  S "If any" `sOf` S "the", plural inParam, S "are out" `sOf` S "bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `andThe` plural calculation, S "stop"]

outputValsAndKnownQuantsDesc = foldlSent [titleize output_, S "the", plural inQty,
  S "from", makeRef2S inputGlassProps `andThe` S "known", plural quantity,
  S "from", makeRef2S sysSetValsFollowingAssumps]

checkGlassSafetyDesc = foldlSent_ [S "If", E (sy isSafePb $&& sy isSafeLR),
  sParen (S "from" +:+ makeRef2S pbIsSafe `sAnd` makeRef2S lrIsSafe) `sC`
  phrase output_, S "the", phrase message, Quote (safeMessage ^. defn),
  S "If the", phrase condition, S "is false, then", phrase output_,
  S "the", phrase message, Quote (notSafe ^. defn)]

outputQuantsDesc :: Sentence
outputQuantsDesc = foldlSent [titleize output_, S "the", plural quantity, S "from", makeRef2S outputQuantsTable]

outputQuantsTable :: LabelledContent
outputQuantsTable = mkValsSourceTable (mkQRTuple iMods ++ mkQRTuple r6DDs) "ReqOutputs"
                              (S "Required" +:+ titleize' output_ `follows` outputQuants)
  where
    r6DDs :: [DataDefinition]
    r6DDs = [risk, strDisFac, nonFL, glaTyFac, dimLL, tolPre, tolStrDisFac, hFromt, aspRat]

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

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

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  S "The", phrase code, S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom
