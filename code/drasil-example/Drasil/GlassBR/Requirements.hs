module Drasil.GlassBR.Requirements (funcReqs, funcReqsTables, inReqDesc, nonfuncReqs) where

import Control.Lens ((^.))

import Language.Drasil
import Drasil.DocLang (inReq, mkQRTuple, mkQRTupleRef, mkValsSourceTable)
import Drasil.DocLang.SRS (datCon, propCorSol)
import Theory.Drasil (DataDefinition)
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (assumption, characteristic, code,
  condition, datumConstraint, environment, funcReqDom, likelyChg, message, mg,
  mis, module_, nonFuncReqDom, output_, property, requirement, srs, system,
  traceyMatrix, type_, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.GlassBR.Assumptions (assumpSV, assumpGL, assumptionConstants)
import Drasil.GlassBR.Concepts (glass)
import Drasil.GlassBR.DataDefs (aspRat, dimLL, glaTyFac, hFromt, loadDF, nonFL, 
  risk, standOffDis, strDisFac, tolPre, tolStrDisFac)
import Drasil.GlassBR.IMods (iMods)
import Drasil.GlassBR.TMods (lrIsSafe, pbIsSafe)
import Drasil.GlassBR.Unitals (blast, isSafeLR, isSafePb, loadSF, notSafe,
  pbTolfail, safeMessage)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownValues, checkGlassSafety, outputValues]

funcReqsTables :: [LabelledContent]
funcReqsTables = [sysSetValsFollowingAssumpsTable, outputValuesTable]

sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownValues, checkGlassSafety, outputValues :: ConceptInstance

sysSetValsFollowingAssumps = cic "sysSetValsFollowingAssumps" sysSetValsFollowingAssumpsDesc "System-Set-Values-Following-Assumptions" funcReqDom
checkInputWithDataCons     = cic "checkInputWithDataCons"     checkInputWithDataConsDesc     "Check-Input-with-Data_Constraints"       funcReqDom
outputValsAndKnownValues   = cic "outputValsAndKnownValues"   outputValsAndKnownValuesDesc   "Output-Values-and-Known-Values"          funcReqDom
checkGlassSafety           = cic "checkGlassSafety"           checkGlassSafetyDesc           "Check-Glass-Safety"                      funcReqDom
outputValues               = cic "outputValues"               outputValuesDesc               "Output-Values"                           funcReqDom

inReqDesc, sysSetValsFollowingAssumpsDesc, checkInputWithDataConsDesc, outputValsAndKnownValuesDesc, checkGlassSafetyDesc :: Sentence

inReqDesc = foldlList Comma List [S "the" +:+ phrase glass +:+ plural dimension,
  phrase type_ `S.sOf` phrase glass, phrase pbTolfail, plural characteristic `S.the_ofThe` phrase blast]

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
  plural inValue, S "to ensure that they do not exceed the", plural datumConstraint,
  S "mentioned in" +:+. makeRef2S (datCon ([]::[Contents]) ([]::[Section])), 
  S "If any" `S.ofThe` plural inValue, S "are out" `S.sOf` S "bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `S.andThe` plural calculation, S "stop"]

outputValsAndKnownValuesDesc = foldlSent [titleize output_, S "the", plural inValue,
  S "from", makeRef2S (inReq EmptyS) `S.andThe` S "known", plural value,
  S "from", makeRef2S sysSetValsFollowingAssumps]

checkGlassSafetyDesc = foldlSent_ [S "If", E (sy isSafePb $&& sy isSafeLR),
  sParen (S "from" +:+ makeRef2S pbIsSafe `S.sAnd` makeRef2S lrIsSafe) `sC`
  phrase output_, S "the", phrase message, Quote (safeMessage ^. defn),
  S "If the", phrase condition, S "is false, then", phrase output_,
  S "the", phrase message, Quote (notSafe ^. defn)]

outputValuesDesc :: Sentence
outputValuesDesc = foldlSent [titleize output_, S "the", plural value, S "from", makeRef2S outputValuesTable]

outputValuesTable :: LabelledContent
outputValuesTable = mkValsSourceTable (mkQRTuple iMods ++ mkQRTuple r6DDs) "ReqOutputs"
                              (S "Required" +:+ titleize' output_ `follows` outputValues)
  where
    r6DDs :: [DataDefinition]
    r6DDs = [risk, strDisFac, nonFL, glaTyFac, dimLL, tolPre, tolStrDisFac, hFromt, aspRat]

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  plural output_ `S.the_ofThe'` phrase code, S "have the",
  plural property, S "described in", makeRef2S (propCorSol [] [])
  ]) "Correct" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  S "The", phrase code, S "is tested with complete",
  phrase vavPlan]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "The", phrase code, S "is modularized with complete",
  phrase mg `S.sAnd` phrase mis]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  S "The", phrase code, S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix `S.inThe` getAcc srs `S.sAnd` phrase mg]) "Maintainable" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  S "The", phrase code, S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom
