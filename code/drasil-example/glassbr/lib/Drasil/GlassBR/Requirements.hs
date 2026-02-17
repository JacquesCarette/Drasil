module Drasil.GlassBR.Requirements (funcReqs, funcReqsTables, inReqDesc, nonfuncReqs) where

import Control.Lens ((^.))

import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (characteristic, condition,
  datumConstraint, funcReqDom, message, output_, system,
  type_, value)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Software (errMsg)

import Drasil.DocLang (inReqWTab, mkQRTuple, mkQRTupleRef, mkValsSourceTable,
  mkMaintainableNFR, mkPortableNFR, mkCorrectNFR, mkVerifiableNFR,
  mkUnderstandableNFR, mkReusableNFR)
import Drasil.DocLang.SRS (datCon)

import Theory.Drasil (DataDefinition)

import Drasil.GlassBR.Assumptions (assumpSV, assumpGL, assumptionConstants)
import Drasil.GlassBR.Concepts (glass)
import Drasil.GlassBR.DataDefs (aspRat, glaTyFac, hFromt, loadDFDD, standOffDis)
import Drasil.GlassBR.IMods (iMods, pbIsSafe, lrIsSafe)
import Drasil.GlassBR.Unitals (blast, isSafeLR, isSafePb, loadSF, notSafe,
  pbTolfail, safeMessage, inputs)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [inputValues, sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownValues, checkGlassSafety, outputValues]

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable, sysSetValsFollowingAssumpsTable, outputValuesTable]

sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownValues, checkGlassSafety, outputValues :: ConceptInstance

sysSetValsFollowingAssumps = cic "sysSetValsFollowingAssumps" sysSetValsFollowingAssumpsDesc "System-Set-Values-Following-Assumptions" funcReqDom
checkInputWithDataCons     = cic "checkInputWithDataCons"     checkInputWithDataConsDesc     "Check-Input-with-Data_Constraints"       funcReqDom
outputValsAndKnownValues   = cic "outputValsAndKnownValues"   outputValsAndKnownValuesDesc   "Output-Values-and-Known-Values"          funcReqDom
checkGlassSafety           = cic "checkGlassSafety"           checkGlassSafetyDesc           "Check-Glass-Safety"                      funcReqDom
outputValues               = cic "outputValues"               outputValuesDesc               "Output-Values"                           funcReqDom

inReqDesc, sysSetValsFollowingAssumpsDesc, checkInputWithDataConsDesc, outputValsAndKnownValuesDesc, checkGlassSafetyDesc :: Sentence

inReqDesc = foldlList Comma List [D.toSent $ pluralNP (NP.the (combineNINI glass dimension)),
  D.toSent $ phraseNP (type_ `of_` glass), phrase pbTolfail,
  D.toSent $ pluralNP (characteristic `the_ofThePS` blast)]

sysSetValsFollowingAssumpsDesc = foldlSent [D.toSent $ atStartNP (the system), S "shall set the known",
    plural value, S "as described in the table for", namedRef sysSetValsFollowingAssumpsTable (S "Required Assignments")]

inputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab (Just inReqDesc) inputs

sysSetValsFollowingAssumpsTable :: LabelledContent
sysSetValsFollowingAssumpsTable =
  mkValsSourceTable
    (mkQRTupleRef r2AQs r2ARs ++ mkQRTuple r2DDs)
    "ReqAssignments"
    (S "Required Assignments")
  where
    r2AQs = loadSF : map dqdWr (take 4 assumptionConstants)
    r2ARs = assumpGL : replicate 4 assumpSV
    r2DDs = [loadDFDD, hFromt, glaTyFac, standOffDis, aspRat]

--FIXME:should constants, LDF, and LSF have some sort of field that holds
-- the assumption(s) that're being followed? (Issue #349)

checkInputWithDataConsDesc = foldlSent [D.toSent $ atStartNP (the system), S "shall check the entered",
  plural inValue, S "to ensure that they do not exceed the" +:+. namedRef (datCon [] []) (plural datumConstraint),
  S "If any" `S.ofThe` plural inValue, S "are out" `S.of_` S "bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `S.andThe` plural calculation, S "stop"]

outputValsAndKnownValuesDesc = foldlSent [titleize output_, D.toSent $ pluralNP (the inValue),
  S "from", refS inputValues `S.andThe` S "known", plural value,
  S "from", refS sysSetValsFollowingAssumps]

checkGlassSafetyDesc = foldlSent_ [S "If", eS $ sy isSafePb $&& sy isSafeLR,
  sParen (S "from" +:+ refS pbIsSafe `S.and_` refS lrIsSafe) `sC`
  phrase output_, D.toSent $ phraseNP (the message), Quote (safeMessage ^. defn),
  S "If the", phrase condition, S "is false, then", phrase output_,
  D.toSent $ phraseNP (the message), Quote (notSafe ^. defn)]

outputValuesDesc :: Sentence
outputValuesDesc = foldlSent [titleize output_, D.toSent $ pluralNP (the value), S "from the table for", namedRef outputValuesTable (S "Required Outputs")]

outputValuesTable :: LabelledContent
outputValuesTable = mkValsSourceTable (mkQRTuple iMods ++ mkQRTuple r6DDs) "ReqOutputs"
                              (S "Required" +:+ titleize' output_)
  where
    r6DDs :: [DataDefinition]
    r6DDs = [glaTyFac, hFromt, aspRat]

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

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

portable :: ConceptInstance
portable = mkPortableNFR "portable" ["Windows", "Mac OSX", "Linux"] "Portablity"
