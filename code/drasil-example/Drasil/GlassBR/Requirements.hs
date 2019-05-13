module Drasil.GlassBR.Requirements (funcReqsList, nonfuncReqs, funcReqs,
  inputGlassPropsTable, propsDeriv) where

import Control.Lens ((^.))
import Data.Function (on)
import Data.List (sortBy)

import Language.Drasil
import Drasil.DocLang (mkEnumSimple, mkListTuple)
import Drasil.DocLang.SRS (datCon, propCorSol)
import qualified Drasil.DocumentLanguage.Units as U (toSentence)

import Data.Drasil.Concepts.Computation (inParam, inQty, inValue)
import Data.Drasil.Concepts.Documentation (assumption, characteristic, code,
  condition, dataDefn, datumConstraint, description, environment, failure,
  funcReqDom, genDefn, inModel, input_, likelyChg, message, mg, mis, module_,
  nonFuncReqDom, output_, property, quantity, requirement, srs, symbol_, system,
  thModel, traceyMatrix, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation, probability)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), andThe, 
  foldlList, foldlSent, foldlSent_, foldlSP, follows, ofThe, ofThe', sAnd, sOf)
import Data.Drasil.Utils (bulletFlat)

import Drasil.GlassBR.Assumptions (assumpSV, assumpGL, assumptionConstants)
import Drasil.GlassBR.Concepts (glass, lShareFac)
import Drasil.GlassBR.DataDefs (aspRat, dimLL, glaTyFac, hFromt, loadDF, nonFL, 
  risk, standOffDis, strDisFac, tolPre, tolStrDisFac)
import Drasil.GlassBR.IMods (gbrIMods)
import Drasil.GlassBR.TMods (lrIsSafe, pbIsSafe)
import Drasil.GlassBR.Unitals (blast, charWeight, glassTy, glass_type, 
  isSafeLR, isSafePb, nomThick, notSafe, pbTol, plateLen, plateWidth, 
  safeMessage, sdx, sdy, sdz, tNT)

{--Functional Requirements--}

funcReqsList :: [Contents]
funcReqsList = (mkEnumSimple (uncurry $ flip mkReqCI) $ zip funcReqs
  funcReqsDetails) ++ [LlC inputGlassPropsTable]

mkReqCI :: (Definition c, HasShortName c, Referable c) => [Sentence] -> c -> ListTuple
mkReqCI e = mkListTuple $ if null e then \x -> Flat $ x ^. defn else
  \x -> Nested (x ^. defn) $ bulletFlat e

funcReqs :: [ConceptInstance]
funcReqs = [inputGlassProps, sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownQuants, checkGlassSafety, outputQuants]

funcReqsDetails :: [[Sentence]]
funcReqsDetails = [[], sysSetValsFollowingAssumpsList, [], [], [],
  chunksToSent outputQuantsList]

inputGlassProps, sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownQuants, checkGlassSafety, outputQuants :: ConceptInstance

inputGlassProps            = cic "inputGlassProps"            inputGlassPropsDesc              "Input-Glass-Props"                       funcReqDom
sysSetValsFollowingAssumps = cic "sysSetValsFollowingAssumps" sysSetValsFollowingAssumpsDesc   "System-Set-Values-Following-Assumptions" funcReqDom
checkInputWithDataCons     = cic "checkInputWithDataCons"     checkInputWithDataConsDesc       "Check-Input-with-Data_Constraints"       funcReqDom
outputValsAndKnownQuants   = cic "outputValsAndKnownQuants"   outputValsAndKnownQuantsDesc     "Output-Values-and-Known-Quantities"      funcReqDom
checkGlassSafety           = cic "checkGlassSafety"           (checkGlassSafetyDesc output_)   "Check-Glass-Safety"                      funcReqDom
outputQuants               = cic "outputQuants"               outputQuantsDesc                 "Output-Quantities"                       funcReqDom

inputGlassPropsDesc, checkInputWithDataConsDesc, outputValsAndKnownQuantsDesc :: Sentence
checkGlassSafetyDesc :: NamedChunk -> Sentence

inputGlassPropsDesc = foldlSent [at_start input_, S "the", plural quantity, S "from",
  makeRef2S inputGlassPropsTable `sC` S "which define the" +:+ foldlList Comma List
  [phrase glass +:+ plural dimension, (glassTy ^. defn), S "tolerable" +:+
  phrase probability `sOf` phrase failure, (plural characteristic `ofThe` 
  phrase blast)]]

inputGlassPropsTable :: LabelledContent
inputGlassPropsTable = llcc (makeTabRef "InputGlassPropsReqInputs") $ 
  Table
  [at_start symbol_, at_start description, S "Units"]
  (mkTable
  [ch,
   at_start, U.toSentence] requiredInputs)
  (S "Required Inputs following" +:+ makeRef2S inputGlassProps) True
  where
    requiredInputs :: [QuantityDict]
    requiredInputs = (map qw [plateLen, plateWidth, charWeight])
      ++ (map qw [pbTol, tNT]) ++ (map qw [sdx, sdy, sdz])
      ++ (map qw [glass_type, nomThick])

sysSetValsFollowingAssumpsDesc :: Sentence
sysSetValsFollowingAssumpsDesc = foldlSent_ [S "The", phrase system, S "shall set the known",
    plural value +: S "as follows"]

sysSetValsFollowingAssumpsList :: [Sentence]
sysSetValsFollowingAssumpsList = [foldlList Comma List (map ch (take 4 assumptionConstants)) `follows` assumpSV,
  ch loadDF +:+ S "from" +:+ makeRef2S loadDF, 
  short lShareFac `follows` assumpGL,
  ch hFromt +:+ S "from" +:+ makeRef2S hFromt,
  ch glaTyFac +:+ S "from" +:+ makeRef2S glaTyFac,
  ch standOffDis +:+ S "from" +:+ makeRef2S standOffDis,
  ch aspRat +:+ S "from" +:+ makeRef2S aspRat]

--FIXME:should constants, LDF, and LSF have some sort of field that holds
-- the assumption(s) that're being followed? (Issue #349)

checkInputWithDataConsDesc = foldlSent [S "The", phrase system, S "shall check the entered",
  plural inValue, S "to ensure that they do not exceed the",
  plural datumConstraint, S "mentioned in" +:+. (makeRef2S $ datCon ([]::[Contents]) ([]::[Section])), 
  S "If any" `sOf` S "the", plural inParam, S "are out" `sOf` S "bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `andThe` plural calculation, S "stop"]

outputValsAndKnownQuantsDesc = foldlSent [titleize output_, S "the", plural inQty,
  S "from", makeRef2S inputGlassProps `andThe` S "known", plural quantity,
  S "from", makeRef2S sysSetValsFollowingAssumps]

checkGlassSafetyDesc cmd = foldlSent_ [S "If", (ch isSafePb), S "∧", (ch isSafeLR),
  sParen (S "from" +:+ (makeRef2S pbIsSafe)
  `sAnd` (makeRef2S lrIsSafe)), S "are true" `sC`
  phrase cmd, S "the", phrase message, Quote (safeMessage ^. defn),
  S "If the", phrase condition, S "is false, then", phrase cmd,
  S "the", phrase message, Quote (notSafe ^. defn)]


outputQuantsDesc :: Sentence
outputQuantsDesc = titleize output_ +:+ S "the following" +: plural quantity

chunksToSent :: [(Sentence, Symbol, Sentence)] -> [Sentence]
chunksToSent = map (\(a, b, c) -> a +:+ sParen (P b) +:+ c)

outputQuantsList :: [(Sentence, Symbol, Sentence)]
outputQuantsList = sortBy (compsy `on` get2) $ (mkReqList gbrIMods) ++ (mkReqList r6DDs)
  where
    r6DDs :: [DataDefinition]
    r6DDs = [risk, strDisFac, nonFL, glaTyFac, dimLL, tolPre, tolStrDisFac, hFromt, aspRat]
    get2 (_, b, _) = b

mkReqList :: (NamedIdea c, HasSymbol c, HasShortName c, Referable c) => [c] -> [(Sentence, Symbol, Sentence)]
mkReqList = map (\c -> (at_start c, symbol c Implementation, sParen (makeRef2S c)))

{--Functional Requirements--}

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
