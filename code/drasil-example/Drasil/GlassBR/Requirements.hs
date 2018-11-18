module Drasil.GlassBR.Requirements (funcReqsList, funcReqs) where

import Control.Lens ((^.))
import Data.Function (on)
import Data.List (sortBy)

import Language.Drasil
import Drasil.DocLang (mkEnumSimple, mkListTuple)
import Drasil.DocLang.SRS (datConLabel)
import qualified Drasil.DocumentLanguage.Units as U (toSentence)

import Data.Drasil.Concepts.Computation (inParam, inQty, inValue)
import Data.Drasil.Concepts.Documentation (characteristic, condition,
  datumConstraint, description, failure, funcReqDom, input_, message, output_,
  quantity, symbol_, system, value)
import Data.Drasil.Concepts.Math (calculation, probability)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), andThe, 
  foldlList, foldlSent, foldlSent_, followA, ofThe, sAnd, sOf)
import Data.Drasil.SI_Units (metre, millimetre)
import Data.Drasil.Utils (bulletFlat)

import Drasil.GlassBR.Assumptions (standardValues, glassLite, assumptionConstants)
import Drasil.GlassBR.Concepts (glass, lShareFac)
import Drasil.GlassBR.DataDefs (aspRat, dimLL, glaTyFac, hFromt, loadDF, nonFL, 
  risk, standOffDis, strDisFac, tolPre, tolStrDisFac)
import Drasil.GlassBR.IMods (gbrIMods)
import Drasil.GlassBR.TMods (lrIsSafe, pbIsSafe)
import Drasil.GlassBR.Unitals (blast, char_weight, glassTy, glass_type, 
  is_safeLR, is_safePb, nom_thick, notSafe, pb_tol, plate_len, plate_width, 
  safeMessage, sdx, sdy, sdz, tNT)

{--Functional Requirements--}

funcReqsList :: [Contents]
funcReqsList = (mkEnumSimple (uncurry $ flip mkReqCI) $ zip funcReqs
  funcReqsDetails) ++ [LlC inputGlassPropsTable]

mkReqCI :: (Definition c, HasShortName c, Referable c) => [Sentence] -> c -> ListTuple
mkReqCI e = mkListTuple $ if length e == 0 then \x -> Flat $ x ^. defn else
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
  makeRefS inputGlassPropsTable `sC` S "which define the" +:+. foldlList Comma List
  [phrase glass +:+ plural dimension, (glassTy ^. defn), S "tolerable" +:+
  phrase probability `sOf` phrase failure, (plural characteristic `ofThe` 
  phrase blast)] +: S "Note", ch plate_len `sAnd` ch plate_width,
  S "will be input in terms of", plural millimetre `sAnd`
  S "will be converted to the equivalent value in", plural metre]

inputGlassPropsTable :: LabelledContent
inputGlassPropsTable = llcc (mkLabelSame "InputGlassPropsReqInputs" Tab) $ 
  Table
  [at_start symbol_, at_start description, S "Units"]
  (mkTable
  [ch,
   at_start, U.toSentence] requiredInputs)
  (S "Required Inputs following" +:+ makeRef2S inputGlassProps) True
  where
    requiredInputs :: [QuantityDict]
    requiredInputs = (map qw [plate_len, plate_width, char_weight])
      ++ (map qw [pb_tol, tNT]) ++ (map qw [sdx, sdy, sdz])
      ++ (map qw [glass_type, nom_thick])

sysSetValsFollowingAssumpsDesc :: Sentence
sysSetValsFollowingAssumpsDesc = foldlSent_ [S "The", phrase system, S "shall set the known",
    plural value +: S "as follows"]

sysSetValsFollowingAssumpsList :: [Sentence]
sysSetValsFollowingAssumpsList = [foldlList Comma List (map ch (take 4 assumptionConstants)) `followA` standardValues,
  ch loadDF +:+ S "from" +:+ makeRefS loadDF, 
  short lShareFac `followA` glassLite,
  ch hFromt +:+ S "from" +:+ makeRefS hFromt,
  ch glaTyFac +:+ S "from" +:+ makeRefS glaTyFac,
  ch standOffDis +:+ S "from" +:+ makeRefS standOffDis,
  ch aspRat +:+ S "from" +:+ makeRefS aspRat]

--FIXME:should constants, LDF, and LSF have some sort of field that holds
-- the assumption(s) that're being followed? (Issue #349)

checkInputWithDataConsDesc = foldlSent [S "The", phrase system, S "shall check the entered",
  plural inValue, S "to ensure that they do not exceed the",
  plural datumConstraint, S "mentioned in" +:+. makeRefS datConLabel, 
  S "If any" `sOf` S "the", plural inParam, S "are out" `sOf` S "bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `andThe` plural calculation, S "stop"]

outputValsAndKnownQuantsDesc = foldlSent [titleize output_, S "the", plural inQty,
  S "from", makeRef2S inputGlassProps `andThe` S "known", plural quantity,
  S "from", makeRef2S sysSetValsFollowingAssumps]

checkGlassSafetyDesc cmd = foldlSent_ [S "If", (ch is_safePb), S "∧", (ch is_safeLR),
  sParen (S "from" +:+ (makeRefS pbIsSafe)
  `sAnd` (makeRefS lrIsSafe)), S "are true" `sC`
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

mkReqList :: (NamedIdea c, HasSymbol c, HasShortName c, HasUID c, Referable c) => [c] -> [(Sentence, Symbol, Sentence)]
mkReqList = map (\c -> (at_start c, symbol c Implementation, sParen (makeRefS c)))
