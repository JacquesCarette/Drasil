module Drasil.GlassBR.Requirements (funcReqsList, funcReqsListOfReqs) where

import Control.Lens ((^.))
import Data.Function (on)
import Data.List (sortBy)

import Language.Drasil
import Drasil.DocLang (mkRequirement)
import Drasil.DocLang.SRS (datConLabel)

import Data.Drasil.Concepts.Computation (inParam, inQty, inValue)
import Data.Drasil.Concepts.Documentation (characteristic, condition, 
  datumConstraint, description, failure, input_, message, output_, quantity, 
  symbol_, system, value)
import Data.Drasil.Concepts.Math (calculation, probability)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), andThe, 
  foldlList, foldlSent, foldlSent_, followA, ofThe, sAnd, sOf)
import Data.Drasil.SI_Units (metre, millimetre)
import Data.Drasil.Utils (noRefs)

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
funcReqsList = funcReqsListOfReqsCon ++ [LlC funcReqsR1Table]

funcReqsListOfReqsCon :: [Contents]
funcReqsListOfReqsCon = map LlC $ funcReqsListOfReqs

funcReqsListOfReqs :: [LabelledContent]
funcReqsListOfReqs = [funcReqsR1, funcReqsR2, funcReqsR3, funcReqsR4, funcReqsR5, funcReqsR6]
funcReqsR1, funcReqsR2, funcReqsR3, funcReqsR4, funcReqsR5, funcReqsR6 :: LabelledContent

funcReqsR1 = mkRequirement "funcReqsR1" req1Desc "Input-Glass-Props"
funcReqsR3 = mkRequirement "funcReqsR3" req3Desc "Check-Input-with-Data_Constraints"
funcReqsR4 = mkRequirement "funcReqsR4" req4Desc "Output-Values-and-Known-Quantities"
funcReqsR5 = mkRequirement "funcReqsR5" (req5Desc (output_)) "Check-Glass-Safety"

req1Desc, req3Desc, req4Desc :: Sentence
req5Desc :: NamedChunk -> Sentence

req1Desc = foldlSent [at_start input_, S "the", plural quantity, S "from",
  makeRef funcReqsR1Table `sC` S "which define the" +:+. foldlList Comma List
  [phrase glass +:+ plural dimension, (glassTy ^. defn), S "tolerable" +:+
  phrase probability `sOf` phrase failure, (plural characteristic `ofThe` 
  phrase blast)] +: S "Note", ch plate_len `sAnd` ch plate_width,
  S "will be input in terms of", plural millimetre `sAnd`
  S "will be converted to the equivalent value in", plural metre]

funcReqsR1Table :: LabelledContent
funcReqsR1Table = llcc (mkLabelSame "R1ReqInputs" Tab) $ 
  Table
  [at_start symbol_, at_start description, S "Units"]
  (mkTable
  [ch,
   at_start, unitToSentence] requiredInputs)
  (S "Required Inputs following R1") True
  where
    requiredInputs :: [QuantityDict]
    requiredInputs = (map qw [plate_len, plate_width, char_weight])
      ++ (map qw [pb_tol, tNT]) ++ (map qw [sdx, sdy, sdz])
      ++ (map qw [glass_type, nom_thick])

funcReqsR2 = llcc funcReqs2Label $
  Enumeration $ Simple $ 
  [(S "System-Set-Values-Following-Assumptions"
   , Nested (foldlSent_ [S "The", phrase system, S "shall set the known", 
    plural value +: S "as follows"])
     $ Bullet $ noRefs $
     map (Flat $) (funcReqsR2List)
   , Just $ (getAdd (funcReqs2Label ^. getRefAdd)))]

funcReqsR2List :: [Sentence]
funcReqsR2List = [foldlList Comma List (map ch (take 4 assumptionConstants)) `followA` standardValues,
  ch loadDF +:+ S "from" +:+ makeRef loadDF, 
  short lShareFac `followA` glassLite,
  ch hFromt +:+ S "from" +:+ makeRef hFromt,
  ch glaTyFac +:+ S "from" +:+ makeRef glaTyFac,
  ch standOffDis +:+ S "from" +:+ makeRef standOffDis,
  ch aspRat +:+ S "from" +:+ makeRef aspRat]

--FIXME:should constants, LDF, and LSF have some sort of field that holds
-- the assumption(s) that're being followed? (Issue #349)

req3Desc = foldlSent [S "The", phrase system, S "shall check the entered",
  plural inValue, S "to ensure that they do not exceed the",
  plural datumConstraint, S "mentioned in" +:+. makeRef datConLabel, 
  S "If any" `sOf` S "the", plural inParam, S "are out" `sOf` S "bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `andThe` plural calculation, S "stop"]

req4Desc = foldlSent [titleize output_, S "the", plural inQty,
  S "from", makeRef funcReqsR1 `andThe` S "known", plural quantity,
  S "from", makeRef funcReqsR2]

req5Desc cmd = foldlSent_ [S "If", (ch is_safePb), S "∧", (ch is_safeLR),
  sParen (S "from" +:+ (makeRef pbIsSafe)
  `sAnd` (makeRef lrIsSafe)), S "are true" `sC`
  phrase cmd, S "the", phrase message, Quote (safeMessage ^. defn),
  S "If the", phrase condition, S "is false, then", phrase cmd,
  S "the", phrase message, Quote (notSafe ^. defn)]

funcReqsR6 = llcc funcReqs6Label $
  Enumeration $ Simple $ 
  [(S "Output-Quantities"
   , Nested (titleize output_ +:+ S "the following" +: plural quantity)
     $ Bullet $ noRefs $ chunksToItemTypes funcReqsR6List
   , Just $ (getAdd (funcReqs6Label ^. getRefAdd)))]

chunksToItemTypes :: [(Sentence, Symbol, Sentence)] -> [ItemType]
chunksToItemTypes = map (\(a, b, c) -> Flat $ a +:+ sParen (P b) +:+ c)

funcReqsR6List :: [(Sentence, Symbol, Sentence)]
funcReqsR6List = sortBy (compsy `on` get2) $ (mkReqList gbrIMods) ++ (mkReqList r6DDs)
  where
    r6DDs :: [DataDefinition]
    r6DDs = [risk, strDisFac, nonFL, glaTyFac, dimLL, tolPre, tolStrDisFac, hFromt, aspRat]
    get2 (_, b, _) = b

mkReqList :: (NamedIdea c, HasSymbol c, HasShortName c, HasUID c, Referable c) => [c] -> [(Sentence, Symbol, Sentence)]
mkReqList = map (\c -> (at_start c, symbol c Implementation, sParen (makeRef c)))

funcReqs2Label, funcReqs6Label :: Label
funcReqs2Label = mkLabelSame "System-Set-Values-Following-Assumptions" (Req FR)
funcReqs6Label = mkLabelSame "Output-Quantities"                       (Req FR)