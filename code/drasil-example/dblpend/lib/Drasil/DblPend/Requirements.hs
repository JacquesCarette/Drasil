module Drasil.DblPend.Requirements (
  funcReqs, nonFuncReqs, funcReqsTables, verifyInptVals
) where

import qualified Data.List.NonEmpty as NE (NonEmpty, fromList)
import Language.Drasil

import Language.Drasil.Document
import Drasil.SRS.Concepts (datCon)
import Drasil.SRS (mkPortableNFR, mkCorrectNFR, inReqWTab, mkQRTuple, outReq)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (datumConstraint, funcReqDom, value)
--  likelyChg, mg, mis, module_, nonFuncReqDom,
--   requirement, srs, traceyMatrix, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)

import Drasil.DblPend.IMods (angleIM_1, angleIM_2, iMods)
import Drasil.DblPend.Unitals (inputs, pendDisAngle_1, pendDisAngle_2,)

--Functional Requirements--
funcReqs :: [ConceptInstance]
funcReqs = [inputValues, verifyInptVals, calcAng, outputValues]

inputValues, outputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab Nothing inputs
(outputValues, _) = outReq Nothing outputsWReqs

outputsWReqs :: NE.NonEmpty (DefinedQuantityDict, Sentence)
outputsWReqs = NE.fromList $ mkQRTuple iMods

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable]

verifyInptVals, calcAng :: ConceptInstance

verifyInptVals = cic "verifyInptVals" verifyInptValsDesc  "Verify-Input-Values"    funcReqDom
calcAng        = cic "calcAng"        calcAngDesc         "Calculate-Angle-Of-Rod" funcReqDom

verifyInptValsDesc, calcAngDesc :: Sentence

verifyInptValsDesc = foldlSent [S "Check the entered", plural inValue,
  S "to ensure that they do not exceed the" +:+.
    namedRef (datCon ([]::[Contents]) ([]::[Section])) (plural datumConstraint),
  S "If any of the", plural inValue, S "are out of bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `S.andThe` plural calculation, S "stop"]

calcAngDesc = foldlSent [S "Calculate the following" +: plural value,
  ch pendDisAngle_1 `S.and_` ch pendDisAngle_2,
  sParen (S "from" +:+ refS angleIM_1 `S.and_` refS angleIM_2)]

--Nonfunctional Requirements--
nonFuncReqs :: [ConceptInstance]
nonFuncReqs = [correct, portable]

correct :: ConceptInstance
correct = mkCorrectNFR "correct" "Correctness"

portable :: ConceptInstance
portable = mkPortableNFR "portable" ["Windows", "Mac OSX", "Linux"] "Portability"
