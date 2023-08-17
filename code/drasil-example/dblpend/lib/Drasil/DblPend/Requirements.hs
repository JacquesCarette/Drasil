module Drasil.DblPend.Requirements where

import Language.Drasil
import Drasil.DocLang.SRS (datCon, propCorSol)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (datumConstraint, funcReqDom,
        output_, value,  nonFuncReqDom, code, environment, propOfCorSol)
--  likelyChg, mg, mis, module_, nonFuncReqDom,
--   requirement, srs, traceyMatrix, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)

import Drasil.DblPend.IMods (angleIM_1, angleIM_2)
import Drasil.DblPend.Unitals (pendDisAngle_1, pendDisAngle_2)

--Functional Requirements--
funcReqs :: [ConceptInstance]
funcReqs = [verifyInptVals, calcAng, outputValues]

verifyInptVals, calcAng, outputValues :: ConceptInstance

verifyInptVals = cic "verifyInptVals" verifyInptValsDesc  "Verify-Input-Values"    funcReqDom
calcAng        = cic "calcAng"        calcAngDesc         "Calculate-Angle-Of-Rod" funcReqDom
outputValues   = cic "outputValues"   outputValuesDesc    "Output-Values"          funcReqDom

verifyInptValsDesc, calcAngDesc, outputValuesDesc :: Sentence

verifyInptValsDesc = foldlSent [S "Check the entered", plural inValue,
  S "to ensure that they do not exceed the" +:+.
    namedRef (datCon ([]::[Contents]) ([]::[Section])) (plural datumConstraint),
  S "If any of the", plural inValue, S "are out of bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `S.andThe` plural calculation, S "stop"]

calcAngDesc = foldlSent [S "Calculate the following" +: plural value +:+ outputList]
outputValuesDesc = foldlSent [atStart output_ +:+ outputList]

outputList :: Sentence
outputList = ch pendDisAngle_1 `S.and_` ch pendDisAngle_2 +:+
    sParen (S "from" +:+ refS angleIM_1 `S.and_` refS angleIM_2)

--Nonfunctional Requirements--
nonFuncReqs :: [ConceptInstance]
nonFuncReqs = [correct, portable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
 atStartNP' (output_ `the_ofThePS` code), S "have the", 
 namedRef (propCorSol [] []) (plural propOfCorSol)]
 ) "Correct" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  atStartNP (the code), S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom
