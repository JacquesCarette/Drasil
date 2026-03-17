{-# LANGUAGE PostfixOperators #-}
module Drasil.BinaryStar.Requirements (funcReqs, funcReqsTables,
  nonFuncReqs) where

import Language.Drasil
import Drasil.DocLang.SRS (datCon)
import Drasil.DocLang (mkPortableNFR, mkCorrectNFR, inReqWTab)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (datumConstraint, funcReqDom,
  nonFuncReqDom, output_)
import Data.Drasil.Concepts.Software (errMsg)

import Drasil.BinaryStar.Unitals (inputs, xPos_1, yPos_1, xPos_2, yPos_2)

---------------------------------------------------------
-- Functional Requirements
---------------------------------------------------------

funcReqs :: [ConceptInstance]
funcReqs = [inputValues, verifyInptVals, calcPositions, verifyOutput,
  outputValues]

inputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab Nothing inputs

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable]

verifyInptVals, calcPositions, verifyOutput, outputValues :: ConceptInstance

verifyInptVals = cic "verifyInptVals"
  (foldlSent [S "Check the entered", plural inValue,
    S "to ensure that they do not exceed the" +:+.
      namedRef (datCon ([]::[Contents]) ([]::[Section])) (plural datumConstraint),
    S "If any of the", plural inValue, S "are out of bounds" `sC`
    S "an", phrase errMsg, S "is displayed" `S.andThe`
    S "computation stops"])
  "Verify-Input-Values" funcReqDom

calcPositions = cic "calcPositions"
  (S "Calculate the positions of both stars over the simulation interval" +:+
   S "by solving the instance models" !.)
  "Calculate-Positions" funcReqDom

verifyOutput = cic "verifyOutput"
  (S "Verify that the computed results satisfy the conservation of" +:+
   S "total mechanical energy within a specified numerical tolerance" !.)
  "Verify-Output" funcReqDom

outputValues = cic "outputValues"
  (atStart output_ +:+
   ch xPos_1 `sC` ch yPos_1 `sC` ch xPos_2 `S.and_` ch yPos_2 !.)
  "Output-Values" funcReqDom

---------------------------------------------------------
-- Nonfunctional Requirements
---------------------------------------------------------

nonFuncReqs :: [ConceptInstance]
nonFuncReqs = [correct, portable, maintainable, usable]

correct :: ConceptInstance
correct = mkCorrectNFR "correct" "Correctness"

portable :: ConceptInstance
portable = mkPortableNFR "portable" ["Linux", "Mac OSX", "Windows"] "Portability"

maintainable :: ConceptInstance
maintainable = cic "maintainable"
  (S "Routine updates to the software shall be manageable and predictable." +:+
   S "When changes are made, their impact on the generated artifacts" +:+
   S "should be clear and traceable" !.)
  "Maintainability" nonFuncReqDom

usable :: ConceptInstance
usable = cic "usable"
  (S "The outputs should be easy to inspect and reuse." +:+
   S "The software should export results in a consistent format" +:+
   S "so that users can post-process and visualize trajectories" +:+
   S "with external tools" !.)
  "Usability" nonFuncReqDom
