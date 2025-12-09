module Drasil.Projectile.Requirements (
  funcReqs, nonfuncReqs, funcReqsTables
) where

import Language.Drasil
import Drasil.DocLang.SRS (datCon)
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.DocLang (mkMaintainableNFR, mkPortableNFR, mkCorrectNFR,
  mkVerifiableNFR, mkUnderstandableNFR, mkReusableNFR, inReqWTab)

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (datumConstraint,
  funcReqDom, output_, value)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)

import Drasil.Projectile.IMods (landPosIM, offsetIM, timeIM)
import Drasil.Projectile.Unitals (flightDur, landPos, offset, inputs)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [inputValues, verifyInVals, calcValues, outputValues]

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable]

inputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab Nothing inputs

verifyInVals, calcValues, outputValues :: ConceptInstance

verifyInVals = cic "verifyInVals" verifyParamsDesc "Verify-Input-Values" funcReqDom
calcValues   = cic "calcValues"   calcValuesDesc   "Calculate-Values"    funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values"       funcReqDom

verifyParamsDesc, calcValuesDesc, outputValuesDesc :: Sentence
verifyParamsDesc = foldlSent [S "Check the entered", plural inValue,
  S "to ensure that they do not exceed the" +:+. namedRef (datCon [] []) (plural datumConstraint),
  S "If any" `S.ofThe` plural inValue `S.are` S "out of bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `S.andThe` plural calculation, S "stop"]
calcValuesDesc = foldlSent [S "Calculate the following" +: plural value,
  foldlList Comma List [
    ch flightDur +:+ fromSource timeIM,
    ch landPos   +:+ fromSource landPosIM,
    ch offset    +:+ fromSource offsetIM
  ]]
outputValuesDesc = atStart output_ +:+. outputs
  where
    outputs = foldlList Comma List $ map foldlSent_ [
        [ch flightDur, fromSource timeIM],
        [ch offset, fromSource offsetIM]
      ]

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
portable = mkPortableNFR "portable" ["Windows", "Mac OSX", "Linux"] "Portability"
