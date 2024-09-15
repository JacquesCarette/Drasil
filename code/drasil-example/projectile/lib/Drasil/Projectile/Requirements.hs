module Drasil.Projectile.Requirements (funcReqs, nonfuncReqs) where

import Language.Drasil
import Drasil.DocLang.SRS (datCon, propCorSol)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.DocLang (mkMaintainableNFR, mkPortableNFR)

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (code, datumConstraint,
  funcReqDom, mg, mis, nonFuncReqDom, output_,
  property, value, vavPlan, propOfCorSol)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)

import Drasil.Projectile.IMods (landPosIM, messageIM, offsetIM, timeIM)
import Drasil.Projectile.Unitals (flightDur, landPos, message, offset)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [verifyInVals, calcValues, outputValues]

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
    ch offset    +:+ fromSource offsetIM,
    ch message   +:+ fromSource messageIM
  ]]
outputValuesDesc = atStart output_ +:+. outputs
  where
    outputs = foldlList Comma List $ map foldlSent_ [ 
        [ch flightDur, fromSource timeIM],
        [ch message, fromSource messageIM], 
        [ch offset, fromSource offsetIM]
      ]

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  atStartNP' (output_ `the_ofThePS` code), S "have the",
  plural property, S "described in", namedRef (propCorSol [] []) (titleize' propOfCorSol)
  ]) "Correctness" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  atStartNP (the code), S "is tested with complete",
  phrase vavPlan]) "Verifiability" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  atStartNP (the code), S "is modularized with complete",
  phraseNP (mg `and_` mis)]) "Understandability" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [atStartNP (the code), S "is modularized"]) "Reusability" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = mkMaintainableNFR "maintainable" 10 "Maintainability"

portable :: ConceptInstance
portable = mkPortableNFR "portable" ["Windows", "Mac OSX", "Linux"] "Portability"
  