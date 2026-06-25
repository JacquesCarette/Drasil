module Drasil.SglPend.Requirements (
  funcReqs, funcReqsTables
) where

import qualified Data.List.NonEmpty as NE (NonEmpty, fromList)

import Language.Drasil
import Language.Drasil.Document
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.SRS (inReqWTab, mkQRTuple, outReq)
import Data.Drasil.Concepts.Documentation (funcReqDom, output_, value)
import Drasil.SglPend.IMods (angularDisplacementIM, iMods)
import Drasil.SglPend.Unitals (lenRod, pendDisplacementAngle, inputs)
import Data.Drasil.Quantities.Physics (angularDisplacement)
import Drasil.DblPend.Requirements(verifyInptVals)

--Functional Requirements--
funcReqs :: [ConceptInstance]
funcReqs = [inputValues, verifyInptVals, calcAngPos, outputValues]

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable]

inputValues, outputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab Nothing inputs
(outputValues, _) = outReq Nothing outputsWReqs

outputsWReqs :: NE.NonEmpty (DefinedQuantityDict, Sentence)
outputsWReqs = NE.fromList $ mkQRTuple iMods

calcAngPos:: ConceptInstance

calcAngPos = cic "calcAngPos" calcAngPosDesc "Calculate-Angular-Position-Of-Mass" funcReqDom

calcAngPosDesc :: Sentence
calcAngPosDesc = foldlSent [S "Calculate the following" +: plural value,
    ch angularDisplacement `S.and_` ch pendDisplacementAngle,
    sParen (S "from" +:+ refS angularDisplacementIM)]
