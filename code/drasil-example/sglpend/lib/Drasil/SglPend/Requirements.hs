module Drasil.SglPend.Requirements (
  funcReqs, funcReqsTables
) where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.DocLang (inReqWTab)
import Data.Drasil.Concepts.Documentation (funcReqDom, output_, value)
import Drasil.SglPend.IMods (angularDisplacementIM)
import Drasil.SglPend.Unitals (lenRod, pendDisplacementAngle, inputs)
import Data.Drasil.Quantities.Physics (angularDisplacement)
import Drasil.DblPend.Requirements(verifyInptVals)

--Functional Requirements--
funcReqs :: [ConceptInstance]
funcReqs = [inputValues, verifyInptVals, calcAngPos, outputValues]

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable]

inputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab Nothing inputs

calcAngPos, outputValues :: ConceptInstance

calcAngPos = cic "calcAngPos" calcAngPosDesc "Calculate-Angular-Position-Of-Mass" funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

calcAngPosDesc, outputValuesDesc :: Sentence
calcAngPosDesc = foldlSent [S "Calculate the following" +: plural value,
    ch angularDisplacement `S.and_` ch pendDisplacementAngle,
    sParen (S "from" +:+ refS angularDisplacementIM)]
outputValuesDesc = foldlSent [atStart output_, ch lenRod, sParen (S "from" +:+ refS angularDisplacementIM)]
