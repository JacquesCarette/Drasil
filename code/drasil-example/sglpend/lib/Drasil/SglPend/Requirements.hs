module Drasil.SglPend.Requirements where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Concepts.Documentation (funcReqDom, output_, value)
import Drasil.SglPend.IMods (angularDisplacementIM)
import Drasil.SglPend.Unitals (lenRod, pendDisplacementAngle)
import Data.Drasil.Quantities.Physics (angularDisplacement)
import Drasil.DblPend.Requirements(verifyInptValsDesc)

--Functional Requirements--
funcReqs :: [ConceptInstance]
funcReqs = [verifyInptVals, calcAngPos, outputValues]

verifyInptVals, calcAngPos, outputValues :: ConceptInstance

verifyInptVals = cic "verifyInptVals" verifyInptValsDesc "Verify-Input-Values" funcReqDom
calcAngPos  = cic "calcAngPos"   calcAngPosDesc   "Calculate-Angular-Position-Of-Mass" funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

calcAngPosDesc, outputValuesDesc :: Sentence
calcAngPosDesc = foldlSent [S "Calculate the following" +: plural value,
    ch angularDisplacement `S.and_` ch pendDisplacementAngle,
    sParen (S "from" +:+ refS angularDisplacementIM)]
outputValuesDesc = foldlSent [atStart output_, ch lenRod, sParen (S "from" +:+ refS angularDisplacementIM)]
