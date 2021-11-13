module Drasil.SglPendulum.Requirements where

import Language.Drasil
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.Concepts.Documentation (funcReqDom, output_, value)
import Drasil.SglPendulum.IMods (angularDisplacementIM)
import Drasil.SglPendulum.Unitals (lenRod, pendDisplacementAngle)
import Data.Drasil.Quantities.Physics (angularDisplacement)
import Drasil.DblPendulum.Requirements(verifyInptValsDesc)

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
