module Drasil.SglPend.Requirements where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Concepts.Documentation (funcReqDom, value)
import Data.Drasil.Quantities.Physics (angularDisplacement)

import Drasil.DblPend.Requirements (verifyInptValsDesc)

import Drasil.SglPend.IMods (angularDisplacementIM)
import Drasil.SglPend.Unitals (pendDisplacementAngle)

--Functional Requirements--
funcReqs :: [ConceptInstance]
funcReqs = [verifyInptVals, calcAngPos]

verifyInptVals, calcAngPos :: ConceptInstance
verifyInptVals = cic "verifyInptVals" verifyInptValsDesc "Verify-Input-Values"                funcReqDom
calcAngPos     = cic "calcAngPos"     calcAngPosDesc     "Calculate-Angular-Position-Of-Mass" funcReqDom

calcAngPosDesc :: Sentence
calcAngPosDesc = foldlSent [S "Calculate the following" +: plural value,
    ch angularDisplacement `S.and_` ch pendDisplacementAngle,
    sParen (S "from" +:+ refS angularDisplacementIM)]
