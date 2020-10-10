module Drasil.DblPendulum.Assumptions (pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin, assumptions) where
    
import Language.Drasil
import Utils.Drasil
import Drasil.DblPendulum.Concepts (pendulumTitle)
import Data.Drasil.Concepts.Documentation (assumpDom) 
import Data.Drasil.Concepts.Math (cartesian, xAxis, yAxis)
import Data.Drasil.Concepts.Physics (gravity, twoD)


assumptions :: [ConceptInstance]
assumptions = [pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin]

pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin :: ConceptInstance 

pend2DMotion    = cic "pend2DMotion"      pend2DMotionDesc    "pend2DMotion"    assumpDom
cartCoord       = cic "cartCoord"         cartCoordDesc       "cartCoord"       assumpDom
cartCoordRight  = cic "cartCoordRight"    cartCoordRightDesc  "cartCoordRight"  assumpDom
yAxisDir        = cic "yAxisDir"          yAxisDirDesc        "yAxisDir"        assumpDom
startOrigin     = cic "startOrigin"       startOriginDesc     "startOrigin"     assumpDom

pend2DMotionDesc :: Sentence
pend2DMotionDesc = S "The" +:+ phrase pendulumTitle +:+ S "motion" `sIs` phrase twoD +:+. sParen (getAcc twoD)

cartCoordDesc :: Sentence
cartCoordDesc = S "A" +:+ (phrase cartesian `sIs` S "used") 

cartCoordRightDesc :: Sentence
cartCoordRightDesc = S "The" +:+ phrase cartesian `sIs` S "right-handed where positive" +:+.
                         phrase xAxis `sAnd` phrase yAxis +:+ S "point right up"

yAxisDirDesc :: Sentence
yAxisDirDesc = S "The direction" `ofThe'` phrase yAxis `sIs` S "directed opposite to" +:+. phrase gravity

startOriginDesc :: Sentence
startOriginDesc = S "The" +:+. (phrase pendulumTitle `sIs` S "attached" `toThe` S "origin")

