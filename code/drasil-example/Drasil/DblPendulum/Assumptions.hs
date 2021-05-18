{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.Assumptions (pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin, assumptions) where
    
import Language.Drasil
import qualified Utils.Drasil.Sentence as S
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (assumpDom) 
import Data.Drasil.Concepts.Math (cartesian, xAxis, yAxis, direction, origin, positive)
import Data.Drasil.Concepts.Physics (gravity, twoD, pendulum, motion)


assumptions :: [ConceptInstance]
assumptions = [pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin]

pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin :: ConceptInstance 

pend2DMotion    = cic "pend2DMotion"      pend2DMotionDesc    "pend2DMotion"    assumpDom
cartCoord       = cic "cartCoord"         cartCoordDesc       "cartCoord"       assumpDom
cartCoordRight  = cic "cartCoordRight"    cartCoordRightDesc  "cartCoordRight"  assumpDom
yAxisDir        = cic "yAxisDir"          yAxisDirDesc        "yAxisDir"        assumpDom
startOrigin     = cic "startOrigin"       startOriginDesc     "startOrigin"     assumpDom

pend2DMotionDesc :: Sentence
pend2DMotionDesc = atStartNP (the (compoundNC pendulum motion)) `S.sIs` phrase twoD +:+. sParen (getAcc twoD)

cartCoordDesc :: Sentence
cartCoordDesc = atStartNP (aNINP cartesian) `S.sIs` (S "used" !.)

cartCoordRightDesc :: Sentence
cartCoordRightDesc = atStartNP (the cartesian) `S.sIs` S "right-handed where" +:+ 
    phraseNP (combineNINP positive (xAxis `and_` yAxis)) +:+. S "point right up"

yAxisDirDesc :: Sentence
yAxisDirDesc = atStartNP (direction `the_ofThe''` yAxis) `S.sIs` S "directed opposite to" +:+. phrase gravity

startOriginDesc :: Sentence
startOriginDesc = atStartNP (the pendulum) `S.sIs` S "attached" `S.toThe` (phrase origin !.)

