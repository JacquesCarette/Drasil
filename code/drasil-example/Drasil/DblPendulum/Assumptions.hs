module Drasil.DblPendulum.Assumptions (pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin, assumptions) where
    
import Language.Drasil
import Utils.Drasil

-- import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)
import Drasil.DblPendulum.Concepts (pendulumTitle)
import Data.Drasil.Concepts.Documentation (assumpDom) 
import Data.Drasil.Concepts.Math (cartesian, xAxis, yAxis, axis)
import Data.Drasil.Concepts.Physics (acceleration, gravity, twoD)
--value)
-- import Data.Drasil.Concepts.Math (cartesian, xAxis, xDir, yAxis, yDir)
-- import Data.Drasil.Concepts.PhysicalProperties (mass)
--(acceleration, collision, distance, gravity, time)
-- import Drasil.Projectile.Concepts (launcher, projectile, target)

assumptions :: [ConceptInstance]
assumptions = [pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin]

pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin :: ConceptInstance 

pend2DMotion    = cic "pend2DMotion"      pend2DMotionDesc    "pend2DMotion"      assumpDom
cartCoord       = cic "cartCoord"         cartCoordDesc       "cartCoord"       assumpDom
cartCoordRight  = cic "cartCoordRight"    cartCoordRightDesc  "cartCoordRight"  assumpDom
yAxisDir        = cic "yAxisDir"          yAxisDirDesc        "yAxisDir"        assumpDom
startOrigin     = cic "startOrigin"       startOriginDesc     "startOrigin"     assumpDom

pend2DMotionDesc :: Sentence
pend2DMotionDesc = S "The" +:+ phrase pendulumTitle +:+ S "motion" `sIs` phrase twoD +:+. sParen (getAcc twoD)

cartCoordDesc :: Sentence
cartCoordDesc = S "A" +:+ (phrase cartesian `sIs` S "used") 

--  +:+. sParen (S "from" +:+ makeRef2S neglectCurv)

cartCoordRightDesc :: Sentence
cartCoordRightDesc = S "The" +:+ phrase cartesian `sIs` S "right-handed where positive" +:+.
                         phrase xAxis `sAnd` phrase yAxis +:+ S "point right up"

yAxisDirDesc :: Sentence
yAxisDirDesc = S "The direction" `ofThe'` phrase yAxis `sIs` S "directed opposite to" +:+. phrase gravity

startOriginDesc :: Sentence
startOriginDesc = S "The" +:+. (phrase pendulumTitle `sIs` S "attached" `toThe` S "origin")

--"The" +:+. (phrase launcher `sIs` S "coincident with the origin")  

-- targetXAxisDesc :: Sentence
-- targetXAxisDesc = S "The" +:+ phrase target +:+ S "lies on the" +:+ phrase xAxis +:+. sParen (S "from" +:+ makeRef2S neglectCurv)

-- posXDirectionDesc :: Sentence
-- posXDirectionDesc = S "The positive" +:+ phrase xDir `sIs` S "from the" +:+. (phrase launcher `toThe` phrase target)

-- constAccelDesc :: Sentence
-- constAccelDesc = S "The" +:+ (phrase acceleration `sIs` S "constant") +:+.
--                  sParen (S "from" +:+ foldlList Comma List (map makeRef2S [accelXZero, accelYGravity, neglectDrag, freeFlight]))

-- accelXZeroDesc :: Sentence
-- accelXZeroDesc = S "The" +:+ phrase acceleration +:+. (S "in the" +:+ phrase xDir `sIs` S "zero")

-- accelYGravityDesc :: Sentence
-- accelYGravityDesc = S "The" +:+ phrase acceleration +:+ S "in the" +:+ phrase yDir `isThe` phrase acceleration +:+
--                     S "due to" +:+ phrase gravity +:+. sParen (S "from" +:+ makeRef2S yAxisGravity)

-- neglectDragDesc :: Sentence
-- neglectDragDesc = S "Air drag" `sIs` S "neglected."

-- pointMassDesc :: Sentence
-- pointMassDesc = (S "size" `sAnd` S "shape") `ofThe'` phrase projectile `sAre`
--                 S "negligible" `sC` S "so that it can be modelled as a point" +:+. phrase mass

-- freeFlightDesc :: Sentence
-- freeFlightDesc = S "The flight" `sIs` S "free; there" `sAre` S "no" +:+ plural collision +:+
--                  S "during" +:+. (S "trajectory" `ofThe` phrase projectile)

-- neglectCurvDesc :: Sentence
-- neglectCurvDesc = S "The" +:+ phrase distance `sIs` S "small enough that" +:+.
--                   (S "curvature" `ofThe` S "Earth can be neglected")

-- timeStartZeroDesc :: Sentence
-- timeStartZeroDesc = atStart time +:+. S "starts at zero"

-- gravAccelValueDesc :: Sentence
-- gravAccelValueDesc = S "The" +:+ phrase acceleration +:+ S "due to" +:+
--   phrase gravity +:+ S "is assumed to have the" +:+ phrase value +:+ 
--   S "provided in" +:+. makeRef2S (SRS.valsOfAuxCons ([]::[Contents]) 
--   ([]::[Section]))
