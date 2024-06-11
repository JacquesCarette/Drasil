module Drasil.DblPend.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalAccel, gravitationalMagnitude)
import Drasil.DblPend.DataDefs (positionXEqn_1)
import Drasil.DblPend.Unitals (lenRod_1, lenRod_2, massObj_1, massObj_2,
  xVel_1, yVel_1, xAccel_1, yAccel_1, angularAccel_1, angularAccel_2,
  tension_1, tension_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2)

-- Velocity X/Y First Object
velXExpr_1, velYExpr_1 :: PExpr
velXExpr_1 = sy angularVel_1 `mul` sy lenRod_1 `mul` cos (sy pendDisAngle_1)
velYExpr_1 = sy angularVel_1 `mul` positionXEqn_1

-- Velocity X/Y Second Object
velXExpr_2, velYExpr_2 :: PExpr
velXExpr_2 = sy xVel_1 `add` (sy angularVel_2 `mul` sy lenRod_2 `mul` cos (sy pendDisAngle_2))
velYExpr_2 = sy yVel_1 `add` (sy angularVel_2 `mul` sy lenRod_2 `mul` sin (sy pendDisAngle_2))

-- Acceleration X/Y First Object
accelXExpr_1, accelYExpr_1 :: PExpr
accelXExpr_1 = neg (square (sy angularVel_1) `mul` sy lenRod_1 `mul` sin (sy pendDisAngle_1))
                `add` (sy angularAccel_1 `mul` sy lenRod_1 `mul` cos (sy pendDisAngle_1))
accelYExpr_1 = (square (sy angularVel_1) `mul` sy lenRod_1 `mul` cos (sy pendDisAngle_1))
                `add` (sy angularAccel_1 `mul` sy lenRod_1 `mul` sin (sy pendDisAngle_1))

-- Acceleration X/Y Second Object
accelXExpr_2, accelYExpr_2 :: PExpr
accelXExpr_2 = sy xAccel_1 $-
                (square (sy angularVel_2) `mul` sy lenRod_2 `mul` sin (sy pendDisAngle_2))
                `add` (sy angularAccel_2 `mul` sy lenRod_2 `mul` cos (sy pendDisAngle_2))
accelYExpr_2 = sy yAccel_1 `add`
                (square (sy angularVel_2) `mul` sy lenRod_2 `mul` cos (sy pendDisAngle_2))
                `add` (sy angularAccel_2 `mul` sy lenRod_2 `mul` sin (sy pendDisAngle_2))

-- Horizontal/Vertical force acting on the first object
xForceWithAngle_1 :: PExpr
xForceWithAngle_1 = neg (sy tension_1 `mul` sin (sy pendDisAngle_1)) `add`
                    (sy tension_2 `mul` sin (sy pendDisAngle_2))

yForceWithAngle_1 :: PExpr
yForceWithAngle_1 = sy tension_1 `mul` cos (sy pendDisAngle_1) $- 
                    (sy tension_2 `mul` cos (sy pendDisAngle_2)) $- 
                    (sy massObj_1 `mul` sy gravitationalAccel)

-- Horizontal/Vertical force acting on the second object
xForceWithAngle_2 :: PExpr
xForceWithAngle_2 = neg (sy tension_2) `mul` sin (sy pendDisAngle_2)

yForceWithAngle_2 :: PExpr
yForceWithAngle_2 = sy tension_2 `mul` cos (sy pendDisAngle_2) $- 
                    (sy massObj_2 `mul` sy gravitationalAccel)

-- Angular acceleration acting on the first object
angularAccelExpr_1 :: PExpr
angularAccelExpr_1 = neg(sy gravitationalMagnitude) `mul`
                   (exactDbl 2 `mul` sy massObj_1 `add` sy massObj_2) `mul` sin (sy pendDisAngle_1 ) $-
                   (sy massObj_2 `mul` sy gravitationalMagnitude `mul`
                   sin (sy pendDisAngle_1 $- (exactDbl 2 `mul` sy pendDisAngle_2))) $-
                   ((exactDbl 2 `mul` sin (sy pendDisAngle_1 $- sy pendDisAngle_2 )) `mul` sy massObj_2 `mul`
                   (
                       square (sy angularVel_2) `mul` sy lenRod_2 `add` 
                       (square (sy angularVel_1) `mul` sy lenRod_1 `mul` cos (sy pendDisAngle_1 $- sy pendDisAngle_2))
                   ))
                   $/
                   sy lenRod_1 `mul` 
                   (
                       exactDbl 2 `mul` sy massObj_1 `add` sy massObj_2 $- 
                       (sy massObj_2 `mul` 
                       cos (exactDbl 2 `mul` sy pendDisAngle_1  $- (exactDbl 2 `mul` sy pendDisAngle_2)))
                   )

-- Angular acceleration acting on the first object
angularAccelExpr_2 :: PExpr
angularAccelExpr_2 = exactDbl 2 `mul` sin (sy pendDisAngle_1 $- sy pendDisAngle_2) `mul`
                   (
                       square (sy angularVel_1) `mul` sy lenRod_1 `mul` (sy massObj_1 `add` sy massObj_2 ) `add`
                       (sy gravitationalMagnitude `mul` (sy massObj_1 `add` sy massObj_2 ) `mul` cos (sy pendDisAngle_1)) `add`
                       (square (sy angularVel_2) `mul` sy lenRod_2 `mul` sy massObj_2 `mul` 
                       cos (sy pendDisAngle_1 $- sy pendDisAngle_2 ))
                   )
                   $/
                   sy lenRod_2 `mul` 
                   (
                       exactDbl 2 `mul` sy massObj_1 `add` sy massObj_2 $- 
                       (sy massObj_2 `mul` 
                       cos (exactDbl 2 `mul` sy pendDisAngle_1  $- (exactDbl 2 `mul` sy pendDisAngle_2)))
                   )

-- Angular acceleration support equations in IM
forceDerivExpr1 :: PExpr
forceDerivExpr1 = sy tension_2 `mul` sin (sy pendDisAngle_2)
forceDerivExpr2 :: PExpr
forceDerivExpr2 = sy tension_2 `mul` cos (sy pendDisAngle_2)

cosAngleExpr1 :: PExpr
cosAngleExpr1 = cos (sy pendDisAngle_1)
sinAngleExpr1 :: PExpr
sinAngleExpr1 = sin (sy pendDisAngle_1)

cosAngleExpr2 :: PExpr
cosAngleExpr2 = cos (sy pendDisAngle_2)
sinAngleExpr2 :: PExpr
sinAngleExpr2 = sin (sy pendDisAngle_2)
