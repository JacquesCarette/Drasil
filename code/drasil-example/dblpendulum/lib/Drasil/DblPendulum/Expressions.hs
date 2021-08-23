module Drasil.DblPendulum.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalAccel)
import Drasil.DblPendulum.DataDefs (positionXDD_1)
import Drasil.DblPendulum.Unitals (lenRod_1, lenRod_2, massObj_1, massObj_2,
  xVel_1, yVel_1, xAccel_1, yAccel_1, angularAccel_1, angularAccel_2,
  tension_1, tension_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2)
import Control.Lens ((^.))

-- Velocity X/Y First Object
velXExpr_1, velYExpr_1 :: Expr
velXExpr_1 = sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)
velYExpr_1 = sy angularVel_1 `mulRe` (positionXDD_1 ^. defnExpr)

-- Velocity X/Y Second Object
velXExpr_2, velYExpr_2 :: Expr
velXExpr_2 = sy xVel_1 `addRe` (sy angularVel_2 `mulRe` sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))
velYExpr_2 = sy yVel_1 `addRe` (sy angularVel_2 `mulRe` sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))

-- Acceleration X/Y First Object
accelXExpr_1, accelYExpr_1 :: Expr
accelXExpr_1 = neg (square (sy angularVel_1) `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1))
                `addRe` (sy angularAccel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1))
accelYExpr_1 = (square (sy angularVel_1) `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1))
                `addRe` (sy angularAccel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1))

-- Acceleration X/Y Second Object
accelXExpr_2, accelYExpr_2 :: Expr
accelXExpr_2 = sy xAccel_1 $-
                (square (sy angularVel_2) `mulRe` sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))
                `addRe` (sy angularAccel_2 `mulRe` sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))
accelYExpr_2 = sy yAccel_1 `addRe`
                (square (sy angularVel_2) `mulRe` sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))
                `addRe` (sy angularAccel_2 `mulRe` sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))

-- Horizontal/Vertical force acting on the first object
xForceWithAngle_1 :: Expr
xForceWithAngle_1 = neg (sy tension_1 `mulRe` sin (sy pendDisAngle_1)) `addRe`
                    (sy tension_2 `mulRe` sin (sy pendDisAngle_2))

yForceWithAngle_1 :: Expr
yForceWithAngle_1 = sy tension_1 `mulRe` cos (sy pendDisAngle_1) $- 
                    (sy tension_2 `mulRe` cos (sy pendDisAngle_2)) $- 
                    (sy massObj_1 `mulRe` sy gravitationalAccel)

-- Horizontal/Vertical force acting on the second object
xForceWithAngle_2 :: Expr
xForceWithAngle_2 = neg (sy tension_2) `mulRe` sin (sy pendDisAngle_2)

yForceWithAngle_2 :: Expr
yForceWithAngle_2 = sy tension_2 `mulRe` cos (sy pendDisAngle_2) $- 
                    (sy massObj_2 `mulRe` sy gravitationalAccel)

-- Angular acceleration acting on the first object
angularAccelExpr_1 :: Expr
angularAccelExpr_1 = neg(sy gravitationalAccel) `mulRe`
                   (exactDbl 2 `mulRe` sy massObj_1 `addRe` sy massObj_2) `mulRe` sin (sy pendDisAngle_1 ) $-
                   (sy massObj_2 `mulRe` sy gravitationalAccel `mulRe`
                   sin (sy pendDisAngle_1 $- (exactDbl 2 `mulRe` sy pendDisAngle_2))) $-
                   ((exactDbl 2 `mulRe` sin (sy pendDisAngle_1 $- sy pendDisAngle_2 )) `mulRe` sy massObj_2 `mulRe`
                   (
                       square (sy angularVel_2) `mulRe` sy lenRod_2 `addRe` 
                       (square (sy angularVel_1) `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1 $- sy pendDisAngle_2))
                   ))
                   $/
                   sy lenRod_1 `mulRe` 
                   (
                       exactDbl 2 `mulRe` sy massObj_1 `addRe` sy massObj_2 $- 
                       (sy massObj_2 `mulRe` 
                       cos (exactDbl 2 `mulRe` sy pendDisAngle_1  $- (exactDbl 2 `mulRe` sy pendDisAngle_2)))
                   )

-- Angular acceleration acting on the first object
angularAccelExpr_2 :: Expr
angularAccelExpr_2 = exactDbl 2 `mulRe` sin (sy pendDisAngle_1 $- sy pendDisAngle_2) `mulRe`
                   (
                       square (sy angularVel_1) `mulRe` sy lenRod_1 `mulRe` (sy massObj_1 `addRe` sy massObj_2 ) `addRe`
                       (sy gravitationalAccel `mulRe` (sy massObj_1 `addRe` sy massObj_2 ) `mulRe` cos (sy pendDisAngle_1)) `addRe`
                       (square (sy angularVel_2) `mulRe` sy lenRod_2 `mulRe` sy massObj_2 `mulRe` 
                       cos (sy pendDisAngle_1 $- sy pendDisAngle_2 ))
                   )
                   $/
                   sy lenRod_2 `mulRe` 
                   (
                       exactDbl 2 `mulRe` sy massObj_1 `addRe` sy massObj_2 $- 
                       (sy massObj_2 `mulRe` 
                       cos (exactDbl 2 `mulRe` sy pendDisAngle_1  $- (exactDbl 2 `mulRe` sy pendDisAngle_2)))
                   )

-- Angular acceleration support equations in IM
forceDerivExpr1 :: Expr
forceDerivExpr1 = sy tension_2 `mulRe` sin (sy pendDisAngle_2)
forceDerivExpr2 :: Expr
forceDerivExpr2 = sy tension_2 `mulRe` cos (sy pendDisAngle_2)

cosAngleExpr1 :: Expr
cosAngleExpr1 = cos (sy pendDisAngle_1)
sinAngleExpr1 :: Expr
sinAngleExpr1 = sin (sy pendDisAngle_1)

cosAngleExpr2 :: Expr
cosAngleExpr2 = cos (sy pendDisAngle_2)
sinAngleExpr2 :: Expr
sinAngleExpr2 = sin (sy pendDisAngle_2)
