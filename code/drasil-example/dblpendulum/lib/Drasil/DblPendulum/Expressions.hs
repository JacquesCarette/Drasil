module Drasil.DblPendulum.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics(velocity, acceleration, gravitationalAccel, time)
import Drasil.DblPendulum.DataDefs (positionXDD_1, positionYDD_1, positionXDD_2, positionYDD_2, 
    positionGDD, accelGDD)
import Drasil.DblPendulum.Unitals (lenRod_1, lenRod_2, angularVel_1, angularVel_2,
    pendDisAngle_2, xVel_1, xVel_2, yVel_1, yVel_2, xPos_1, xPos_2, yPos_1,
    yPos_2, xAccel_1, yAccel_1, xAccel_2, yAccel_2, pendDisAngle_1, angularAccel_1,
    angularAccel_2, tension_1, tension_2, massObj_1, massObj_2)
import Control.Lens ((^.))

-- Velocity X/Y First Object
velXExpr_1, velYExpr_1 :: Expr
velXExpr_1 = sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)
velYExpr_1 = sy angularVel_1 `mulRe` (positionXDD_1 ^. defnExpr)

velDerivEqn1, velXDerivEqn2_1, velXDerivEqn3_1, velXDerivEqn4_1 :: Expr
velDerivEqn1    = sy velocity $= positionGDD ^. defnExpr
velXDerivEqn2_1 = sy xPos_1 $= positionXDD_1 ^. defnExpr
velXDerivEqn3_1 = sy xVel_1 $= deriv (positionXDD_1 ^. defnExpr) time
velXDerivEqn4_1 = sy xVel_1 $= sy lenRod_1 `mulRe` deriv (sin (sy pendDisAngle_1)) time

velYDerivEqn2_1,velYDerivEqn3_1,velYDerivEqn4_1 :: Expr
velYDerivEqn2_1 = sy yPos_1 $= positionYDD_1 ^. defnExpr
velYDerivEqn3_1 = sy yVel_1 $= neg (deriv (sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)) time)
velYDerivEqn4_1 = sy yVel_1 $= neg (sy lenRod_1 `mulRe` deriv (cos (sy pendDisAngle_1)) time)

-- Velocity X/Y Second Object
velXExpr_2, velYExpr_2 :: Expr
velXExpr_2 = sy xVel_1 `addRe` (sy angularVel_2 `mulRe` sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))
velYExpr_2 = sy yVel_1 `addRe` (sy angularVel_2 `mulRe` sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))

velXDerivEqn2_2, velXDerivEqn3_2 :: Expr
velXDerivEqn2_2 = sy xPos_2 $= positionXDD_2 ^. defnExpr
velXDerivEqn3_2 = sy xVel_2 $= deriv (positionXDD_2 ^. defnExpr) time

velYDerivEqn2_2,velYDerivEqn3_2 :: Expr
velYDerivEqn2_2 = sy yPos_2 $= positionYDD_2 ^. defnExpr
velYDerivEqn3_2 = sy yVel_2 $= neg (deriv (positionYDD_2 ^. defnExpr) time)

-- Acceleration X/Y First Object
accelXExpr_1, accelYExpr_1 :: Expr
accelXExpr_1 = neg (square (sy angularVel_1) `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1))
                `addRe` (sy angularAccel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1))
accelYExpr_1 = (square (sy angularVel_1) `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1))
                `addRe` (sy angularAccel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1))

accelDerivEqn1, accelXDerivEqn3_1, accelXDerivEqn4_1 :: Expr
accelDerivEqn1    = sy acceleration $= accelGDD ^. defnExpr
accelXDerivEqn3_1 = sy xAccel_1 $= deriv (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)) time
accelXDerivEqn4_1 = sy xAccel_1 $= deriv (sy angularVel_1) time `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)
                    $- (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1) `mulRe` deriv (sy pendDisAngle_1) time)

accelYDerivEqn3_1, accelYDerivEqn4_1 :: Expr
accelYDerivEqn3_1 = sy yAccel_1  $= deriv (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)) time
accelYDerivEqn4_1 = sy yAccel_1 $= deriv (sy angularVel_1) time `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)
                    `addRe` (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1) `mulRe` deriv (sy pendDisAngle_1) time)

-- Acceleration X/Y Second Object
accelXExpr_2, accelYExpr_2 :: Expr
accelXExpr_2 = sy xAccel_1 $-
                (square (sy angularVel_2) `mulRe` sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))
                `addRe` (sy angularAccel_2 `mulRe` sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))
accelYExpr_2 = sy yAccel_1 `addRe`
                (square (sy angularVel_2) `mulRe` sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))
                `addRe` (sy angularAccel_2 `mulRe` sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))

accelXDerivEqn3_2 :: Expr
accelXDerivEqn3_2 = sy xAccel_2 $= deriv velXExpr_2 time

accelYDerivEqn3_2 :: Expr
accelYDerivEqn3_2 = sy yAccel_2  $= deriv velYExpr_2 time

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

-- Angular acceleration explanation in IM
angularAccelDerivEqns :: [Expr]
angularAccelDerivEqns = [angularAccelDerivEqn1, angularAccelDerivEqn2, angularAccelDerivEqn3, angularAccelDerivEqn4,
                       angularAccelDerivEqn5, angularAccelDerivEqn6, angularAccelDerivEqn7, angularAccelDerivEqn8]

angularAccelDerivEqn1, angularAccelDerivEqn2, angularAccelDerivEqn3, angularAccelDerivEqn4,
  angularAccelDerivEqn5, angularAccelDerivEqn6, angularAccelDerivEqn7, angularAccelDerivEqn8 :: Expr
angularAccelDerivEqn1 = sy massObj_1 `mulRe` sy xAccel_1 $=
                      neg (sy tension_1) `mulRe` sin (sy pendDisAngle_1) $- (sy massObj_2 `mulRe` sy xAccel_2)
angularAccelDerivEqn2 = sy massObj_1 `mulRe` sy yAccel_1 $=
                      sy tension_1 `mulRe` cos (sy pendDisAngle_1) $- (sy massObj_2 `mulRe` sy yAccel_2) $-
                      (sy massObj_2 `mulRe` sy gravitationalAccel) $- (sy massObj_1 `mulRe` sy gravitationalAccel)
angularAccelDerivEqn3 = sy tension_1 `mulRe` sin (sy pendDisAngle_1) `mulRe` cos (sy pendDisAngle_1) $=
                      neg (cos (sy pendDisAngle_1)) `mulRe`
                      ((sy massObj_1 `mulRe` sy xAccel_1) `addRe` (sy massObj_2 `mulRe` sy xAccel_2))
angularAccelDerivEqn4 = sy tension_1 `mulRe` sin (sy pendDisAngle_1) `mulRe` cos (sy pendDisAngle_1) $=
                      sin (sy pendDisAngle_1) `mulRe` 
                      (
                          (sy massObj_1 `mulRe` sy yAccel_1) `addRe` (sy massObj_2 `mulRe` sy yAccel_2) `addRe`
                          (sy massObj_2 `mulRe` sy gravitationalAccel) `addRe` (sy massObj_1 `mulRe` sy gravitationalAccel)
                      )
angularAccelDerivEqn5 = sin (sy pendDisAngle_1) `mulRe` 
                      (
                          (sy massObj_1 `mulRe` sy yAccel_1) `addRe` (sy massObj_2 `mulRe` sy yAccel_2) `addRe`
                          (sy massObj_2 `mulRe` sy gravitationalAccel) `addRe` (sy massObj_1 `mulRe` sy gravitationalAccel)
                      ) $=
                      neg (cos (sy pendDisAngle_1)) `mulRe` 
                      ((sy massObj_1 `mulRe` sy xAccel_1) `addRe` (sy massObj_2 `mulRe` sy xAccel_2))
angularAccelDerivEqn6 = sy tension_2 `mulRe` sin(sy pendDisAngle_2) `mulRe` cos (sy pendDisAngle_2) $=
                      neg (cos (sy pendDisAngle_2)) `mulRe` sy massObj_2 `mulRe` sy xAccel_2
angularAccelDerivEqn7 = sy tension_1 `mulRe` sin (sy pendDisAngle_2 ) `mulRe` cos (sy pendDisAngle_2) $=
                      sin (sy pendDisAngle_2) `mulRe`
                      ((sy massObj_2 `mulRe` sy yAccel_2) `addRe` (sy massObj_2 `mulRe` sy gravitationalAccel))
angularAccelDerivEqn8 = sin (sy pendDisAngle_2) `mulRe` 
                      ((sy massObj_2 `mulRe` sy yAccel_2) `addRe` (sy massObj_2 `mulRe` sy gravitationalAccel)) $=
                      neg (cos (sy pendDisAngle_2)) `mulRe` sy massObj_2 `mulRe` sy xAccel_2

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
