module Drasil.DblPendulum.Derivations where

import Prelude hiding (sin, cos)

import Language.Drasil (ModelExprC(..), ExprC(..),
  Express(..), ModelExpr, DefiningExpr(..))

import Data.Drasil.Quantities.Physics(velocity, acceleration, gravitationalMagnitude, time)
import Drasil.DblPendulum.DataDefs
import Drasil.DblPendulum.Expressions (velXExpr_2, velYExpr_2)
import Drasil.DblPendulum.Unitals (lenRod_1, massObj_1, massObj_2,
  xPos_1, xPos_2, yPos_1, yPos_2, xVel_1, xVel_2, yVel_1, yVel_2, xAccel_1, xAccel_2,
  yAccel_1, yAccel_2, tension_1, tension_2, angularVel_1, pendDisAngle_1, pendDisAngle_2)
import Control.Lens ((^.))


-- Velocity X/Y First Object
velDerivEqn1, velXDerivEqn2_1, velXDerivEqn3_1, velXDerivEqn4_1 :: ModelExpr
velDerivEqn1    = sy velocity $= positionGQD ^. defnExpr
velXDerivEqn2_1 = sy xPos_1 $= positionXEqn_1
velXDerivEqn3_1 = sy xVel_1 $= deriv positionXEqn_1 time
velXDerivEqn4_1 = sy xVel_1 $= sy lenRod_1 `mulRe` deriv (sin (sy pendDisAngle_1)) time

velYDerivEqn2_1,velYDerivEqn3_1,velYDerivEqn4_1 :: ModelExpr
velYDerivEqn2_1 = sy yPos_1 $= express (positionYQD_1 ^. defnExpr)
velYDerivEqn3_1 = sy yVel_1 $= neg (deriv (sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)) time)
velYDerivEqn4_1 = sy yVel_1 $= neg (sy lenRod_1 `mulRe` deriv (cos (sy pendDisAngle_1)) time)

-- Velocity X/Y Second Object
velXDerivEqn2_2, velXDerivEqn3_2 :: ModelExpr
velXDerivEqn2_2 = sy xPos_2 $= express (positionXQD_2 ^. defnExpr)
velXDerivEqn3_2 = sy xVel_2 $= deriv positionXEqn_2 time

velYDerivEqn2_2,velYDerivEqn3_2 :: ModelExpr
velYDerivEqn2_2 = sy yPos_2 $= express (positionYQD_2 ^. defnExpr)
velYDerivEqn3_2 = sy yVel_2 $= neg (deriv positionYEqn_2 time)

-- Acceleration X/Y First Object

accelDerivEqn1, accelXDerivEqn3_1, accelXDerivEqn4_1 :: ModelExpr
accelDerivEqn1    = sy acceleration $= accelGQD ^. defnExpr
accelXDerivEqn3_1 = sy xAccel_1 $= deriv (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)) time
accelXDerivEqn4_1 = sy xAccel_1 $= deriv (sy angularVel_1) time `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)
                    $- (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1) `mulRe` deriv (sy pendDisAngle_1) time)

accelYDerivEqn3_1, accelYDerivEqn4_1 :: ModelExpr
accelYDerivEqn3_1 = sy yAccel_1  $= deriv (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)) time
accelYDerivEqn4_1 = sy yAccel_1 $= deriv (sy angularVel_1) time `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)
                    `addRe` (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1) `mulRe` deriv (sy pendDisAngle_1) time)    

-- Acceleration X/Y Second Object
accelXDerivEqn3_2 :: ModelExpr
accelXDerivEqn3_2 = sy xAccel_2 $= deriv velXExpr_2 time

accelYDerivEqn3_2 :: ModelExpr
accelYDerivEqn3_2 = sy yAccel_2 $= deriv velYExpr_2 time


-- Angular acceleration explanation in IM
angularAccelDerivEqns :: [ModelExpr]
angularAccelDerivEqns = [angularAccelDerivEqn1, angularAccelDerivEqn2, angularAccelDerivEqn3, angularAccelDerivEqn4,
                       angularAccelDerivEqn5, angularAccelDerivEqn6, angularAccelDerivEqn7, angularAccelDerivEqn8]

angularAccelDerivEqn1, angularAccelDerivEqn2, angularAccelDerivEqn3, angularAccelDerivEqn4,
  angularAccelDerivEqn5, angularAccelDerivEqn6, angularAccelDerivEqn7, angularAccelDerivEqn8 :: ModelExpr
angularAccelDerivEqn1 = sy massObj_1 `mulRe` sy xAccel_1 $=
                      neg (sy tension_1) `mulRe` sin (sy pendDisAngle_1) $- (sy massObj_2 `mulRe` sy xAccel_2)
angularAccelDerivEqn2 = sy massObj_1 `mulRe` sy yAccel_1 $=
                      sy tension_1 `mulRe` cos (sy pendDisAngle_1) $- (sy massObj_2 `mulRe` sy yAccel_2) $-
                      (sy massObj_2 `mulRe` sy gravitationalMagnitude) $- (sy massObj_1 `mulRe` sy gravitationalMagnitude)
angularAccelDerivEqn3 = sy tension_1 `mulRe` sin (sy pendDisAngle_1) `mulRe` cos (sy pendDisAngle_1) $=
                      neg (cos (sy pendDisAngle_1)) `mulRe`
                      ((sy massObj_1 `mulRe` sy xAccel_1) `addRe` (sy massObj_2 `mulRe` sy xAccel_2))
angularAccelDerivEqn4 = sy tension_1 `mulRe` sin (sy pendDisAngle_1) `mulRe` cos (sy pendDisAngle_1) $=
                      sin (sy pendDisAngle_1) `mulRe` 
                      (
                          (sy massObj_1 `mulRe` sy yAccel_1) `addRe` (sy massObj_2 `mulRe` sy yAccel_2) `addRe`
                          (sy massObj_2 `mulRe` sy gravitationalMagnitude) `addRe` (sy massObj_1 `mulRe` sy gravitationalMagnitude)
                      )
angularAccelDerivEqn5 = sin (sy pendDisAngle_1) `mulRe` 
                      (
                          (sy massObj_1 `mulRe` sy yAccel_1) `addRe` (sy massObj_2 `mulRe` sy yAccel_2) `addRe`
                          (sy massObj_2 `mulRe` sy gravitationalMagnitude) `addRe` (sy massObj_1 `mulRe` sy gravitationalMagnitude)
                      ) $=
                      neg (cos (sy pendDisAngle_1)) `mulRe` 
                      ((sy massObj_1 `mulRe` sy xAccel_1) `addRe` (sy massObj_2 `mulRe` sy xAccel_2))
angularAccelDerivEqn6 = sy tension_2 `mulRe` sin(sy pendDisAngle_2) `mulRe` cos (sy pendDisAngle_2) $=
                      neg (cos (sy pendDisAngle_2)) `mulRe` sy massObj_2 `mulRe` sy xAccel_2
angularAccelDerivEqn7 = sy tension_1 `mulRe` sin (sy pendDisAngle_2 ) `mulRe` cos (sy pendDisAngle_2) $=
                      sin (sy pendDisAngle_2) `mulRe`
                      ((sy massObj_2 `mulRe` sy yAccel_2) `addRe` (sy massObj_2 `mulRe` sy gravitationalMagnitude))
angularAccelDerivEqn8 = sin (sy pendDisAngle_2) `mulRe` 
                      ((sy massObj_2 `mulRe` sy yAccel_2) `addRe` (sy massObj_2 `mulRe` sy gravitationalMagnitude)) $=
                      neg (cos (sy pendDisAngle_2)) `mulRe` sy massObj_2 `mulRe` sy xAccel_2
