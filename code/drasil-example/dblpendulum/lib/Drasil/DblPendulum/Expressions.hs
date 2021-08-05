module Drasil.DblPendulum.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics(xPos, yPos, velocity, angularVelocity,
    angularAccel, xAccel, yAccel, acceleration, tension, gravitationalAccel,
    angularFrequency, torque, momentOfInertia, time,
    momentOfInertia, period, position)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.DblPendulum.DataDefs (positionXDD_1, positionYDD_1, positionXDD_2, positionYDD_2, positionGDD, accelGDD)
import Drasil.DblPendulum.Unitals (lenRod_1, lenRod_2, angularVel_1, angularVel_2,
    pendDisAngle_2, xVel_1, xVel_2, yVel_1, yVel_2, xPos_1, xPos_2, yPos_1,
    yPos_2, xAccel_1, yAccel_1, xAccel_2, yAccel_2, pendDisAngle_1, initialPendAngle, angularAccel_1,
    angularAccel_2)
import Control.Lens ((^.))

-- Velocity X/Y First Object
velXExpr_1, velYExpr_1 :: Expr
velXExpr_1 = sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)
velYExpr_1 = sy angularVel_1 `mulRe` (positionXDD_1 ^. defnExpr)

velDerivEqn1, velXDerivEqn2_1, velXDerivEqn3_1, velXDerivEqn4_1 :: Expr
velDerivEqn1 = sy velocity $= positionGDD ^. defnExpr
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
accelDerivEqn1 = sy acceleration $= accelGDD ^. defnExpr
accelXDerivEqn3_1 = sy xAccel_1 $= deriv (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)) time
accelXDerivEqn4_1 = sy xAccel_1 $= deriv (sy angularVel_1) time `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)
                    $- (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1) `mulRe` deriv (sy pendDisAngle_1) time)

accelYDerivEqn3_1, accelYDerivEqn4_1 :: Expr
accelYDerivEqn3_1 = sy yAccel_1  $= deriv (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)) time
accelYDerivEqn4_1 = sy yAccel_1 $= deriv (sy angularVel_1) time `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)
                    `addRe` (sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1) `mulRe` deriv (sy pendDisAngle_1) time)

-- Acceleration X/Y Second Object
accelXExpr_2, accelYExpr_2 :: Expr
accelXExpr_2 = sy xAccel_1 `addRe`
                neg (square (sy angularVel_2) `mulRe` sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))
                `addRe` (sy angularAccel_2 `mulRe` sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))
accelYExpr_2 = sy yAccel_1 `addRe`
                (square (sy angularVel_2) `mulRe` sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))
                `addRe` (sy angularAccel_2 `mulRe` sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))

accelXDerivEqn3_2 :: Expr
accelXDerivEqn3_2 = sy xAccel_2 $= deriv velXExpr_2 time

accelYDerivEqn3_2 :: Expr
accelYDerivEqn3_2 = sy yAccel_2  $= deriv velYExpr_2 time

-- Horizontal/Vertical force acting on the first object
hForceOnPendulumViaComponent, hForceOnPendulumViaAngle :: Expr
hForceOnPendulumViaComponent = sy mass `mulRe` sy xAccel_1
hForceOnPendulumViaAngle = neg (sy tension `mulRe` sin (sy pendDisAngle_1))

vForceOnPendulumViaComponent, vForceOnPendulumViaAngle :: Expr
vForceOnPendulumViaComponent = sy mass `mulRe` sy yAccel_1
vForceOnPendulumViaAngle = sy tension `mulRe` cos (sy pendDisAngle_1)
                            $- (sy mass `mulRe` sy gravitationalAccel)

-- Horizontal/Vertical force acting on the second object


-- Angular Frequency Of the First Object
angFrequencyExpr :: Expr
angFrequencyExpr = sqrt (sy gravitationalAccel $/ sy lenRod_1)


angFrequencyDerivEqns :: [Expr]
angFrequencyDerivEqns = [angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3,
                     angFrequencyDerivEqn4, angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7]

angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3, angFrequencyDerivEqn4,
                   angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7 :: Expr

angFrequencyDerivEqn1 = sy torque $= neg (sy lenRod_1) `mulRe` (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisAngle_1))
angFrequencyDerivEqn2 = sy momentOfInertia `mulRe` sy angularAccel $= neg (sy lenRod_1) `mulRe` (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisAngle_1))
angFrequencyDerivEqn3 = sy momentOfInertia `mulRe` deriv (deriv (sy pendDisAngle_1) time) time $= neg (sy lenRod_1)
             `mulRe` sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisAngle_1)
angFrequencyDerivEqn4 = (sy mass `mulRe` square (sy lenRod_1)) `mulRe` deriv (deriv (sy pendDisAngle_1) time) time $= neg (sy lenRod_1)
             `mulRe` sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisAngle_1)
angFrequencyDerivEqn5 = deriv (deriv (sy pendDisAngle_1) time) time $= neg (sy gravitationalAccel $/ sy lenRod_1) `mulRe` sin (sy pendDisAngle_1)
angFrequencyDerivEqn6 = deriv (deriv (sy pendDisAngle_1) time) time $= neg (sy gravitationalAccel $/ sy lenRod_1) `mulRe` sy pendDisAngle_1
angFrequencyDerivEqn7 = sy angularFrequency $= sqrt (sy gravitationalAccel $/ sy lenRod_1)

-- Angular Frequency Of the Second Object


-- Period of Motion in the First Object
periodPendExpr :: Expr
periodPendExpr = exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod_1 $/ sy gravitationalAccel)

periodPendDerivEqns :: [Expr]
periodPendDerivEqns = [periodPendDerivEqn1, periodPendDerivEqn2]

periodPendDerivEqn1, periodPendDerivEqn2 :: Expr
periodPendDerivEqn1 = sy angularFrequency $= sqrt (sy gravitationalAccel $/ sy lenRod_1)
periodPendDerivEqn2 = sy period $= exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod_1 $/ sy gravitationalAccel)

-- Period of Motion in the Second Object


-- Angular Displacement in the First Object
angularDisplacementExpr :: Expr
angularDisplacementExpr = sy initialPendAngle `mulRe` cos (sy angularFrequency `mulRe` sy time)

angularDisplacementDerivEqns :: [Expr]
angularDisplacementDerivEqns = [angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
                                 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5]

angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5 :: Expr
angularDisplacementDerivEqn1 = sy torque $= sy momentOfInertia `mulRe` sy angularAccel
angularDisplacementDerivEqn2 = neg (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisAngle_1) `mulRe` sy lenRod_1) $= (sy mass `mulRe` square (sy lenRod_1))
                                `mulRe` deriv (deriv (sy pendDisAngle_1) time) time
angularDisplacementDerivEqn3 = deriv (deriv (sy pendDisAngle_1) time) time `addRe` ((sy gravitationalAccel $/ sy lenRod_1) `mulRe` sin (sy pendDisAngle_1)) $= exactDbl 0
angularDisplacementDerivEqn4 = deriv (deriv (sy pendDisAngle_1) time) time `addRe` ((sy gravitationalAccel $/ sy lenRod_1) `mulRe` sy pendDisAngle_1) $= exactDbl 0
angularDisplacementDerivEqn5 = apply1 pendDisAngle_1 time $= sy initialPendAngle `mulRe` cos ( sy angularFrequency `mulRe` sy time)

-- Angular Displacement in the Second Object

