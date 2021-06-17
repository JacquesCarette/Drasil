module Drasil.DblPendulum.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics(xPos, yPos, velocity, angularVelocity, xVel, yVel,
    angularAccel, xAccel, yAccel, acceleration, tension, gravitationalAccel,
    angularFrequency, torque, momentOfInertia, time,
    momentOfInertia, period, position)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle)

-- Velocity IX/IY
velocityIXExpr, velocityIYExpr :: Expr
velocityIXExpr = sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)
velocityIYExpr = sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)

velocityIDerivEqn1, velocityIXDerivEqn2, velocityIXDerivEqn3, velocityIXDerivEqn4 :: Expr
velocityIDerivEqn1 = sy velocity $= deriv (sy position) time
velocityIXDerivEqn2 = sy xPos $= sy lenRod `mulRe` sin (sy pendDisplacementAngle)
velocityIXDerivEqn3 = sy xVel $= deriv (sy lenRod `mulRe` sin (sy pendDisplacementAngle)) time
velocityIXDerivEqn4 = sy xVel $= sy lenRod `mulRe` deriv (sin (sy pendDisplacementAngle)) time

velocityIYDerivEqn2,velocityIYDerivEqn3,velocityIYDerivEqn4 :: Expr
velocityIYDerivEqn2 = sy yPos $= neg (sy lenRod `mulRe` cos (sy pendDisplacementAngle))
velocityIYDerivEqn3 = sy yVel $= neg (deriv (sy lenRod `mulRe` cos (sy pendDisplacementAngle)) time)
velocityIYDerivEqn4 = sy yVel $= neg (sy lenRod `mulRe` deriv (cos (sy pendDisplacementAngle)) time)

-- Acceleration IX/IY
accelerationIXExpr, accelerationIYExpr :: Expr
accelerationIXExpr = neg (square (sy angularVelocity) `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle))
                    `addRe` (sy angularAccel `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle))
accelerationIYExpr = (square (sy angularVelocity) `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle))
                    `addRe` (sy angularAccel `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle))

accelerationIDerivEqn1, accelerationIXDerivEqn3, accelerationIXDerivEqn4 :: Expr
accelerationIDerivEqn1 = sy acceleration $= deriv (sy velocity) time 
accelerationIXDerivEqn3 = sy xAccel $= deriv (sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)) time
accelerationIXDerivEqn4 = sy xAccel $= deriv (sy angularVelocity) time `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)
                        $- (sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle) `mulRe` deriv (sy pendDisplacementAngle) time)

accelerationIYDerivEqn3, accelerationIYDerivEqn4 :: Expr
accelerationIYDerivEqn3 = sy yAccel $= deriv (sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)) time
accelerationIYDerivEqn4 = sy yAccel $= deriv (sy angularVelocity) time `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)
                        `addRe` (sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle) `mulRe` deriv (sy pendDisplacementAngle) time)

-- Horizontal/Vertical force acting on the pendulum
hForceOnPendulumViaComponent, hForceOnPendulumViaAngle :: Expr
hForceOnPendulumViaComponent = sy mass `mulRe` sy xAccel
hForceOnPendulumViaAngle = neg (sy tension `mulRe` sin (sy pendDisplacementAngle))

vForceOnPendulumViaComponent, vForceOnPendulumViaAngle :: Expr
vForceOnPendulumViaComponent = sy mass `mulRe` sy yAccel
vForceOnPendulumViaAngle = sy tension `mulRe` cos (sy pendDisplacementAngle)
                            $- (sy mass `mulRe` sy gravitationalAccel)

-- Angular Frequency Of Pendulum
angFrequencyExpr :: Expr
angFrequencyExpr = sqrt (sy gravitationalAccel $/ sy lenRod)


angFrequencyDerivEqns :: [Expr]
angFrequencyDerivEqns = [angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3,
                     angFrequencyDerivEqn4, angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7]

angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3, angFrequencyDerivEqn4,
                   angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7 :: Expr

angFrequencyDerivEqn1 = sy torque $= neg (sy lenRod) `mulRe` (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle))
angFrequencyDerivEqn2 = sy momentOfInertia `mulRe` sy angularAccel $= neg (sy lenRod) `mulRe` (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle))
angFrequencyDerivEqn3 = sy momentOfInertia `mulRe` deriv (deriv (sy pendDisplacementAngle) time) time $= neg (sy lenRod)
             `mulRe` sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle)
angFrequencyDerivEqn4 = (sy mass `mulRe` square (sy lenRod)) `mulRe` deriv (deriv (sy pendDisplacementAngle) time) time $= neg (sy lenRod)
             `mulRe` sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle)
angFrequencyDerivEqn5 = deriv (deriv (sy pendDisplacementAngle) time) time $= neg (sy gravitationalAccel $/ sy lenRod) `mulRe` sin (sy pendDisplacementAngle)
angFrequencyDerivEqn6 = deriv (deriv (sy pendDisplacementAngle) time) time $= neg (sy gravitationalAccel $/ sy lenRod) `mulRe` sy pendDisplacementAngle
angFrequencyDerivEqn7 = sy angularFrequency $= sqrt (sy gravitationalAccel $/ sy lenRod)

-- Period of Pendulum Motion
periodPendExpr :: Expr
periodPendExpr = exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod $/ sy gravitationalAccel)

periodPendDerivEqns :: [Expr]
periodPendDerivEqns = [periodPendDerivEqn1, periodPendDerivEqn2]

periodPendDerivEqn1, periodPendDerivEqn2 :: Expr
periodPendDerivEqn1 = sy angularFrequency $= sqrt (sy gravitationalAccel $/ sy lenRod)
periodPendDerivEqn2 = sy period $= exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod $/ sy gravitationalAccel)
