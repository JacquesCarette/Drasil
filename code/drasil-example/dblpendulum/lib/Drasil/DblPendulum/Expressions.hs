module Drasil.DblPendulum.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics(xPos, yPos, velocity, angularVelocity, xVel, yVel,
    angularAccel, xAccel, yAccel, acceleration, tension, gravitationalAccel,
    angularFrequency, torque, momentOfInertia, time,
    momentOfInertia, period, position)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.DblPendulum.Unitals (lenRod, lenRod_1, lenRod_2, angularVel_1, angularVel_2,
    pendDisAngle_1, pendDisAngle_2,
    pendDisplacementAngle, initialPendAngle, )

-- Velocity IX/IY First Object
velocityIXExpr_1, velocityIYExpr_1 :: Expr
velocityIXExpr_1 = sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)
velocityIYExpr_1 = sy angularVel_1 `mulRe` sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)

velocityIDerivEqn1_1, velocityIXDerivEqn2_1, velocityIXDerivEqn3_1, velocityIXDerivEqn4_1 :: Expr
velocityIDerivEqn1_1 = sy velocity $= deriv (sy position) time
velocityIXDerivEqn2_1 = sy xPos $= sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)
velocityIXDerivEqn3_1 = sy xVel $= deriv (sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)) time
velocityIXDerivEqn4_1 = sy xVel $= sy lenRod_1 `mulRe` deriv (sin (sy pendDisAngle_1)) time

velocityIYDerivEqn2_1,velocityIYDerivEqn3_1,velocityIYDerivEqn4_1 :: Expr
velocityIYDerivEqn2_1 = sy yPos $= neg (sy lenRod_1 `mulRe` cos (sy pendDisAngle_1))
velocityIYDerivEqn3_1 = sy yVel $= neg (deriv (sy lenRod_1 `mulRe` cos (sy pendDisAngle_1)) time)
velocityIYDerivEqn4_1 = sy yVel $= neg (sy lenRod_1 `mulRe` deriv (cos (sy pendDisAngle_1)) time)

-- Velocity IX/IY Second Object


-- Acceleration IX/IY First Object
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

-- Acceleration IX/IY Second Object


-- Horizontal/Vertical force acting on the first object
hForceOnPendulumViaComponent, hForceOnPendulumViaAngle :: Expr
hForceOnPendulumViaComponent = sy mass `mulRe` sy xAccel
hForceOnPendulumViaAngle = neg (sy tension `mulRe` sin (sy pendDisplacementAngle))

vForceOnPendulumViaComponent, vForceOnPendulumViaAngle :: Expr
vForceOnPendulumViaComponent = sy mass `mulRe` sy yAccel
vForceOnPendulumViaAngle = sy tension `mulRe` cos (sy pendDisplacementAngle)
                            $- (sy mass `mulRe` sy gravitationalAccel)

-- Horizontal/Vertical force acting on the second object


-- Angular Frequency Of the First Object
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

-- Angular Frequency Of the Second Object


-- Period of Motion in the First Object
periodPendExpr :: Expr
periodPendExpr = exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod $/ sy gravitationalAccel)

periodPendDerivEqns :: [Expr]
periodPendDerivEqns = [periodPendDerivEqn1, periodPendDerivEqn2]

periodPendDerivEqn1, periodPendDerivEqn2 :: Expr
periodPendDerivEqn1 = sy angularFrequency $= sqrt (sy gravitationalAccel $/ sy lenRod)
periodPendDerivEqn2 = sy period $= exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod $/ sy gravitationalAccel)

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
angularDisplacementDerivEqn2 = neg (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle) `mulRe` sy lenRod) $= (sy mass `mulRe` square (sy lenRod))
                                `mulRe` deriv (deriv (sy pendDisplacementAngle) time) time
angularDisplacementDerivEqn3 = deriv (deriv (sy pendDisplacementAngle) time) time `addRe` ((sy gravitationalAccel $/ sy lenRod) `mulRe` sin (sy pendDisplacementAngle)) $= exactDbl 0
angularDisplacementDerivEqn4 = deriv (deriv (sy pendDisplacementAngle) time) time `addRe` ((sy gravitationalAccel $/ sy lenRod) `mulRe` sy pendDisplacementAngle) $= exactDbl 0
angularDisplacementDerivEqn5 = apply1 pendDisplacementAngle time $= sy initialPendAngle `mulRe` cos ( sy angularFrequency `mulRe` sy time)

-- Angular Displacement in the Second Object

