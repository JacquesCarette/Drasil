module Drasil.SglPend.Derivations where

import Prelude hiding (sin, cos, tan, sqrt)

import Language.Drasil

import Data.Drasil.Quantities.Physics (acceleration, angularAccel, angularFrequency,
  angularVelocity, gravitationalAccel, momentOfInertia, period, position, time,
  torque, velocity, xAccel, xPos, xVel, yAccel, yPos, yVel)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.SglPend.Unitals (lenRod, pendDisplacementAngle, initialPendAngle)

-- Velocity IX/IY
velocityIDerivEqn1, velocityIXDerivEqn2, velocityIXDerivEqn3, velocityIXDerivEqn4 :: ModelExpr
velocityIDerivEqn1 = sy velocity $= deriv (sy position) time
velocityIXDerivEqn2 = sy xPos $= sy lenRod `mulRe` sin (sy pendDisplacementAngle)
velocityIXDerivEqn3 = sy xVel $= deriv (sy lenRod `mulRe` sin (sy pendDisplacementAngle)) time
velocityIXDerivEqn4 = sy xVel $= sy lenRod `mulRe` deriv (sin (sy pendDisplacementAngle)) time

velocityIXDerivEqns :: [ModelExpr]
velocityIXDerivEqns = [velocityIDerivEqn1, velocityIXDerivEqn2, velocityIXDerivEqn3, velocityIXDerivEqn4]

velocityIYDerivEqn2,velocityIYDerivEqn3,velocityIYDerivEqn4 :: ModelExpr
velocityIYDerivEqn2 = sy yPos $= neg (sy lenRod `mulRe` cos (sy pendDisplacementAngle))
velocityIYDerivEqn3 = sy yVel $= neg (deriv (sy lenRod `mulRe` cos (sy pendDisplacementAngle)) time)
velocityIYDerivEqn4 = sy yVel $= neg (sy lenRod `mulRe` deriv (cos (sy pendDisplacementAngle)) time)

velocityIYDerivEqns :: [ModelExpr]
velocityIYDerivEqns = [velocityIDerivEqn1, velocityIYDerivEqn2, velocityIYDerivEqn3, velocityIYDerivEqn4]

-- Acceleration IX/IY
accelerationIDerivEqn1, accelerationIXDerivEqn3, accelerationIXDerivEqn4 :: ModelExpr
accelerationIDerivEqn1 = sy acceleration $= deriv (sy velocity) time 
accelerationIXDerivEqn3 = sy xAccel $= deriv (sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)) time
accelerationIXDerivEqn4 = sy xAccel $= deriv (sy angularVelocity) time `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)
                        $- (sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle) `mulRe` deriv (sy pendDisplacementAngle) time)

accelerationIYDerivEqn3, accelerationIYDerivEqn4 :: ModelExpr
accelerationIYDerivEqn3 = sy yAccel $= deriv (sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)) time
accelerationIYDerivEqn4 = sy yAccel $= deriv (sy angularVelocity) time `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)
                        `addRe` (sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle) `mulRe` deriv (sy pendDisplacementAngle) time)

-- Angular Frequency Of Pendulum
angFrequencyDerivEqns :: [ModelExpr]
angFrequencyDerivEqns = [angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3,
                     angFrequencyDerivEqn4, angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7]

angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3, angFrequencyDerivEqn4,
                   angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7 :: ModelExpr

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
periodPendDerivEqns :: [ModelExpr]
periodPendDerivEqns = [periodPendDerivEqn1, periodPendDerivEqn2]

periodPendDerivEqn1, periodPendDerivEqn2 :: ModelExpr
periodPendDerivEqn1 = sy angularFrequency $= sqrt (sy gravitationalAccel $/ sy lenRod)
periodPendDerivEqn2 = sy period $= exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod $/ sy gravitationalAccel)

-- Angular Displacement
angularDisplacementDerivEqns :: [ModelExpr]
angularDisplacementDerivEqns = [angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
                                 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5]

angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5 :: ModelExpr
angularDisplacementDerivEqn1 = sy torque $= sy momentOfInertia `mulRe` sy angularAccel
angularDisplacementDerivEqn2 = neg (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle) `mulRe` sy lenRod) $= (sy mass `mulRe` square (sy lenRod))
                                `mulRe` deriv (deriv (sy pendDisplacementAngle) time) time
angularDisplacementDerivEqn3 = deriv (deriv (sy pendDisplacementAngle) time) time `addRe` ((sy gravitationalAccel $/ sy lenRod) `mulRe` sin (sy pendDisplacementAngle)) $= exactDbl 0
angularDisplacementDerivEqn4 = deriv (deriv (sy pendDisplacementAngle) time) time `addRe` ((sy gravitationalAccel $/ sy lenRod) `mulRe` sy pendDisplacementAngle) $= exactDbl 0
angularDisplacementDerivEqn5 = apply1 pendDisplacementAngle time $= sy initialPendAngle `mulRe` cos ( sy angularFrequency `mulRe` sy time)
