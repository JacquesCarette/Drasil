module Drasil.SglPend.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (angularAccel, angularFrequency, angularVelocity,
    gravitationalAccel, tension, time, xAccel, yAccel)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.SglPend.Unitals (lenRod, pendDisplacementAngle, initialPendAngle)

-- Velocity IX/IY
velocityIXExpr, velocityIYExpr :: Expr
velocityIXExpr = sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)
velocityIYExpr = sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)

-- Acceleration IX/IY
accelerationIXExpr, accelerationIYExpr :: Expr
accelerationIXExpr = neg (square (sy angularVelocity) `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle))
                    `addRe` (sy angularAccel `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle))
accelerationIYExpr = (square (sy angularVelocity) `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle))
                    `addRe` (sy angularAccel `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle))

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

-- Period of Pendulum Motion
periodPendExpr :: Expr
periodPendExpr = exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod $/ sy gravitationalAccel)

-- Angular Displacement
angularDisplacementExpr :: Expr
angularDisplacementExpr = sy initialPendAngle `mulRe` cos (sy angularFrequency `mulRe` sy time)
