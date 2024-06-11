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
velocityIXExpr = sy angularVelocity `mul` sy lenRod `mul` cos (sy pendDisplacementAngle)
velocityIYExpr = sy angularVelocity `mul` sy lenRod `mul` sin (sy pendDisplacementAngle)

-- Acceleration IX/IY
accelerationIXExpr, accelerationIYExpr :: Expr
accelerationIXExpr = neg (square (sy angularVelocity) `mul` sy lenRod `mul` sin (sy pendDisplacementAngle))
                    `add` (sy angularAccel `mul` sy lenRod `mul` cos (sy pendDisplacementAngle))
accelerationIYExpr = (square (sy angularVelocity) `mul` sy lenRod `mul` cos (sy pendDisplacementAngle))
                    `add` (sy angularAccel `mul` sy lenRod `mul` sin (sy pendDisplacementAngle))

-- Horizontal/Vertical force acting on the pendulum
hForceOnPendulumViaComponent, hForceOnPendulumViaAngle :: Expr
hForceOnPendulumViaComponent = sy mass `mul` sy xAccel
hForceOnPendulumViaAngle = neg (sy tension `mul` sin (sy pendDisplacementAngle))

vForceOnPendulumViaComponent, vForceOnPendulumViaAngle :: Expr
vForceOnPendulumViaComponent = sy mass `mul` sy yAccel
vForceOnPendulumViaAngle = sy tension `mul` cos (sy pendDisplacementAngle)
                            $- (sy mass `mul` sy gravitationalAccel)

-- Angular Frequency Of Pendulum
angFrequencyExpr :: Expr
angFrequencyExpr = sqrt (sy gravitationalAccel $/ sy lenRod)

-- Period of Pendulum Motion
periodPendExpr :: Expr
periodPendExpr = exactDbl 2 `mul` sy QM.pi_ `mul` sqrt (sy lenRod $/ sy gravitationalAccel)

-- Angular Displacement
angularDisplacementExpr :: Expr
angularDisplacementExpr = sy initialPendAngle `mul` cos (sy angularFrequency `mul` sy time)
