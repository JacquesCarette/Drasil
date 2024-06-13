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
velocityIXExpr = sy angularVelocity $*  sy lenRod $*  cos (sy pendDisplacementAngle)
velocityIYExpr = sy angularVelocity $*  sy lenRod $*  sin (sy pendDisplacementAngle)

-- Acceleration IX/IY
accelerationIXExpr, accelerationIYExpr :: Expr
accelerationIXExpr = neg (square (sy angularVelocity) $*  sy lenRod $*  sin (sy pendDisplacementAngle))
                    `add` (sy angularAccel $*  sy lenRod $*  cos (sy pendDisplacementAngle))
accelerationIYExpr = (square (sy angularVelocity) $*  sy lenRod $*  cos (sy pendDisplacementAngle))
                    `add` (sy angularAccel $*  sy lenRod $*  sin (sy pendDisplacementAngle))

-- Horizontal/Vertical force acting on the pendulum
hForceOnPendulumViaComponent, hForceOnPendulumViaAngle :: Expr
hForceOnPendulumViaComponent = sy mass $*  sy xAccel
hForceOnPendulumViaAngle = neg (sy tension $*  sin (sy pendDisplacementAngle))

vForceOnPendulumViaComponent, vForceOnPendulumViaAngle :: Expr
vForceOnPendulumViaComponent = sy mass $*  sy yAccel
vForceOnPendulumViaAngle = sy tension $*  cos (sy pendDisplacementAngle)
                            $- (sy mass $*  sy gravitationalAccel)

-- Angular Frequency Of Pendulum
angFrequencyExpr :: Expr
angFrequencyExpr = sqrt (sy gravitationalAccel $/ sy lenRod)

-- Period of Pendulum Motion
periodPendExpr :: Expr
periodPendExpr = exactDbl 2 $*  sy QM.pi_ $*  sqrt (sy lenRod $/ sy gravitationalAccel)

-- Angular Displacement
angularDisplacementExpr :: Expr
angularDisplacementExpr = sy initialPendAngle $*  cos (sy angularFrequency $*  sy time)
