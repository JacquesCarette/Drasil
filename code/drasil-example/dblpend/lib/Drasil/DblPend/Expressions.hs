{-# LANGUAGE RankNTypes #-}

module Drasil.DblPend.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalMagnitude)
import Drasil.DblPend.Unitals (lenRod_1, lenRod_2, massObj_1, massObj_2,
  angularAccel_1, angularAccel_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2)

-- Angle helpers
cosAngleExpr1, sinAngleExpr1, cosAngleExpr2, sinAngleExpr2 :: PExpr
cosAngleExpr1 = cos (sy pendDisAngle_1)
sinAngleExpr1 = sin (sy pendDisAngle_1)
cosAngleExpr2 = cos (sy pendDisAngle_2)
sinAngleExpr2 = sin (sy pendDisAngle_2)

-- Direction vectors
vector :: PExpr -> PExpr -> PExpr
vector x y = vect [x, y]

directionVector_1 :: PExpr
directionVector_1 = vector cosAngleExpr1 sinAngleExpr1

perpDirectionVector_1 :: PExpr
perpDirectionVector_1 = vector (neg sinAngleExpr1) cosAngleExpr1

directionVector_2 :: PExpr
directionVector_2 = vector cosAngleExpr2 sinAngleExpr2

perpDirectionVector_2 :: PExpr
perpDirectionVector_2 = vector (neg sinAngleExpr2) cosAngleExpr2

-- Velocity of mass 1: v1 = ω1 * l1 * [cos θ1, sin θ1]
mvVelExpr_1 :: PExpr
mvVelExpr_1 = (sy angularVel_1 $* sy lenRod_1) `cScale` directionVector_1

-- Velocity of mass 2 relative component: ω2 * l2 * [cos θ2, sin θ2]
mvVelComponent_2 :: PExpr
mvVelComponent_2 = (sy angularVel_2 $* sy lenRod_2) `cScale` directionVector_2

-- Total velocity mass 2: v2 = v1 + v2_rel
mvVelExpr_2 :: PExpr
mvVelExpr_2 = mvVelExpr_1 `cAdd` mvVelComponent_2

-- Accelerations for mass 1
centripetalAccel_1 :: PExpr
centripetalAccel_1 = (square (sy angularVel_1) $* sy lenRod_1) `cScale` perpDirectionVector_1

tangentialAccel_1 :: PExpr
tangentialAccel_1 = (sy angularAccel_1 $* sy lenRod_1) `cScale` directionVector_1

mvAccelExpr_1 :: PExpr
mvAccelExpr_1 = centripetalAccel_1 `cAdd` tangentialAccel_1

-- Accelerations for mass 2
centripetalAccel_2 :: PExpr
centripetalAccel_2 = (square (sy angularVel_2) $* sy lenRod_2) `cScale` perpDirectionVector_2 

tangentialAccel_2 :: PExpr
tangentialAccel_2 = (sy angularAccel_2 $* sy lenRod_2) `cScale` directionVector_2

mvAccelComponent_2 :: PExpr
mvAccelComponent_2 = centripetalAccel_2 `cAdd` tangentialAccel_2

mvAccelExpr_2 :: PExpr
mvAccelExpr_2 = mvAccelExpr_1 `cAdd` mvAccelComponent_2

-- Forces
gravitationalForce_1 :: PExpr
gravitationalForce_1 = vector (int 0) (neg (sy massObj_1 $* sy gravitationalMagnitude))

gravitationalForce_2 :: PExpr
gravitationalForce_2 = vector (int 0) (neg (sy massObj_2 $* sy gravitationalMagnitude))

tensionVec_1 :: PExpr
tensionVec_1 = sy massObj_1 `cScale` vector (neg sinAngleExpr1) cosAngleExpr1

tensionVec_2 :: PExpr
tensionVec_2 = sy massObj_2 `cScale` vector (neg sinAngleExpr2) cosAngleExpr2

inertialForce_1 :: PExpr
inertialForce_1 = sy massObj_1 `cScale` mvAccelExpr_1

inertialForce_2 :: PExpr
inertialForce_2 = sy massObj_2 `cScale` mvAccelExpr_2

mvForceExpr_1 :: PExpr
mvForceExpr_1 = inertialForce_1 `cAdd` gravitationalForce_1

mvForceExpr_2 :: PExpr
mvForceExpr_2 = inertialForce_2 `cAdd` gravitationalForce_2

-- Force derivatives 
forceDerivExpr1 :: PExpr
forceDerivExpr1 = sy massObj_1 `cScale` mvAccelExpr_1

forceDerivExpr2 :: PExpr
forceDerivExpr2 = sy massObj_2 `cScale` mvAccelExpr_2

angularAccelExpr_1 :: PExpr
angularAccelExpr_1 = neg(sy gravitationalMagnitude) $* 
                   (exactDbl 2 $* sy massObj_1 $+ sy massObj_2) $* sin (sy pendDisAngle_1 ) $-
                   (sy massObj_2 $* sy gravitationalMagnitude $* 
                   sin (sy pendDisAngle_1 $- (exactDbl 2 $* sy pendDisAngle_2))) $-
                   ((exactDbl 2 $* sin (sy pendDisAngle_1 $- sy pendDisAngle_2 )) $* sy massObj_2 $* 
                   (
                       square (sy angularVel_2) $* sy lenRod_2 $+ 
                       (square (sy angularVel_1) $* sy lenRod_1 $* cos (sy pendDisAngle_1 $- sy pendDisAngle_2))
                   ))
                   $/
                   sy lenRod_1 $* 
                   (
                       exactDbl 2 $* sy massObj_1 $+ sy massObj_2 $- 
                       (sy massObj_2 $* 
                       cos (exactDbl 2 $* sy pendDisAngle_1 $- (exactDbl 2 $* sy pendDisAngle_2)))
                   )

angularAccelExpr_2 :: PExpr
angularAccelExpr_2 = exactDbl 2 $* sin (sy pendDisAngle_1 $- sy pendDisAngle_2) $* 
                   (
                       square (sy angularVel_1) $* sy lenRod_1 $* (sy massObj_1 $+ sy massObj_2 ) $+
                       (sy gravitationalMagnitude $* (sy massObj_1 $+ sy massObj_2 ) $* cos (sy pendDisAngle_1)) $+
                       (square (sy angularVel_2) $* sy lenRod_2 $* sy massObj_2 $* 
                       cos (sy pendDisAngle_1 $- sy pendDisAngle_2 ))
                   )
                   $/
                   sy lenRod_2 $* 
                   (
                       exactDbl 2 $* sy massObj_1 $+ sy massObj_2 $- 
                       (sy massObj_2 $* 
                       cos (exactDbl 2 $* sy pendDisAngle_1 $- (exactDbl 2 $* sy pendDisAngle_2)))
                   )