module Drasil.DblPend.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalAccel, gravitationalMagnitude)
import Drasil.DblPend.Unitals (lenRod_1, lenRod_2, massObj_1, massObj_2,
  angularAccel_1, angularAccel_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2)

-- Clifford Velocity for First Object (represented as 2D vector)
mvVelExpr_1 :: PExpr
mvVelExpr_1 = vec2D (sy angularVel_1 $* sy lenRod_1 $* cos (sy pendDisAngle_1))
                    (sy angularVel_1 $* sy lenRod_1 $* sin (sy pendDisAngle_1))

-- Clifford Velocity for Second Object  
mvVelExpr_2 :: PExpr
mvVelExpr_2 = vec2D (mvVelExpr_1 $+ (sy angularVel_2 $* sy lenRod_2 $* cos (sy pendDisAngle_2)))
                    (mvVelExpr_1 $+ (sy angularVel_2 $* sy lenRod_2 $* sin (sy pendDisAngle_2)))

-- Clifford Acceleration for First Object (represented as 2D vector)
mvAccelExpr_1 :: PExpr
mvAccelExpr_1 = vec2D (neg (square (sy angularVel_1) $* sy lenRod_1 $* sin (sy pendDisAngle_1))
                      $+ (sy angularAccel_1 $* sy lenRod_1 $* cos (sy pendDisAngle_1)))
                     ((square (sy angularVel_1) $* sy lenRod_1 $* cos (sy pendDisAngle_1))
                      $+ (sy angularAccel_1 $* sy lenRod_1 $* sin (sy pendDisAngle_1)))

-- Clifford Acceleration for Second Object  
mvAccelExpr_2 :: PExpr
mvAccelExpr_2 = vec2D (mvAccelExpr_1 $+ (sy angularVel_2 $* sy lenRod_2 $* neg (sin (sy pendDisAngle_2))))
                     (mvAccelExpr_1 $+ (sy angularVel_2 $* sy lenRod_2 $* cos (sy pendDisAngle_2)))

-- Clifford Force for First Object (represented as multivector in 2D)
mvForceExpr_1 :: PExpr
mvForceExpr_1 = vec2D (sy massObj_1 $* sy angularAccel_1 $* sy lenRod_1 $* sin (sy pendDisAngle_1))
                     (sy massObj_1 $* sy angularAccel_1 $* sy lenRod_1 $* cos (sy pendDisAngle_1) $- (sy massObj_1 $* sy gravitationalMagnitude))

-- Clifford Force for Second Object
mvForceExpr_2 :: PExpr  
mvForceExpr_2 = vec2D (sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2 $* sin (sy pendDisAngle_2))
                     (sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2 $* cos (sy pendDisAngle_2) $- (sy massObj_2 $* sy gravitationalMagnitude))

-- Horizontal/Vertical force acting on the second object (using multivector principles)
xForceWithAngle_2 :: PExpr
xForceWithAngle_2 = neg (sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2) $* sin (sy pendDisAngle_2)

yForceWithAngle_2 :: PExpr
yForceWithAngle_2 = sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2 $* cos (sy pendDisAngle_2) $- 
                    (sy massObj_2 $* sy gravitationalAccel)

-- Angular acceleration acting on the first object
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

-- Angular acceleration acting on the first object
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

-- Angular acceleration support equations in IM (using Clifford algebra principles)
forceDerivExpr1 :: PExpr
forceDerivExpr1 = sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2 $* sin (sy pendDisAngle_2)
forceDerivExpr2 :: PExpr
forceDerivExpr2 = sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2 $* cos (sy pendDisAngle_2)

cosAngleExpr1 :: PExpr
cosAngleExpr1 = cos (sy pendDisAngle_1)
sinAngleExpr1 :: PExpr
sinAngleExpr1 = sin (sy pendDisAngle_1)

cosAngleExpr2 :: PExpr
cosAngleExpr2 = cos (sy pendDisAngle_2)
sinAngleExpr2 :: PExpr
sinAngleExpr2 = sin (sy pendDisAngle_2)
