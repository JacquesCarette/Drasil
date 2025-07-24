module Drasil.DblPend.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalMagnitude)
import Drasil.DblPend.Unitals (lenRod_1, lenRod_2, massObj_1, massObj_2,
  angularAccel_1, angularAccel_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2)

-- Clifford algebra basis elements for 2D space  
e1_clif :: Expr
e1_clif = int 0 `cScale` (vec2D (int 1) (int 0)) `cAdd` int 1 `cScale` (vec2D (int 1) (int 0))

e2_clif :: Expr
e2_clif = int 0 `cScale` (vec2D (int 0) (int 1)) `cAdd` int 1 `cScale` (vec2D (int 0) (int 1))

-- Direction vectors using pure Clifford algebra
directionVector_1 :: Expr
directionVector_1 = cos (sy pendDisAngle_1) `cScale` e1_clif `cAdd` sin (sy pendDisAngle_1) `cScale` e2_clif

directionVector_2 :: Expr  
directionVector_2 = cos (sy pendDisAngle_2) `cScale` e1_clif `cAdd` sin (sy pendDisAngle_2) `cScale` e2_clif

perpDirectionVector_1 :: Expr
perpDirectionVector_1 = neg (sin (sy pendDisAngle_1)) `cScale` e1_clif `cAdd` cos (sy pendDisAngle_1) `cScale` e2_clif

perpDirectionVector_2 :: Expr
perpDirectionVector_2 = neg (sin (sy pendDisAngle_2)) `cScale` e1_clif `cAdd` cos (sy pendDisAngle_2) `cScale` e2_clif

-- Velocity expressions using Clifford algebra
mvVelExpr_1 :: Expr
mvVelExpr_1 = (sy angularVel_1 $* sy lenRod_1) `cScale` directionVector_1

mvVelComponent_2 :: Expr
mvVelComponent_2 = (sy angularVel_2 $* sy lenRod_2) `cScale` directionVector_2

mvVelExpr_2 :: Expr
mvVelExpr_2 = mvVelExpr_1 `cAdd` mvVelComponent_2

-- Acceleration expressions using Clifford algebra
centripetalAccel_1 :: Expr
centripetalAccel_1 = neg (square (sy angularVel_1) $* sy lenRod_1) `cScale` directionVector_1

tangentialAccel_1 :: Expr
tangentialAccel_1 = (sy angularAccel_1 $* sy lenRod_1) `cScale` perpDirectionVector_1

mvAccelExpr_1 :: Expr
mvAccelExpr_1 = centripetalAccel_1 `cAdd` tangentialAccel_1

centripetalAccel_2 :: Expr
centripetalAccel_2 = neg (square (sy angularVel_2) $* sy lenRod_2) `cScale` directionVector_2

tangentialAccel_2 :: Expr
tangentialAccel_2 = (sy angularAccel_2 $* sy lenRod_2) `cScale` perpDirectionVector_2

mvAccelComponent_2 :: Expr
mvAccelComponent_2 = centripetalAccel_2 `cAdd` tangentialAccel_2

mvAccelExpr_2 :: Expr
mvAccelExpr_2 = mvAccelExpr_1 `cAdd` mvAccelComponent_2

-- Forces using pure Clifford algebra  
gravitationalForce_1 :: Expr
gravitationalForce_1 = neg (sy massObj_1 $* sy gravitationalMagnitude) `cScale` e2_clif

gravitationalForce_2 :: Expr  
gravitationalForce_2 = neg (sy massObj_2 $* sy gravitationalMagnitude) `cScale` e2_clif

inertialForce_1 :: Expr
inertialForce_1 = sy massObj_1 `cScale` mvAccelExpr_1

inertialForce_2 :: Expr
inertialForce_2 = sy massObj_2 `cScale` mvAccelExpr_2

mvForceExpr_1 :: Expr
mvForceExpr_1 = inertialForce_1 `cAdd` gravitationalForce_1

mvForceExpr_2 :: Expr  
mvForceExpr_2 = inertialForce_2 `cAdd` gravitationalForce_2

-- Angular acceleration as scalar expressions (these remain scalar as they represent rotational magnitudes)
angularAccelExpr_1 :: Expr
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

angularAccelExpr_2 :: Expr
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


