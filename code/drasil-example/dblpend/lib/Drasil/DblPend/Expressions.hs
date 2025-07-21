module Drasil.DblPend.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalAccel, gravitationalMagnitude)
import Drasil.DblPend.Unitals (lenRod_1, lenRod_2, massObj_1, massObj_2,
  angularAccel_1, angularAccel_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2)

-- Basis vectors for 2D Clifford algebra
e1_2D :: PExpr
e1_2D = vect [int 1, int 0]  -- Standard basis vector e₁

e2_2D :: PExpr  
e2_2D = vect [int 0, int 1]  -- Standard basis vector e₂

-- Scalar unit
scalarUnit :: PExpr
scalarUnit = gradeSelect 0 (vect [int 1, int 0, int 0, int 0])  -- Pure scalar in 2D space

-- Clifford Velocity for First Object using direction vectors
mvVelExpr_1 :: PExpr
mvVelExpr_1 = (sy angularVel_1 $* sy lenRod_1) `cScale` directionVector_1

-- Additional velocity component for second object using direction vectors
mvVelComponent_2 :: PExpr
mvVelComponent_2 = (sy angularVel_2 $* sy lenRod_2) `cScale` directionVector_2

-- Clifford Velocity for Second Object (using proper Clifford addition)
mvVelExpr_2 :: PExpr
mvVelExpr_2 = mvVelExpr_1 `cAdd` mvVelComponent_2

-- Clifford Acceleration for First Object using direction vectors and proper scaling
-- Centripetal acceleration component
centripetalAccel_1 :: PExpr
centripetalAccel_1 = (neg (square (sy angularVel_1) $* sy lenRod_1)) `cScale` directionVector_1

-- Tangential acceleration component  
tangentialAccel_1 :: PExpr
tangentialAccel_1 = (sy angularAccel_1 $* sy lenRod_1) `cScale` perpDirectionVector_1

-- Total acceleration for first object
mvAccelExpr_1 :: PExpr
mvAccelExpr_1 = centripetalAccel_1 `cAdd` tangentialAccel_1

-- Additional acceleration component for second object
centripetalAccel_2 :: PExpr
centripetalAccel_2 = (neg (square (sy angularVel_2) $* sy lenRod_2)) `cScale` directionVector_2

tangentialAccel_2 :: PExpr
tangentialAccel_2 = (sy angularAccel_2 $* sy lenRod_2) `cScale` perpDirectionVector_2

mvAccelComponent_2 :: PExpr
mvAccelComponent_2 = centripetalAccel_2 `cAdd` tangentialAccel_2

-- Clifford Acceleration for Second Object (using proper Clifford addition)
mvAccelExpr_2 :: PExpr
mvAccelExpr_2 = mvAccelExpr_1 `cAdd` mvAccelComponent_2

-- Clifford Force expressions using proper scaling and vector operations

-- Gravitational force vector (always downward)
gravitationalForce_1 :: PExpr
gravitationalForce_1 = (neg (sy massObj_1 $* sy gravitationalMagnitude)) `cScale` e2_2D

gravitationalForce_2 :: PExpr  
gravitationalForce_2 = (neg (sy massObj_2 $* sy gravitationalMagnitude)) `cScale` e2_2D

-- Inertial force for first object (mass × acceleration)
inertialForce_1 :: PExpr
inertialForce_1 = (sy massObj_1) `cScale` mvAccelExpr_1

-- Total force for first object
mvForceExpr_1 :: PExpr
mvForceExpr_1 = inertialForce_1 `cAdd` gravitationalForce_1

-- Inertial force for second object
inertialForce_2 :: PExpr
inertialForce_2 = (sy massObj_2) `cScale` mvAccelExpr_2

-- Total force for second object
mvForceExpr_2 :: PExpr  
mvForceExpr_2 = inertialForce_2 `cAdd` gravitationalForce_2

-- Force component extraction using Clifford algebra operations

-- Extract x-component (horizontal) of second object force using dot product with e1
xForceWithAngle_2 :: PExpr
xForceWithAngle_2 = gradeSelect 1 mvForceExpr_2 $. e1_2D

-- Extract y-component (vertical) of second object force using dot product with e2  
yForceWithAngle_2 :: PExpr
yForceWithAngle_2 = gradeSelect 1 mvForceExpr_2 $. e2_2D

-- Alternative: direct force component calculation for verification
xForceWithAngle_2_Alt :: PExpr
xForceWithAngle_2_Alt = neg (sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2) $* sin (sy pendDisAngle_2)

yForceWithAngle_2_Alt :: PExpr
yForceWithAngle_2_Alt = sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2 $* cos (sy pendDisAngle_2) $- 
                        (sy massObj_2 $* sy gravitationalAccel)

-- Advanced Clifford algebra operations and demonstrations

-- Extract the scalar (grade-0) component from a force vector
scalarForceComponent_1 :: PExpr
scalarForceComponent_1 = gradeSelect 0 mvForceExpr_1

-- Extract the vector (grade-1) component from a force vector  
vectorForceComponent_1 :: PExpr
vectorForceComponent_1 = gradeSelect 1 mvForceExpr_1

-- Extract grade-2 (bivector) component (should be zero for pure vectors)
bivectorForceComponent_1 :: PExpr
bivectorForceComponent_1 = gradeSelect 2 mvForceExpr_1

-- Demonstrate geometric product (combines inner and outer products)
forceGeometricProduct :: PExpr
forceGeometricProduct = mvForceExpr_1 `geometricProd` mvForceExpr_2

-- Demonstrate wedge product (antisymmetric outer product) - creates bivector
forceWedgeProduct :: PExpr
forceWedgeProduct = mvForceExpr_1 `wedgeProd` mvForceExpr_2

-- Extract the bivector part of the geometric product
forceWedgeFromGeometric :: PExpr
forceWedgeFromGeometric = gradeSelect 2 forceGeometricProduct

-- Extract the scalar part of the geometric product (dot product)
forceDotFromGeometric :: PExpr  
forceDotFromGeometric = gradeSelect 0 forceGeometricProduct

-- Verify: geometric product = dot product + wedge product
-- forceGeometricProduct should equal (forceDotFromGeometric + forceWedgeFromGeometric)

-- Example of scaling a Clifford object
scaledForce_1 :: PExpr
scaledForce_1 = (sy massObj_1) `cScale` mvVelExpr_1

-- Example of Clifford subtraction
forceDifference :: PExpr
forceDifference = mvForceExpr_1 `cSub` mvForceExpr_2

-- Magnitude of force vectors using norm operation
forceMagnitude_1 :: PExpr
forceMagnitude_1 = norm mvForceExpr_1

forceMagnitude_2 :: PExpr
forceMagnitude_2 = norm mvForceExpr_2

-- Alternative constructions demonstrating different Clifford approaches

-- Alternative velocity construction using explicit basis scaling (should equal mvVelExpr_1)
mvVelExpr_1_Alt :: PExpr
mvVelExpr_1_Alt = ((sy angularVel_1 $* sy lenRod_1 $* cos (sy pendDisAngle_1)) `cScale` e1_2D) 
                  `cAdd` 
                  ((sy angularVel_1 $* sy lenRod_1 $* sin (sy pendDisAngle_1)) `cScale` e2_2D)

-- Yet another alternative using direction vector scaling (should also equal mvVelExpr_1)
mvVelExpr_1_Alt2 :: PExpr
mvVelExpr_1_Alt2 = (sy angularVel_1 $* sy lenRod_1) `cScale` directionVector_1

-- Demonstrate multiple equivalent constructions for the same physical quantity

-- Extract x-component (e₁ coefficient) from velocity using dot product
xComponent_vel1 :: PExpr  
xComponent_vel1 = gradeSelect 1 mvVelExpr_1 $. e1_2D

-- Extract y-component (e₂ coefficient) from velocity using dot product
yComponent_vel1 :: PExpr
yComponent_vel1 = gradeSelect 1 mvVelExpr_1 $. e2_2D

-- Physics-specific Clifford algebra applications

-- Angular velocity as a bivector (rotation in the plane)
angularVelBivector_1 :: PExpr
angularVelBivector_1 = (sy angularVel_1) `cScale` (e1_2D `wedgeProd` e2_2D)

angularVelBivector_2 :: PExpr
angularVelBivector_2 = (sy angularVel_2) `cScale` (e1_2D `wedgeProd` e2_2D)

-- Position vectors for the pendulum masses
positionVector_1 :: PExpr
positionVector_1 = (sy lenRod_1) `cScale` directionVector_1

positionVector_2 :: PExpr
positionVector_2 = positionVector_1 `cAdd` ((sy lenRod_2) `cScale` directionVector_2)

-- Torque as bivector (position wedge force)
torqueBivector_1 :: PExpr
torqueBivector_1 = positionVector_1 `wedgeProd` mvForceExpr_1

torqueBivector_2 :: PExpr
torqueBivector_2 = positionVector_2 `wedgeProd` mvForceExpr_2

-- Total system energy components using Clifford algebra
-- Kinetic energy involves velocity magnitude squared
kineticEnergyFactor_1 :: PExpr
kineticEnergyFactor_1 = mvVelExpr_1 $. mvVelExpr_1  -- dot product gives magnitude squared

kineticEnergyFactor_2 :: PExpr
kineticEnergyFactor_2 = mvVelExpr_2 $. mvVelExpr_2

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

-- Angular acceleration support equations using Clifford algebra principles
-- These represent force components as scalars that can be scaled with direction vectors
forceDerivMagnitude1 :: PExpr
forceDerivMagnitude1 = sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2

forceDerivMagnitude2 :: PExpr
forceDerivMagnitude2 = sy massObj_2 $* sy angularAccel_2 $* sy lenRod_2

-- Force derivative expressions as Clifford vectors
forceDerivExpr1 :: PExpr
forceDerivExpr1 = forceDerivMagnitude1 `cScale` (sinAngleExpr2 `cScale` e1_2D)

forceDerivExpr2 :: PExpr
forceDerivExpr2 = forceDerivMagnitude2 `cScale` (cosAngleExpr2 `cScale` e2_2D)

-- Combined force derivative as a single Clifford vector
combinedForceDerivExpr :: PExpr
combinedForceDerivExpr = forceDerivExpr1 `cAdd` forceDerivExpr2

-- Trigonometric expressions using Clifford basis representation
cosAngleExpr1 :: PExpr
cosAngleExpr1 = cos (sy pendDisAngle_1)

sinAngleExpr1 :: PExpr
sinAngleExpr1 = sin (sy pendDisAngle_1)

cosAngleExpr2 :: PExpr
cosAngleExpr2 = cos (sy pendDisAngle_2)

sinAngleExpr2 :: PExpr
sinAngleExpr2 = sin (sy pendDisAngle_2)

-- Unit direction vectors for each pendulum using Clifford representation
directionVector_1 :: PExpr
directionVector_1 = vec2D cosAngleExpr1 sinAngleExpr1

directionVector_2 :: PExpr  
directionVector_2 = vec2D cosAngleExpr2 sinAngleExpr2

-- Perpendicular direction vectors (rotated 90 degrees)
perpDirectionVector_1 :: PExpr
perpDirectionVector_1 = vec2D (neg sinAngleExpr1) cosAngleExpr1

perpDirectionVector_2 :: PExpr
perpDirectionVector_2 = vec2D (neg sinAngleExpr2) cosAngleExpr2
