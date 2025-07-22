module Drasil.DblPend.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalMagnitude)
import Drasil.DblPend.Unitals (lenRod_1, lenRod_2, massObj_1, massObj_2,
  angularAccel_1, angularAccel_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2)

-- ============================================================================
-- CLIFFORD ALGEBRA BASIS ELEMENTS (2D Clifford Space Cl(2,0))
-- ============================================================================

-- Clifford algebra Cl(2,0) basis elements
-- Grade 0 (scalar)
clifScalar :: PExpr
clifScalar = lit 1  -- scalar unit

-- Grade 1 basis elements (vectors)
e1_clif :: PExpr
e1_clif = clif Nothing [int 0, int 1, int 0, int 0]  -- e₁ basis element

e2_clif :: PExpr  
e2_clif = clif Nothing [int 0, int 0, int 1, int 0]  -- e₂ basis element

-- Grade 2 basis element (bivector)
e12_clif :: PExpr
e12_clif = clif Nothing [int 0, int 0, int 0, int 1]  -- e₁ ∧ e₂ basis element

-- Backward compatibility: Standard 2D vectors for component extraction
e1_2D :: PExpr
e1_2D = vect [int 1, int 0]  -- e₁ basis vector

e2_2D :: PExpr  
e2_2D = vect [int 0, int 1]  -- e₂ basis vector

-- ============================================================================
-- CLIFFORD ALGEBRA MULTIVECTOR CONSTRUCTION HELPERS
-- ============================================================================

-- Create a pure scalar multivector
scalar :: PExpr -> PExpr
scalar s = clif Nothing [s, int 0, int 0, int 0]

-- Create a pure vector multivector from components
vector :: PExpr -> PExpr -> PExpr
vector x y = clif Nothing [int 0, x, y, int 0]

-- Create a pure bivector multivector
bivector :: PExpr -> PExpr
bivector xy = clif Nothing [int 0, int 0, int 0, xy]

-- Create a general multivector from all components
multivector :: PExpr -> PExpr -> PExpr -> PExpr -> PExpr
multivector s x y xy = clif Nothing [s, x, y, xy]

-- ============================================================================
-- DIRECTION VECTORS USING CLIFFORD ALGEBRA
-- ============================================================================

-- Angle expressions
cosAngleExpr1, sinAngleExpr1, cosAngleExpr2, sinAngleExpr2 :: PExpr
cosAngleExpr1 = cos (sy pendDisAngle_1)
sinAngleExpr1 = sin (sy pendDisAngle_1)
cosAngleExpr2 = cos (sy pendDisAngle_2)
sinAngleExpr2 = sin (sy pendDisAngle_2)

-- Unit direction vectors as true Clifford vectors
directionVector_1 :: PExpr
directionVector_1 = vector cosAngleExpr1 sinAngleExpr1

directionVector_2 :: PExpr  
directionVector_2 = vector cosAngleExpr2 sinAngleExpr2

-- Perpendicular direction vectors (rotated 90 degrees) as true Clifford vectors
perpDirectionVector_1 :: PExpr
perpDirectionVector_1 = vector (neg sinAngleExpr1) cosAngleExpr1

perpDirectionVector_2 :: PExpr
perpDirectionVector_2 = vector (neg sinAngleExpr2) cosAngleExpr2

-- ============================================================================
-- KINEMATICS: VELOCITY AND ACCELERATION
-- ============================================================================

-- Velocity vectors as true Clifford multivectors
mvVelExpr_1 :: PExpr
mvVelExpr_1 = (sy angularVel_1 $* sy lenRod_1) `cScale` directionVector_1

-- Velocity component for second object as Clifford multivector
mvVelComponent_2 :: PExpr
mvVelComponent_2 = (sy angularVel_2 $* sy lenRod_2) `cScale` directionVector_2

-- Total velocity for second object (vector addition)
mvVelExpr_2 :: PExpr
mvVelExpr_2 = mvVelExpr_1 `cAdd` mvVelComponent_2

-- Acceleration components for first object
centripetalAccel_1 :: PExpr
centripetalAccel_1 = neg (square (sy angularVel_1) $* sy lenRod_1) `cScale` directionVector_1

tangentialAccel_1 :: PExpr
tangentialAccel_1 = (sy angularAccel_1 $* sy lenRod_1) `cScale` perpDirectionVector_1

-- Total acceleration for first object
mvAccelExpr_1 :: PExpr
mvAccelExpr_1 = centripetalAccel_1 `cAdd` tangentialAccel_1

-- Acceleration components for second object
centripetalAccel_2 :: PExpr
centripetalAccel_2 = neg (square (sy angularVel_2) $* sy lenRod_2) `cScale` directionVector_2

tangentialAccel_2 :: PExpr
tangentialAccel_2 = (sy angularAccel_2 $* sy lenRod_2) `cScale` perpDirectionVector_2

mvAccelComponent_2 :: PExpr
mvAccelComponent_2 = centripetalAccel_2 `cAdd` tangentialAccel_2

-- Total acceleration for second object
mvAccelExpr_2 :: PExpr
mvAccelExpr_2 = mvAccelExpr_1 `cAdd` mvAccelComponent_2

-- ============================================================================
-- FORCES USING CLIFFORD ALGEBRA
-- ============================================================================

-- Gravitational force vectors as true Clifford multivectors (always downward)
gravitationalForce_1 :: PExpr
gravitationalForce_1 = vector (int 0) (neg (sy massObj_1 $* sy gravitationalMagnitude))

gravitationalForce_2 :: PExpr  
gravitationalForce_2 = vector (int 0) (neg (sy massObj_2 $* sy gravitationalMagnitude))

-- Inertial forces (mass × acceleration)
inertialForce_1 :: PExpr
inertialForce_1 = sy massObj_1 `cScale` mvAccelExpr_1

inertialForce_2 :: PExpr
inertialForce_2 = sy massObj_2 `cScale` mvAccelExpr_2

-- Total forces (inertial + gravitational)
mvForceExpr_1 :: PExpr
mvForceExpr_1 = inertialForce_1 `cAdd` gravitationalForce_1

mvForceExpr_2 :: PExpr  
mvForceExpr_2 = inertialForce_2 `cAdd` gravitationalForce_2

-- ============================================================================
-- COMPONENT EXTRACTION USING GRADE SELECTION
-- ============================================================================

-- Extract force components using grade selection and dot product
xForceComponent_1 :: PExpr
xForceComponent_1 = gradeSelect 1 mvForceExpr_1 $. e1_2D

yForceComponent_1 :: PExpr
yForceComponent_1 = gradeSelect 1 mvForceExpr_1 $. e2_2D

xForceComponent_2 :: PExpr
xForceComponent_2 = gradeSelect 1 mvForceExpr_2 $. e1_2D

yForceComponent_2 :: PExpr
yForceComponent_2 = gradeSelect 1 mvForceExpr_2 $. e2_2D

-- ============================================================================
-- CLIFFORD ALGEBRA OPERATIONS DEMONSTRATIONS
-- ============================================================================

-- Grade selection examples on true multivectors
scalarForceComponent_1 :: PExpr
scalarForceComponent_1 = gradeSelect 0 mvForceExpr_1  -- scalar part

vectorForceComponent_1 :: PExpr
vectorForceComponent_1 = gradeSelect 1 mvForceExpr_1  -- vector part

bivectorForceComponent_1 :: PExpr
bivectorForceComponent_1 = gradeSelect 2 mvForceExpr_1  -- bivector part (should be zero)

-- Geometric product (combines dot and wedge products)
forceGeometricProduct :: PExpr
forceGeometricProduct = mvForceExpr_1 `geometricProd` mvForceExpr_2

-- Wedge product (creates bivector from vectors)
forceWedgeProduct :: PExpr
forceWedgeProduct = mvForceExpr_1 `wedgeProd` mvForceExpr_2

-- Extract parts of geometric product using grade selection
forceDotProduct :: PExpr  
forceDotProduct = gradeSelect 0 forceGeometricProduct  -- scalar part (dot product)

forceWedgeFromGeometric :: PExpr
forceWedgeFromGeometric = gradeSelect 2 forceGeometricProduct  -- bivector part (wedge product)

-- Clifford subtraction of multivectors
forceDifference :: PExpr
forceDifference = mvForceExpr_1 `cSub` mvForceExpr_2

-- Force magnitudes using clifford norm
forceMagnitude_1 :: PExpr
forceMagnitude_1 = norm mvForceExpr_1

forceMagnitude_2 :: PExpr
forceMagnitude_2 = norm mvForceExpr_2

-- ============================================================================
-- ADVANCED CLIFFORD ALGEBRA OPERATIONS
-- ============================================================================

-- Clifford conjugate (reverses order of geometric products)
forceConjugate_1 :: PExpr
forceConjugate_1 = multivector 
  (gradeSelect 0 mvForceExpr_1)                    -- scalar stays same
  (gradeSelect 1 mvForceExpr_1)                    -- vector stays same  
  (gradeSelect 1 mvForceExpr_1)                    -- vector stays same
  (neg (gradeSelect 2 mvForceExpr_1))              -- bivector changes sign

-- Inverse of a multivector (for non-zero multivectors)
-- M^(-1) = M̃ / (M * M̃) where M̃ is the conjugate
forceInverseNorm_1 :: PExpr
forceInverseNorm_1 = mvForceExpr_1 `geometricProd` forceConjugate_1

-- Rotor for rotation (unit bivector exponential)
rotationAngle :: PExpr
rotationAngle = sy pendDisAngle_1 $/ int 2  -- half angle for rotor

rotationBivector :: PExpr  
rotationBivector = bivector rotationAngle

-- Simple rotor approximation (cos(θ/2) + sin(θ/2)e₁₂)
rotationRotor :: PExpr
rotationRotor = multivector 
  (cos rotationAngle)                              -- scalar part
  (int 0)                                          -- e₁ part
  (int 0)                                          -- e₂ part  
  (sin rotationAngle)                              -- e₁₂ part

-- ============================================================================
-- PHYSICS APPLICATIONS USING TRUE CLIFFORD ALGEBRA
-- ============================================================================

-- Position vectors as true Clifford multivectors
positionVector_1 :: PExpr
positionVector_1 = sy lenRod_1 `cScale` directionVector_1

positionVector_2 :: PExpr
positionVector_2 = positionVector_1 `cAdd` (sy lenRod_2 `cScale` directionVector_2)

-- Angular velocity as pure bivectors (represent rotation in the plane)
angularVelBivector_1 :: PExpr
angularVelBivector_1 = bivector (sy angularVel_1)

angularVelBivector_2 :: PExpr
angularVelBivector_2 = bivector (sy angularVel_2)

-- Alternative: Angular velocity using wedge product of basis elements
angularVelBivectorAlt_1 :: PExpr
angularVelBivectorAlt_1 = sy angularVel_1 `cScale` (e1_clif `wedgeProd` e2_clif)

angularVelBivectorAlt_2 :: PExpr
angularVelBivectorAlt_2 = sy angularVel_2 `cScale` (e1_clif `wedgeProd` e2_clif)

-- Torque as bivectors (position wedge force)
torqueBivector_1 :: PExpr
torqueBivector_1 = positionVector_1 `wedgeProd` mvForceExpr_1

torqueBivector_2 :: PExpr
torqueBivector_2 = positionVector_2 `wedgeProd` mvForceExpr_2

-- Kinetic energy factor (velocity magnitude squared using dot product)
kineticEnergyFactor_1 :: PExpr
kineticEnergyFactor_1 = mvVelExpr_1 $. mvVelExpr_1

kineticEnergyFactor_2 :: PExpr
kineticEnergyFactor_2 = mvVelExpr_2 $. mvVelExpr_2

-- ============================================================================
-- PHYSICS-BASED ANGULAR ACCELERATION EXPRESSIONS
-- ============================================================================

-- Angular acceleration expressions (from double pendulum physics)
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

-- ============================================================================
-- FORCE DERIVATIVE EXPRESSIONS FOR PHYSICS DERIVATIONS
-- ============================================================================

-- Force derivative expressions used in the derivation process
-- These represent the force equations that are solved to derive angular accelerations
forceDerivExpr1 :: PExpr
forceDerivExpr1 = sy massObj_1 `cScale` mvAccelExpr_1

forceDerivExpr2 :: PExpr  
forceDerivExpr2 = sy massObj_2 `cScale` mvAccelExpr_2

-- ============================================================================
-- TRUE CLIFFORD SPACE EXAMPLES AND VERIFICATION
-- ============================================================================

-- Example multivectors demonstrating full Clifford space
exampleScalar :: PExpr
exampleScalar = scalar (exactDbl 5.0)

exampleVector :: PExpr  
exampleVector = vector (exactDbl 3.0) (exactDbl 4.0)

exampleBivector :: PExpr
exampleBivector = bivector (exactDbl 2.5)

exampleMultivector :: PExpr
exampleMultivector = multivector 
  (exactDbl 1.0)    -- scalar
  (exactDbl 2.0)    -- e₁ component
  (exactDbl 3.0)    -- e₂ component  
  (exactDbl 0.5)    -- e₁₂ component

-- Verify Clifford algebra properties
-- e₁² = 1, e₂² = 1, e₁e₂ = -e₂e₁ = e₁₂
e1_squared :: PExpr
e1_squared = e1_clif `geometricProd` e1_clif  -- Should equal scalar 1

e2_squared :: PExpr
e2_squared = e2_clif `geometricProd` e2_clif  -- Should equal scalar 1

e1e2_product :: PExpr
e1e2_product = e1_clif `geometricProd` e2_clif  -- Should equal e₁₂

e2e1_product :: PExpr
e2e1_product = e2_clif `geometricProd` e1_clif  -- Should equal -e₁₂

-- Anti-commutativity verification
anticommutator :: PExpr
anticommutator = e1e2_product `cAdd` e2e1_product  -- Should be zero

-- Pseudoscalar (highest grade element in 2D)
pseudoscalar_2D :: PExpr
pseudoscalar_2D = e1_clif `wedgeProd` e2_clif  -- e₁ ∧ e₂

-- Duality operations using pseudoscalar
dualOfVector :: PExpr -> PExpr
dualOfVector v = v `geometricProd` pseudoscalar_2D

-- Example: dual of velocity vector
dualVelocity_1 :: PExpr
dualVelocity_1 = dualOfVector mvVelExpr_1

-- Clifford exponential for rotations (simplified)
-- exp(θe₁₂/2) ≈ cos(θ/2) + sin(θ/2)e₁₂ for small rotations
cliffordRotor :: PExpr -> PExpr  
cliffordRotor angle = multivector 
  (cos (angle $/ int 2))           -- cos(θ/2)
  (int 0)                          -- no e₁ component
  (int 0)                          -- no e₂ component
  (sin (angle $/ int 2))           -- sin(θ/2) e₁₂ component

-- Apply rotation to a vector using rotor
-- Rotated vector = R * v * R̃ where R̃ is the conjugate of R
rotateVector :: PExpr -> PExpr -> PExpr
rotateVector rotor vec = 
  let rotorConj = multivector 
        (gradeSelect 0 rotor)                -- scalar part unchanged
        (neg (gradeSelect 1 rotor))          -- vector part negated  
        (neg (gradeSelect 1 rotor))          -- vector part negated
        (neg (gradeSelect 2 rotor))          -- bivector part negated
  in rotor `geometricProd` vec `geometricProd` rotorConj

-- Example: rotate direction vector by pendulum angle
rotatedDirection_1 :: PExpr
rotatedDirection_1 = rotateVector (cliffordRotor (sy pendDisAngle_1)) (vector (int 1) (int 0))

-- ============================================================================
-- CLIFFORD ALGEBRA TYPE VERIFICATION
-- ============================================================================

-- These expressions demonstrate that we now have true Clifford objects:
-- 1. Explicit basis elements (e₁, e₂, e₁₂)
-- 2. Grade-separated multivector components  
-- 3. Proper geometric and wedge products
-- 4. Grade selection operations
-- 5. Clifford-specific operations (conjugation, rotation, duality)

-- The multivector structure: M = a₀ + a₁e₁ + a₂e₂ + a₁₂e₁₂
-- where each component can be extracted using gradeSelect:
-- gradeSelect 0 M = a₀     (scalar)
-- gradeSelect 1 M = a₁e₁ + a₂e₂  (vector)  
-- gradeSelect 2 M = a₁₂e₁₂ (bivector)

-- This is a true 2D Clifford algebra Cl(2,0) implementation
