{-# LANGUAGE RankNTypes #-}

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

-- Grade 0 (scalar)
clifScalar :: PExpr
clifScalar = int 1  -- scalar unit

-- Grade 1 basis elements (vectors)
e1_clif :: PExpr
e1_clif = vect [int 1, int 0]  -- e₁

e2_clif :: PExpr
e2_clif = vect [int 0, int 1]  -- e₂

-- For component extraction
e1_2D :: PExpr
e1_2D = e1_clif

e2_2D :: PExpr
e2_2D = e2_clif

-- Pseudoscalar (bivector) for 2D
e12_clif :: PExpr
e12_clif = e1_clif `wedgeProd` e2_clif  -- e₁ ∧ e₂

-- ============================================================================
-- CONSTRUCTORS / HELPERS
-- ============================================================================

scalar :: PExpr -> PExpr
scalar s = s

vector :: PExpr -> PExpr -> PExpr
vector x y = vect [x, y]

bivector :: PExpr -> PExpr
bivector xy = xy `cScale` e12_clif

multivector :: PExpr -> PExpr -> PExpr -> PExpr -> PExpr
multivector s x y xy = s $+ vector x y $+ bivector xy

cscale :: PExpr -> PExpr -> PExpr
cscale = cScale

cadd :: PExpr -> PExpr -> PExpr
cadd = cAdd

-- ============================================================================
-- ANGLE HELPERS
-- ============================================================================

cosAngleExpr1, sinAngleExpr1, cosAngleExpr2, sinAngleExpr2 :: PExpr
cosAngleExpr1 = cos (sy pendDisAngle_1)
sinAngleExpr1 = sin (sy pendDisAngle_1)
cosAngleExpr2 = cos (sy pendDisAngle_2)
sinAngleExpr2 = sin (sy pendDisAngle_2)

-- ============================================================================
-- DIRECTION VECTORS (choose convention to match component form)
-- ----------------------------------------------------------------------------
-- We use: r = l * (cos θ e1 + sin θ e2)
-- Tangent (perp) = [-sin θ, cos θ]
-- ============================================================================

directionVector_1 :: PExpr
directionVector_1 = vector cosAngleExpr1 sinAngleExpr1

directionVector_2 :: PExpr
directionVector_2 = vector cosAngleExpr2 sinAngleExpr2

perpDirectionVector_1 :: PExpr
perpDirectionVector_1 = vector (neg sinAngleExpr1) cosAngleExpr1

perpDirectionVector_2 :: PExpr
perpDirectionVector_2 = vector (neg sinAngleExpr2) cosAngleExpr2

-- ============================================================================
-- KINEMATICS: VELOCITY & ACCELERATION (GA vectors) + component extraction
-- ============================================================================

-- Velocity of mass 1: v1 = ω1 * l1 * [cos θ1, sin θ1]
mvVelExpr_1 :: PExpr
mvVelExpr_1 = (sy angularVel_1 $* sy lenRod_1) `cScale` directionVector_1

-- Velocity of mass 2 relative component: ω2 * l2 * [cos θ2, sin θ2]
mvVelComponent_2 :: PExpr
mvVelComponent_2 = (sy angularVel_2 $* sy lenRod_2) `cScale` directionVector_2

-- Total velocity mass 2: v2 = v1 + v2_rel
mvVelExpr_2 :: PExpr
mvVelExpr_2 = mvVelExpr_1 `cAdd` mvVelComponent_2

-- Component-wise velocities (keeps API compatibility)
velXExpr_1 :: PExpr
velXExpr_1 = mvVelExpr_1 $. e1_2D

velYExpr_1 :: PExpr
velYExpr_1 = mvVelExpr_1 $. e2_2D

velXExpr_2 :: PExpr
velXExpr_2 = mvVelExpr_2 $. e1_2D

velYExpr_2 :: PExpr
velYExpr_2 = mvVelExpr_2 $. e2_2D

-- Accelerations for mass 1:
-- centripetal: -ω1^2 * l1 * directionVector_1
-- tangential:  α1 * l1 * perpDirectionVector_1
centripetalAccel_1 :: PExpr
centripetalAccel_1 = (square (sy angularVel_1) $* sy lenRod_1) `cScale` perpDirectionVector_1

tangentialAccel_1 :: PExpr
tangentialAccel_1 = (sy angularAccel_1 $* sy lenRod_1) `cScale` directionVector_1

mvAccelExpr_1 :: PExpr
mvAccelExpr_1 = centripetalAccel_1 `cAdd` tangentialAccel_1

accelXExpr_1 :: PExpr
accelXExpr_1 = mvAccelExpr_1 $. e1_2D

accelYExpr_1 :: PExpr
accelYExpr_1 = mvAccelExpr_1 $. e2_2D

-- Accelerations for mass 2: a2 = a1 + relative centripetal + relative tangential
centripetalAccel_2 :: PExpr
centripetalAccel_2 = (square (sy angularVel_2) $* sy lenRod_2) `cScale` perpDirectionVector_2 

tangentialAccel_2 :: PExpr
tangentialAccel_2 = (sy angularAccel_2 $* sy lenRod_2) `cScale` directionVector_2

mvAccelComponent_2 :: PExpr
mvAccelComponent_2 = centripetalAccel_2 `cAdd` tangentialAccel_2

mvAccelExpr_2 :: PExpr
mvAccelExpr_2 = mvAccelExpr_1 `cAdd` mvAccelComponent_2

accelXExpr_2 :: PExpr
accelXExpr_2 = mvAccelExpr_2 $. e1_2D

accelYExpr_2 :: PExpr
accelYExpr_2 = mvAccelExpr_2 $. e2_2D

-- ============================================================================
-- FORCES (GA vectors) and component extraction
-- ============================================================================

-- Gravity vectors (downwards)
gravitationalForce_1 :: PExpr
gravitationalForce_1 = vector (int 0) (neg (sy massObj_1 $* sy gravitationalMagnitude))

gravitationalForce_2 :: PExpr
gravitationalForce_2 = vector (int 0) (neg (sy massObj_2 $* sy gravitationalMagnitude))

-- Tension vectors are along the rods (rod direction used in component-style)
-- In many component derivations rod direction = [-sin θ, cos θ] for x/y decomposition of tension.
tensionVec_1 :: PExpr
tensionVec_1 = (sy massObj_1) `cScale` (vector (neg sinAngleExpr1) cosAngleExpr1)

-- Note: above is placeholder scaling; keep your actual tension symbol 'tension_1' if you have it in unitals.

tensionVec_2 :: PExpr
tensionVec_2 = (sy massObj_2) `cScale` (vector (neg sinAngleExpr2) cosAngleExpr2)
-- As above, replace the numeric scaling with sy tension_2 if tension is a defined quantity in Unitals.

-- Net inertial forces (mass * acceleration) + gravity (matches your earlier forceDerivExpr)
inertialForce_1 :: PExpr
inertialForce_1 = sy massObj_1 `cScale` mvAccelExpr_1

inertialForce_2 :: PExpr
inertialForce_2 = sy massObj_2 `cScale` mvAccelExpr_2

mvForceExpr_1 :: PExpr
mvForceExpr_1 = inertialForce_1 `cAdd` gravitationalForce_1

mvForceExpr_2 :: PExpr
mvForceExpr_2 = inertialForce_2 `cAdd` gravitationalForce_2

-- Component extraction for forces (keeps compatibility)
xForceComponent_1 :: PExpr
xForceComponent_1 = mvForceExpr_1 $. e1_2D

yForceComponent_1 :: PExpr
yForceComponent_1 = mvForceExpr_1 $. e2_2D

xForceComponent_2 :: PExpr
xForceComponent_2 = mvForceExpr_2 $. e1_2D

yForceComponent_2 :: PExpr
yForceComponent_2 = mvForceExpr_2 $. e2_2D

-- ============================================================================
-- CLIFFORD (GA) EXTRAS: geometric product, wedge, norm, etc.
-- ============================================================================

forceGeometricProduct :: PExpr
forceGeometricProduct = mvForceExpr_1 `geometricProd` mvForceExpr_2

forceWedgeProduct :: PExpr
forceWedgeProduct = mvForceExpr_1 `wedgeProd` mvForceExpr_2

forceDotProduct :: PExpr
forceDotProduct = mvForceExpr_1 $. mvForceExpr_2

forceDifference :: PExpr
forceDifference = mvForceExpr_1 `cSub` mvForceExpr_2

forceMagnitude_1 :: PExpr
forceMagnitude_1 = norm mvForceExpr_1

forceMagnitude_2 :: PExpr
forceMagnitude_2 = norm mvForceExpr_2

-- ============================================================================
-- ROTORS / DUALS
-- ============================================================================

-- Dual: use geometric product with pseudoscalar I (e12_clif)
dualOfVector :: PExpr -> PExpr
dualOfVector v = v `geometricProd` e12_clif

dualVelocity_1 :: PExpr
dualVelocity_1 = dualOfVector mvVelExpr_1

-- Rotor constructor (unit rotor for rotation by 'angle')
cliffordRotor :: PExpr -> PExpr
cliffordRotor angle = cos (angle $/ int 2) $+ (sin (angle $/ int 2) `cScale` e12_clif)

-- Rotor inverse for unit rotor R = a + b e12 is R^{-1} = a - b e12
rotorInverse :: PExpr -> PExpr
rotorInverse r = fstPart r $+ (neg (sndPart r) `cScale` e12_clif)

-- Helpers (assume rotor of the form cos(t/2) + sin(t/2) e12)
fstPart :: PExpr -> PExpr
fstPart _ = cos rotationAngle

sndPart :: PExpr -> PExpr
sndPart _ = sin rotationAngle

rotationAngle :: PExpr
rotationAngle = sy pendDisAngle_1 $/ int 2

-- Proper rotation via conjugation: v' = R v R^{-1}
rotateVector :: PExpr -> PExpr -> PExpr
rotateVector rotor v = rotor `geometricProd` v `geometricProd` rotorInverse rotor

rotatedDirection_1 :: PExpr
rotatedDirection_1 = rotateVector (cliffordRotor (sy pendDisAngle_1)) (vector (int 1) (int 0))

-- ============================================================================
-- TORQUE, KINETIC ENERGY, etc.
-- ============================================================================

torqueBivector_1 :: PExpr
torqueBivector_1 = positionVector_1 `wedgeProd` mvForceExpr_1
  where positionVector_1 = sy lenRod_1 `cScale` directionVector_1

torqueBivector_2 :: PExpr
torqueBivector_2 = positionVector_2 `wedgeProd` mvForceExpr_2
  where positionVector_2 = (sy lenRod_1 `cScale` directionVector_1) `cAdd` (sy lenRod_2 `cScale` directionVector_2)

kineticEnergyFactor_1 :: PExpr
kineticEnergyFactor_1 = mvVelExpr_1 $. mvVelExpr_1

kineticEnergyFactor_2 :: PExpr
kineticEnergyFactor_2 = mvVelExpr_2 $. mvVelExpr_2

-- ============================================================================
-- ANGULAR ACCELERATION EXPRESSIONS (fixed denominator grouping)
-- ============================================================================

angularAccelExpr_1 :: PExpr
angularAccelExpr_1 =
  let num1 = neg (sy gravitationalMagnitude) $* (exactDbl 2 $* sy massObj_1 $+ sy massObj_2) $* sin (sy pendDisAngle_1)
      num2 = sy massObj_2 $* sy gravitationalMagnitude $* sin (sy pendDisAngle_1 $- (exactDbl 2 $* sy pendDisAngle_2))
      num3 = (exactDbl 2 $* sin (sy pendDisAngle_1 $- sy pendDisAngle_2)) $* sy massObj_2 $*
             ( square (sy angularVel_2) $* sy lenRod_2
               $+ ( square (sy angularVel_1) $* sy lenRod_1 $* cos (sy pendDisAngle_1 $- sy pendDisAngle_2))
             )
      numerator = num1 $- num2 $- num3
      denominator = sy lenRod_1 $* ( exactDbl 2 $* sy massObj_1 $+ sy massObj_2 $-
                        ( sy massObj_2 $* cos ( exactDbl 2 $* sy pendDisAngle_1 $- (exactDbl 2 $* sy pendDisAngle_2) ) ) )
  in numerator $/ denominator

angularAccelExpr_2 :: PExpr
angularAccelExpr_2 =
  let numerator = exactDbl 2 $* sin (sy pendDisAngle_1 $- sy pendDisAngle_2) $*
                  ( square (sy angularVel_1) $* sy lenRod_1 $* (sy massObj_1 $+ sy massObj_2)
                    $+ ( sy gravitationalMagnitude $* (sy massObj_1 $+ sy massObj_2) $* cos (sy pendDisAngle_1) )
                    $+ ( square (sy angularVel_2) $* sy lenRod_2 $* sy massObj_2 $* cos (sy pendDisAngle_1 $- sy pendDisAngle_2) )
                  )
      denominator = sy lenRod_2 $* ( exactDbl 2 $* sy massObj_1 $+ sy massObj_2 $-
                        ( sy massObj_2 $* cos ( exactDbl 2 $* sy pendDisAngle_1 $- (exactDbl 2 $* sy pendDisAngle_2) ) ) )
  in numerator $/ denominator

-- ============================================================================
-- FORCE DERIVATIVES (mass * acceleration) kept for derivations
-- ============================================================================

forceDerivExpr1 :: PExpr
forceDerivExpr1 = sy massObj_1 `cScale` mvAccelExpr_1

forceDerivExpr2 :: PExpr
forceDerivExpr2 = sy massObj_2 `cScale` mvAccelExpr_2

-- ============================================================================
-- EXAMPLES / VERIFICATION
-- ============================================================================

exampleScalar :: PExpr
exampleScalar = scalar (dbl 5.0)

exampleVector :: PExpr
exampleVector = vector (dbl 3.0) (dbl 4.0)

exampleBivector :: PExpr
exampleBivector = bivector (dbl 2.5)

exampleMultivector :: PExpr
exampleMultivector = multivector
  (dbl 1.0)
  (dbl 2.0)
  (dbl 3.0)
  (dbl 0.5)

e1_squared :: PExpr
e1_squared = e1_clif `geometricProd` e1_clif

e2_squared :: PExpr
e2_squared = e2_clif `geometricProd` e2_clif

e1e2_product :: PExpr
e1e2_product = e1_clif `geometricProd` e2_clif

e2e1_product :: PExpr
e2e1_product = e2_clif `geometricProd` e1_clif

anticommutator :: PExpr
anticommutator = e1e2_product `cAdd` e2e1_product

-- End of module
