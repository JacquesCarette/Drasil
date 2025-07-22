module Drasil.DblPend.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalMagnitude)
import Drasil.DblPend.Unitals (lenRod_1, lenRod_2, massObj_1, massObj_2,
  angularAccel_1, angularAccel_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2)

-- Clifford algebra basis elements
clifScalar :: PExpr
clifScalar = lit 1

e1_clif :: PExpr
e1_clif = clif Nothing [int 0, int 1, int 0, int 0]

e2_clif :: PExpr  
e2_clif = clif Nothing [int 0, int 0, int 1, int 0]

e12_clif :: PExpr
e12_clif = clif Nothing [int 0, int 0, int 0, int 1]

e1_2D :: PExpr
e1_2D = clif Nothing [int 0, int 1, int 0, int 0]

e2_2D :: PExpr  
e2_2D = clif Nothing [int 0, int 0, int 1, int 0]

-- Multivector constructors
scalar :: PExpr -> PExpr
scalar s = clif Nothing [s, int 0, int 0, int 0]

vector :: PExpr -> PExpr -> PExpr
vector x y = clif Nothing [int 0, x, y, int 0]

bivector :: PExpr -> PExpr
bivector xy = clif Nothing [int 0, int 0, int 0, xy]

multivector :: PExpr -> PExpr -> PExpr -> PExpr -> PExpr
multivector s x y xy = clif Nothing [s, x, y, xy]

-- Direction vectors
cosAngleExpr1, sinAngleExpr1, cosAngleExpr2, sinAngleExpr2 :: PExpr
cosAngleExpr1 = cos (sy pendDisAngle_1)
sinAngleExpr1 = sin (sy pendDisAngle_1)
cosAngleExpr2 = cos (sy pendDisAngle_2)
sinAngleExpr2 = sin (sy pendDisAngle_2)

directionVector_1 :: PExpr
directionVector_1 = vector cosAngleExpr1 sinAngleExpr1

directionVector_2 :: PExpr  
directionVector_2 = vector cosAngleExpr2 sinAngleExpr2

perpDirectionVector_1 :: PExpr
perpDirectionVector_1 = vector (neg sinAngleExpr1) cosAngleExpr1

perpDirectionVector_2 :: PExpr
perpDirectionVector_2 = vector (neg sinAngleExpr2) cosAngleExpr2

-- Velocity and acceleration
mvVelExpr_1 :: PExpr
mvVelExpr_1 = (sy angularVel_1 $* sy lenRod_1) `cScale` directionVector_1

mvVelComponent_2 :: PExpr
mvVelComponent_2 = (sy angularVel_2 $* sy lenRod_2) `cScale` directionVector_2

mvVelExpr_2 :: PExpr
mvVelExpr_2 = mvVelExpr_1 `cAdd` mvVelComponent_2

centripetalAccel_1 :: PExpr
centripetalAccel_1 = neg (square (sy angularVel_1) $* sy lenRod_1) `cScale` directionVector_1

tangentialAccel_1 :: PExpr
tangentialAccel_1 = (sy angularAccel_1 $* sy lenRod_1) `cScale` perpDirectionVector_1

mvAccelExpr_1 :: PExpr
mvAccelExpr_1 = centripetalAccel_1 `cAdd` tangentialAccel_1

centripetalAccel_2 :: PExpr
centripetalAccel_2 = neg (square (sy angularVel_2) $* sy lenRod_2) `cScale` directionVector_2

tangentialAccel_2 :: PExpr
tangentialAccel_2 = (sy angularAccel_2 $* sy lenRod_2) `cScale` perpDirectionVector_2

mvAccelComponent_2 :: PExpr
mvAccelComponent_2 = centripetalAccel_2 `cAdd` tangentialAccel_2

mvAccelExpr_2 :: PExpr
mvAccelExpr_2 = mvAccelExpr_1 `cAdd` mvAccelComponent_2

-- Forces
gravitationalForce_1 :: PExpr
gravitationalForce_1 = vector (int 0) (neg (sy massObj_1 $* sy gravitationalMagnitude))

gravitationalForce_2 :: PExpr  
gravitationalForce_2 = vector (int 0) (neg (sy massObj_2 $* sy gravitationalMagnitude))

inertialForce_1 :: PExpr
inertialForce_1 = sy massObj_1 `cScale` mvAccelExpr_1

inertialForce_2 :: PExpr
inertialForce_2 = sy massObj_2 `cScale` mvAccelExpr_2

mvForceExpr_1 :: PExpr
mvForceExpr_1 = inertialForce_1 `cAdd` gravitationalForce_1

mvForceExpr_2 :: PExpr  
mvForceExpr_2 = inertialForce_2 `cAdd` gravitationalForce_2

-- Component extraction
xForceComponent_1 :: PExpr
xForceComponent_1 = gradeSelect 1 mvForceExpr_1 $. e1_2D

yForceComponent_1 :: PExpr
yForceComponent_1 = gradeSelect 1 mvForceExpr_1 $. e2_2D

xForceComponent_2 :: PExpr
xForceComponent_2 = gradeSelect 1 mvForceExpr_2 $. e1_2D

yForceComponent_2 :: PExpr
yForceComponent_2 = gradeSelect 1 mvForceExpr_2 $. e2_2D

-- Clifford operations
scalarForceComponent_1 :: PExpr
scalarForceComponent_1 = gradeSelect 0 mvForceExpr_1

vectorForceComponent_1 :: PExpr
vectorForceComponent_1 = gradeSelect 1 mvForceExpr_1

bivectorForceComponent_1 :: PExpr
bivectorForceComponent_1 = gradeSelect 2 mvForceExpr_1

forceGeometricProduct :: PExpr
forceGeometricProduct = mvForceExpr_1 `geometricProd` mvForceExpr_2

forceWedgeProduct :: PExpr
forceWedgeProduct = mvForceExpr_1 `wedgeProd` mvForceExpr_2

forceDotProduct :: PExpr  
forceDotProduct = gradeSelect 0 forceGeometricProduct

forceWedgeFromGeometric :: PExpr
forceWedgeFromGeometric = gradeSelect 2 forceGeometricProduct

forceDifference :: PExpr
forceDifference = mvForceExpr_1 `cSub` mvForceExpr_2

forceMagnitude_1 :: PExpr
forceMagnitude_1 = norm mvForceExpr_1

forceMagnitude_2 :: PExpr
forceMagnitude_2 = norm mvForceExpr_2

-- Advanced operations
forceConjugate_1 :: PExpr
forceConjugate_1 = multivector 
  (gradeSelect 0 mvForceExpr_1)
  (gradeSelect 1 mvForceExpr_1)
  (gradeSelect 1 mvForceExpr_1)
  (neg (gradeSelect 2 mvForceExpr_1))

forceInverseNorm_1 :: PExpr
forceInverseNorm_1 = mvForceExpr_1 `geometricProd` forceConjugate_1

rotationAngle :: PExpr
rotationAngle = sy pendDisAngle_1 $/ int 2

rotationBivector :: PExpr  
rotationBivector = bivector rotationAngle

rotationRotor :: PExpr
rotationRotor = multivector 
  (cos rotationAngle)
  (int 0)
  (int 0)
  (sin rotationAngle)

-- Physics applications
positionVector_1 :: PExpr
positionVector_1 = sy lenRod_1 `cScale` directionVector_1

positionVector_2 :: PExpr
positionVector_2 = positionVector_1 `cAdd` (sy lenRod_2 `cScale` directionVector_2)

angularVelBivector_1 :: PExpr
angularVelBivector_1 = bivector (sy angularVel_1)

angularVelBivector_2 :: PExpr
angularVelBivector_2 = bivector (sy angularVel_2)

angularVelBivectorAlt_1 :: PExpr
angularVelBivectorAlt_1 = sy angularVel_1 `cScale` (e1_clif `wedgeProd` e2_clif)

angularVelBivectorAlt_2 :: PExpr
angularVelBivectorAlt_2 = sy angularVel_2 `cScale` (e1_clif `wedgeProd` e2_clif)

torqueBivector_1 :: PExpr
torqueBivector_1 = positionVector_1 `wedgeProd` mvForceExpr_1

torqueBivector_2 :: PExpr
torqueBivector_2 = positionVector_2 `wedgeProd` mvForceExpr_2

kineticEnergyFactor_1 :: PExpr
kineticEnergyFactor_1 = mvVelExpr_1 $. mvVelExpr_1

kineticEnergyFactor_2 :: PExpr
kineticEnergyFactor_2 = mvVelExpr_2 $. mvVelExpr_2

-- Angular acceleration expressions
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

-- Force derivatives
forceDerivExpr1 :: PExpr
forceDerivExpr1 = sy massObj_1 `cScale` mvAccelExpr_1

forceDerivExpr2 :: PExpr  
forceDerivExpr2 = sy massObj_2 `cScale` mvAccelExpr_2

-- Examples and verification
exampleScalar :: PExpr
exampleScalar = scalar (exactDbl 5.0)

exampleVector :: PExpr  
exampleVector = vector (exactDbl 3.0) (exactDbl 4.0)

exampleBivector :: PExpr
exampleBivector = bivector (exactDbl 2.5)

exampleMultivector :: PExpr
exampleMultivector = multivector 
  (exactDbl 1.0)
  (exactDbl 2.0)
  (exactDbl 3.0)
  (exactDbl 0.5)

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

pseudoscalar_2D :: PExpr
pseudoscalar_2D = e1_clif `wedgeProd` e2_clif

dualOfVector :: PExpr -> PExpr
dualOfVector v = v `geometricProd` pseudoscalar_2D

dualVelocity_1 :: PExpr
dualVelocity_1 = dualOfVector mvVelExpr_1

cliffordRotor :: PExpr -> PExpr  
cliffordRotor angle = multivector 
  (cos (angle $/ int 2))
  (int 0)
  (int 0)
  (sin (angle $/ int 2))

rotateVector :: PExpr -> PExpr -> PExpr
rotateVector rotor vec = 
  let rotorConj = multivector 
        (gradeSelect 0 rotor)
        (neg (gradeSelect 1 rotor))
        (neg (gradeSelect 1 rotor))
        (neg (gradeSelect 2 rotor))
  in rotor `geometricProd` vec `geometricProd` rotorConj

rotatedDirection_1 :: PExpr
rotatedDirection_1 = rotateVector (cliffordRotor (sy pendDisAngle_1)) (vector (int 1) (int 0))


