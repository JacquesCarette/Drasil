module Drasil.DblPend.Unitals where

import Prelude hiding (sin, cos)

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Space (ClifKind(Vector))
import qualified Language.Drasil.Space as S
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
  requirement, refBy, refName, srs, typUnc)
import Drasil.Metadata (dataDefn, genDefn, inModel, thModel)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, radian, kilogram, newton)
import qualified Data.Drasil.Quantities.Physics as QP (position, force, velocity,
  angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, time)
import Data.Drasil.Concepts.Physics (twoD)
import Data.Drasil.Concepts.Math as CM (angle, xDir, yDir)
import Data.Drasil.Quantities.Math as QM (unitVect, pi_)
import Drasil.DblPend.Concepts (firstRod, secondRod, firstObject, secondObject, horizontalPos,
  verticalPos, horizontalVel, verticalVel, horizontalAccel, verticalAccel)
import Data.Drasil.Units.Physics (velU, accelU, angVelU, angAccelU)
import Data.Drasil.Quantities.Physics (gravitationalAccelConst)


symbols:: [QuantityDict]
symbols = map qw unitalChunks ++ map qw unitless ++ [qw pendDisAngle] ++ map qw constants

acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, refBy, refName, srs, thModel, typUnc]

inputs :: [QuantityDict]
inputs = map qw [lenRod_1, lenRod_2, massObj_1, massObj_2, pendDisAngle_1, pendDisAngle_2] 

outputs :: [QuantityDict]
outputs = [qw xPos_1, qw mvVel_1, qw mvAccel_1]  -- Including Clifford algebra quantities to show in SRS

constants :: [ConstQDef]
constants = [gravitationalAccelConst]

-- Clifford algebra helper functions
vecDim :: S.Dimension
vecDim = S.Fixed 2

-- | Helper function to create Clifford vector spaces of a given dimension
realVect :: S.Dimension -> Space
realVect d = S.ClifS d S.Vector Real

unitalChunks :: [UnitalChunk]
unitalChunks = [ 
  lenRod_1, lenRod_2, massObj_1, massObj_2, pendDisAngle_1, pendDisAngle_2,
  xVel_1, xVel_2, yVel_1, yVel_2, xPos_1, xPos_2, yPos_1, yPos_2, xAccel_1,
  yAccel_1, xAccel_2, yAccel_2, angularVel_1, angularVel_2, angularAccel_1, angularAccel_2,
  mvVel_1, mvVel_2, mvAccel_1, mvAccel_2, mvForce_1, mvForce_2, tension_1, tension_2,
  QPP.mass, QP.force, QP.gravitationalAccel, QP.tension, QP.acceleration,
  QP.time, QP.velocity, QP.position]
  
lenRod_1, lenRod_2, massObj_1, massObj_2, pendDisAngle_1, pendDisAngle_2, 
  xVel_1, xVel_2, yVel_1, yVel_2, xPos_1, xPos_2, yPos_1, yPos_2, xAccel_1, 
  yAccel_1, xAccel_2, yAccel_2, angularVel_1, angularVel_2, angularAccel_1, angularAccel_2,
  mvVel_1, mvVel_2, mvAccel_1, mvAccel_2, mvForce_1, mvForce_2, tension_1, tension_2 :: UnitalChunk

lenRod_1 = uc' "l_1" (len `ofThe` firstRod)
        (phraseNP (len `the_ofThe` firstRod)) -- Fix me, can have more information 
        (sub cL label1) Real metre

lenRod_2 = uc' "l_2" (len `ofThe` secondRod)
        (phraseNP (len `the_ofThe` secondRod))
        (sub cL label2) Real metre

massObj_1 = uc' "m_1" (mass `ofThe` firstObject)
        (phraseNP (mass `the_ofThe` firstObject))
        (sub lM label1) Real kilogram

massObj_2 = uc' "m_2" (mass `ofThe` secondObject)
        (phraseNP (mass `the_ofThe` secondObject))
        (sub lM label2) Real kilogram

xPos_1 = uc' "p_x1" (horizontalPos `ofThe` firstObject)
        (phraseNP (QP.position `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lP (Concat [labelx, label1])) Real metre

xPos_2 = uc' "p_x2" (horizontalPos `ofThe` secondObject)
        (phraseNP (QP.position `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lP (Concat [labelx, label2])) Real metre

-- Vector position of mass 1
posVec_1 :: Expr
posVec_1 = (l1 $* sin t1) `cScale` e1_basis `cAdd` (l1 $* cos t1) `cScale` e2_basis

yPos_1 = uc' "p_y1" (verticalPos `ofThe` firstObject)
        (phraseNP (QP.position `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lP (Concat [labely, label1])) Real metre

yPos_2 = uc' "p_y2" (verticalPos `ofThe` secondObject)
        (phraseNP (QP.position `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lP (Concat [labely, label2])) Real metre

-- Vector position of mass 2
posVec_2 :: Expr
posVec_2 = posVec_1 `cAdd` ((l2 $* sin t2) `cScale` e1_basis `cAdd` (l2 $* cos t2) `cScale` e2_basis)

xVel_1 = uc' "v_x1" (horizontalVel `ofThe` firstObject)
        (phraseNP (QP.velocity `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label1])) Real velU

xVel_2 = uc' "v_x2" (horizontalVel `ofThe` secondObject)
        (phraseNP (QP.velocity `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label2])) Real velU

-- Clifford velocity representation for first object
mvVel_1 = uc' "v_mv1" (QP.velocity `ofThe` firstObject)
        (phraseNP (QP.velocity `the_ofThe` firstObject))
        (sub lV label1) (realVect vecDim) velU

-- Clifford velocity representation for second object
mvVel_2 = uc' "v_mv2" (QP.velocity `ofThe` secondObject)
        (phraseNP (QP.velocity `the_ofThe` secondObject))
        (sub lV label2) (realVect vecDim) velU

yVel_1 = uc' "v_y1" (verticalVel `ofThe` firstObject)
        (phraseNP (QP.velocity `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label1])) Real velU

yVel_2 = uc' "v_y2" (verticalVel `ofThe` secondObject)
        (phraseNP (QP.velocity `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label2])) Real velU



xAccel_1 = uc' "a_x1" (horizontalAccel `ofThe` firstObject)
        (phraseNP (QP.acceleration `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lA (Concat [labelx, label1])) Real accelU

xAccel_2 = uc' "a_x2" (horizontalAccel `ofThe` secondObject)
        (phraseNP (QP.acceleration `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lA (Concat [labelx, label2])) Real accelU

-- Clifford acceleration representation for first object
mvAccel_1 = uc' "a_mv1" (QP.acceleration `ofThe` firstObject)
        (phraseNP (QP.acceleration `the_ofThe` firstObject))
        (sub lA label1) (realVect vecDim) accelU

yAccel_1 = uc' "a_y1" (verticalAccel `ofThe` firstObject)
        (phraseNP (QP.acceleration `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lA (Concat [labely, label1])) Real accelU

yAccel_2 = uc' "a_y2" (verticalAccel `ofThe` secondObject)
        (phraseNP (QP.acceleration `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lA (Concat [labely, label2])) Real accelU

-- Clifford acceleration representation for second object
mvAccel_2 = uc' "a_mv2" (QP.acceleration `ofThe` secondObject)
        (phraseNP (QP.acceleration `the_ofThe` secondObject))
        (sub lA label2) (realVect vecDim) accelU

angularVel_1 = uc' "omega_1" (QP.angularVelocity `ofThe` firstObject)
        (phraseNP (QP.angularVelocity `the_ofThe` firstObject))
        (sub cOmega label1) Real angVelU

angularVel_2 = uc' "omega_2" (QP.angularVelocity `ofThe` secondObject)
        (phraseNP (QP.angularVelocity `the_ofThe` secondObject))
        (sub cOmega label2) Real angVelU

-- angularAccel_1 = uc' "alpha_x1" (QP.angularAccel `ofThe` firstObject)
--         (phraseNP (QP.angularAccel `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
--         (sub lAlpha label1) Real angAccelU

-- angularAccel_2 = uc' "alpha_y1" (QP.angularAccel `ofThe` secondObject)
--         (phraseNP (QP.angularAccel `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
--         (sub lAlpha label2) Real angAccelU

angularAccel_1 = uc' "alpha_1" (QP.angularAccel `ofThe` firstObject)
        (phraseNP (QP.angularAccel `the_ofThe` firstRod))
        (sub lAlpha label1) Real angAccelU
angularAccel_2 = uc' "alpha_2" (QP.angularAccel `ofThe` secondObject)
        (phraseNP (QP.angularAccel `the_ofThe` secondRod))
        (sub lAlpha label2) Real angAccelU

-- Clifford force representation for first object
mvForce_1 = uc' "F_mv1" (QP.force `ofThe` firstObject)
        (phraseNP (QP.force `the_ofThe` firstObject))
        (sub lF label1) (realVect vecDim) newton

-- Clifford force representation for second object  
mvForce_2 = uc' "F_mv2" (QP.force `ofThe` secondObject)
        (phraseNP (QP.force `the_ofThe` secondObject))
        (sub lF label2) (realVect vecDim) newton

pendDisAngle_1 = uc' "theta_1" (angle `ofThe` firstRod)
        (phraseNP (angle `the_ofThe` firstRod))
        (sub lTheta label1) Real radian

pendDisAngle_2 = uc' "theta_2" (angle `ofThe` secondRod)
        (phraseNP (angle `the_ofThe` secondRod))
        (sub lTheta label2) Real radian

unitless :: [DefinedQuantityDict]
unitless = [QM.unitVect, QM.pi_]

lRod, label1, label2, labelx, labely, initial, lTheta':: Symbol
lRod = label "rod"
labelx = label "x"
labely = label "y"
initial = label "i"
label1  = Integ 1
label2  = Integ 2
lTheta'  = label "theta"

----------------
-- CONSTRAINT --
----------------
lenRodCon_1, lenRodCon_2, pendDisAngleCon_1, pendDisAngleCon_2, massCon_1, massCon_2 
  :: ConstrConcept
lenRodCon_1       = constrained' lenRod_1 [gtZeroConstr] (dbl 1)
lenRodCon_2       = constrained' lenRod_2 [gtZeroConstr] (dbl 1)
pendDisAngleCon_1 = constrained' pendDisAngle_1 [gtZeroConstr] (dbl 30)
pendDisAngleCon_2 = constrained' pendDisAngle_2 [gtZeroConstr] (dbl 30)
massCon_1         = constrained' massObj_1 [gtZeroConstr] (dbl 0.5)
massCon_2         = constrained' massObj_2 [gtZeroConstr] (dbl 0.5)

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt) [lenRodCon_1, lenRodCon_2, massCon_1, massCon_2]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) [pendDisAngleCon_1, pendDisAngleCon_2]

pendDisAngle :: ConstrConcept
pendDisAngle = cuc' "pendDisAngle"
  (nounPhraseSP "dependent variables")
  "column vector of displacement of rods with its derivatives"
  lTheta' radian (ClifS (VDim "2") Vector Real)
  [physRange $ UpFrom (Inc, exactDbl 0)] (exactDbl 0)

-- Missing variable definitions
l1, l2 :: Expr
l1 = sy lenRod_1
l2 = sy lenRod_2

t1, t2 :: Expr  
t1 = sy pendDisAngle_1
t2 = sy pendDisAngle_2

o1, o2 :: Expr
o1 = sy angularVel_1
o2 = sy angularVel_2

t1dd, t2dd :: Expr
t1dd = sy angularAccel_1
t2dd = sy angularAccel_2

-- Initial condition variables for ODEs
initial_theta1, initial_omega1, initial_theta2, initial_omega2 :: Expr
initial_theta1 = dbl 1.3463968515384828 -- 3*pi/7
initial_omega1 = exactDbl 0
initial_theta2 = dbl 2.356194490192345  -- 3*pi/4
initial_omega2 = exactDbl 0

-- Basis vectors for 2D Clifford algebra
e1_basis :: PExpr  
e1_basis = vect [int 1, int 0]  -- Standard basis vector e₁

e2_basis :: PExpr
e2_basis = vect [int 0, int 1]  -- Standard basis vector e₂

-- Scalar tension forces for traditional derivations (constraint forces)
tension_1 = uc' "T_1" (QP.tension `ofThe` firstRod)
        (phraseNP (QP.tension `the_ofThe` firstRod))
        (sub lT label1) Real newton

tension_2 = uc' "T_2" (QP.tension `ofThe` secondRod)
        (phraseNP (QP.tension `the_ofThe` secondRod))
        (sub lT label2) Real newton

