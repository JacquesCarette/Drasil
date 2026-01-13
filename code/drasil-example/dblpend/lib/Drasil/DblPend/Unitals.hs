module Drasil.DblPend.Unitals where

import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.Metadata (dataDefn, genDefn, inModel, thModel, requirement, srs)
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
  refBy, refName, typUnc)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, radian, kilogram, newton)
import qualified Data.Drasil.Quantities.Physics as QP (position, force, velocity,
  angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, time)
import Data.Drasil.Concepts.Physics (twoD)
import Data.Drasil.Concepts.Math as CM (angle, xDir, yDir)
import Drasil.DblPend.Concepts (firstRod, secondRod, firstObject, secondObject, horizontalPos,
  verticalPos, horizontalVel, verticalVel, horizontalAccel, verticalAccel)
import Data.Drasil.Units.Physics (velU, accelU, angVelU, angAccelU)
import Data.Drasil.Quantities.Physics (gravitationalAccelConst)

symbols:: [DefinedQuantityDict]
symbols = map dqdWr unitalChunks ++ [dqdWr pendDisAngle] ++ map dqdWr constants

acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, refBy, refName, srs, thModel, typUnc]

inputs :: [DefinedQuantityDict]
inputs = map dqdWr [lenRod_1, lenRod_2, massObj_1, massObj_2]

outputs :: [DefinedQuantityDict]
outputs = [dqdWr pendDisAngle]

constants :: [ConstQDef]
constants = [gravitationalAccelConst]

unitalChunks :: [UnitalChunk]
unitalChunks = [
  lenRod_1, lenRod_2, massObj_1, massObj_2,
  pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2,
  xVel_1, xVel_2, yVel_1, yVel_2, xPos_1, xPos_2, yPos_1, yPos_2, xAccel_1,
  yAccel_1, xAccel_2, yAccel_2, angularAccel_1, angularAccel_2, tension_1,
  tension_2, QPP.mass, QP.force, QP.gravitationalAccel, QP.acceleration,
  QP.time, QP.velocity, QP.position]

lenRod_1, lenRod_2, massObj_1, massObj_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2,
  xPos_1, xPos_2, yPos_1, yPos_2, xVel_1, yVel_1, xVel_2, yVel_2, xAccel_1,
  yAccel_1, xAccel_2, yAccel_2,
  angularAccel_1, angularAccel_2, tension_1, tension_2 :: UnitalChunk

lenRod_1 = uc' "l_1" (len `ofThe` firstRod)
        (D.toSent $ phraseNP (len `the_ofThe` firstRod)) -- Fix me, can have more information
        (sub cL label1) Real metre

lenRod_2 = uc' "l_2" (len `ofThe` secondRod)
        (D.toSent $ phraseNP (len `the_ofThe` secondRod))
        (sub cL label2) Real metre

massObj_1 = uc' "m_1" (mass `ofThe` firstObject)
        (D.toSent $ phraseNP (mass `the_ofThe` firstObject))
        (sub lM label1) Real kilogram

massObj_2 = uc' "m_2" (mass `ofThe` secondObject)
        (D.toSent $ phraseNP (mass `the_ofThe` secondObject))
        (sub lM label2) Real kilogram

xPos_1 = uc' "p_x1" (horizontalPos `ofThe` firstObject)
        (D.toSent (phraseNP (QP.position `the_ofThe` firstObject)) `S.inThe` phrase CM.xDir)
        (sub lP (Concat [labelx, label1])) Real metre

xPos_2 = uc' "p_x2" (horizontalPos `ofThe` secondObject)
        (D.toSent (phraseNP (QP.position `the_ofThe` secondObject)) `S.inThe` phrase CM.xDir)
        (sub lP (Concat [labelx, label2])) Real metre

yPos_1 = uc' "p_y1" (verticalPos `ofThe` firstObject)
        (D.toSent (phraseNP (QP.position `the_ofThe` firstObject)) `S.inThe` phrase CM.yDir)
        (sub lP (Concat [labely, label1])) Real metre

yPos_2 = uc' "p_y2" (verticalPos `ofThe` secondObject)
        (D.toSent (phraseNP (QP.position `the_ofThe` secondObject)) `S.inThe` phrase CM.yDir)
        (sub lP (Concat [labely, label2])) Real metre

xVel_1 = uc' "v_x1" (horizontalVel `ofThe` firstObject)
        (D.toSent (phraseNP (QP.angularVelocity `the_ofThe` firstObject)) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label1])) Real velU

xVel_2 = uc' "v_x2" (horizontalVel `ofThe` secondObject)
        (D.toSent (phraseNP (QP.angularVelocity `the_ofThe` secondObject)) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label2])) Real velU

yVel_1 = uc' "v_y1" (verticalVel `ofThe` firstObject)
        (D.toSent (phraseNP (QP.angularVelocity `the_ofThe` firstObject)) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label1])) Real velU

yVel_2 = uc' "v_y2" (verticalVel `ofThe` secondObject)
        (D.toSent (phraseNP (QP.angularVelocity `the_ofThe` secondObject)) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label2])) Real velU

xAccel_1 = uc' "a_x1" (horizontalAccel `ofThe` firstObject)
        (D.toSent (phraseNP (QP.acceleration `the_ofThe` firstObject)) `S.inThe` phrase CM.xDir)
        (sub lA (Concat [labelx, label1])) Real accelU

xAccel_2 = uc' "a_x2" (horizontalAccel `ofThe` secondObject)
        (D.toSent (phraseNP (QP.acceleration `the_ofThe` secondObject)) `S.inThe` phrase CM.xDir)
        (sub lA (Concat [labelx, label2])) Real accelU

yAccel_1 = uc' "a_y1" (verticalAccel `ofThe` firstObject)
        (D.toSent (phraseNP (QP.acceleration `the_ofThe` firstObject)) `S.inThe` phrase CM.yDir)
        (sub lA (Concat [labely, label1])) Real accelU

yAccel_2 = uc' "a_y2" (verticalAccel `ofThe` secondObject)
        (D.toSent (phraseNP (QP.acceleration `the_ofThe` secondObject)) `S.inThe` phrase CM.yDir)
        (sub lA (Concat [labely, label2])) Real accelU

angularAccel_1 = uc' "alpha_x1" (QP.angularAccel `ofThe` firstObject)
        (D.toSent (phraseNP (QP.angularAccel `the_ofThe` firstObject)) `S.inThe` phrase CM.xDir)
        (sub lAlpha label1) Real angAccelU

angularAccel_2 = uc' "alpha_y1" (QP.angularAccel `ofThe` secondObject)
        (D.toSent (phraseNP (QP.angularAccel `the_ofThe` secondObject)) `S.inThe` phrase CM.yDir)
        (sub lAlpha label2) Real angAccelU

tension_1 = uc' "T_1" (QP.tension `ofThe` firstObject)
        (D.toSent $ phraseNP (QP.tension `the_ofThe` firstObject))
        (sub (vec cT) label1) Real newton

tension_2 = uc' "T_2" (QP.tension `ofThe` secondObject)
        (D.toSent $ phraseNP (QP.tension `the_ofThe` secondObject))
        (sub (vec cT) label2) Real newton

angularVel_1 = uc' "w_1" (QP.angularVelocity `ofThe` firstObject)
        (D.toSent $ phraseNP (QP.angularVelocity `the_ofThe` firstObject))
        (sub lW label1) Real angVelU

angularVel_2 = uc' "w_2" (QP.angularVelocity `ofThe` secondObject)
        (D.toSent $ phraseNP (QP.angularVelocity `the_ofThe` secondObject))
        (sub lW label2) Real angVelU

pendDisAngle_1 = uc' "theta_1" (angle `ofThe` firstRod)
        (D.toSent $ phraseNP (angle `the_ofThe` firstRod))
        (sub lTheta label1) Real radian

pendDisAngle_2 = uc' "theta_2" (angle `ofThe` secondRod)
        (D.toSent $ phraseNP (angle `the_ofThe` secondRod))
        (sub lTheta label2) Real radian

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
pendDisAngleCon_1 = constrained' pendDisAngle_1 [gtZeroConstr] (dbl 30) -- FIXME: These are "reasonable values," but they're not used in the SRS?
pendDisAngleCon_2 = constrained' pendDisAngle_2 [gtZeroConstr] (dbl 30)
massCon_1         = constrained' massObj_1 [gtZeroConstr] (dbl 0.5)
massCon_2         = constrained' massObj_2 [gtZeroConstr] (dbl 0.5)

inConstraints :: [ConstrConcept]
inConstraints = [lenRodCon_1, lenRodCon_2, massCon_1, massCon_2]

outConstraints :: [UncertQ]
outConstraints = [uq pendDisAngle defaultUncrt]

pendDisAngle :: ConstrConcept
pendDisAngle = cuc' "pendDisAngle"
  (nounPhraseSP "dependent variables")
  "column vector of displacement of rods with its derivatives"
  lTheta' radian (Vect Real)
  [physRange $ UpFrom (Inc, exactDbl 0)] (exactDbl 0)
