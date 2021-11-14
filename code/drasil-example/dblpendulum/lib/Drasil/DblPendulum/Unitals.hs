module Drasil.DblPendulum.Unitals where

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst, requirement, srs, typUnc)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, degree, kilogram, newton)
import qualified Data.Drasil.Quantities.Physics as QP (position, force, velocity,
  angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, time)
import Data.Drasil.Concepts.Physics (twoD)
import Data.Drasil.Concepts.Math as CM (angle, xDir, yDir)
import Data.Drasil.Quantities.Math as QM (unitVect, unitVectj, pi_)
import Drasil.DblPendulum.Concepts (firstRod, secondRod, firstObject, secondObject, horizontalPos,
  verticalPos, horizontalVel, verticalVel, horizontalAccel, verticalAccel)
import Data.Drasil.Units.Physics (velU, accelU, angVelU, angAccelU)


symbols:: [QuantityDict]
symbols = map qw unitalChunks ++ map qw unitless

acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, srs, thModel, typUnc]

inputs :: [QuantityDict]
inputs = map qw [lenRod_1, lenRod_2, pendDisAngle_1, pendDisAngle_2, massObj_1, massObj_2] 

outputs :: [QuantityDict]
outputs = map qw [angularAccel_1, angularAccel_2]

units :: [UnitalChunk]
units = map ucw unitalChunks

unitalChunks :: [UnitalChunk]
unitalChunks = [ 
  lenRod_1, lenRod_2, massObj_1, massObj_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2, xVel_1, xVel_2, yVel_1, yVel_2,
  xPos_1, xPos_2, yPos_1, yPos_2, xAccel_1, yAccel_1, xAccel_2, yAccel_2,
  angularAccel_1, angularAccel_2, tension_1, tension_2,
  QPP.mass, QP.force, QP.gravitationalAccel, QP.tension, QP.acceleration,
  QP.time, QP.velocity, QP.position]
  
lenRod_1, lenRod_2, massObj_1, massObj_2, angularVel_1, angularVel_2, 
  pendDisAngle_1, pendDisAngle_2,
  xPos_1, xPos_2, yPos_1, yPos_2, xVel_1, yVel_1, xVel_2, yVel_2, xAccel_1,
  yAccel_1, xAccel_2, yAccel_2,
  angularAccel_1, angularAccel_2, tension_1, tension_2 :: UnitalChunk

lenRod_1 = makeUCWDS "l_1" (nounPhraseSent $ phraseNP(len `the_ofThe` firstRod))
        (S "The" +:+ phraseNP (len `the_ofThe` firstRod)) -- Fix me, can have more information 
        (sub cL label1) metre

lenRod_2 = makeUCWDS "l_2" (nounPhraseSent $ phraseNP(len `the_ofThe` secondRod))
        (S "The" +:+ phraseNP (len `the_ofThe` secondRod))
        (sub cL label2) metre

massObj_1 = makeUCWDS "m_1" (nounPhraseSent $ phraseNP (mass `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (mass `the_ofThe` firstObject))
        (sub lM label1) kilogram

massObj_2 = makeUCWDS "m_2" (nounPhraseSent $ phraseNP (mass `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (mass `the_ofThe` secondObject))
        (sub lM label2) kilogram

xPos_1 = makeUCWDS "p_x1" (nounPhraseSent $ phraseNP (horizontalPos `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.position `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lP (Concat [labelx, label1])) metre

xPos_2 = makeUCWDS "p_x2" (nounPhraseSent $ phraseNP (horizontalPos `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.position `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lP (Concat [labelx, label2])) metre

yPos_1 = makeUCWDS "p_y1" (nounPhraseSent $ phraseNP (verticalPos `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.position `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lP (Concat [labely, label1])) metre

yPos_2 = makeUCWDS "p_y2" (nounPhraseSent $ phraseNP (verticalPos `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.position `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lP (Concat [labely, label2])) metre

xVel_1 = makeUCWDS "v_x1" (nounPhraseSent $ phraseNP (horizontalVel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label1])) velU

xVel_2 = makeUCWDS "v_x2" (nounPhraseSent $ phraseNP (horizontalVel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label2])) velU

yVel_1 = makeUCWDS "v_y1" (nounPhraseSent $ phraseNP (verticalVel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label1])) velU

yVel_2 = makeUCWDS "v_y2" (nounPhraseSent $ phraseNP (verticalVel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label2])) velU

xAccel_1 = makeUCWDS "a_x1" (nounPhraseSent $ phraseNP (horizontalAccel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.acceleration `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lA (Concat [labelx, label1])) accelU

xAccel_2 = makeUCWDS "a_x2" (nounPhraseSent $ phraseNP (horizontalAccel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.acceleration `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lA (Concat [labelx, label2])) accelU

yAccel_1 = makeUCWDS "a_y1" (nounPhraseSent $ phraseNP (verticalAccel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.acceleration `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lA (Concat [labely, label1])) accelU

yAccel_2 = makeUCWDS "a_y2" (nounPhraseSent $ phraseNP (verticalAccel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.acceleration `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lA (Concat [labely, label2])) accelU

angularAccel_1 = makeUCWDS "alpha_x1" (nounPhraseSent $ phraseNP (QP.angularAccel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.angularAccel `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lAlpha label1) angAccelU

angularAccel_2 = makeUCWDS "alpha_y1" (nounPhraseSent $ phraseNP (QP.angularAccel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.angularAccel `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lAlpha label2) angAccelU

tension_1 = makeUCWDS "T_1" (nounPhraseSent $ phraseNP (QP.tension `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.tension `the_ofThe` firstObject))
        (sub (vec cT) label1) newton

tension_2 = makeUCWDS "T_2" (nounPhraseSent $ phraseNP (QP.tension `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.tension `the_ofThe` secondObject))
        (sub (vec cT) label2) newton

angularVel_1 = makeUCWDS "w_1" (nounPhraseSent $ phraseNP (QP.angularVelocity `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` firstObject))
        (sub lW label1) angVelU

angularVel_2 = makeUCWDS "w_2" (nounPhraseSent $ phraseNP (QP.angularVelocity `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` secondObject))
        (sub lW label2) angVelU

pendDisAngle_1 = makeUCWDS "theta_1" (nounPhraseSent $ phraseNP (angle `the_ofThe` firstRod))
        (S "The" +:+ phraseNP (angle `the_ofThe` firstRod))
        (sub lTheta label1) degree

pendDisAngle_2 = makeUCWDS "theta_2" (nounPhraseSent $ phraseNP (angle `the_ofThe` secondRod))
        (S "The" +:+ phraseNP (angle `the_ofThe` secondRod))
        (sub lTheta label2) degree

unitless :: [DefinedQuantityDict]
unitless = [QM.unitVect, QM.unitVectj, QM.pi_]

lRod, label1, label2, labelx, labely, initial:: Symbol
lRod = label "rod"
labelx = label "x"
labely = label "y"
initial = label "i"
label1  = Integ 1
label2  = Integ 2

----------------
-- CONSTRAINT --
----------------
lenRodCon_1, lenRodCon_2, pendDisAngleCon_1, pendDisAngleCon_2, massCon_1, massCon_2,
  angAccelOutCon_1, angAccelOutCon_2 :: ConstrConcept
lenRodCon_1       = constrained' lenRod_1 [gtZeroConstr] (dbl 1)
lenRodCon_2       = constrained' lenRod_2 [gtZeroConstr] (dbl 1)
pendDisAngleCon_1 = constrained' pendDisAngle_1 [gtZeroConstr] (dbl 30)
pendDisAngleCon_2 = constrained' pendDisAngle_2 [gtZeroConstr] (dbl 30)
massCon_1         = constrained' massObj_1 [gtZeroConstr] (dbl 0.5)
massCon_2         = constrained' massObj_2 [gtZeroConstr] (dbl 0.5)
angAccelOutCon_1  = constrained' angularAccel_1 [gtZeroConstr] (exactDbl 0)
angAccelOutCon_2  = constrained' angularAccel_2 [gtZeroConstr] (exactDbl 0)

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt) [lenRodCon_1, lenRodCon_2, pendDisAngleCon_1, pendDisAngleCon_2,
  massCon_1, massCon_2]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) [angAccelOutCon_1, angAccelOutCon_2]
