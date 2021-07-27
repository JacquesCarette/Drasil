module Drasil.DblPendulum.Unitals where

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import Data.Drasil.Constraints (gtZeroConstr)

import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst, object,
        requirement, srs, typUnc)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, degree, radian, kilogram)
import qualified Data.Drasil.Quantities.Physics as QP (position, ixPos, xPos, force, velocity,
  angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, yAccel,
  xAccel, yVel, xVel, iyPos, yPos, time, torque, momentOfInertia, angularDisplacement,
  angularFrequency, frequency, period)
import Data.Drasil.Concepts.Physics (pendulum, twoD)
import Data.Drasil.Concepts.Math as CM (angle, iAngle)
import Data.Drasil.Quantities.Math as QM (unitVect, unitVectj, pi_)
import Drasil.DblPendulum.Concepts (rod)


symbols:: [QuantityDict]
symbols = map qw unitalChunks ++ map qw unitless

acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, srs, thModel, typUnc]

inputs :: [QuantityDict]
inputs = map qw [lenRod, QPP.mass, QP.angularAccel, pendDisplacementAngle, initialPendAngle] 

outputs :: [QuantityDict]
outputs = map qw [pendDisplacementAngle]

units :: [UnitaryConceptDict]
units = map ucw unitalChunks

unitalChunks :: [UnitalChunk]
unitalChunks = [lenRod, lenRod_1, lenRod_2, mass_1, mass_2, QPP.mass, QP.force, QP.ixPos, QP.xPos, QP.yPos,
   QP.angularVelocity, QP.angularAccel, QP.gravitationalAccel, QP.tension, QP.acceleration,
   QP.yAccel, QP.xAccel, QP.yVel, QP.xVel, QP.iyPos, QP.time, QP.velocity, QP.position, QP.torque,
   QP.momentOfInertia, QP.angularDisplacement, QP.angularVelocity, initialPendAngle,
   QP.angularFrequency, QP.frequency, QP.period, pendDisplacementAngle]

lenRod, lenRod_1, lenRod_2, mass_1, mass_2, pendDisplacementAngle, initialPendAngle :: UnitalChunk

-- fix me, replace lenRod with lenOne
lenRod = makeUCWDS "l_rod" (cn "length of the rod")
        (phraseNP (len `the_ofThe` rod))
        (sub cL lRod) metre

lenRod_1 = makeUCWDS "l_1" (cn "length of the rod")
        (phraseNP (len `the_ofThe` rod))
        (sub cL label1) metre

lenRod_2 = makeUCWDS "l_2" (cn "length of the rod")
        (phraseNP (len `the_ofThe` rod))
        (sub cL label2) metre

mass_1 = makeUCWDS "m_1" (cn "mass of the object")
        (phraseNP (mass `the_ofThe` object))
        (sub lM label1) kilogram

mass_2 = makeUCWDS "m_2" (cn "mass of the object")
        (phraseNP (mass `the_ofThe` object))
        (sub lM label2) kilogram

pendDisplacementAngle = makeUCWDS "pendDisplacementAngle" (cn "displacement angle of the pendulum")
        (phraseNP (angle `the_ofThe` pendulum))
        (sub lTheta lP) degree

initialPendAngle = makeUCWDS "initialPendAngle" (cn "initial pendulum angle")
        (phraseNP (NP.the (CM.iAngle `of_` pendulum)))
        (sub lTheta lI) radian


unitless :: [DefinedQuantityDict]
unitless = [QM.unitVect, QM.unitVectj, QM.pi_]
-----------------------

lRod, label1, label2:: Symbol
lRod = label "rod"
label1  = Integ 1
label2  = Integ 2

-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

lenRodCons, pendDisplacementAngleOutCons, angAccelOutCons, initialPendAngleCons :: ConstrConcept

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt)
  [lenRodCons, initialPendAngleCons]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) 
  [angAccelOutCons, pendDisplacementAngleOutCons]


lenRodCons     = constrained' lenRod        [gtZeroConstr] (dbl 44.2)
initialPendAngleCons  = constrained' initialPendAngle    [gtZeroConstr] (dbl 2.1)
--gravAccelCons  = constrained' QP.gravitationalAccel    [gtZeroConstr] (dbl 9.8)
pendDisplacementAngleOutCons  = constrained' pendDisplacementAngle    [gtZeroConstr] (dbl 2.1)
angAccelOutCons    = constrained' QP.angularAccel    [gtZeroConstr] (exactDbl 0)


