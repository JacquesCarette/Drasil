module Drasil.DblPendulum.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
import Data.Drasil.IdeaDicts
import Utils.Drasil
import Data.Drasil.Constraints (gtZeroConstr)
--import Control.Lens((^.))

import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
        requirement, srs, typUnc)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, degree, radian)
import qualified Data.Drasil.Quantities.Physics as QP (position, ixPos, force, velocity,
  angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, yAccel,
  xAccel, yVel, xVel, iyPos, time, position, torque, momentOfInertia, angularDisplacement,
  angularFrequency, frequency, period)
import Data.Drasil.Concepts.Physics (pendulum, twoD)
import Data.Drasil.Concepts.Math as CM (angle)
import Data.Drasil.Quantities.Math as QM (unitVect, unitVectj, pi_)


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
units = map ucw unitalChunks ++ map ucw [lenRod, pendDisplacementAngle, initialPendAngle]

unitalChunks :: [UnitalChunk]
unitalChunks = [lenRod, QPP.mass, QP.force, QP.ixPos,
   QP.angularVelocity, QP.angularAccel, QP.gravitationalAccel, QP.tension, QP.acceleration,
   QP.yAccel, QP.xAccel, QP.yVel, QP.xVel, QP.iyPos, QP.time, QP.velocity, QP.position, QP.torque,
   QP.momentOfInertia, QP.angularDisplacement, QP.angularVelocity, initialPendAngle,
   QP.angularFrequency, QP.frequency, QP.period, pendDisplacementAngle]

lenRod, pendDisplacementAngle, initialPendAngle :: UnitalChunk

lenRod = makeUCWDS "l_rod" (cn "length of rod")
        (S "the" +:+ phrase len `ofThe` S "rod")
        (sub cL lRod) metre

pendDisplacementAngle = makeUCWDS "pendDisplacementAngle" (cn "displacement angle of pendulum")
        (S "the" +:+ phrase angle `ofThe` phrase pendulum)
        (sub lTheta lP) degree

initialPendAngle = makeUCWDS "initialPendAngle" (cn "initial pendulum angle")
        (S "the initial angle of" +:+ phrase pendulum)
        (sub lTheta lI) radian


unitless :: [DefinedQuantityDict]
unitless = [QM.unitVect, QM.unitVectj, QM.pi_]
-----------------------
lRod :: Symbol

lRod = Label "rod"

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
angAccelOutCons    = constrained' QP.angularAccel    [gtZeroConstr] (dbl 0.0)


