module Drasil.SglPend.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, radian)
import qualified Data.Drasil.Quantities.Physics as QP (position, ixPos, xPos, force, velocity,
  angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, yAccel,
  xAccel, yVel, xVel, iyPos, yPos, time, torque, momentOfInertia, angularDisplacement,
  angularFrequency, frequency, period)
import Data.Drasil.Concepts.Physics (pendulum)
import Data.Drasil.Concepts.Math as CM (angle, iAngle)
import Data.Drasil.Quantities.Math as QM (unitVect, unitVectj, pi_)
import Drasil.DblPend.Concepts (rod)
import Drasil.DblPend.Unitals (lRod)


symbols:: [QuantityDict]
symbols = map qw unitalChunks ++ map qw unitless

inputs :: [QuantityDict]
inputs = map qw [lenRod, QPP.mass, QP.angularAccel, pendDisplacementAngle, initialPendAngle]

outputs :: [QuantityDict]
outputs = [qw pendDisplacementAngle]

units :: [UnitalChunk]
units = map ucw unitalChunks

unitalChunks :: [UnitalChunk]
unitalChunks = [lenRod, QPP.mass, QP.force, QP.ixPos, QP.xPos, QP.yPos,
   QP.angularVelocity, QP.angularAccel, QP.gravitationalAccel, QP.tension, QP.acceleration,
   QP.yAccel, QP.xAccel, QP.yVel, QP.xVel, QP.iyPos, QP.time, QP.velocity, QP.position, QP.torque,
   QP.momentOfInertia, QP.angularDisplacement, QP.angularVelocity, initialPendAngle,
   QP.angularFrequency, QP.frequency, QP.period, lenRod, pendDisplacementAngle, initialPendAngle]

lenRod, pendDisplacementAngle, initialPendAngle :: UnitalChunk

lenRod = uc' "l_rod" (cn "length of the rod")
        (phraseNP (len `the_ofThe` rod))
        (sub cL lRod) Real metre

pendDisplacementAngle = uc' "pendDisplacementAngle" (cn "displacement angle of the pendulum")
        (phraseNP (angle `the_ofThe` pendulum))
        (sub lTheta lP) Real radian
        -- (sub lTheta lP) (mkFunction [Real] Real) radian

initialPendAngle = uc' "initialPendAngle" (cn "initial pendulum angle")
        (phraseNP (NP.the (CM.iAngle `of_` pendulum)))
        (sub lTheta lI) Real radian

unitless :: [DefinedQuantityDict]
unitless = [QM.unitVect, QM.unitVectj, QM.pi_]

-----------------------
-- CONSTRAINT CHUNKS --
-----------------------
lenRodCons, pendDisplacementAngleOutCons, angAccelOutCons, initialPendAngleCons :: ConstrConcept

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt) [lenRodCons, initialPendAngleCons]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) [angAccelOutCons, pendDisplacementAngleOutCons]

lenRodCons                   = constrained' lenRod                [gtZeroConstr] (dbl 44.2)
initialPendAngleCons         = constrained' initialPendAngle      [gtZeroConstr] (dbl 2.1)
pendDisplacementAngleOutCons = constrained' pendDisplacementAngle [gtZeroConstr] (dbl 2.1)
angAccelOutCons              = constrained' QP.angularAccel       [gtZeroConstr] (exactDbl 0)
