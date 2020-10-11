module Drasil.DblPendulum.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
--import Data.Drasil.IdeaDicts
--import Theory.Drasil (mkQuantDef)
import Utils.Drasil
import Data.Drasil.Constraints (gtZeroConstr)
--import Control.Lens((^.))

-- import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
--   requirement, srs, typUnc)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, degree)
--(distance, oneD, twoD)

import qualified Data.Drasil.Quantities.Physics as QP (position, ixPos, force, velocity,
 angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, yAccel, xAccel, yVel, xVel, iyPos, time, position)
import Data.Drasil.Concepts.Physics (pendulum)
import Data.Drasil.Concepts.Math as CM (angle, unitV)
import Data.Drasil.Quantities.Math as QM (unitVect, unitVectj)

symbols:: [QuantityDict]
symbols = map qw unitalChunks ++ map qw unitless
-- map qw [mass, ixPos, position, lenRod, pendAngle]
--symbols = map qw unitalChunks

-- -- FIXME: Move to Defs?
-- acronyms :: [CI]
-- acronyms = [oneD, twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
--   physSyst, requirement, srs, thModel, typUnc]

inputs :: [QuantityDict]
inputs = map qw [lenRod, QP.force] 
--mass, length, angle, g, t

outputs :: [QuantityDict]
outputs = map qw [QP.position]

units :: [UnitaryConceptDict]
units = map ucw unitalChunks ++ map ucw [lenRod, pendAngle]

unitalChunks :: [UnitalChunk]
unitalChunks = [lenRod, QPP.mass, QP.force, QP.ixPos, pendAngle,
   QP.angularVelocity, QP.angularAccel, QP.gravitationalAccel, QP.tension, QP.acceleration,
   QP.yAccel, QP.xAccel, QP.yVel, QP.xVel, QP.iyPos, QP.time, QP.velocity, QP.position]

lenRod, pendAngle :: UnitalChunk

lenRod = makeUCWDS "l_rod" (cn "length of rod")
        (S "the" +:+ phrase len `ofThe` S "rod")
        (sub cL lRod) metre

pendAngle = makeUCWDS "pendAngle" (cn "angle of pendulum")
        (S "the" +:+ phrase angle `ofThe` phrase pendulum)
        lTheta degree

unitless :: [DefinedQuantityDict]
unitless = [QM.unitVect, QM.unitVectj]
-----------------------
lRod :: Symbol

lRod = Label "rod"

-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

lenRodCons, massCons, gravAccelCons :: ConstrConcept

inputConstraints :: [UncertQ]
inputConstraints = map (`uq` defaultUncrt)
  [lenRodCons, massCons, gravAccelCons]

outputConstraints :: [UncertQ]
outputConstraints = map (`uq` defaultUncrt) 
  [posOutCons, veloOutCons]


lenRodCons     = constrained' QPP.len        [gtZeroConstr] (dbl 44.2)
massCons       = constrained' QP.velocity   [gtZeroConstr] (dbl 56.2)
gravAccelCons  = constrained' QP.velocity    [gtZeroConstr] (dbl 74.5)
posOutCons	   = constrained' QP.velocity    [gtZeroConstr] (dbl 74.5)
veloOutCons    = constrained' QP.velocity    [gtZeroConstr] (dbl 74.5)
