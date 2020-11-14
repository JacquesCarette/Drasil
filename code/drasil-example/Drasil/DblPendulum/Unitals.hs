module Drasil.DblPendulum.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
import Data.Drasil.IdeaDicts
import Theory.Drasil (mkQuantDef)
import Utils.Drasil
import Data.Drasil.Constraints (gtZeroConstr)
--import Control.Lens((^.))

import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
        requirement, srs, typUnc)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, degree)
import qualified Data.Drasil.Quantities.Physics as QP (position, ixPos, force, velocity,
  angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, yAccel,
  xAccel, yVel, xVel, iyPos, time, position)
import Data.Drasil.Concepts.Physics (pendulum, twoD)
import Data.Drasil.Concepts.Math as CM (angle)
import Data.Drasil.Quantities.Math as QM (unitVect, unitVectj)

symbols:: [QuantityDict]
symbols = map qw unitalChunks ++ map qw unitless

acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, srs, thModel, typUnc]

inputs :: [QuantityDict]
inputs = map qw [lenRod, QP.force] 

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

lenRodCons, massCons, gravAccelCons, pendAngleCons, angAccelOutCons :: ConstrConcept

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt)
  [lenRodCons, massCons, gravAccelCons, pendAngleCons]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) 
  [angAccelOutCons]


lenRodCons     = constrained' lenRod        [gtZeroConstr] (dbl 44.2)
massCons       = constrained' QPP.mass   [gtZeroConstr] (dbl 56.2)
gravAccelCons  = constrained' QP.gravitationalAccel    [gtZeroConstr] (dbl 9.8)
pendAngleCons  = constrained' pendAngle    [gtZeroConstr] (dbl 2.1)
angAccelOutCons    = constrained' QP.angularAccel    [gtZeroConstr] (dbl 0.0)


