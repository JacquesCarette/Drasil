module Drasil.DblPendulum.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
--import Data.Drasil.IdeaDicts
--import Theory.Drasil (mkQuantDef)
import Utils.Drasil

-- import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
--   requirement, srs, typUnc)
import Data.Drasil.Quantities.PhysicalProperties (len, mass)
import Data.Drasil.SI_Units (metre, degree)
--(distance, oneD, twoD)

import qualified Data.Drasil.Quantities.Physics as QP (position, ixPos, force)
import Data.Drasil.Concepts.Physics (pendulum)
import Data.Drasil.Concepts.Math (angle)

symbols:: [QuantityDict]
symbols = map qw unitalChunks
--symbols = map qw [mass, ixPos, position, lenRod, pendAngle]
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
unitalChunks = [lenRod, mass, QP.force, QP.ixPos, pendAngle]

lenRod, pendAngle :: UnitalChunk

lenRod = makeUCWDS "l_rod" (cn "length of rod")
        (S "the" +:+ phrase len `ofThe` S "rod")
        (sub cL lRod) metre

pendAngle = makeUCWDS "pendAngle" (cn "angle of pendulum")
        (S "the" +:+ phrase angle `ofThe` phrase pendulum)
        lTheta degree


--Labels
lRod :: Symbol

lRod = Label "rod"



