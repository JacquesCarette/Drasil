module Drasil.BinaryStar.Unitals where

import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
  refBy, refName, requirement, srs, typUnc)
import Data.Drasil.Concepts.Theory (dataDefn, genDefn, inModel, thModel)
import qualified Data.Drasil.Quantities.Physics as QP (position, velocity,
  acceleration, gravitationalAccel, force, time)
import Data.Drasil.Concepts.Physics (twoD)
import Data.Drasil.Concepts.Math (xDir, yDir)
import Data.Drasil.Quantities.Physics (gravitationalConst, gravitationalConstValue)
import Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import Data.Drasil.SI_Units (metre, kilogram, second)
import Data.Drasil.Units.Physics (velU, accelU)

import Drasil.BinaryStar.Concepts (starOne, starTwo)

---------------------------------------------------------
-- Exported lists
---------------------------------------------------------

-- | All symbols for the Symbol Table
symbols :: [DefinedQuantityDict]
symbols = map dqdWr [mass_1, mass_2, xPos_1, yPos_1, xPos_2, yPos_2,
  xVel_1, yVel_1, xVel_2, yVel_2, xAccel_1, yAccel_1, xAccel_2, yAccel_2,
  xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0,
  xVel_1_0, yVel_1_0, xVel_2_0, yVel_2_0,
  tFinal, sepDist]
  ++ map dqdWr [QP.velocity, QP.position, QP.acceleration, QP.force, QP.time,
     gravitationalConst, QPP.mass]
  ++ map dqdWr constants

-- | Acronyms for the Abbreviations table
acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, refBy, refName, srs, thModel, typUnc]

-- | Input variables (what the user provides)
inputs :: [DefinedQuantityDict]
inputs = map dqdWr [mass_1, mass_2, xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0,
  xVel_1_0, yVel_1_0, xVel_2_0, yVel_2_0, tFinal]

-- | Output variables (what the system calculates)
outputs :: [DefinedQuantityDict]
outputs = map dqdWr [xPos_1, yPos_1, xPos_2, yPos_2]

-- | Constants
constants :: [ConstQDef]
constants = [gravitationalConstValue]

---------------------------------------------------------
-- Unital chunks (physical quantities with symbols & units)
---------------------------------------------------------

unitalChunks :: [UnitalChunk]
unitalChunks = [
  -- masses
  mass_1, mass_2,
  -- positions (state variables)
  xPos_1, yPos_1, xPos_2, yPos_2,
  -- initial positions
  xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0,
  -- velocities (state variables)
  xVel_1, yVel_1, xVel_2, yVel_2,
  -- initial velocities
  xVel_1_0, yVel_1_0, xVel_2_0, yVel_2_0,
  -- accelerations
  xAccel_1, yAccel_1, xAccel_2, yAccel_2,
  -- time
  tFinal,
  -- separation distance
  sepDist]

---------------------------------------------------------
-- Mass quantities
---------------------------------------------------------

mass_1, mass_2 :: UnitalChunk

mass_1 = uc' "m_1" (mass `ofThe` starOne)
  (D.toSent $ phraseNP (mass `the_ofThe` starOne))
  (sub lM label1) Real kilogram

mass_2 = uc' "m_2" (mass `ofThe` starTwo)
  (D.toSent $ phraseNP (mass `the_ofThe` starTwo))
  (sub lM label2) Real kilogram

---------------------------------------------------------
-- Position quantities (x, y for each star)
---------------------------------------------------------

xPos_1, yPos_1, xPos_2, yPos_2 :: UnitalChunk

xPos_1 = uc' "x_1" (QP.position `ofThe` starOne)
  (D.toSent (phraseNP (QP.position `the_ofThe` starOne)) `S.inThe` phrase xDir)
  (sub lX label1) Real metre

yPos_1 = uc' "y_1" (QP.position `ofThe` starOne)
  (D.toSent (phraseNP (QP.position `the_ofThe` starOne)) `S.inThe` phrase yDir)
  (sub lY label1) Real metre

xPos_2 = uc' "x_2" (QP.position `ofThe` starTwo)
  (D.toSent (phraseNP (QP.position `the_ofThe` starTwo)) `S.inThe` phrase xDir)
  (sub lX label2) Real metre

yPos_2 = uc' "y_2" (QP.position `ofThe` starTwo)
  (D.toSent (phraseNP (QP.position `the_ofThe` starTwo)) `S.inThe` phrase yDir)
  (sub lY label2) Real metre

---------------------------------------------------------
-- Initial position quantities
---------------------------------------------------------

xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0 :: UnitalChunk

xPos_1_0 = uc' "x_1_0" (QP.position `ofThe` starOne)
  (S "initial" +:+ D.toSent (phraseNP (QP.position `the_ofThe` starOne)) `S.inThe` phrase xDir)
  (sub lX (Concat [label1, label0])) Real metre

yPos_1_0 = uc' "y_1_0" (QP.position `ofThe` starOne)
  (S "initial" +:+ D.toSent (phraseNP (QP.position `the_ofThe` starOne)) `S.inThe` phrase yDir)
  (sub lY (Concat [label1, label0])) Real metre

xPos_2_0 = uc' "x_2_0" (QP.position `ofThe` starTwo)
  (S "initial" +:+ D.toSent (phraseNP (QP.position `the_ofThe` starTwo)) `S.inThe` phrase xDir)
  (sub lX (Concat [label2, label0])) Real metre

yPos_2_0 = uc' "y_2_0" (QP.position `ofThe` starTwo)
  (S "initial" +:+ D.toSent (phraseNP (QP.position `the_ofThe` starTwo)) `S.inThe` phrase yDir)
  (sub lY (Concat [label2, label0])) Real metre

---------------------------------------------------------
-- Velocity quantities
---------------------------------------------------------

xVel_1, yVel_1, xVel_2, yVel_2 :: UnitalChunk

xVel_1 = uc' "vx_1" (QP.velocity `ofThe` starOne)
  (D.toSent (phraseNP (QP.velocity `the_ofThe` starOne)) `S.inThe` phrase xDir)
  (sub lV (Concat [labelx, label1])) Real velU

yVel_1 = uc' "vy_1" (QP.velocity `ofThe` starOne)
  (D.toSent (phraseNP (QP.velocity `the_ofThe` starOne)) `S.inThe` phrase yDir)
  (sub lV (Concat [labely, label1])) Real velU

xVel_2 = uc' "vx_2" (QP.velocity `ofThe` starTwo)
  (D.toSent (phraseNP (QP.velocity `the_ofThe` starTwo)) `S.inThe` phrase xDir)
  (sub lV (Concat [labelx, label2])) Real velU

yVel_2 = uc' "vy_2" (QP.velocity `ofThe` starTwo)
  (D.toSent (phraseNP (QP.velocity `the_ofThe` starTwo)) `S.inThe` phrase yDir)
  (sub lV (Concat [labely, label2])) Real velU

---------------------------------------------------------
-- Initial velocity quantities
---------------------------------------------------------

xVel_1_0, yVel_1_0, xVel_2_0, yVel_2_0 :: UnitalChunk

xVel_1_0 = uc' "vx_1_0" (QP.velocity `ofThe` starOne)
  (S "initial" +:+ D.toSent (phraseNP (QP.velocity `the_ofThe` starOne)) `S.inThe` phrase xDir)
  (sub lV (Concat [labelx, label1, label0])) Real velU

yVel_1_0 = uc' "vy_1_0" (QP.velocity `ofThe` starOne)
  (S "initial" +:+ D.toSent (phraseNP (QP.velocity `the_ofThe` starOne)) `S.inThe` phrase yDir)
  (sub lV (Concat [labely, label1, label0])) Real velU

xVel_2_0 = uc' "vx_2_0" (QP.velocity `ofThe` starTwo)
  (S "initial" +:+ D.toSent (phraseNP (QP.velocity `the_ofThe` starTwo)) `S.inThe` phrase xDir)
  (sub lV (Concat [labelx, label2, label0])) Real velU

yVel_2_0 = uc' "vy_2_0" (QP.velocity `ofThe` starTwo)
  (S "initial" +:+ D.toSent (phraseNP (QP.velocity `the_ofThe` starTwo)) `S.inThe` phrase yDir)
  (sub lV (Concat [labely, label2, label0])) Real velU

---------------------------------------------------------
-- Acceleration quantities
---------------------------------------------------------

xAccel_1, yAccel_1, xAccel_2, yAccel_2 :: UnitalChunk

xAccel_1 = uc' "ax_1" (QP.acceleration `ofThe` starOne)
  (D.toSent (phraseNP (QP.acceleration `the_ofThe` starOne)) `S.inThe` phrase xDir)
  (sub lA (Concat [labelx, label1])) Real accelU

yAccel_1 = uc' "ay_1" (QP.acceleration `ofThe` starOne)
  (D.toSent (phraseNP (QP.acceleration `the_ofThe` starOne)) `S.inThe` phrase yDir)
  (sub lA (Concat [labely, label1])) Real accelU

xAccel_2 = uc' "ax_2" (QP.acceleration `ofThe` starTwo)
  (D.toSent (phraseNP (QP.acceleration `the_ofThe` starTwo)) `S.inThe` phrase xDir)
  (sub lA (Concat [labelx, label2])) Real accelU

yAccel_2 = uc' "ay_2" (QP.acceleration `ofThe` starTwo)
  (D.toSent (phraseNP (QP.acceleration `the_ofThe` starTwo)) `S.inThe` phrase yDir)
  (sub lA (Concat [labely, label2])) Real accelU

---------------------------------------------------------
-- Other quantities
---------------------------------------------------------

tFinal :: UnitalChunk
tFinal = uc' "t_final" (nounPhraseSP "final time")
  (S "end time of the simulation interval")
  (sub lT (label "final")) Real second

sepDist :: UnitalChunk
sepDist = uc' "r_12" (nounPhraseSP "separation distance")
  (S "distance between the two stars")
  (sub lR (Concat [label1, label2])) Real metre

---------------------------------------------------------
-- Symbol helpers
---------------------------------------------------------

label1, label2, label0, labelx, labely :: Symbol
label1 = Integ 1
label2 = Integ 2
label0 = Integ 0
labelx = label "x"
labely = label "y"

---------------------------------------------------------
-- Constraints (input validation)
---------------------------------------------------------

massCon_1, massCon_2 :: ConstrConcept
massCon_1 = constrained' mass_1 [gtZeroConstr] (dbl 1.0e30)
massCon_2 = constrained' mass_2 [gtZeroConstr] (dbl 1.0e30)

tFinalCon :: ConstrConcept
tFinalCon = constrained' tFinal [gtZeroConstr] (dbl 1.0e8)

-- | Input constraints with uncertainty
inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt) [massCon_1, massCon_2, tFinalCon]

-- | Output constraints (positions have no specific bounds)
outConstraints :: [UncertQ]
outConstraints = []
