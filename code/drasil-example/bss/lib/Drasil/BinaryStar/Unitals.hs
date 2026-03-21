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
  acceleration, energy, gravitationalAccel, force, time)
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
  tFinal, sepDist,
  massMin, massMax, rMax, vMax, tMin, tMax]
  ++ map dqdWr [QP.velocity, QP.position, QP.acceleration, QP.force, QP.time,
     QP.energy, gravitationalConst, QPP.mass]
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
constants = gravitationalConstValue : specParamValues

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
-- Specification Parameters (Table: Specification Parameter Values)
-- These define the software constraint bounds.
---------------------------------------------------------

massMin, massMax, rMax, vMax, tMin, tMax :: UnitalChunk

massMin = uc' "m_min" (nounPhraseSP "minimum stellar mass")
  (S "lower bound for stellar mass (approximately 0.05 solar masses)")
  (sub lM (label "min")) Real kilogram

massMax = uc' "m_max" (nounPhraseSP "maximum stellar mass")
  (S "upper bound for stellar mass (approximately 50 solar masses)")
  (sub lM (label "max")) Real kilogram

rMax = uc' "r_max" (nounPhraseSP "maximum initial distance")
  (S "upper bound for initial position magnitude (covers wide binary separations)")
  (sub lR (label "max")) Real metre

vMax = uc' "v_max" (nounPhraseSP "maximum initial speed")
  (S "upper bound for initial velocity magnitude (below relativistic threshold)")
  (sub lV (label "max")) Real velU

tMin = uc' "t_min" (nounPhraseSP "minimum simulation time")
  (S "lower bound for simulation duration")
  (sub lT (label "min")) Real second

tMax = uc' "t_max" (nounPhraseSP "maximum simulation time")
  (S "upper bound for simulation duration")
  (sub lT (label "max")) Real second

-- | Spec parameter values
specParamValues :: [ConstQDef]
specParamValues =
  [ mkQuantDef massMin (dbl 1.0e29)   -- ~0.05 M_sun
  , mkQuantDef massMax (dbl 1.0e32)   -- ~50 M_sun
  , mkQuantDef rMax    (dbl 1.0e13)   -- ~67 AU
  , mkQuantDef vMax    (dbl 1.0e6)    -- ~0.3% c
  , mkQuantDef tMin    (dbl 1.0e3)    -- ~17 min
  , mkQuantDef tMax    (dbl 1.0e10)   -- ~317 yr
  ]

---------------------------------------------------------
-- Constraints (input validation)
---------------------------------------------------------

-- | Mass constraints: physical (> 0) + software (m_min <= m <= m_max)
massCon_1, massCon_2 :: ConstrConcept
massCon_1 = constrained' mass_1
  [gtZeroConstr,
   sfwrRange $ Bounded (Inc, sy massMin) (Inc, sy massMax)]
  (dbl 2.0e30)

massCon_2 = constrained' mass_2
  [gtZeroConstr,
   sfwrRange $ Bounded (Inc, sy massMin) (Inc, sy massMax)]
  (dbl 1.6e30)

-- | Initial position constraints: software (|x| <= r_max)
xPosCon_1_0, yPosCon_1_0, xPosCon_2_0, yPosCon_2_0 :: ConstrConcept
xPosCon_1_0 = constrained' xPos_1_0
  [sfwrRange $ Bounded (Inc, neg (sy rMax)) (Inc, sy rMax)]
  (dbl 7.5e10)

yPosCon_1_0 = constrained' yPos_1_0
  [sfwrRange $ Bounded (Inc, neg (sy rMax)) (Inc, sy rMax)]
  (dbl 0)

xPosCon_2_0 = constrained' xPos_2_0
  [sfwrRange $ Bounded (Inc, neg (sy rMax)) (Inc, sy rMax)]
  (dbl (-7.5e10))

yPosCon_2_0 = constrained' yPos_2_0
  [sfwrRange $ Bounded (Inc, neg (sy rMax)) (Inc, sy rMax)]
  (dbl 0)

-- | Initial velocity constraints: software (|v| <= v_max)
xVelCon_1_0, yVelCon_1_0, xVelCon_2_0, yVelCon_2_0 :: ConstrConcept
xVelCon_1_0 = constrained' xVel_1_0
  [sfwrRange $ Bounded (Inc, neg (sy vMax)) (Inc, sy vMax)]
  (dbl 0)

yVelCon_1_0 = constrained' yVel_1_0
  [sfwrRange $ Bounded (Inc, neg (sy vMax)) (Inc, sy vMax)]
  (dbl 1.0e4)

xVelCon_2_0 = constrained' xVel_2_0
  [sfwrRange $ Bounded (Inc, neg (sy vMax)) (Inc, sy vMax)]
  (dbl 0)

yVelCon_2_0 = constrained' yVel_2_0
  [sfwrRange $ Bounded (Inc, neg (sy vMax)) (Inc, sy vMax)]
  (dbl (-1.0e4))

-- | Time constraint: physical (>= 0) + software (t_min <= t <= t_max)
tFinalCon :: ConstrConcept
tFinalCon = constrained' tFinal
  [physRange $ UpFrom (Inc, exactDbl 0),
   sfwrRange $ Bounded (Inc, sy tMin) (Inc, sy tMax)]
  (dbl 3.15e7)

-- | Input constraints with uncertainty
inConstraints :: [UncertQ]
inConstraints =
  [ uq massCon_1   (uncty 0.05 Nothing)   -- 5%
  , uq massCon_2   (uncty 0.05 Nothing)   -- 5%
  , uq xPosCon_1_0 (uncty 0.01 Nothing)   -- 1%
  , uq yPosCon_1_0 (uncty 0.01 Nothing)   -- 1%
  , uq xPosCon_2_0 (uncty 0.01 Nothing)   -- 1%
  , uq yPosCon_2_0 (uncty 0.01 Nothing)   -- 1%
  , uq xVelCon_1_0 (uncty 0.05 Nothing)   -- 5%
  , uq yVelCon_1_0 (uncty 0.05 Nothing)   -- 5%
  , uq xVelCon_2_0 (uncty 0.05 Nothing)   -- 5%
  , uq yVelCon_2_0 (uncty 0.05 Nothing)   -- 5%
  , uq tFinalCon   exact                   -- no uncertainty
  ]

-- | Output constraints (positions have no specific bounds)
outConstraints :: [UncertQ]
outConstraints = []
