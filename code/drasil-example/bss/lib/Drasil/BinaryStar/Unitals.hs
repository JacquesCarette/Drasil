module Drasil.BinaryStar.Unitals where

import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.Database(mkUid)

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
  refBy, refName, requirement, srs, typUnc)
import Data.Drasil.Concepts.Theory (dataDefn, genDefn, inModel, thModel)
import qualified Data.Drasil.Quantities.Physics as QP (position, velocity,
  acceleration, energy, force, fOfGravity, time)
import Data.Drasil.Concepts.Physics (twoD)
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
  massMin, massMax, rMax, vMax, tMax]
  ++ map dqdWr [QP.velocity, QP.position, QP.acceleration, QP.force,
     QP.fOfGravity, QP.time, QP.energy, gravitationalConst, QPP.mass]
  ++ [index, numbBodies, dqdWr bssStateVar]
  ++ map dqdWr constants

-- | Acronyms for the Abbreviations table
acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, refBy, refName, srs, thModel, typUnc]

-- | Input variables (what the user provides)
inputs :: NE.NonEmpty DefinedQuantityDict
inputs = NE.map dqdWr $ mass_1 :| [mass_2, xPos_1_0, yPos_1_0, xPos_2_0,
  yPos_2_0, xVel_1_0, yVel_1_0, xVel_2_0, yVel_2_0, tFinal]

-- | Output variables (what the system calculates)
outputs :: NE.NonEmpty DefinedQuantityDict
outputs = dqdWr bssStateVar :| []

-- | Constants
constants :: [ConstQDef]
constants = gravitationalConstValue : specParamValues

---------------------------------------------------------
-- Unital chunks (physical quantities with symbols & units)
---------------------------------------------------------

unitalChunks :: [DefinedQuantityDict]
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

mass_1, mass_2 :: DefinedQuantityDict

mass_1 = quant (mkUid "m_1") (mass `ofThe` starOne)
  (D.toSent $ phraseNP (mass `the_ofThe` starOne))
  (sub lM label1) Real kilogram

mass_2 = quant (mkUid "m_2") (mass `ofThe` starTwo)
  (D.toSent $ phraseNP (mass `the_ofThe` starTwo))
  (sub lM label2) Real kilogram

---------------------------------------------------------
-- Position quantities (x, y for each star)
---------------------------------------------------------

xPos_1, yPos_1, xPos_2, yPos_2 :: DefinedQuantityDict

xPos_1 = quant (mkUid "x_1") (nounPhraseSP "x-position of the first star")
  (S "x-component of the" +:+ phrase QP.position `S.ofThe` phrase starOne)
  (sub lX label1) Real metre

yPos_1 = quant (mkUid "y_1") (nounPhraseSP "y-position of the first star")
  (S "y-component of the" +:+ phrase QP.position `S.ofThe` phrase starOne)
  (sub lY label1) Real metre

xPos_2 = quant (mkUid "x_2") (nounPhraseSP "x-position of the second star")
  (S "x-component of the" +:+ phrase QP.position `S.ofThe` phrase starTwo)
  (sub lX label2) Real metre

yPos_2 = quant (mkUid "y_2") (nounPhraseSP "y-position of the second star")
  (S "y-component of the" +:+ phrase QP.position `S.ofThe` phrase starTwo)
  (sub lY label2) Real metre

---------------------------------------------------------
-- Initial position quantities
---------------------------------------------------------

xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0 :: DefinedQuantityDict

xPos_1_0 = quant (mkUid "x_1_0") (nounPhraseSP "initial x-position of the first star")
  (S "initial x-component of the" +:+ phrase QP.position `S.ofThe` phrase starOne)
  (sup (sub lX label1) label0) Real metre

yPos_1_0 = quant (mkUid "y_1_0") (nounPhraseSP "initial y-position of the first star")
  (S "initial y-component of the" +:+ phrase QP.position `S.ofThe` phrase starOne)
  (sup (sub lY label1) label0) Real metre

xPos_2_0 = quant (mkUid "x_2_0") (nounPhraseSP "initial x-position of the second star")
  (S "initial x-component of the" +:+ phrase QP.position `S.ofThe` phrase starTwo)
  (sup (sub lX label2) label0) Real metre

yPos_2_0 = quant (mkUid "y_2_0") (nounPhraseSP "initial y-position of the second star")
  (S "initial y-component of the" +:+ phrase QP.position `S.ofThe` phrase starTwo)
  (sup (sub lY label2) label0) Real metre

---------------------------------------------------------
-- Velocity quantities
---------------------------------------------------------

xVel_1, yVel_1, xVel_2, yVel_2 :: DefinedQuantityDict

xVel_1 = quant (mkUid "vx_1") (nounPhraseSP "x-velocity of the first star")
  (S "x-component of the" +:+ phrase QP.velocity `S.ofThe` phrase starOne)
  (sub lV (Concat [labelx, label1])) Real velU

yVel_1 = quant (mkUid "vy_1") (nounPhraseSP "y-velocity of the first star")
  (S "y-component of the" +:+ phrase QP.velocity `S.ofThe` phrase starOne)
  (sub lV (Concat [labely, label1])) Real velU

xVel_2 = quant (mkUid "vx_2") (nounPhraseSP "x-velocity of the second star")
  (S "x-component of the" +:+ phrase QP.velocity `S.ofThe` phrase starTwo)
  (sub lV (Concat [labelx, label2])) Real velU

yVel_2 = quant (mkUid "vy_2") (nounPhraseSP "y-velocity of the second star")
  (S "y-component of the" +:+ phrase QP.velocity `S.ofThe` phrase starTwo)
  (sub lV (Concat [labely, label2])) Real velU

---------------------------------------------------------
-- Initial velocity quantities
---------------------------------------------------------

xVel_1_0, yVel_1_0, xVel_2_0, yVel_2_0 :: DefinedQuantityDict

xVel_1_0 = quant (mkUid "vx_1_0") (nounPhraseSP "initial x-velocity of the first star")
  (S "initial x-component of the" +:+ phrase QP.velocity `S.ofThe` phrase starOne)
  (sup (sub lV (Concat [labelx, label1])) label0) Real velU

yVel_1_0 = quant (mkUid "vy_1_0") (nounPhraseSP "initial y-velocity of the first star")
  (S "initial y-component of the" +:+ phrase QP.velocity `S.ofThe` phrase starOne)
  (sup (sub lV (Concat [labely, label1])) label0) Real velU

xVel_2_0 = quant (mkUid "vx_2_0") (nounPhraseSP "initial x-velocity of the second star")
  (S "initial x-component of the" +:+ phrase QP.velocity `S.ofThe` phrase starTwo)
  (sup (sub lV (Concat [labelx, label2])) label0) Real velU

yVel_2_0 = quant (mkUid "vy_2_0") (nounPhraseSP "initial y-velocity of the second star")
  (S "initial y-component of the" +:+ phrase QP.velocity `S.ofThe` phrase starTwo)
  (sup (sub lV (Concat [labely, label2])) label0) Real velU

---------------------------------------------------------
-- Acceleration quantities
---------------------------------------------------------

xAccel_1, yAccel_1, xAccel_2, yAccel_2 :: DefinedQuantityDict

xAccel_1 = quant (mkUid "ax_1") (nounPhraseSP "x-acceleration of the first star")
  (S "x-component of the" +:+ phrase QP.acceleration `S.ofThe` phrase starOne)
  (sub lA (Concat [labelx, label1])) Real accelU

yAccel_1 = quant (mkUid "ay_1") (nounPhraseSP "y-acceleration of the first star")
  (S "y-component of the" +:+ phrase QP.acceleration `S.ofThe` phrase starOne)
  (sub lA (Concat [labely, label1])) Real accelU

xAccel_2 = quant (mkUid "ax_2") (nounPhraseSP "x-acceleration of the second star")
  (S "x-component of the" +:+ phrase QP.acceleration `S.ofThe` phrase starTwo)
  (sub lA (Concat [labelx, label2])) Real accelU

yAccel_2 = quant (mkUid "ay_2") (nounPhraseSP "y-acceleration of the second star")
  (S "y-component of the" +:+ phrase QP.acceleration `S.ofThe` phrase starTwo)
  (sub lA (Concat [labely, label2])) Real accelU

---------------------------------------------------------
-- Other quantities
---------------------------------------------------------

tFinal :: DefinedQuantityDict
tFinal = quant (mkUid "t_final") (nounPhraseSP "final time")
  (S "end time of the simulation interval")
  (sub lT (label "final")) Real second

sepDist :: DefinedQuantityDict
sepDist = quant (mkUid "r_12") (nounPhraseSP "separation distance")
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
-- Summation index quantities (for general COM formula)
---------------------------------------------------------

-- | Summation index variable i
index :: DefinedQuantityDict
index = quantNoUnit (mkUid "index") (nounPhraseSP "index")
  (S "a number representing a single body")
  lI Integer

-- | Number of bodies n
numbBodies :: DefinedQuantityDict
numbBodies = quantNoUnit (mkUid "n") (nounPhraseSP "number of bodies")
  (S "the number of point masses in the system")
  lN Integer

---------------------------------------------------------
-- ODE state vector (for code generation)
---------------------------------------------------------

-- | State vector: [x₁, y₁, x₂, y₂, vx₁, vy₁, vx₂, vy₂]
bssStateVar :: ConstrConcept
bssStateVar = cuc' "bssStateVar"
  (nounPhraseSP "dependent variables")
  "column vector of positions and velocities of the two stars"
  lQ metre (Vect Real)
  [] (exactDbl 0)

---------------------------------------------------------
-- Specification Parameters (Table: Specification Parameter Values)
-- These define the software constraint bounds.
---------------------------------------------------------

massMin, massMax, rMax, vMax, tMax :: DefinedQuantityDict

massMin = quant (mkUid "m_min") (nounPhraseSP "minimum stellar mass")
  (S "lower bound for stellar mass; below this threshold objects cannot sustain hydrogen fusion")
  (sub lM (label "min")) Real kilogram

massMax = quant (mkUid "m_max") (nounPhraseSP "maximum stellar mass")
  (S "upper bound for stellar mass; beyond this range radiation effects violate the point-mass assumption")
  (sub lM (label "max")) Real kilogram

rMax = quant (mkUid "r_max") (nounPhraseSP "maximum initial distance from origin")
  (S "upper bound for each star's initial distance from the center of mass")
  (sub lR (label "max")) Real metre

vMax = quant (mkUid "v_max") (nounPhraseSP "maximum initial speed")
  (S "upper bound for initial velocity magnitude; ensures classical mechanics remains valid")
  (sub lV (label "max")) Real velU

tMax = quant (mkUid "t_max") (nounPhraseSP "maximum simulation time")
  (S "upper bound for simulation duration; limits numerical error accumulation over long integrations")
  (sub lT (label "max")) Real second

-- | Spec parameter values
specParamValues :: [ConstQDef]
specParamValues =
  [ mkQuantDef massMin (dbl 1.0e29)   -- approx.0.05 M_sun
  , mkQuantDef massMax (dbl 1.0e32)   -- approx.50 M_sun
  , mkQuantDef rMax    (dbl 1.0e13)   -- approx.67 AU
  , mkQuantDef vMax    (dbl 1.0e6)    -- approx.0.3% c
  , mkQuantDef tMax    (dbl 1.0e10)   -- approx.317 yr
  ]

---------------------------------------------------------
-- Constraints (input validation)
---------------------------------------------------------

-- | Mass constraints: physical (> 0) + software (m_min <= m <= m_max)
massCon_1, massCon_2 :: ConstrConcept
massCon_1 = constrainedWithRationale mass_1
  [gtZeroConstr,
   sfwrRange $ Bounded (Inc, sy massMin) (Inc, sy massMax)]
  (dbl 2.0e30)
  (S "approximately 1 solar mass")

massCon_2 = constrainedWithRationale mass_2
  [gtZeroConstr,
   sfwrRange $ Bounded (Inc, sy massMin) (Inc, sy massMax)]
  (dbl 1.6e30)
  (S "approximately 0.8 solar masses")

-- | Initial position constraints: software (|x| <= r_max)
xPosCon_1_0, yPosCon_1_0, xPosCon_2_0, yPosCon_2_0 :: ConstrConcept
xPosCon_1_0 = constrainedWithRationale xPos_1_0
  [sfwrRange $ Bounded (Inc, neg (sy rMax)) (Inc, sy rMax)]
  (dbl 7.5e10)
  (S "approximately 0.5 AU; satisfies COM constraint with star 2")

yPosCon_1_0 = constrainedWithRationale yPos_1_0
  [sfwrRange $ Bounded (Inc, neg (sy rMax)) (Inc, sy rMax)]
  (dbl 0)
  (S "stars initially on the x-axis")

xPosCon_2_0 = constrainedWithRationale xPos_2_0
  [sfwrRange $ Bounded (Inc, neg (sy rMax)) (Inc, sy rMax)]
  (dbl (-9.375e10))
  (S "derived from COM constraint")

yPosCon_2_0 = constrainedWithRationale yPos_2_0
  [sfwrRange $ Bounded (Inc, neg (sy rMax)) (Inc, sy rMax)]
  (dbl 0)
  (S "stars initially on the x-axis")

-- | Initial velocity constraints: software (|v| <= v_max)
xVelCon_1_0, yVelCon_1_0, xVelCon_2_0, yVelCon_2_0 :: ConstrConcept
xVelCon_1_0 = constrainedWithRationale xVel_1_0
  [sfwrRange $ Bounded (Inc, neg (sy vMax)) (Inc, sy vMax)]
  (dbl 2.0e3)
  (S "non-zero x-velocity produces a general elliptical orbit")

yVelCon_1_0 = constrainedWithRationale yVel_1_0
  [sfwrRange $ Bounded (Inc, neg (sy vMax)) (Inc, sy vMax)]
  (dbl 9.0e3)
  (S "yields a bound orbit for the typical masses and separation")

xVelCon_2_0 = constrainedWithRationale xVel_2_0
  [sfwrRange $ Bounded (Inc, neg (sy vMax)) (Inc, sy vMax)]
  (dbl (-2.5e3))
  (S "derived from COM constraint")

yVelCon_2_0 = constrainedWithRationale yVel_2_0
  [sfwrRange $ Bounded (Inc, neg (sy vMax)) (Inc, sy vMax)]
  (dbl (-1.125e4))
  (S "derived from COM constraint")

-- | Time constraint: physical (>= 0) + software (t <= t_max)
tFinalCon :: ConstrConcept
tFinalCon = constrainedWithRationale tFinal
  [physRange $ UpFrom (Inc, exactDbl 0),
   sfwrRange $ UpTo (Inc, sy tMax)]
  (dbl 1.0e5)
  (S "short integration window")

-- | Input constraints with uncertainty
inConstraints :: [UncertQ]
inConstraints =
  [ uqDirect massCon_1   (uncty 0.05 Nothing)   -- 5%
  , uqDirect massCon_2   (uncty 0.05 Nothing)   -- 5%
  , uqDirect xPosCon_1_0 (uncty 0.01 Nothing)   -- 1%
  , uqDirect yPosCon_1_0 (uncty 0.01 Nothing)   -- 1%
  , uqDirect xPosCon_2_0 (uncty 0.01 Nothing)   -- 1%
  , uqDirect yPosCon_2_0 (uncty 0.01 Nothing)   -- 1%
  , uqDirect xVelCon_1_0 (uncty 0.05 Nothing)   -- 5%
  , uqDirect yVelCon_1_0 (uncty 0.05 Nothing)   -- 5%
  , uqDirect xVelCon_2_0 (uncty 0.05 Nothing)   -- 5%
  , uqDirect yVelCon_2_0 (uncty 0.05 Nothing)   -- 5%
  , uqDirect tFinalCon   exact                   -- no uncertainty
  ]

-- | Output constraints (none; bssStateVar has no physically meaningful bounds)
outConstraints :: [UncertQ]
outConstraints = []
