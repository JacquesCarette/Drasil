module Drasil.BinaryStar.TMods (tMods) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import Theory.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Data.List.NonEmpty as NE

import Data.Drasil.Quantities.Physics (velocity, position, acceleration,
  force, time, gravitationalConst)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Theories.Physics (newtonSL)

import Drasil.BinaryStar.Unitals (mass_1, mass_2, sepDist,
  xPos_1, yPos_1, xPos_2, yPos_2,
  xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0)

tMods :: [TheoryModel]
tMods = [centerOfMassTM, newtonSL, velocityTM, accelTM, gravLawTM, relPosTM]
-- TM4 (Newton's Second Law) is reused from drasil-data as newtonSL.

---------------------------------------------------------
-- TM1: Center-of-Mass Constraint (n=2 specialization)
-- In the COM reference frame (Assumption A7), the initial
-- positions must satisfy:
--   m₁·x₁₀ + m₂·x₂₀ = 0
--   m₁·y₁₀ + m₂·y₂₀ = 0
-- This uses ConstraintSet (like SSP's equilibrium TM)
-- to express relational invariants across multiple variables.
---------------------------------------------------------
centerOfMassTM :: TheoryModel
centerOfMassTM = tmNoRefs (equationalConstraints' centerOfMassCS)
  "centerOfMass" [centerOfMassNote]

centerOfMassRels :: [ModelExpr]
centerOfMassRels =
  [ (sy mass_1 $* sy xPos_1_0) $+ (sy mass_2 $* sy xPos_2_0) $= int 0
  , (sy mass_1 $* sy yPos_1_0) $+ (sy mass_2 $* sy yPos_2_0) $= int 0
  ]

centerOfMassCS :: ConstraintSet ModelExpr
centerOfMassCS = mkConstraintSet
  (dccWDS "centerOfMassCS"
    (nounPhraseSP "center-of-mass constraint")
    centerOfMassNote) $
  NE.fromList centerOfMassRels

centerOfMassNote :: Sentence
centerOfMassNote = foldlSent
  [S "In the center-of-mass reference frame, the initial",
   plural position, S "of the two stars must satisfy",
   S "the constraint that the weighted sum of", plural position,
   S "is zero, where the weights are the", plural mass,
   S "of each star"]

---------------------------------------------------------
-- TM2: Velocity  v(t) = dr(t)/dt
---------------------------------------------------------
velocityTM :: TheoryModel
velocityTM = tmNoRefs (equationalModel' velocityQD)
  "velocity" [velocityNote]

velocityQD :: ModelQDef
velocityQD = mkQuantDef' velocity
  (nounPhraseSP "velocity") velocityExpr

velocityExpr :: ModelExpr
velocityExpr = deriv (sy position) time

velocityNote :: Sentence
velocityNote = foldlSent
  [S "Velocity", ch velocity `S.is`
   S "defined as the time derivative of the",
   phrase position, S "vector"]

---------------------------------------------------------
-- TM3: Acceleration  a(t) = dv(t)/dt
---------------------------------------------------------
accelTM :: TheoryModel
accelTM = tmNoRefs (equationalModel' accelQD)
  "acceleration" [accelNote]

accelQD :: ModelQDef
accelQD = mkQuantDef' acceleration
  (nounPhraseSP "acceleration") accelExpr

accelExpr :: ModelExpr
accelExpr = deriv (sy velocity) time

accelNote :: Sentence
accelNote = foldlSent
  [S "Acceleration", ch acceleration `S.is`
   S "defined as the time derivative of",
   phrase velocity]

---------------------------------------------------------
-- TM5: Newton's Law of Universal Gravitation
-- F₁₂ = -G * m₁ * m₂ / r₁₂² (scalar magnitude form)
---------------------------------------------------------
gravLawTM :: TheoryModel
gravLawTM = tmNoRefs (equationalModel' gravLawQD)
  "UniversalGravLaw" [gravLawNote]

gravLawQD :: ModelQDef
gravLawQD = mkQuantDef' force
  (nounPhraseSP "Newton's law of universal gravitation") gravLawExpr

gravLawExpr :: ModelExpr
gravLawExpr = neg (sy gravitationalConst) $*
  (sy mass_1 $* sy mass_2 $/ square (sy sepDist))

gravLawNote :: Sentence
gravLawNote = foldlSent
  [S "The gravitational", phrase force,
   S "between two bodies is proportional to the product of their",
   plural mass `S.and_`
   S "inversely proportional to the square of the distance between them"]

---------------------------------------------------------
-- TM6: Relative Position and Separation
-- r₁₂ = sqrt((x₁-x₂)² + (y₁-y₂)²)
---------------------------------------------------------
relPosTM :: TheoryModel
relPosTM = tmNoRefs (equationalModel' relPosQD)
  "relPosAndSep" [relPosNote]

relPosQD :: ModelQDef
relPosQD = mkQuantDef' sepDist
  (nounPhraseSP "relative position and separation") relPosExpr

relPosExpr :: ModelExpr
relPosExpr = sqrt (square (sy xPos_1 $- sy xPos_2)
             $+ square (sy yPos_1 $- sy yPos_2))

relPosNote :: Sentence
relPosNote = foldlSent
  [S "The relative", phrase position, S "vector is defined as",
   S "the difference of the two star", plural position,
   S "(i.e., r12 = r1 - r2).", ch sepDist `S.isThe`
   S "corresponding separation distance (the magnitude of the relative",
   phrase position, S "vector), computed from the",
   phrase position, S "coordinates",
   ch xPos_1 `sC` ch yPos_1 `sC` ch xPos_2 `sC` ch yPos_2]
