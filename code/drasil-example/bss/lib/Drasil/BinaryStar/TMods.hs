module Drasil.BinaryStar.TMods (tMods, centerOfMassTM, velocityTM, accelTM,
  gravLawTM, relPosTM) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import Language.Drasil.Document (refS)
import Theory.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Data.List.NonEmpty as NE

import Data.Drasil.Quantities.Physics (position, fOfGravity, gravitationalConst)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Concepts.Math (xDir, yDir)
import Data.Drasil.Theories.Physics (newtonSL, accelerationTM, velocityTM)

import Drasil.BinaryStar.Unitals (mass_1, mass_2, sepDist,
  xPos_1, yPos_1, xPos_2, yPos_2,
  xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0,
  index, numbBodies)
import Drasil.BinaryStar.Assumptions (inertialFrame, newtonianGravity,
  nonzeroSeparation, planar)

-- | TM2 (velocity), TM3 (acceleration), TM4 (Newton's second law)
-- are reused from drasil-data, following the DblPend pattern.
accelTM :: TheoryModel
accelTM = accelerationTM

tMods :: [TheoryModel]
tMods = [centerOfMassTM, newtonSL, velocityTM, accelTM, gravLawTM, relPosTM]

---------------------------------------------------------
-- TM1: Center-of-Mass Constraint (n=2 specialization)
---------------------------------------------------------
centerOfMassTM :: TheoryModel
centerOfMassTM = tmNoRefs (equationalConstraints' centerOfMassCS)
  "centerOfMass" [centerOfMassNote]

centerOfMassRels :: [ModelExpr]
centerOfMassRels =
  [ -- General n-body COM constraint
    defsum (eqSymb index) (int 1) (sy numbBodies)
      (idx (sy mass) (sy index) $* idx (sy position) (sy index)) $= int 0
  , -- n=2 specialization (x-component)
    (sy mass_1 $* sy xPos_1_0) $+ (sy mass_2 $* sy xPos_2_0) $= int 0
  , -- n=2 specialization (y-component)
    (sy mass_1 $* sy yPos_1_0) $+ (sy mass_2 $* sy yPos_2_0) $= int 0
  ]

centerOfMassCS :: ConstraintSet ModelExpr
centerOfMassCS = mkConstraintSet
  (cncpt'' "centerOfMassCS"
    (nounPhraseSP "center-of-mass constraint")
    centerOfMassNote) $
  NE.fromList centerOfMassRels

centerOfMassNote :: Sentence
centerOfMassNote = foldlSent
  [S "The first equation gives the general center-of-mass constraint",
   S "for a system of", ch numbBodies, S "point", plural mass `sC`
   S "where", ch index, S "indexes each body.",
   S "In the center-of-mass reference frame",
   sParen (refS inertialFrame) `sC`
   S "the origin is chosen so that the mass-weighted sum of",
   plural position, S "vanishes.",
   S "The remaining equations specialize to the binary case",
   S "(n = 2), decomposing the vector constraint into",
   phrase xDir `S.and_` phrase yDir,
   S "components of the initial", plural position]

---------------------------------------------------------
-- TM5: Newton's Law of Universal Gravitation
-- F₁₂ = -G * m₁ * m₂ / r₁₂² (scalar magnitude form)
---------------------------------------------------------
gravLawTM :: TheoryModel
gravLawTM = tmNoRefs (equationalModel' gravLawQD)
  "UniversalGravLaw" [gravLawNote]

gravLawQD :: ModelQDef
gravLawQD = mkQuantDef' fOfGravity
  (nounPhraseSP "Newton's law of universal gravitation") gravLawExpr

gravLawExpr :: ModelExpr
gravLawExpr = sy gravitationalConst $*
  (sy mass_1 $* sy mass_2 $/ square (sy sepDist))

gravLawNote :: Sentence
gravLawNote = foldlSent
  [ch fOfGravity `S.isThe`
   S "magnitude of the gravitational force between two bodies;",
   S "it is proportional to the product of their",
   plural mass `S.and_`
   S "inversely proportional to the square of the distance between them.",
   S "The force acts along the relative-position vector",
   sParen (refS relPosTM) `sC`
   S "directed from one body toward the other.",
   S "This assumes Newtonian gravitation",
   sParen (refS newtonianGravity) `S.and_`
   S "requires the separation distance to be positive",
   sParen (refS nonzeroSeparation)]

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
   ch xPos_1 `sC` ch yPos_1 `sC` ch xPos_2 `sC` ch yPos_2 +:+.
   S "",
   S "The motion is confined to a 2D plane",
   sParen (refS planar)]
