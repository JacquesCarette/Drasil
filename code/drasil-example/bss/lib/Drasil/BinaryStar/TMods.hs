module Drasil.BinaryStar.TMods (tMods) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import Theory.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Quantities.Physics (velocity, position, acceleration,
  force, time, gravitationalConst)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Theories.Physics (newtonSL)

import Drasil.BinaryStar.Unitals (mass_1, mass_2, sepDist,
  xPos_1, yPos_1, xPos_2, yPos_2)

tMods :: [TheoryModel]
tMods = [newtonSL, velocityTM, accelTM, gravLawTM, relPosTM]
-- TM1 (Center-of-Mass) omitted for now: Drasil doesn't easily support
-- summation notation. Can be added later.
-- TM4 (Newton's Second Law) is reused from drasil-data as newtonSL.

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
  [ch sepDist `S.isThe`
   S "separation distance between the two stars, computed from their",
   phrase position, S "coordinates",
   ch xPos_1 `sC` ch yPos_1 `sC` ch xPos_2 `sC` ch yPos_2]
