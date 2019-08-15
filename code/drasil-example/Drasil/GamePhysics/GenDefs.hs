module Drasil.GamePhysics.GenDefs (generalDefns, accelGravityGD) where

import Language.Drasil
import Utils.Drasil
--import Data.Drasil.Concepts.Physics as CP (rigidBody, time)
import Theory.Drasil (GenDefn, gd)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration,
 gravitationalAccel, gravitationalConst)
import Drasil.GamePhysics.Unitals (mLarger, dispNorm, dispUnit)

----- General Models -----

generalDefns :: [GenDefn]
generalDefns = [accelGravityGD]


{-conservationOfMomentGDef :: RelationConcept
conservationOfMomentGDef = makeRC "conservOfMoment" (nounPhraseSP "Conservation of Momentum") 
  conservationOfMomentDesc conservationOfMomentRel

conservationOfMomentRel :: Relation
conservationOfMomentRel = UnaryOp $ Summation Nothing
  C massI

conservationOfMomentDesc :: Sentence
conservationOfMomentDesc = foldlSent [S "In an isolated system,",
  S "where the sum of external", phrase impulseS, S "acting on the system is zero,",
  S "the total momentum of the bodies is constant (conserved)"
  ]

--[mass, initialVelocity, finalVelocity]

conservationOfMomentDeriv :: Sentence
conservationOfMomentDeriv = foldlSent [S "When bodies collide, they exert",
  S "an equal (force) on each other in opposite directions" +:+.
  S "This is Newton's third law:",
  S "(expr1)",
  S "The objects collide with each other for the exact same amount of", 
  phrase time, getS time,
  S "The above equation is equal to the", phrase impulseS, 
  S "(GD1 ref)",
  S "(expr2)",
  S "The", phrase impulseS, S "is equal to the change in momentum:",
  S "(expr3)",
  S "Substituting 2*ref to 2* into 1*ref to 1* yields:",
  S "(expr4)",
  S "Expanding and rearranging the above formula gives",
  S "(expr5)",
  S "Generalizing for multiple (k) colliding objects:",
  S "(expr6)"
  ]
-}


--------------------------Acceleration due to gravity----------------------------
accelGravityGD :: GenDefn
accelGravityGD = gd accelGravityRC (getUnit QP.acceleration) Nothing 
   [accelGravitySrc] "accelGravity" [{-Notes-}]
  

accelGravityRC :: RelationConcept
accelGravityRC = makeRC "accelGravityRC" (nounPhraseSP "Acceleration due to gravity") 
  accelGravityDesc accelGravityRel

accelGravityRel :: Relation
accelGravityRel = sy QP.gravitationalAccel $=  sy QP.gravitationalConst * sy mLarger/
                  (sy dispNorm $^ 2) * sy dispUnit

accelGravitySrc :: Reference
accelGravitySrc = makeURI "accelGravitySrc" "https://en.wikipedia.org/wiki/Gravitational_acceleration" $
  shortname' "Definition of Gravitational Acceleration"

accelGravityDesc :: Sentence
accelGravityDesc = foldlSent [S "Acceleration due to gravity"]

-- [gravitationalAccel, mass, gravitationalConst]

-- accelerationDueToGravityDeriv :: Sentence
-- accelerationDueToGravityDeriv = foldlSent [S "From Newton's law of universal",
--   S "gravitation (T3 ref), we have:",
--   S "(expr1)",
--   S "Equation 3 **ref to 3** governs the gravitational attraction between two",
--   S "bodies. Suppose that one of the bodies is significantly more massive than",
--   S "other, so that we concern ourselves with the force the massive body exerts",
--   S "on the lighter body" +:+. S "Further suppose that the coordinate system is",
--   S "chosen such that this force acts on a line which lies along one of the",
--   S "principle axes (A2 ref)" +:+. S "Then our unit vector", S "(expr2)", S "for",
--   S "the x or y axes (A3 ref), respectively"
--   S "Given the above assumptions, let M and m be the", phrase mass, 
--   S "of the massive and",
--   S "light body, respectively" +:+. S "Using 3 **ref to 3** and equating this",
--   S "with Newton's second law (T1 ref) for the force experienced by the light",
--   S "body, we get:",
--   S "(expr3)",
--   S "where", getS gravitationalConst, S "is", phrase gravitationalAccel,
--   S "Dividing 4 **ref to 4**",
--   S "by m, and resolving this into separate x and y components:",
--   S "(expr4)",
--   S "(expr5)",
--   S "Thus:",
--   S "(expr6)"
--   ]

