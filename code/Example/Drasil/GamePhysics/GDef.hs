module Drasil.GamePhysics.GDefs where

import Drasil.GamePhysics.Unitals

import Language.Drasil
import Data.Drasil.Utils (foldlSent)
import Data.Drasil.Concepts.Physics (rigidBody)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Prelude hiding (id)
import Control.Lens ((^.))

----- General Models -----

cpGDefs :: [RelationConcept]
--cpGDefs = []

impulseGDef :: RelationConcept
impulseGDef = makeRC "impulse" (nounPhraseSP "Impulse") 
  impulseDesc impulseRel

impulseRel :: Relation
impulseRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr

impulseDesc :: Sentence
impulseDesc = foldlSent [S "An (impulse) (symbol) occurs when a (force)",
  S "(symbol) acts over an interval of time."]

--[impulse, force, changeInMomentum, mass, changeInVelocity]

impulseDeriv :: Sentence
impulseDeriv = foldlSent [S "Newton's second law of motion (ref to T1)", 
  S "states" +: S "(expr1)", 
  S "rearranging" +: S "(expr2)", 
  S "Integrating the right side" +: S "(expr3)",
  ]

conservationOfMomentGDef :: RelationConcept
conservationOfMomentGDef = makeRC "conservOfMoment" (nounPhraseSP "Conservation of Momentum") 
  conservationOfMomentDesc conservationOfMomentRel

conservationOfMomentRel :: Relation
conservationOfMomentRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr

conservationOfMomentDesc :: Sentence
conservationOfMomentDesc = foldlSent [S "In an isolated system,",
  S "where the sum of external (impulses) acting on the system is zero,",
  S "the total momentum of the bodies is constant (conserved).",
  ]

--[mass, initialVelocity, finalVelocity]

conservationOfMomentDeriv :: Sentence
conservationOfMomentDeriv = foldlSent [S "When bodies collide, they exert",
  S "an equal (force) on each other in opposite directions" +:+.
  S "This is Newton's third law:",
  S "(expr1)",
  S "The objects collide with each other for the exact same amount of (time) (symbol)",
  S "The above equation is equal to the (impulse) (GD1 ref)",
  S "(expr2)",
  S "The (impulse) is equal to the change in momentum:",
  S "(expr3)",
  S "Substituting 2*ref to 2* into 1*ref to 1* yields:",
  S "(expr4)",
  S "Expanding and rearranging the above formula gives",
  S "(expr5)",
  S "Generalizing for multiple (k) colliding objects:",
  S "(expr6)"
  ]


accelerationDueToGravityGDef :: RelationConcept
accelerationDueToGravityGDef = makeRC "accelDueToGrav" 
  (nounPhraseSP "Acceleration due to gravity") 
  accelerationDueToGravityDesc accelerationDueToGravityRel

accelerationDueToGravityRel :: Relation
accelerationDueToGravityRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr

accelerationDueToGravityDesc :: Sentence
accelerationDueToGravityDesc = foldlSent [S ""]

-- [forceDueToGravity, mass, gravityConst]

accelerationDueToGravityDeriv :: Sentence
accelerationDueToGravityDeriv = foldlSent [S "From Newton's law of universal",
  S "gravitation (T3 ref), we have:",
  S "(expr1)",
  S "Equation 3 **ref to 3** governs the gravitational attraction between two",
  S "bodies. Suppose that one of the bodies is significantly more massive than",
  S "other, so that we concern ourselves with the force the massive body exerts",
  S "on the lighter body" +:+. S "Further suppose that the coordinate system is",
  S "chosen such that this force acts on a line which lies along one of the",
  S "principle axes (A2 ref)" +:+. S "Then our unit vector", S "(expr2)", S "for",
  S "the x or y axes (A3 ref), respectively"
  S "Given the above assumptions, let M and m be the mass of the massive and",
  S "light body, respectively" +:+. S "Using 3 **ref to 3** and equating this",
  S "with Newton's second law (T1 ref) for the force experienced by the light",
  S "body, we get:",
  S "(expr3)",
  S "where (gravity symbol) is (phrase gravity accel). Dividing 4 **ref to 4**",
  S "by m, and resolving this into separate x and y components:",
  S "(expr4)",
  S "(expr5)",
  S "Thus:",
  S "(expr6)"
  ]