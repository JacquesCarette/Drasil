-- %%%%%%%%%%%%%%%%%%%%%%%%%%
-- WARNING
-- File not actually used in drasil.cabal for GamePhysics
-- so it is not actually 'plugged in'.  The definitions in here
-- may not type check anymore!
-- %%%%%%%%%%%%%%%%%%%%%%%%%%
module Drasil.GamePhysics.GDefs (cpGDefs) where

import Language.Drasil
import Data.Drasil.Concepts.Physics (rigidBody)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Utils (foldlSent)

----- General Models -----

cpGDefs :: [RelationConcept]
--cpGDefs = []

impulseGDef :: RelationConcept
impulseGDef = makeRC "impulse" (nounPhraseSP "Impulse") 
  impulseDesc impulseRel

impulseRel :: Relation
impulseRel = (C impulseS) := (Integral C force) -- replace with proper Expr

impulseDesc :: Sentence
impulseDesc = foldlSent [S "An", (phrase impulseS), (getS impulseS), 
  S "occurs when a", (phrase force), (getS force), 
  S "acts over an interval of", (phrase time)]

--[impulseS, force, changeInMomentum, mass, changeInVelocity]

impulseDeriv :: Sentence
impulseDeriv = foldlSent [S "Newton's second law of motion (ref to T1)", 
  S "states" +: S "(expr1)", 
  S "rearranging" +: S "(expr2)", 
  S "Integrating the right side" +: S "(expr3)"
  ]

conservationOfMomentGDef :: RelationConcept
conservationOfMomentGDef = makeRC "conservOfMoment" (nounPhraseSP "Conservation of Momentum") 
  conservationOfMomentDesc conservationOfMomentRel

conservationOfMomentRel :: Relation
conservationOfMomentRel = UnaryOp $ Summation Nothing
  C massI

conservationOfMomentDesc :: Sentence
conservationOfMomentDesc = foldlSent [S "In an isolated system,",
  S "where the sum of external", (phrase impulseS), S "acting on the system is zero,",
  S "the total momentum of the bodies is constant (conserved)"
  ]

--[mass, initialVelocity, finalVelocity]

conservationOfMomentDeriv :: Sentence
conservationOfMomentDeriv = foldlSent [S "When bodies collide, they exert",
  S "an equal (force) on each other in opposite directions" +:+.
  S "This is Newton's third law:",
  S "(expr1)",
  S "The objects collide with each other for the exact same amount of", 
  (phrase time), (getS time),
  S "The above equation is equal to the", (phrase impulseS), 
  S "(GD1 ref)",
  S "(expr2)",
  S "The", (phrase impulseS), S "is equal to the change in momentum:",
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

-- [gravitationalAccel, mass, gravitationalConst]

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
  S "Given the above assumptions, let M and m be the", (phrase mass), 
  S "of the massive and",
  S "light body, respectively" +:+. S "Using 3 **ref to 3** and equating this",
  S "with Newton's second law (T1 ref) for the force experienced by the light",
  S "body, we get:",
  S "(expr3)",
  S "where", (getS gravitationalConst), S "is", (phrase gravitationalAccel) 
  S "Dividing 4 **ref to 4**",
  S "by m, and resolving this into separate x and y components:",
  S "(expr4)",
  S "(expr5)",
  S "Thus:",
  S "(expr6)"
  ]


relativeVelocityInCollisionsGDef :: RelationConcept
relativeVelocityInCollisionsGDef = makeRC "relVeloInColl"
  (nounPhraseSP "Relative velocity in collision")
  relativeVelocityInCollisionsDesc relativeVelocityInCollisionsRel

relativeVelocityInCollisionsDesc :: Sentence
relativeVelocityInCollisionsDesc = foldlSent [S "In a collision, the",
  (phrase velocity), S "of", S "rigid body A", 
  S "colliding with another body B relative to that",
  S "body, (symbol vAB), is the difference between the", (plural velocity), 
  S "of A", S "and B at point P"
  ]

--[velocityAB, collisionPoint, velocityAP, velocityBP]

relativeVelocityInCollisionsRel :: Relation
relativeVelocityInCollisionsRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr

coefficientOfRestitutionGDef :: RelationConcept
coefficientOfRestitutionGDef = makeRC "coeffOfRest" 
  (nounPhraseSP "Coefficient of restitution")
  coefficientOfRestitutionDesc coefficientOfRestitutionRel

coefficientOfRestitutionDesc :: Sentence
coefficientOfRestitutionDesc = foldlSent [S "The", (phrase restitutionCoef), 
  (getS restitutionCoef), S "is",
  S "a unitless, dimensionless quantity that determines the elasticity of a",
  S "collision between two bodies. (symbol/expr)[CR = 1] results in an elastic",
  S "collision, while (symbol/expr)[CR < 1] results in an inelastic collision,",
  S "and (symbol/expr)[CR = 0] results in a totally inelastic collision"
  ]

--[restitutionCoef, normCollisionVect, initRelativeVelocityAB, finalRelativeVelocityAB]

coefficientOfRestitutionRel :: Relation
coefficientOfRestitutionRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr


torqueGDef :: RelationConcept
torqueGDef = makeRC "torque"
  (nounPhraseSP "Torque")
  torqueDesc torqueRel

torqueDesc :: Sentence
torqueDesc = foldlSent [S "The", (phrase torque), (getS torque), 
  S "on a body measures the", S "the tendency of a", (phrase force), 
  S "to rotate the body around an axis or pivot"
  ]

--[torque, force, position]

torqueRel :: Relation
torqueRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr

momentOfInertiaGDef :: RelationConcept
momentOfInertiaGDef = makeRC "momentOfInertia"
  (nounPhraseSP "Moment of Inertia")
  momentOfInertiaDesc momentOfInertiaRel

momentOfInertiaDesc :: Sentence
momentOfInertiaDesc = foldlSent []


--[momentOfInertia, numOfParticles, massI, distanceBtwParticleI]

momentOfInertiaRel :: Relation
momentOfInertiaRel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time] -- replace with proper Expr
