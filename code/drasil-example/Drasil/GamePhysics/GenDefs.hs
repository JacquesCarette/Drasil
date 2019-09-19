module Drasil.GamePhysics.GenDefs (generalDefns, accelGravityGD, impulseGD,
 ) where

import Language.Drasil
import Utils.Drasil
--import Data.Drasil.Concepts.Physics as CP (rigidBody, time)
import Theory.Drasil (GenDefn, gd)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration,
 gravitationalAccel, gravitationalConst, restitutionCoef, impulseS, displacement, force, acceleration)
import Drasil.GamePhysics.Unitals (mLarger, dispUnit, massA, massB,
  momtInertA, momtInertB, normalLen, normalVect, perpLenA, perpLenB, initRelVel)
import Drasil.GamePhysics.DataDefs (collisionAssump, rightHandAssump,
  rigidTwoDAssump)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)

----- General Models -----

generalDefns :: [GenDefn]
generalDefns = [accelGravityGD, impulseGD]


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
   [accelGravitySrc] "accelGravity" [accelGravityDesc]
  

accelGravityRC :: RelationConcept
accelGravityRC = makeRC "accelGravityRC" (nounPhraseSP "Acceleration due to gravity") 
  accelGravityDesc accelGravityRel

accelGravityRel :: Relation
accelGravityRel = sy QP.gravitationalAccel $= negate (sy QP.gravitationalConst * sy mLarger/
                  (sy QP.displacement $^ 2) * sy dispUnit)

accelGravitySrc :: Reference
accelGravitySrc = makeURI "accelGravitySrc" "https://en.wikipedia.org/wiki/Gravitational_acceleration" $
  shortname' "Definition of Gravitational Acceleration"

accelGravityDesc :: Sentence
accelGravityDesc = foldlSent [S "If one of the", plural QPP.mass, S "is much larger than the other",
  S "it is convenient to define a gravitational field around the larger mass as shown above.",
  S "The negative sign in the equation indicates that the", phrase QP.force, S "is an attractive",
  phrase QP.force]

----------------------------Impulse for Collision--------------------------------------------

impulseGD :: GenDefn
impulseGD = gd impulseRC (getUnit QP.impulseS) Nothing 
  [impulseSrc] "impulse" [rigidTwoDAssump, rightHandAssump, collisionAssump]

impulseRC :: RelationConcept
impulseRC = makeRC "impulseRC" (nounPhraseSP "Impulse for Collision") 
  impulseDesc impulseRel

impulseRel :: Relation
impulseRel = sy QP.impulseS $= (negate (1 + sy QP.restitutionCoef) * sy initRelVel $.
  sy normalVect) / (((1 / sy massA) + (1 / sy massB)) *
  (sy normalLen $^ 2) +
  ((sy perpLenA $^ 2) / sy momtInertA) +
  ((sy perpLenB $^ 2) / sy momtInertB))

impulseSrc :: Reference
impulseSrc = makeURI "impulseSrc" "http://www.chrishecker.com/images/e/e7/Gdmphys3.pdf" $
  shortname' "Impulse for Collision Ref"

impulseDesc :: Sentence
impulseDesc = foldlSent [S "Impulse for Collision"]
