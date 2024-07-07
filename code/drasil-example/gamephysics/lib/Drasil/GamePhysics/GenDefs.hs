{-# LANGUAGE PostfixOperators #-}
module Drasil.GamePhysics.GenDefs (generalDefns, accelGravityGD, impulseGD) where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (GenDefn, gd, equationalModel')
import Utils.Drasil (weave)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration,
 gravitationalAccel, gravitationalConst, restitutionCoef, impulseS, force,
 fOfGravity)
import Drasil.GamePhysics.Unitals (mLarger, dispNorm, dVect, massA, massB,
  momtInertA, momtInertB, normalLen, normalVect, perpLenA, perpLenB, initRelVel,
  mass_1, mass_2, sqrDist, distMass)
import Drasil.GamePhysics.DataDefs (collisionAssump, rightHandAssump,
  rigidTwoDAssump)
import Data.Drasil.Concepts.Math as CM (line, cartesian)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import Drasil.GamePhysics.TMods (newtonLUG)

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
accelGravityGD = gd (equationalModel' accelGravityQD) (getUnit QP.acceleration) (Just accelGravityDeriv)
   [dRef accelGravitySrc] "accelGravity" [accelGravityDesc]

accelGravityQD :: ModelQDef
accelGravityQD = mkQuantDef' QP.gravitationalAccel (nounPhraseSP "Acceleration due to gravity") accelGravityExpr

accelGravityDesc :: Sentence
accelGravityDesc = foldlSent [S "If one of the", plural QPP.mass, S "is much larger than the other" `sC`
  (S "it is convenient to define a gravitational field around the larger mass as shown above" !.),
  S "The negative sign in the equation indicates that the", phrase QP.force, S "is an attractive",
  phrase QP.force]

accelGravityExpr :: PExpr
accelGravityExpr = neg ((sy QP.gravitationalConst $* sy mLarger $/
  square (sy dispNorm)) $* sy dVect)

accelGravitySrc :: Reference
accelGravitySrc = makeURI "accelGravitySrc" "https://en.wikipedia.org/wiki/Gravitational_acceleration" $
  shortname' $ S "Definition of Gravitational Acceleration"

accelGravityDeriv :: Derivation
accelGravityDeriv = mkDerivName (phrase QP.gravitationalAccel)
                      (weave [accelGravityDerivSentences, map eS accelGravityDerivEqns])

accelGravityDerivSentences :: [Sentence]
accelGravityDerivSentences = map foldlSentCol [accelGravityDerivSentence1,
 accelGravityDerivSentence2, accelGravityDerivSentence3, accelGravityDerivSentence4,
 accelGravityDerivSentence5]

accelGravityDerivSentence1 :: [Sentence]
accelGravityDerivSentence1 = [S "From", namedRef newtonLUG (S "Newton's law of universal gravitation") `sC` S "we have"]


accelGravityDerivSentence2 :: [Sentence]
accelGravityDerivSentence2 = [(S "The above equation governs the gravitational attraction between two bodies" !.),
        S "Suppose that one of the bodies is significantly more massive than the other" `sC`
        S "so that we concern ourselves with the", phrase QP.force,
        S "the massive body",
        (S "exerts on the lighter body" !.), S "Further" `sC` S "suppose that the", phrase cartesian `S.is`
        S "chosen such that this", phrase QP.force, S "acts on a", phrase line,
        (S "which lies along one of the principal axes" !.),
        S "Then our", getTandS dVect, S "for the x or y axes is"]

accelGravityDerivSentence3 :: [Sentence]
accelGravityDerivSentence3 = [S "Given the above assumptions" `sC` S "let", ch mLarger `S.and_` ch QPP.mass,
        S "be", phrase QPP.mass `S.the_ofThe` (S "massive and light body respectively" !.),
        S "Equating", ch QP.force, S "above with Newton's second law",
        S "for the", phrase QP.force, S "experienced by the light body" `sC` S "we get"]

accelGravityDerivSentence4 :: [Sentence]
accelGravityDerivSentence4 = [S "where", (ch QP.gravitationalAccel `S.isThe` phrase QP.gravitationalAccel !.),
        S "Dividing the above equation by", ch QPP.mass `sC` S " we have"]

accelGravityDerivSentence5 :: [Sentence]
accelGravityDerivSentence5 = [S "and thus the negative sign indicates that the", phrase QP.force `S.is`
                               S "an attractive", phrase QP.force]

accelGravityDerivEqn1 :: PExpr
accelGravityDerivEqn1 = sy QP.force $= (sy QP.gravitationalConst $* (sy mass_1 $* sy mass_2) $/
                        sy sqrDist) $* sy dVect

accelGravityDerivEqn2 :: PExpr
accelGravityDerivEqn2 = sy dVect $= (sy distMass $/ sy dispNorm)

accelGravityDerivEqn3 :: PExpr
accelGravityDerivEqn3 = sy QP.fOfGravity $= sy QP.gravitationalConst $* 
                         (sy mLarger $* sy QPP.mass $/ sy sqrDist) $* sy dVect
                         $= sy QPP.mass $* sy QP.gravitationalAccel

accelGravityDerivEqn4 :: PExpr
accelGravityDerivEqn4 = sy QP.gravitationalConst $* (sy mLarger $/ sy sqrDist) $* sy dVect $= sy QP.gravitationalAccel

accelGravityDerivEqn5 :: PExpr
accelGravityDerivEqn5 = sy QP.gravitationalAccel $= neg (sy QP.gravitationalConst $* (sy mLarger $/ sy sqrDist)) $* sy dVect

accelGravityDerivEqns :: (ExprC r, LiteralC r) => [r]
accelGravityDerivEqns = [accelGravityDerivEqn1, accelGravityDerivEqn2, accelGravityDerivEqn3,
                         accelGravityDerivEqn4, accelGravityDerivEqn5]



----------------------------Impulse for Collision--------------------------------------------

impulseGD :: GenDefn
impulseGD = gd (equationalModel' impulseQD) (getUnit QP.impulseS) Nothing
  [dRef impulseSrc] "impulse" [rigidTwoDAssump, rightHandAssump, collisionAssump]

impulseQD :: ModelQDef
impulseQD = mkQuantDef' QP.impulseS (nounPhraseSP "Impulse for Collision") impulseExpr

impulseExpr :: PExpr
impulseExpr = (neg (exactDbl 1 $+ sy QP.restitutionCoef) $* sy initRelVel $.
  sy normalVect) $/ ((recip_ (sy massA) $+ recip_ (sy massB)) $* 
  square (sy normalLen) $+
  (square (sy perpLenA) $/ sy momtInertA) $+
  (square (sy perpLenB) $/ sy momtInertB))

impulseSrc :: Reference
impulseSrc = makeURI "impulseSrc" "http://www.chrishecker.com/images/e/e7/Gdmphys3.pdf" $
  shortname' $ S "Impulse for Collision Ref"
