module Drasil.Projectile.GenDefs (genDefns) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (symbol_)
import Data.Drasil.Quantities.Physics (acceleration, constAccel, fSpeed,
  iSpeed, iVel, ixVel, time, velocity, xAccel, xDist, yAccel, yVel)

import Data.Drasil.Utils (weave)

import Drasil.Projectile.Assumptions (accelYGravity, accelXZero, launchOrigin,
  pointMass, targetXAxis)
import Drasil.Projectile.DataDefs (speedY)
import Drasil.Projectile.TMods (accelerationTM)
import Drasil.Projectile.Unitals (launAngle)

genDefns :: [GenDefn]
genDefns = [rectVelGD, airTimeGD, distanceGD, distanceRefinedGD]

----------
rectVelGD :: GenDefn
rectVelGD = gdNoRefs rectVelRC (getUnit fSpeed) rectVelDeriv "rectVel" [EmptyS]

rectVelRC :: RelationConcept
rectVelRC = makeRC "rectVelRC" (nounPhraseSP "rectilinear velocity as a function of time for constant acceleration")
            EmptyS rectVelRel

rectVelRel :: Relation
rectVelRel = sy fSpeed $= sy iSpeed + sy constAccel * sy time

rectVelDeriv :: Derivation
rectVelDeriv = (S "Detailed derivation" `sOf` S "rectilinear" +:+ phrase velocity :+: S ":") :
               weave [rectVelDerivSents, map E rectVelDerivEqns]

rectVelDerivSents :: [Sentence]
rectVelDerivSents = [rectVelDerivSent1, rectVelDerivSent2, rectVelDerivSent3]

rectVelDerivSent1, rectVelDerivSent2, rectVelDerivSent3 :: Sentence
rectVelDerivSent1 = foldlSent_ [
  S "Assume we have rectilinear motion" `sOf` S "a particle",
  sParen (S "of negligible size" `sAnd` S "shape" +:+ makeRef2S pointMass) :+:
  S ";" +:+. (S "that is" `sC` S "motion" `sIn` S "a straight line"), S "The" +:+.
  (phrase velocity `sIs` E (sy velocity) `andThe` phrase acceleration `sIs`
  E (sy acceleration)), S "The motion" `sIn` makeRef2S accelerationTM `sIs`
  S "now one-dimensional with a", phrase constAccel `sC` S "represented by" +:+.
  E (sy constAccel), S "The", phrase iVel, sParen (S "at" +:+ E (sy time $= 0)) `sIs`
  S "represented by" +:+. E (sy iVel), S "From", makeRef2S accelerationTM `sC`
  S "using the above", plural symbol_ +: S "we have"]

rectVelDerivSent2 = S "Rearranging" `sAnd` S "integrating" `sC` S "we" +: S "have"
rectVelDerivSent3 = S "Performing the integration" `sC` S "we" +: S "have"

rectVelDerivEqns :: [Expr]
rectVelDerivEqns = [rectVelDerivEqn1, rectVelDerivEqn2, rectVelRel]

rectVelDerivEqn1, rectVelDerivEqn2 :: Expr
rectVelDerivEqn1 = sy constAccel $= deriv (sy velocity) time
rectVelDerivEqn2 = defint (eqSymb velocity) (sy iVel) (sy velocity) 1 $= defint (eqSymb time) 0 (sy time) (sy constAccel)

----------
airTimeGD :: GenDefn
airTimeGD = gdNoRefs airTimeRC (getUnit time) [airTimeDeriv] "airTime" [EmptyS]

airTimeRC :: RelationConcept
airTimeRC = makeRC "airTimeR" (nounPhraseSP "air time") EmptyS airTimeRel

airTimeRel :: Relation
airTimeRel = sy time $= BinaryOp Frac (2 * sy iSpeed * sin (sy launAngle)) (sy yAccel)

airTimeDeriv :: Sentence
airTimeDeriv = foldlSent [at_start airTimeGD `sIs` S "derived from" +:+.
  makeRef2S speedY `sAnd` makeRef2S rectVelGD, S "It also comes from the",
  S "fact that the", phrase yVel, S "at the maximum height is zero" `sAnd`
  S "that the maximum height" `sIs` S "halfway point" `ofThe` S "trajectory",
  sParen (S "from" +:+ makeRef2S launchOrigin `sAnd` makeRef2S targetXAxis)]

----------
distanceGD :: GenDefn
distanceGD = gdNoRefs distanceRC (getUnit xDist) [{-Derivation-}] "distance" [makeRef2S accelYGravity]

distanceRC :: RelationConcept
distanceRC = makeRC "distanceRC" (nounPhraseSP "distance in the x-direction") EmptyS distanceRel

distanceRel :: Relation
distanceRel = sy xDist $= sy ixVel * sy time + BinaryOp Frac (sy xAccel * square (sy time)) 2

----------
distanceRefinedGD :: GenDefn
distanceRefinedGD = gdNoRefs distanceRefinedRC (getUnit xDist) [distanceRefinedDeriv] "distanceRefined" [makeRef2S accelXZero]

distanceRefinedRC :: RelationConcept
distanceRefinedRC = makeRC "distanceRefinedRC" (nounPhraseSP "distance in the x-direction (refined)") EmptyS distanceRefinedRel

distanceRefinedRel :: Relation
distanceRefinedRel = sy xDist $= BinaryOp Frac (2 * square (sy iSpeed) * sin (sy launAngle) *
                      cos (sy launAngle)) (sy yAccel)

distanceRefinedDeriv :: Sentence
distanceRefinedDeriv = foldlSent [at_start distanceRefinedGD, S "is derived from",
  foldlList Comma List $ map makeRef2S [airTimeGD, distanceGD]]
