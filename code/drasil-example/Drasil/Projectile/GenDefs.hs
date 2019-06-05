module Drasil.Projectile.GenDefs (genDefns) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (vector)
import Data.Drasil.Concepts.Physics (cartesian, twoD)

import Data.Drasil.Quantities.Physics (acceleration, fSpeed, iPos, iSpeed,
  iVel, ixPos, ixVel, iyPos, iyVel, position, time, velocity, xAccel, xConstAccel, xDist,
  xPos, xVel, yAccel, yConstAccel, yPos, yVel)
import qualified Data.Drasil.Quantities.Physics as QP (constAccel)

import Data.Drasil.Utils (weave)

import Drasil.Projectile.Assumptions (accelYGravity, accelXZero, cartSyst,
  constAccel, launchOrigin, pointMass, targetXAxis, twoDMotion)
import Drasil.Projectile.DataDefs (speedY)
import Drasil.Projectile.TMods (accelerationTM, velocityTM)
import Drasil.Projectile.Unitals (launAngle)

genDefns :: [GenDefn]
genDefns = [rectVelGD, rectPosGD, velVecGD, posVecGD, airTimeGD, distanceGD, distanceRefinedGD]

----------
rectVelGD :: GenDefn
rectVelGD = gdNoRefs rectVelRC (getUnit fSpeed) rectVelDeriv "rectVel" [EmptyS]

rectVelRC :: RelationConcept
rectVelRC = makeRC "rectVelRC" (nounPhraseSP "rectilinear velocity as a function of time for constant acceleration")
            EmptyS rectVelRel

rectVelRel :: Relation
rectVelRel = sy fSpeed $= sy iSpeed + sy QP.constAccel * sy time

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
  S "now one-dimensional with a", phrase QP.constAccel `sC` S "represented by" +:+.
  E (sy QP.constAccel), S "The", phrase iVel, sParen (S "at" +:+ E (sy time $= 0)) `sIs`
  S "represented by" +:+. E (sy iVel), S "From", makeRef2S accelerationTM `sC`
  S "using the above", plural symbol_ +: S "we have"]

rectVelDerivSent2 = S "Rearranging" `sAnd` S "integrating" `sC` S "we" +: S "have"
rectVelDerivSent3 = S "Performing the integration" `sC` S "we" +: S "have"

rectVelDerivEqns :: [Expr]
rectVelDerivEqns = [rectVelDerivEqn1, rectVelDerivEqn2, rectVelRel]

rectVelDerivEqn1, rectVelDerivEqn2 :: Expr
rectVelDerivEqn1 = sy QP.constAccel $= deriv (sy velocity) time
rectVelDerivEqn2 = defint (eqSymb velocity) (sy iVel) (sy velocity) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy QP.constAccel)

----------
rectPosGD :: GenDefn
rectPosGD = gdNoRefs rectPosRC (getUnit position) rectPosDeriv "rectPos" [EmptyS]

rectPosRC :: RelationConcept
rectPosRC = makeRC "rectPosRC" (nounPhraseSP "rectilinear position as a function of time for constant acceleration")
            EmptyS rectPosRel

rectPosRel :: Relation
rectPosRel = sy position $= sy iPos + sy iVel * sy time + sy QP.constAccel * square (sy time) / 2

rectPosDeriv :: Derivation
rectPosDeriv = (S "Detailed derivation" `sOf` S "rectilinear" +:+ phrase position :+: S ":") :
               weave [rectPosDerivSents, map E rectPosDerivEqns]

rectPosDerivSents :: [Sentence]
rectPosDerivSents = [rectPosDerivSent1, rectPosDerivSent2, rectPosDerivSent3, rectPosDerivSent4]

rectPosDerivSent1, rectPosDerivSent2, rectPosDerivSent3, rectPosDerivSent4 :: Sentence
rectPosDerivSent1 = foldlSent [
  S "From", makeRef2S velocityTM `sC` S "using the", plural symbol_,
  foldlList Comma List (map (\x -> E (sy x) +:+ S "for" +:+ phrase x)
    [QP.constAccel, iVel, iPos]) +: S "we have"]
rectPosDerivSent2 = S "Rearranging" `sAnd` S "integrating" `sC` S "we" +: S "have"
rectPosDerivSent3 = S "From" +:+ makeRef2S rectVelGD +:+ S "we can replace" +: E (sy velocity)
rectPosDerivSent4 = S "Performing the integration" `sC` S "we" +: S "have"

rectPosDerivEqns :: [Expr]
rectPosDerivEqns = [rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3, rectPosRel]

rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3 :: Expr
rectPosDerivEqn1 = sy velocity $= deriv (sy position) time
rectPosDerivEqn2 = defint (eqSymb position) (sy iPos) (sy position) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy velocity)
rectPosDerivEqn3 = defint (eqSymb position) (sy iPos) (sy position) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy iVel + sy QP.constAccel * sy time)

----------
velVecGD :: GenDefn
velVecGD = gdNoRefs velVecRC (getUnit velocity) velVecDeriv "velVec" [EmptyS]

velVecRC :: RelationConcept
velVecRC = makeRC "velVecRC" (nounPhraseSP "velocity vector as a function of time")
            EmptyS velVecRel

velVecRel :: Relation
velVecRel = sy velocity $= vec2D (sy ixVel + sy xConstAccel * sy time) (sy iyVel + sy yConstAccel * sy time)

velVecDeriv :: Derivation
velVecDeriv = [S "Detailed derivation" `sOf` phrase velocity +:+ phrase vector :+: S ":",
               velVecDerivSent, E velVecDerivEqn]

velVecDerivSent :: Sentence
velVecDerivSent = foldlSent_ [
  S "For a", phrase twoD, phrase cartesian, sParen (makeRef2S twoDMotion `sAnd` makeRef2S cartSyst) `sC`
  S "we can represent the", phrase velocity, phrase vector, S "as",
  E (sy velocity $= vec2D (sy xVel) (sy yVel)) `andThe` phrase acceleration,
  phrase vector, S "as" +:+. E (sy acceleration $= vec2D (sy xAccel) (sy yAccel)),
  S "The", phrase acceleration `sIs` S "assumed to be constant", sParen (makeRef2S constAccel) `andThe`
  phrase QP.constAccel `sIs` S "represented as" +:+. E (sy QP.constAccel $= vec2D (sy xConstAccel) (sy yConstAccel)),
  S "The", phrase iVel, sParen (S "at" +:+ E (sy time $= 0)) `sIs` S "represented by" +:+.
  E (sy iVel $= vec2D (sy ixVel) (sy iyVel)), S "Since we have a", phrase cartesian `sC`
  makeRef2S rectVelGD, S "can be applied to each", phrase coordinate +:
  (S "direction" `sC` S "to yield")]

velVecDerivEqn :: Expr
velVecDerivEqn = sy velocity $= vec2D (sy xVel) (sy yVel) $=
    vec2D (sy ixVel + sy xConstAccel * sy time) (sy iyVel + sy yConstAccel * sy time)

----------
posVecGD :: GenDefn
posVecGD = gdNoRefs posVecRC (getUnit position) posVecDeriv "posVec" [EmptyS]

posVecRC :: RelationConcept
posVecRC = makeRC "posVecRC" (nounPhraseSP "position vector as a function of time")
            EmptyS posVecRel

posVecRel :: Relation
posVecRel = sy position $= vec2D
              (sy ixPos + sy ixVel * sy time + sy xConstAccel * square (sy time) / 2)
              (sy iyPos + sy iyVel * sy time + sy yConstAccel * square (sy time) / 2)

posVecDeriv :: Derivation
posVecDeriv = [S "Detailed derivation" `sOf` phrase position +:+ phrase vector :+: S ":",
               posVecDerivSent, E posVecDerivEqn]

posVecDerivSent :: Sentence
posVecDerivSent = foldlSent_ [
  S "For a", phrase twoD, phrase cartesian, sParen (makeRef2S twoDMotion `sAnd` makeRef2S cartSyst) `sC`
  S "we can represent the", phrase position, phrase vector, S "as",
  E (sy position $= vec2D (sy xPos) (sy yPos)) `sC` S "the", phrase velocity,
  phrase vector, S "as", E (sy velocity $= vec2D (sy xVel) (sy yVel)) `andThe`
  phrase acceleration, phrase vector, S "as" +:+. E (sy acceleration $= vec2D (sy xAccel) (sy yAccel)),
  S "The", phrase acceleration `sIs` S "assumed to be constant", sParen (makeRef2S constAccel) `andThe`
  phrase QP.constAccel `sIs` S "represented as" +:+. E (sy QP.constAccel $= vec2D (sy xConstAccel) (sy yConstAccel)),
  S "The", phrase iVel, sParen (S "at" +:+ E (sy time $= 0)) `sIs`
  S "represented by" +:+. E (sy iVel $= vec2D (sy ixVel) (sy iyVel)), S "Since we have a",
  phrase cartesian `sC` makeRef2S rectPosGD, S "can be applied to each", phrase coordinate +:
  (S "direction" `sC` S "to yield")]

posVecDerivEqn :: Expr
posVecDerivEqn = sy position $= vec2D (sy xPos) (sy xPos) $=
    vec2D (sy ixPos + sy ixVel * sy time + sy xConstAccel * square (sy time) / 2)
          (sy iyPos + sy iyVel * sy time + sy yConstAccel * square (sy time) / 2)

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
