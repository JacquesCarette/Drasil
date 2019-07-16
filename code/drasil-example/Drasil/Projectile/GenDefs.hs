module Drasil.Projectile.GenDefs (genDefns, posVecGD) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, TheoryModel, gd, gdNoRefs)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (oneD, rectilinear, twoD)

import Data.Drasil.Quantities.Physics (acceleration, constAccelV, iPos, iSpeed,
  iVel, ixPos, ixVel, iyPos, iyVel, position, scalarAccel, scalarPos, speed,
  time, velocity, xAccel, xConstAccel, xPos, xVel, yAccel, yConstAccel, yPos, yVel)
import qualified Data.Drasil.Quantities.Physics as QP (constAccel)

import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import Drasil.Projectile.References (hibbeler2004)
import Drasil.Projectile.TMods (accelerationTM, velocityTM)

genDefns :: [GenDefn]
genDefns = [rectVelGD, rectPosGD, velVecGD, posVecGD]

----------
rectVelGD :: GenDefn
rectVelGD = gd rectVelRC (getUnit speed) (Just rectVelDeriv)
  [makeCiteInfo hibbeler2004 $ Page [8]] "rectVel" [{-Notes-}]

rectVelRC :: RelationConcept
rectVelRC = makeRC "rectVelRC" (nounPhraseSent $ foldlSent_ 
            [atStart rectilinear, sParen $ getAcc oneD, phrase velocity,
             S "as a function" `sOf` phrase time, S "for", phrase QP.constAccel])
            EmptyS rectVelRel

rectVelRel :: Relation
rectVelRel = sy speed $= sy iSpeed + sy QP.constAccel * sy time

rectVelDeriv :: Derivation
rectVelDeriv = mkDerivName (phrase rectilinear +:+ phrase velocity)
               (weave [rectVelDerivSents, map E rectVelDerivEqns])

rectVelDerivSents :: [Sentence]
rectVelDerivSents = [rectDeriv velocity acceleration motSent iVel accelerationTM, rearrAndIntSent, performIntSent]
  where
    motSent = foldlSent [S "The motion" `sIn` makeRef2S accelerationTM `sIs` S "now", phrase oneD,
                         S "with a", phrase QP.constAccel `sC` S "represented by", E (sy QP.constAccel)]

rectVelDerivEqns :: [Expr]
rectVelDerivEqns = [rectVelDerivEqn1, rectVelDerivEqn2, rectVelRel]

rectVelDerivEqn1, rectVelDerivEqn2 :: Expr
rectVelDerivEqn1 = sy QP.constAccel $= deriv (sy speed) time
rectVelDerivEqn2 = defint (eqSymb speed) (sy iSpeed) (sy speed) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy QP.constAccel)

----------
rectPosGD :: GenDefn
rectPosGD = gd rectPosRC (getUnit scalarPos) (Just rectPosDeriv)
  [makeCiteInfo hibbeler2004 $ Page [8]] "rectPos" [{-Notes-}]

rectPosRC :: RelationConcept
rectPosRC = makeRC "rectPosRC" (nounPhraseSent $ foldlSent_ 
            [atStart rectilinear, sParen $ getAcc oneD, phrase position,
             S "as a function" `sOf` phrase time, S "for", phrase QP.constAccel])
            EmptyS rectPosRel

rectPosRel :: Relation
rectPosRel = sy scalarPos $= sy iPos + sy iSpeed * sy time + sy QP.constAccel * square (sy time) / 2

rectPosDeriv :: Derivation
rectPosDeriv = mkDerivName (phrase rectilinear +:+ phrase position)
               (weave [rectPosDerivSents, map E rectPosDerivEqns])

rectPosDerivSents :: [Sentence]
rectPosDerivSents = [rectDeriv position velocity motSent iPos velocityTM,
  rearrAndIntSent, fromReplace rectVelGD speed, performIntSent]
    where
      motSent = S "The motion" `sIn` makeRef2S velocityTM `sIs` S "now" +:+. phrase oneD

rectPosDerivEqns :: [Expr]
rectPosDerivEqns = [rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3, rectPosRel]

rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3 :: Expr
rectPosDerivEqn1 = sy speed $= deriv (sy scalarPos) time
rectPosDerivEqn2 = defint (eqSymb scalarPos) (sy iPos) (sy scalarPos) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy speed)
rectPosDerivEqn3 = defint (eqSymb scalarPos) (sy iPos) (sy scalarPos) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy iSpeed + sy QP.constAccel * sy time)

----------
velVecGD :: GenDefn
velVecGD = gdNoRefs velVecRC (getUnit velocity)
           (Just velVecDeriv) "velVec" [{-Notes-}]

velVecRC :: RelationConcept
velVecRC = makeRC "velVecRC" (nounPhraseSent $ foldlSent_ 
           [atStart velocity, S "vector as a function" `sOf` phrase time, S "for",
            getAcc twoD, S "motion under", phrase QP.constAccel])
           EmptyS velVecRel

velVecRel :: Relation
velVecRel = sy velocity $= vec2D (sy ixVel + sy xConstAccel * sy time) (sy iyVel + sy yConstAccel * sy time)

velVecDeriv :: Derivation
velVecDeriv = mkDerivName (phrase velocity +:+ phrase vector) [velVecDerivSent, E velVecRel]

velVecDerivSent :: Sentence
velVecDerivSent = vecDeriv [(velocity, velocityXY), (acceleration, accelerationXY)] rectVelGD

----------
posVecGD :: GenDefn
posVecGD = gdNoRefs posVecRC (getUnit position) 
           (Just posVecDeriv) "posVec" [{-Notes-}]

posVecRC :: RelationConcept
posVecRC = makeRC "posVecRC" (nounPhraseSent $ foldlSent_ 
           [atStart position, S "vector as a function" `sOf` phrase time, S "for",
            getAcc twoD, S "motion under", phrase QP.constAccel])
           EmptyS posVecRel

posVecRel :: Relation
posVecRel = sy position $= vec2D
              (sy ixPos + sy ixVel * sy time + sy xConstAccel * square (sy time) / 2)
              (sy iyPos + sy iyVel * sy time + sy yConstAccel * square (sy time) / 2)

posVecDeriv :: Derivation
posVecDeriv = mkDerivName (phrase position +:+ phrase vector) [posVecDerivSent, E posVecRel]

posVecDerivSent :: Sentence
posVecDerivSent = vecDeriv [(position, positionXY), (velocity, velocityXY), (acceleration, accelerationXY)] rectPosGD

-- Helper for making rectilinear derivations
rectDeriv :: UnitalChunk -> UnitalChunk -> Sentence -> UnitalChunk -> TheoryModel -> Sentence
rectDeriv c1 c2 motSent initc ctm = foldlSent_ [
  S "Assume we have", phrase rectilinear, S "motion" `sOf` S "a particle",
  sParen (S "of negligible size" `sAnd` S "shape" `sC` S "from" +:+ makeRef2S pointMass) :+:
  S ";" +:+. (S "that is" `sC` S "motion" `sIn` S "a straight line"), S "The" +:+.
  (phrase c1 `sIs` getScalar c1 `andThe` phrase c2 `sIs` getScalar c2), motSent,
  S "The", phrase initc, sParen (S "at" +:+ E (sy time $= 0) `sC` S "from" +:+
  makeRef2S timeStartZero) `sIs` S "represented by" +:+. getScalar initc,
  S "From", makeRef2S ctm `sC` S "using the above", plural symbol_ +: S "we have"]
  where
    getScalar c
      | c == position     = E (sy scalarPos)
      | c == velocity     = E (sy speed)
      | c == acceleration = E (sy scalarAccel)
      | c == iPos         = E (sy iPos)
      | c == iVel         = E (sy iSpeed)
      | otherwise         = error "Not implemented in getScalar"

rearrAndIntSent, performIntSent :: Sentence
rearrAndIntSent   = S "Rearranging" `sAnd` S "integrating" `sC` S "we" +: S "have"
performIntSent    = S "Performing the integration" `sC` S "we have the required" +: phrase equation

-- Helper for making vector derivations
vecDeriv :: [(UnitalChunk, Expr)] -> GenDefn -> Sentence
vecDeriv vecs gdef = foldlSentCol [
  S "For a", phrase twoD, phrase cartesian, sParen (makeRef2S twoDMotion `sAnd` makeRef2S cartSyst) `sC`
  S "we can represent" +:+. foldlList Comma List 
  (map (\(c, e) -> foldlSent_ [S "the", phrase c, phrase vector, S "as", E e]) vecs),
  S "The", phrase acceleration `sIs` S "assumed to be constant",sParen (makeRef2S constAccel) `andThe`
  phrase constAccelV `sIs` S "represented as" +:+. E constAccelXY, S "The",
  phrase iVel, sParen (S "at" +:+ E (sy time $= 0) `sC` S "from" +:+ makeRef2S timeStartZero) `sIs`
  S "represented by" +:+. E (sy iVel $= vec2D (sy ixVel) (sy iyVel)), S "Since we have a",
  phrase cartesian `sC` makeRef2S gdef, S "can be applied to each", phrase coordinate `sOf`
  S "the", phrase ((fst . head) vecs), phrase vector, S "to yield the required", phrase equation]

-- Helper expressions that represent the vectors of quantities as components
positionXY, velocityXY, accelerationXY, constAccelXY :: Expr
positionXY     = sy position     $= vec2D (sy xPos)        (sy yPos)
velocityXY     = sy velocity     $= vec2D (sy xVel)        (sy yVel)
accelerationXY = sy acceleration $= vec2D (sy xAccel)      (sy yAccel)
constAccelXY   = sy constAccelV  $= vec2D (sy xConstAccel) (sy yConstAccel)
