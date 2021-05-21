module Drasil.Projectile.GenDefs (genDefns, genDefns0, posVecGD) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, TheoryModel, gd, gdNoRefs, ModelKinds(OthModel, EquationalModel))
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (oneD, rectilinear, twoD)

import Data.Drasil.Quantities.Physics (acceleration, constAccelV, iPos, iSpeed,
  iVel, ixVel, iyVel, position, scalarAccel, scalarPos, speed,
  time, velocity, positionVec)
import qualified Data.Drasil.Quantities.Physics as QP (constAccel)
import Data.Drasil.Theories.Physics (accelerationTM, velocityTM)

import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import Drasil.Projectile.Concepts (rectVel)
import qualified Drasil.Projectile.Expressions as E (speed', rectVelDerivEqn1, rectVelDerivEqn2,
  scalarPos', rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3, velVecExpr, posVecExpr,
  positionXY, velocityXY, accelerationXY, constAccelXY)
import Drasil.Projectile.References (hibbeler2004)

genDefns :: [GenDefn]
genDefns = [rectVelGD, rectPosGD, velVecGD, posVecGD]

-- TODO: after converting rectVelGD & rectPosGD to an EquationalModel, this should be removed
genDefns0 :: [GenDefn]
genDefns0 = [rectVelGD]

----------
rectVelGD :: GenDefn
rectVelGD = gd (OthModel rectVelRC) (getUnit speed) (Just rectVelDeriv)
  [makeCiteInfo hibbeler2004 $ Page [8]] "rectVel" [{-Notes-}]

-- TODO: This causes a collision due to `speed` being used by velMag in DataDefs!
{-
rectVelQD :: QDefinition 
rectVelQD = mkQuantDef' speed (nounPhraseSent $ foldlSent_ 
            [atStart rectilinear, sParen $ getAcc oneD, phrase velocity,
             S "as a function" `S.of_` phrase time, S "for", phrase QP.constAccel])
            rectVelExpr
-}

rectVelRC :: RelationConcept
rectVelRC = makeRC "rectVelRC" (nounPhraseSent $ foldlSent_ 
            [atStart rectilinear, sParen $ getAcc oneD, phrase velocity,
             S "as a function" `S.of_` phrase time, S "for", phrase QP.constAccel])
            EmptyS $ sy speed $= E.speed'

rectVelDeriv :: Derivation
rectVelDeriv = mkDerivName (phrase rectVel)
               (weave [rectVelDerivSents, map E rectVelDerivEqns])

rectVelDerivSents :: [Sentence]
rectVelDerivSents = [rectDeriv velocity acceleration motSent iVel accelerationTM, rearrAndIntSent, performIntSent]
  where
    motSent = foldlSent [S "The motion" `S.in_` makeRef2S accelerationTM `S.is` S "now", phrase oneD,
                         S "with a", phrase QP.constAccel `sC` S "represented by", E (sy QP.constAccel)]

rectVelDerivEqns :: [Expr]
rectVelDerivEqns = [E.rectVelDerivEqn1, E.rectVelDerivEqn2, sy speed $= E.speed']

----------
rectPosGD :: GenDefn
rectPosGD = gd (EquationalModel rectPosQD) (getUnit scalarPos) (Just rectPosDeriv)
  [makeCiteInfo hibbeler2004 $ Page [8]] "rectPos" [{-Notes-}]

rectPosQD :: QDefinition
rectPosQD = mkQuantDef' scalarPos (nounPhraseSent $ foldlSent_ 
            [atStart rectilinear, sParen $ getAcc oneD, phrase position,
             S "as a function" `S.of_` phrase time, S "for", phrase QP.constAccel])
            E.scalarPos'

rectPosDeriv :: Derivation
rectPosDeriv = mkDerivName (phrase rectilinear +:+ phrase position)
               (weave [rectPosDerivSents, map E rectPosDerivEqns])

rectPosDerivSents :: [Sentence]
rectPosDerivSents = [rectDeriv position velocity motSent iPos velocityTM,
  rearrAndIntSent, fromReplace rectVelGD speed, performIntSent]
    where
      motSent = S "The motion" `S.in_` makeRef2S velocityTM `S.is` S "now" +:+. phrase oneD

rectPosDerivEqns :: [Expr]
rectPosDerivEqns = [E.rectPosDerivEqn1, E.rectPosDerivEqn2, E.rectPosDerivEqn3, sy scalarPos $= E.scalarPos']

----------
velVecGD :: GenDefn
velVecGD = gdNoRefs (EquationalModel velVecQD) (getUnit velocity)
           (Just velVecDeriv) "velVec" [{-Notes-}]

velVecQD :: QDefinition 
velVecQD = mkQuantDef' velocity (nounPhraseSent $ foldlSent_ 
           [atStart velocity, S "vector as a function" `S.of_` phrase time, S "for",
            getAcc twoD, S "motion under", phrase QP.constAccel]) E.velVecExpr

velVecDeriv :: Derivation
velVecDeriv = mkDerivName (phrase velocity +:+ phrase vector) [velVecDerivSent, 
  E $ sy velocity $= E.velVecExpr]

velVecDerivSent :: Sentence
velVecDerivSent = vecDeriv [(velocity, E.velocityXY), (acceleration, E.accelerationXY)] rectVelGD

----------
posVecGD :: GenDefn
posVecGD = gdNoRefs (EquationalModel posVecQD) (getUnit position) 
           (Just posVecDeriv) "posVec" [{-Notes-}]

posVecQD :: QDefinition
posVecQD = mkQuantDef' position (nounPhraseSent $ foldlSent_ 
           [atStart position, S "vector as a function" `S.of_` phrase time, S "for",
            getAcc twoD, S "motion under", phrase QP.constAccel]) E.posVecExpr

posVecDeriv :: Derivation
posVecDeriv = mkDerivName (phrase positionVec) [posVecDerivSent, E $ relat posVecQD]

posVecDerivSent :: Sentence
posVecDerivSent =
  vecDeriv [(position, E.positionXY), (velocity, E.velocityXY), (acceleration, E.accelerationXY)] rectPosGD

-- Helper for making rectilinear derivations
rectDeriv :: UnitalChunk -> UnitalChunk -> Sentence -> UnitalChunk -> TheoryModel -> Sentence
rectDeriv c1 c2 motSent initc ctm = foldlSent_ [
  S "Assume we have", phrase rectilinear, S "motion" `S.of_` S "a particle",
  sParen (S "of negligible size" `S.and_` S "shape" `sC` S "from" +:+ makeRef2S pointMass) :+:
  S ";" +:+. (S "that is" `sC` S "motion" `S.in_` S "a straight line"), S "The" +:+.
  (phrase c1 `S.is` getScalar c1 `S.andThe` phrase c2 `S.is` getScalar c2), motSent,
  S "The", phrase initc, sParen (S "at" +:+ E (sy time $= 0) `sC` S "from" +:+
  makeRef2S timeStartZero) `S.is` S "represented by" +:+. getScalar initc,
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
rearrAndIntSent   = S "Rearranging" `S.and_` S "integrating" `sC` S "we" +: S "have"
performIntSent    = S "Performing the integration" `sC` S "we have the required" +: phrase equation

-- Helper for making vector derivations
vecDeriv :: [(UnitalChunk, Expr)] -> GenDefn -> Sentence
vecDeriv vecs gdef = foldlSentCol [
  S "For a", phrase twoD, phrase cartesian, sParen (makeRef2S twoDMotion `S.and_` makeRef2S cartSyst) `sC`
  S "we can represent" +:+. foldlList Comma List 
  (map (\(c, e) -> foldlSent_ [phraseNP (the c), phrase vector, S "as", E e]) vecs),
  atStartNP (the acceleration) `S.is` S "assumed to be constant", sParen (makeRef2S constAccel) `S.andThe`
  phrase constAccelV `S.is` S "represented as" +:+. E E.constAccelXY, 
  atStartNP (the iVel) +:+ sParen (S "at" +:+ E (sy time $= 0) `sC` S "from" +:+ makeRef2S timeStartZero) `S.is`
  S "represented by" +:+. E (sy iVel $= vec2D (sy ixVel) (sy iyVel)), 
  S "Since we have a",
  phrase cartesian `sC` makeRef2S gdef, S "can be applied to each", phrase coordinate `S.of_`
  S "the", phrase ((fst . head) vecs), phrase vector, S "to yield the required", phrase equation]

