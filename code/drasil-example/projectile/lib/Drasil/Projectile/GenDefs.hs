{-# LANGUAGE PostfixOperators #-}
module Drasil.Projectile.GenDefs (genDefns, posVecGD) where

import Prelude hiding (cos, sin)

import Data.Drasil.Citations (hibbeler2004)
import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (oneD, rectilinear, twoD, motion)
import Data.Drasil.Quantities.Physics (acceleration, constAccelV, iPos, iSpeed,
  iVel, ixVel, iyVel, position, scalarAccel, scalarPos,
  time, velocity, positionVec, speed)
import qualified Data.Drasil.Quantities.Physics as QP (constAccel)
import Data.Drasil.Theories.Physics (accelerationTM, velocityTM)
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Language.Drasil.Development as D
import Theory.Drasil (GenDefn, TheoryModel, gd, gdNoRefs, equationalModel', Derivation,
  mkDerivName)
import Utils.Drasil (weave)

import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import Drasil.Projectile.Concepts (rectVel)
import qualified Drasil.Projectile.Derivations as D
import qualified Drasil.Projectile.Expressions as E
import Drasil.Projectile.Unitals (projSpeed, projPos)

genDefns :: [GenDefn]
genDefns = [rectVelGD, rectPosGD, velVecGD, posVecGD]

----------
rectVelGD :: GenDefn
rectVelGD = gd (equationalModel' rectVelQD) (getUnit projSpeed) (Just rectVelDeriv)
  [dRefInfo hibbeler2004 $ Page [8]] "rectVel" [{-Notes-}]

rectVelQD :: ModelQDef
rectVelQD = mkQuantDef' projSpeed
  -- TODO: make this into a proper phrase
  (cn "rectilinear velocity as a function of time for constant acceleration")
            E.speed'

rectVelDeriv :: Derivation
rectVelDeriv = mkDerivName (phrase rectVel)
               (weave [rectVelDerivSents, rectVelDerivEqns])

rectVelDerivSents :: [Sentence]
rectVelDerivSents = [rectDeriv velocity acceleration motSent iVel accelerationTM, rearrAndIntSent, performIntSent]
  where
    motSent = foldlSent [D.toSent (atStartNP (the motion)) `S.in_` refS accelerationTM `S.is` S "now", phrase oneD,
                         S "with a", phrase QP.constAccel `sC` S "represented by", eS' QP.constAccel]

rectVelDerivEqns :: [Sentence]
rectVelDerivEqns = map eS D.rectVelDeriv ++ [eS' rectVelQD]

----------
rectPosGD :: GenDefn
rectPosGD = gd (equationalModel' rectPosQD) (getUnit projPos) (Just rectPosDeriv)
  [dRefInfo hibbeler2004 $ Page [8]] "rectPos" [{-Notes-}]

rectPosQD :: ModelQDef
rectPosQD = mkQuantDef' projPos
  -- TODO: make this into a proper phrase
  (cn "rectilinear position as a function of time for constant acceleration")
            E.scalarPos'

rectPosDeriv :: Derivation
rectPosDeriv = mkDerivName (phrase rectilinear +:+ phrase position)
               (weave [rectPosDerivSents, rectPosDerivEqns])

rectPosDerivSents :: [Sentence]
rectPosDerivSents = [rectDeriv position velocity motSent iPos velocityTM,
  rearrAndIntSent, fromReplace rectVelGD speed, performIntSent]
    where
      motSent = D.toSent (atStartNP (the motion)) `S.in_` refS velocityTM `S.is` S "now" +:+. phrase oneD

rectPosDerivEqns :: [Sentence]
rectPosDerivEqns = map eS D.rectPosDeriv ++ [eS' rectPosQD]

----------
velVecGD :: GenDefn
velVecGD = gdNoRefs (equationalModel' velVecQD) (getUnit velocity)
           (Just velVecDeriv) "velVec" [{-Notes-}]

velVecQD :: ModelQDef
velVecQD = mkQuantDef' velocity
  -- TODO: make this into a proper phrase
  (cn "velocity vector as a function of time for 2D motion under constant acceleration")
    E.velVecExpr

velVecDeriv :: Derivation
velVecDeriv = mkDerivName (phrase velocity +:+ phrase vector) [velVecDerivSent,
  E $ defines (sy velocity) E.velVecExpr]

velVecDerivSent :: Sentence
velVecDerivSent = vecDeriv [(velocity, E.velocityXY), (acceleration, E.accelerationXY)] rectVelGD

----------
posVecGD :: GenDefn
posVecGD = gdNoRefs (equationalModel' posVecQD) (getUnit position)
           (Just posVecDeriv) "posVec" [{-Notes-}]

-- TODO: this has gotten 'inlined' too much, need to re-use combinators
posVecQD :: ModelQDef
posVecQD = mkQuantDef' position (nounPhraseSent
  (D.S "Position vector as a function of time for two-dimensional motion under constant acceleration"))
  E.posVecExpr

posVecDeriv :: Derivation
posVecDeriv = mkDerivName (phrase positionVec) [posVecDerivSent, eS' posVecQD]

posVecDerivSent :: Sentence
posVecDerivSent =
  vecDeriv [(position, E.positionXY), (velocity, E.velocityXY), (acceleration, E.accelerationXY)] rectPosGD

-- Helper for making rectilinear derivations
rectDeriv :: UnitalChunk -> UnitalChunk -> Sentence -> UnitalChunk -> TheoryModel -> Sentence
rectDeriv c1 c2 motSent initc ctm = foldlSent_ [
  S "Assume we have", D.toSent (phraseNP (combineNINI rectilinear motion)) `S.ofA` S "particle",
  sParen (S "of negligible size" `S.and_` S "shape" `sC` S "from" +:+ refS pointMass) :+:
  S ";" +:+. (S "that is" `sC` S "motion" `S.in_` S "a straight line"),
  (D.toSent (atStartNP (the c1)) `S.is` getScalar c1 `S.andThe` phrase c2 `S.is` getScalar c2 !.), motSent,
  D.toSent $ atStartNP (the initc), sParen (S "at" +:+ eS (sy time $= exactDbl 0) `sC` S "from" +:+
  refS timeStartZero) `S.is` S "represented by" +:+. getScalar initc,
  S "From", refS ctm `S.in_` short oneD `sC` S "and using the above", plural symbol_ +: S "we have"]
  where
    getScalar c
      | c == position     = eS' scalarPos
      | c == velocity     = eS' speed
      | c == acceleration = eS' scalarAccel
      | c == iPos         = eS' iPos
      | c == iVel         = eS' iSpeed
      | otherwise         = error "Not implemented in getScalar"

rearrAndIntSent, performIntSent :: Sentence
rearrAndIntSent = S "Rearranging" `S.and_` S "integrating" `sC` S "we" +: S "have"
performIntSent  = S "Performing the integration" `sC` S "we have the required" +: phrase equation

-- Helper for making vector derivations
vecDeriv :: [(UnitalChunk, ModelExpr)] -> GenDefn -> Sentence
vecDeriv vecs gdef = foldlSentCol [
  S "For a", D.toSent $ phraseNP (combineNINI twoD cartesian), sParen (refS twoDMotion `S.and_` refS cartSyst) `sC`
  S "we can represent" +:+. foldlList Comma List
  (map (\(c, e) -> foldlSent_ [D.toSent $ phraseNP (the c), phrase vector, S "as", eS e]) vecs),
  D.toSent (atStartNP (the acceleration)) `S.is` S "assumed to be constant", sParen (refS constAccel) `S.andThe`
  phrase constAccelV `S.is` S "represented as" +:+. eS E.constAccelXY,
  D.toSent (atStartNP (the iVel)) +:+ sParen (S "at" +:+ eS (sy time $= exactDbl 0) `sC` S "from" +:+ refS timeStartZero) `S.is`
  S "represented by" +:+. eS (sy iVel $= vec2D (sy ixVel) (sy iyVel)),
  S "Since we have a",
  phrase cartesian `sC` refS gdef, S "can be applied to each", D.toSent $ phraseNP (coordinate `ofThe`
  (fst . head) vecs), phrase vector, S "to yield the required", phrase equation]
