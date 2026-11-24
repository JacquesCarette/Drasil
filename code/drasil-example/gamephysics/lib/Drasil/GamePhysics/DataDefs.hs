module Drasil.GamePhysics.DataDefs (dataDefs, ctrOfMassDD,
  linDispDD, linVelDD, linAccDD, angDispDD, angVelDD, angAccelDD, torqueDD,
  kEnergyDD, coeffRestitutionDD, reVelInCollDD, impulseVDD, momentOfInertiaDD,
  collisionAssump, rightHandAssump, rigidTwoDAssump, potEnergyDD,) where

import Control.Lens ((^.))

import Language.Drasil
import Language.Drasil.Document

import Theory.Drasil
import Data.List.Extras (weave)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD, assumpAD, assumpCT, assumpDI)
import Drasil.GamePhysics.Derivations (impulseVDerivEqns)
import Drasil.GamePhysics.References (chaslesWiki)
import Drasil.GamePhysics.Unitals (finRelVel, initRelVel, mTot, massj,
  normalVect, posCM, posj, rOB, rRot, velAP, velB, velBP, velO)

import Data.Drasil.Concepts.Math (rightHand)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody, twoD)

import qualified Data.Drasil.Quantities.Math as QM (orientation)
import Data.Drasil.Quantities.Physics (angularAccel, angularDisplacement,
  angularVelocity, chgInVelocity, displacement, energy, force,
  gravitationalAccel, height, impulseV, kEnergy, linearAccel,
  linearDisplacement, linearVelocity, momentOfInertia, position, potEnergy,
  restitutionCoef, time, torque, velocity)

import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)

import Data.Drasil.Theories.Physics (torqueDD)
----- Data Definitions -----

dataDefs :: [DataDefinition]
dataDefs = [ctrOfMassDD, linDispDD, linVelDD, linAccDD, angDispDD,
 angVelDD, angAccelDD, chaslesDD, torqueDD, kEnergyDD, coeffRestitutionDD,
 reVelInCollDD, impulseVDD, potEnergyDD, momentOfInertiaDD]

-- DD1 : Centre of mass --

ctrOfMassDD :: DataDefinition
ctrOfMassDD = ddMENoRefs ctrOfMass Nothing "ctrOfMass" [rigidBodyAssump]

ctrOfMass :: ModelQDef
ctrOfMass = mkQuantDef posCM ctrOfMassEqn

-- FIXME (variable "i") is a horrible hack
ctrOfMassEqn :: ModelExpr
ctrOfMassEqn = sumAll (variable "j") (sy massj $* sy posj) $/ sy mTot

-- DD2 : Linear displacement --

--FIXME: Why do QDefinitions only have term and not defn? They should have both!
--FIXME: HACK - Adding fixme defn for now, since NP can't (currently) incorporate sentence.
--fixme :: NP
--fixme = nounPhraseSP $ "FIXME: THIS HACK NEEDS TO BE UPDATED TO A NOUNPHRASE, " ++
--  "SEE Drasil.GamePhysics.DataDefs for more info"
{-
linDispQDef :: Sentence
linDispQDef = foldl (+:+) (EmptyS) def
  where def = [phrase $ QP.linearDisplacement ^. term, S "of a",
              phrase $ rigidBody ^. term, S "as a function of",
              phrase $ QP.time ^. term, ch QP.time,
              S "also equal to the derivate of its linear",
              phrase $ QP.velocity ^. term, S "with respect to",
              phrase $ QP.time ^. term, ch QP.time]
-}

linDispDD :: DataDefinition
linDispDD = ddMENoRefs linDisp Nothing "linDisp" [rigidBodyAssump]

linDisp :: ModelQDef
linDisp = mkQuantDef linearDisplacement dispEqn

dispEqn :: ModelExpr
dispEqn = deriv (apply1 position time) time
{-
dd2descr :: Sentence
dd2descr = S "linear" +:+ (QP.displacement ^. term) +:+ S "of a" +:+
  ( rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.position ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD3 : Linear velocity --

{-
linVelQDef :: Sentence
linVelQDef = foldl (+:+) (EmptyS) def
  where def = [phrase $ QP.linearVelocity ^. term, S "of a",
              phrase $ rigidBody ^. term, S "as a function of" ,
              phrase $ QP.time ^. term, QP.time,
              S "also equal to the derivative of its linear",
              phrase $ QP.velocity ^. term, S "with respect to",
              phrase $ QP.time ^. term, ch QP.time]
-}

linVelDD :: DataDefinition
linVelDD = ddMENoRefs linVel Nothing "linVel" [rigidBodyAssump]

linVel :: ModelQDef
linVel = mkQuantDef linearVelocity velEqn

velEqn :: ModelExpr
velEqn = deriv (apply1 displacement time) time
{-
dd3descr :: Sentence
dd3descr = S "linear" +:+ (QP.velocity ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.velocity ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD4 : Linear acceleration --

linAccDD :: DataDefinition
linAccDD = ddMENoRefs linAcc Nothing "linAcc" [rigidBodyAssump]

linAcc :: ModelQDef
linAcc = mkQuantDef linearAccel accelEqn

accelEqn :: ModelExpr
accelEqn = deriv (apply1 velocity time) time
{-
dd4descr :: Sentence
dd4descr = S "linear" +:+ (accel ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (accel ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD5 : Angular displacement --

angDispDD :: DataDefinition
angDispDD = ddMENoRefs angDisp Nothing "angDisp" [rigidTwoDAssump]

angDisp :: ModelQDef
angDisp = mkQuantDef angularDisplacement angDispEqn

angDispEqn :: ModelExpr
angDispEqn = deriv (apply1 QM.orientation time) time
{-
dd5descr :: Sentence
dd5descr = (QP.angularDisplacement ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QM.orientation ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD6 : Angular velocity --

angVelDD :: DataDefinition
angVelDD = ddMENoRefs angVel Nothing "angVel" [rigidTwoDAssump]

angVel :: ModelQDef
angVel = mkQuantDef angularVelocity angVelEqn

angVelEqn :: ModelExpr
angVelEqn = deriv (apply1 angularDisplacement time) time
{-
dd6descr :: Sentence
dd6descr = ((QP.angularVelocity ^. term)) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QP.angularDisplacement ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD7 : Angular acceleration --
-----------------------------------DD8 Angular Acceleration-------------------
angAccelDD :: DataDefinition
angAccelDD = ddMENoRefs angAccel Nothing "angAccel" [rigidTwoDAssump]

angAccel :: ModelQDef
angAccel = mkQuantDef angularAccel angAccelEqn

angAccelEqn :: ModelExpr
angAccelEqn = deriv (apply1 angularVelocity time) time

{-
dd7descr :: Sentence
dd7descr = (QP.angularAccel ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ ((QP.angularVelocity ^. term)) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD8 : Impulse for collision response --

-- Currently a super crude implementation requiring lots of custom chunks;
-- need norms and cross products

-------------------------DD8 Impulse for Collision-------------------------------
--moved to GenDefs.hs

------------------------DD9 Chasles Theorem----------------------------------
chaslesDD :: DataDefinition
chaslesDD = ddE chasles [dRef chaslesWiki] Nothing "chaslesThm"
  [chaslesThmNote, rigidBodyAssump]

chasles :: SimpleQDef
chasles = mkQuantDef' velB (nounPhraseSP "Chasles' theorem") chaslesEqn

-- The last two terms in the denominator should be cross products.
chaslesEqn :: Expr
chaslesEqn = sy velO $+ cross (sy  angularVelocity) (sy rOB)

chaslesThmNote :: Sentence
chaslesThmNote = foldlSent [D.toSent (atStartNP (the linearVelocity)),
  ch velB `S.of_` S "any point B in a", phrase CP.rigidBody `S.isThe` S "sum" `S.ofThe`
  phrase linearVelocity +:+ ch velO `S.ofThe` phrase CP.rigidBody,
  S "at the origin (axis of rotation)" `S.andThe` S "resultant vector from",
  S "cross product" `S.the_ofThe` phrasePoss CP.rigidBody,
  getTandS angularVelocity `S.andThe` getTandS rOB]

---------------DD10 Impulse(Vector)-----------------------------------------------------------------------
impulseVDD :: DataDefinition
impulseVDD = ddENoRefs impulseVQD (Just impulseVDeriv) "impulseV"
  [impulseVDesc, rigidBodyAssump]

impulseVQD :: SimpleQDef
impulseVQD = mkQuantDef impulseV impulseVEqn

impulseVEqn :: Expr
impulseVEqn = sy QPP.mass $* sy chgInVelocity

impulseVDesc :: Sentence
impulseVDesc = foldlSent [S "An", getTandS impulseV, S "occurs when a",
  getTandS force, S "acts over a body over an interval" `S.of_` phrase time]

impulseVDeriv :: Derivation
impulseVDeriv = mkDerivName (phrase impulseV) (weave impulseVDerivSentences $ eS <$> impulseVDerivEqns)

impulseVDerivSentences :: [Sentence]
impulseVDerivSentences = foldlSentCol <$> [impulseVDerivSentence1,
 impulseVDerivSentence2, impulseVDerivSentence3]

impulseVDerivSentence1 :: [Sentence]
impulseVDerivSentence1 = [S "Newton's second law of motion states"]

impulseVDerivSentence2 :: [Sentence]
impulseVDerivSentence2 = [S "Rearranging"]

impulseVDerivSentence3 :: [Sentence]
impulseVDerivSentence3 = [S "Integrating the right hand side"]

-----------------DD11 Relative Velocity in Collision-------------------------------------------------------
reVelInCollDD :: DataDefinition
reVelInCollDD = ddENoRefs reVelInColl Nothing "reVeInColl"
  [reVelInCollDesc, rigidBodyAssump]

reVelInColl :: SimpleQDef
reVelInColl = mkQuantDef initRelVel reVelInCollEqn

reVelInCollEqn :: Expr
reVelInCollEqn = sy velAP `vSub` sy velBP

reVelInCollDesc :: Sentence
reVelInCollDesc = foldlSent [S "In a collision, the", D.toSent (phraseNP (velocity `ofA` CP.rigidBody)),
  S "A colliding with another", phrase CP.rigidBody,
  S "B relative to that body", ch initRelVel `S.isThe` S "difference between the",
  plural velocity, S "of A and B at point P"]
-----------------DD13 Torque-------------------------------------------------------------------------------

-- Imported from Theories.Physics

----------------------DD14 Coefficient of Restitution--------------------------
coeffRestitutionDD :: DataDefinition
coeffRestitutionDD = ddENoRefs coeffRestitution Nothing "coeffRestitution"
 [coeffRestitutionDesc]

coeffRestitution :: SimpleQDef
coeffRestitution = mkQuantDef restitutionCoef coeffRestitutionEqn

coeffRestitutionEqn :: Expr
coeffRestitutionEqn = neg $ sy finRelVel $.
  sy normalVect $/ sy initRelVel $.
  sy normalVect

coeffRestitutionDesc :: Sentence
coeffRestitutionDesc = foldlSent [S "The", getTandS restitutionCoef,
  S "determines the elasticity" `S.ofA` S "collision between two" +:+. plural CP.rigidBody,
  foldlList Comma List [
  eS (sy restitutionCoef $= exactDbl 1) +:+ S "results" `S.in_` S "an elastic collision",
  eS (sy restitutionCoef $< exactDbl 1) +:+ S "results" `S.in_` S "an inelastic collision",
  eS (sy restitutionCoef $= exactDbl 0) +:+ S "results" `S.in_` S "a totally inelastic collision"]]
-----------------------DD15 Kinetic Energy--------------------------------
kEnergyDD :: DataDefinition
kEnergyDD = ddENoRefs kEnergyQD Nothing "kEnergy"
 [kEnergyDesc, rigidTwoDAssump, noDampingAssump]

kEnergyQD :: SimpleQDef
kEnergyQD = mkQuantDef kEnergy kEnergyEqn

kEnergyEqn :: Expr
kEnergyEqn = sy QPP.mass $* half (square (norm (sy velocity)))

kEnergyDesc :: Sentence
kEnergyDesc = foldlSent [atStart kEnergy `S.is` (kEnergy ^. defn)]
-----------------------DD16 Moment Of Inertia--------------------------------------------------------

momentOfInertiaDD :: DataDefinition
momentOfInertiaDD = ddMENoRefs momentOfInertiaQD Nothing "momentOfInertia"
 [momentOfInertiaDesc, rigidBodyAssump]

momentOfInertiaQD :: ModelQDef
momentOfInertiaQD = mkQuantDef momentOfInertia momentOfInertiaEqn

momentOfInertiaEqn :: ModelExpr
momentOfInertiaEqn = sumAll (variable "j") $ sy massj $* square (sy rRot)

momentOfInertiaDesc :: Sentence
momentOfInertiaDesc = foldlSent [S "The", getTandS momentOfInertia,
 S "of a body measures how much", phrase torque,
 S "is needed" `S.for` S "the body to achieve angular acceleration about the axis of rotation"]

---------------------------DD17 Potential Energy-------------------------------------------

potEnergyDD :: DataDefinition
potEnergyDD = ddENoRefs potEnergyQD Nothing "potEnergy"
 [potEnergyDesc, rigidTwoDAssump, noDampingAssump]

potEnergyQD :: SimpleQDef
potEnergyQD = mkQuantDef potEnergy potEnergyEqn

potEnergyEqn :: Expr
potEnergyEqn = sy QPP.mass $* sy gravitationalAccel $* sy height

potEnergyDesc :: Sentence
potEnergyDesc = foldlSent [D.toSent (atStartNP (the potEnergy)) `S.of_`
  S "an object" `S.isThe` phrase energy, S "held by an object because of its",
  phrase position, S "to other objects"]

---

collisionAssump, noDampingAssump, rightHandAssump, rigidBodyAssump, rigidTwoDAssump :: Sentence
collisionAssump = S "All collisions" `S.are` S "vertex-to-edge" +:+. fromSource assumpCT
noDampingAssump = S "No damping occurs during the simulation" +:+. fromSource assumpDI
rightHandAssump = S "A" +:+ phrase rightHand `S.is` S "used" +:+. fromSource assumpAD
rigidBodyAssump = S "All bodies are assumed to be rigid" +:+. fromSource assumpOT
rigidTwoDAssump = foldlSent [S "All bodies are assumed to be rigid",
  fromSource assumpOT `S.and_` phrase CP.twoD, fromSource assumpOD]
