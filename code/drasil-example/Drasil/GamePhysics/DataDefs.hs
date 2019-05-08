module Drasil.GamePhysics.DataDefs (cpDDefs, cpQDefs, dataDefns,
  ctrOfMassDD, linDispDD, linVelDD, linAccDD, angDispDD,
  angVelDD, angAccelDD, impulseDD, torqueDD, kEnergyDD, coeffRestitutionDD, reVelInCollDD) where

import Language.Drasil

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD, assumpAD, assumpCT, assumpDI)

import Drasil.GamePhysics.Unitals (initRelVel, mass_A, mass_B, massI,
  momtInert_A, momtInert_B, mTot, normalLen, normalVect,
  perpLen_A, perpLen_B, posCM, posI, velB, velO, rOB, finRelVel, velAP, velBP)

import qualified Data.Drasil.Quantities.Math as QM (orientation)

import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)

import qualified Data.Drasil.Quantities.Physics as QP (angularAccel, 
  angularDisplacement, angularVelocity, displacement, impulseS, linearAccel, 
  linearDisplacement, linearVelocity, position, restitutionCoef, time, velocity,
  force, torque, kEnergy, energy)

import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)

import Data.Drasil.SentenceStructures (foldlSent)

----- Data Definitions -----

dataDefns :: [DataDefinition]
dataDefns = [ctrOfMassDD, linDispDD, linVelDD, linAccDD, angDispDD,
 angVelDD, angAccelDD, impulseDD, chaslesDD, torqueDD, kEnergyDD, coeffRestitutionDD, reVelInCollDD]

cpDDefs :: [QDefinition]
cpDDefs = [ctrOfMass, linDisp, linVel, linAcc, angDisp,
  angVel, angAccel, impulse, chasles, torque, kEnergy, coeffRestitution]

cpQDefs :: [Block QDefinition]
cpQDefs = map (\x -> Parallel x []) cpDDefs
-- DD1 : Centre of mass --

ctrOfMassDD :: DataDefinition
ctrOfMassDD = mkDD ctrOfMass [{-- References --}] [{-- Derivation --}] "ctrOfMass" 
  [makeRef2S assumpOT, makeRef2S assumpOD]

ctrOfMass :: QDefinition
ctrOfMass = mkQuantDef posCM ctrOfMassEqn

-- FIXME (Atomic "i") is a horrible hack
ctrOfMassEqn :: Expr
ctrOfMassEqn = (sum_all (Atomic "i") ((sy massI) * (sy posI))) / (sy mTot)

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
              phrase $ CP.rigidBody ^. term, S "as a function of",
              phrase $ QP.time ^. term, ch QP.time,
              S "also equal to the derivate of its linear",
              phrase $ QP.velocity ^. term, S "with respect to",
              phrase $ QP.time ^. term, ch QP.time]
-}

linDispDD :: DataDefinition
linDispDD = mkDD linDisp [{-- References --}] [{-- Derivation --}] "linDisp" 
  [makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI]

linDisp :: QDefinition
linDisp = mkQuantDef QP.linearDisplacement dispEqn

dispEqn :: Expr
dispEqn = deriv (apply1 QP.position QP.time) QP.time
{-
dd2descr :: Sentence
dd2descr = S "linear" +:+ (QP.displacement ^. term) +:+ S "of a" +:+
  ( CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.position ^. term) +:+ 
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD3 : Linear velocity --

{-
linVelQDef :: Sentence
linVelQDef = foldl (+:+) (EmptyS) def
  where def = [phrase $ QP.linearVelocity ^. term, S "of a",
              phrase $ CP.rigidBody ^. term, S "as a function of" ,
              phrase $ QP.time ^. term, QP.time,
              S "also equal to the derivative of its linear",
              phrase $ QP.velocity ^. term, S "with respect to",
              phrase $ QP.time ^. term, ch QP.time]
-}

linVelDD :: DataDefinition
linVelDD = mkDD linVel [{-- References --}] [{-- Derivation --}] "linVel"
  [makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI]

linVel :: QDefinition
linVel = mkQuantDef QP.linearVelocity velEqn

velEqn :: Expr
velEqn = deriv (apply1 QP.displacement QP.time) QP.time
{-
dd3descr :: Sentence
dd3descr = S "linear" +:+ (QP.velocity ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.velocity ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD4 : Linear acceleration --

linAccDD :: DataDefinition
linAccDD = mkDD linAcc [{-- References --}] [{-- Derivation --}] "linAcc"
  [makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI]

linAcc :: QDefinition
linAcc = mkQuantDef QP.linearAccel accelEqn

accelEqn :: Expr
accelEqn = deriv (apply1 QP.velocity QP.time) QP.time
{-
dd4descr :: Sentence
dd4descr = S "linear" +:+ (accel ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (accel ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD5 : Angular displacement --

angDispDD :: DataDefinition
angDispDD = mkDD angDisp [{-- References --}] [{-- Derivation --}] "angDisp"
  [makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI]

angDisp :: QDefinition
angDisp = mkQuantDef QP.angularDisplacement angDispEqn

angDispEqn :: Expr
angDispEqn = deriv (apply1 QM.orientation QP.time) QP.time
{-
dd5descr :: Sentence
dd5descr = (QP.angularDisplacement ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QM.orientation ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD6 : Angular velocity --

angVelDD :: DataDefinition
angVelDD = mkDD angVel [{-- References --}] [{-- Derivation --}] "angVel"
  [makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI]

angVel :: QDefinition
angVel = mkQuantDef QP.angularVelocity angVelEqn

angVelEqn :: Expr
angVelEqn = deriv (apply1 QP.angularDisplacement QP.time) QP.time
{-
dd6descr :: Sentence
dd6descr = ((QP.angularVelocity ^. term)) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QP.angularDisplacement ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD7 : Angular acceleration --
-----------------------------------DD8 Angular Acceleration-------------------
angAccelDD :: DataDefinition
angAccelDD = mkDD angAccel [{-- References --}] [{-- Derivation --}] "angAccel"
  [makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI]

angAccel :: QDefinition
angAccel = mkQuantDef QP.angularAccel angAccelEqn

angAccelEqn :: Expr
angAccelEqn = deriv (apply1 QP.angularVelocity QP.time) QP.time
{-
dd7descr :: Sentence
dd7descr = (QP.angularAccel ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ ((QP.angularVelocity ^. term)) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD8 : Impulse for collision response --

-- Currently a super crude implementation requiring lots of custom chunks;
-- need norms and cross products

-------------------------DD8 Impulse for Collision-------------------------------

impulseDD :: DataDefinition
impulseDD = mkDD impulse [{-- References --}] [{-- Derivation --}] "impulse"
  [makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpAD, makeRef2S assumpCT]

impulse :: QDefinition
impulse = mkQuantDef QP.impulseS impulseEqn

-- The last two terms in the denominator should be cross products.
impulseEqn :: Expr
impulseEqn = ((negate (1 + (sy QP.restitutionCoef))) * (sy initRelVel) $.
  (sy normalVect)) / ((((1 / (sy mass_A))) + (1 / (sy mass_B))) *
  ((sy normalLen) $^ 2) +
  (((sy perpLen_A) $^ 2) / (sy momtInert_A)) +
  (((sy perpLen_B) $^ 2)/ (sy momtInert_B)))
{-
--NOTE: Removed an extra "the" that was showing up in the output.
dd8descr :: Sentence
dd8descr = (impulseScl ^. term) +:+ S "used to determine" +:+
  (CP.collision ^. term) +:+ S "response between two" +:+ 
  irregPlur (CP.rigidBody ^. term)
-}
------------------------DD9 Chasles Theorem----------------------------------
chaslesDD :: DataDefinition
chaslesDD = mkDD chasles [{-- References --}] [{-- Derivation --}] "chalses"
  [chaslesThmDesc, makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI]

chasles :: QDefinition
chasles = mkQuantDef velB chaslesEqn

-- The last two terms in the denominator should be cross products.
chaslesEqn :: Expr
chaslesEqn = (sy velO) + (cross (sy  QP.angularVelocity) (sy rOB))

chaslesThmDesc :: Sentence
chaslesThmDesc = foldlSent [S "The linear", (phrase QP.velocity),
  (ch velB), (sParen $ Sy $ unit_symb velB), S "of any point B in a",
  (phrase CP.rigidBody), makeRef2S assumpOT, S "is the sum of the linear",
  (phrase QP.velocity), (ch velO),
  (sParen $ Sy $ unit_symb velO), S "of the", (phrase $ CP.rigidBody),
  S "at the origin (axis of rotation) and the",
  S "resultant vector from the cross product of the",
  (phrase CP.rigidBody) :+: S "'s", (phrase QP.angularVelocity), 
  (ch QP.angularVelocity), 
  (sParen $ Sy $ unit_symb  QP.angularVelocity), S "and the", 
  (phrase rOB) `sC` (ch rOB), 
  (sParen $ Sy $ unit_symb rOB)]

-----------------DD11 Relative Velocity in Collision------------------------------------------------------- 
reVelInCollDD :: DataDefinition
reVelInCollDD = mkDD reVelInColl [{-- References --}] [{-- Derivation --}] "reVeInColl"
  [reVelInCollDesc]

reVelInColl :: QDefinition
reVelInColl = mkQuantDef initRelVel reVelInCollEqn

reVelInCollEqn :: Expr
reVelInCollEqn = (sy velAP) - (sy velBP)

reVelInCollDesc :: Sentence
reVelInCollDesc = foldlSent [S "In a collision, the", (phrase QP.velocity), 
  S "of a", (phrase $ CP.rigidBody),makeRef2S assumpOT, 
  S "A colliding with another", (phrase CP.rigidBody),
  S "B relative to that body", (ch initRelVel),
  S "is the difference between the", (plural QP.velocity),
  S "of A and B at point P"]
-----------------DD13 Torque-------------------------------------------------------------------------------

torqueDD :: DataDefinition
torqueDD = mkDD torque [{-- References --}] [{-- Derivation --}] "torque"
 [torqueDesc] 

torque :: QDefinition
torque = mkQuantDef QP.torque torqueEqn

torqueEqn :: Expr
torqueEqn = (cross (sy QP.displacement) (sy  QP.force))

torqueDesc :: Sentence
torqueDesc = foldlSent [S "The", (phrase torque), 
  S "on a body measures the", S "the tendency of a", (phrase QP.force), 
  S "to rotate the body around an axis or pivot"]

----------------------DD14 Coefficient of Restitution--------------------------
coeffRestitutionDD :: DataDefinition
coeffRestitutionDD = mkDD coeffRestitution [{-- References --}] [{-- Derivation --}] "coeffRestitution"
 [coeffRestitutionDesc]

coeffRestitution :: QDefinition
coeffRestitution = mkQuantDef QP.restitutionCoef coeffRestitutionEqn

coeffRestitutionEqn :: Expr
coeffRestitutionEqn = -(sy finRelVel) $.
  (sy normalVect)/ (sy initRelVel) $.
  (sy normalVect)

coeffRestitutionDesc :: Sentence
coeffRestitutionDesc = foldlSent [S "The", (phrase QP.restitutionCoef), (ch QP.restitutionCoef), 
  S "is a unitless, dimensionless quantity that determines the", 
  S "elasticity of a collision between two" +:+.(plural CP.rigidBody), 
  (E $ sy QP.restitutionCoef $= 1), S "results in an elastic collision, while",
  (E $ sy QP.restitutionCoef $< 1), S "results in an inelastic collision,",
  S "and", (E $ sy QP.restitutionCoef $= 0), S "results in a totally inelastic collision"]
-----------------------DD15 Kinetic Energy--------------------------------  
kEnergyDD :: DataDefinition
kEnergyDD = mkDD kEnergy [{-- References --}] [{-- Derivation --}] "kEnergy"
 [kEnergyDesc,makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI] 

kEnergy :: QDefinition
kEnergy = mkQuantDef QP.kEnergy kEnergyEqn

kEnergyEqn :: Expr
kEnergyEqn = ((sy QPP.mass)*(sy  QP.velocity) $^ 2)/2

kEnergyDesc :: Sentence
kEnergyDesc = foldlSent [S "The", (phrase QP.kEnergy),
 S "of an object is the", (phrase QP.energy),
 S "it possess due to its motion"]
  
