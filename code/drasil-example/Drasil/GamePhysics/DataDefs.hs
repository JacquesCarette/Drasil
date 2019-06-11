module Drasil.GamePhysics.DataDefs (qDefs, blockQDefs, dataDefns,
  ctrOfMassDD, linDispDD, linVelDD, linAccDD, angDispDD,
  angVelDD, angAccelDD, impulseDD, torqueDD, kEnergyDD, coeffRestitutionDD, reVelInCollDD, impulseVDD) where

import Language.Drasil
import Database.Drasil (Block(Parallel))
import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)
import Utils.Drasil

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD, assumpAD, assumpCT, assumpDI)

import Drasil.GamePhysics.Unitals (initRelVel, massA, massB, massI,
  momtInertA, momtInertB, mTot, normalLen, normalVect,
  perpLenA, perpLenB, posCM, posI, velB, velO, rOB, finRelVel, velAP, velBP, velo_1, velo_2, timeT, time_1, time_2)

import qualified Data.Drasil.Quantities.Math as QM (orientation)

import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)

import qualified Data.Drasil.Quantities.Physics as QP (angularAccel, 
  angularDisplacement, angularVelocity, displacement, impulseS, linearAccel, 
  linearDisplacement, linearVelocity, position, restitutionCoef, time, velocity,
  force, kEnergy, energy, impulseV, chgInVelocity, acceleration, potEnergy, height, gravitationalAccel)

import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import Data.Drasil.Theories.Physics (torque, torqueDD)
----- Data Definitions -----

dataDefns :: [DataDefinition]
dataDefns = [ctrOfMassDD, linDispDD, linVelDD, linAccDD, angDispDD,
 angVelDD, angAccelDD, impulseDD, chaslesDD, torqueDD, kEnergyDD,
 coeffRestitutionDD, reVelInCollDD, impulseVDD, potEnergyDD]

qDefs :: [QDefinition]
qDefs = [ctrOfMass, linDisp, linVel, linAcc, angDisp,
  angVel, angAccel, impulse, chasles, torque, kEnergy, coeffRestitution, potEnergy]

blockQDefs :: [Block QDefinition]
blockQDefs = map (\x -> Parallel x []) qDefs
-- DD1 : Centre of mass --

ctrOfMassDD :: DataDefinition
ctrOfMassDD = ddNoRefs ctrOfMass [{-- Derivation --}] "ctrOfMass" 
  [makeRef2S assumpOT, makeRef2S assumpOD]

ctrOfMass :: QDefinition
ctrOfMass = mkQuantDef posCM ctrOfMassEqn

-- FIXME (Atomic "i") is a horrible hack
ctrOfMassEqn :: Expr
ctrOfMassEqn = sumAll (Atomic "i") (sy massI * sy posI) / sy mTot

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
linDispDD = ddNoRefs linDisp [{-- Derivation --}] "linDisp" 
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
linVelDD = ddNoRefs linVel [{-- Derivation --}] "linVel"
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
linAccDD = ddNoRefs linAcc [{-- Derivation --}] "linAcc"
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
angDispDD = ddNoRefs angDisp [{-- Derivation --}] "angDisp"
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
angVelDD = ddNoRefs angVel [{-- Derivation --}] "angVel"
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
angAccelDD = ddNoRefs angAccel [{-- Derivation --}] "angAccel"
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
impulseDD = ddNoRefs impulse [{-- Derivation --}] "impulse"
  [makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpAD, makeRef2S assumpCT]

impulse :: QDefinition
impulse = mkQuantDef QP.impulseS impulseEqn

-- The last two terms in the denominator should be cross products.
impulseEqn :: Expr
impulseEqn = (negate (1 + sy QP.restitutionCoef) * sy initRelVel $.
  sy normalVect) / (((1 / sy massA) + (1 / sy massB)) *
  (sy normalLen $^ 2) +
  ((sy perpLenA $^ 2) / sy momtInertA) +
  ((sy perpLenB $^ 2) / sy momtInertB))
{-
--NOTE: Removed an extra "the" that was showing up in the output.
dd8descr :: Sentence
dd8descr = (impulseScl ^. term) +:+ S "used to determine" +:+
  (CP.collision ^. term) +:+ S "response between two" +:+ 
  irregPlur (CP.rigidBody ^. term)
-}
------------------------DD9 Chasles Theorem----------------------------------
chaslesDD :: DataDefinition
chaslesDD = ddNoRefs chasles [{-- Derivation --}] "chalses"
  [chaslesThmDesc, makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI]

chasles :: QDefinition
chasles = mkQuantDef velB chaslesEqn

-- The last two terms in the denominator should be cross products.
chaslesEqn :: Expr
chaslesEqn = sy velO + cross (sy  QP.angularVelocity) (sy rOB)

chaslesThmDesc :: Sentence
chaslesThmDesc = foldlSent [S "The linear", phrase QP.velocity,
  ch velB, sParen $ Sy $ unit_symb velB, S "of any point B in a",
  phrase CP.rigidBody, makeRef2S assumpOT, S "is the sum of the linear",
  phrase QP.velocity, ch velO,
  sParen $ Sy $ unit_symb velO, S "of the", phrase CP.rigidBody,
  S "at the origin (axis of rotation) and the",
  S "resultant vector from the cross product of the",
  phrase CP.rigidBody :+: S "'s", phrase QP.angularVelocity, 
  ch QP.angularVelocity, 
  sParen $ Sy $ unit_symb  QP.angularVelocity, S "and the", 
  phrase rOB `sC` ch rOB, 
  sParen $ Sy $ unit_symb rOB]

---------------DD10 Impulse(Vector)-----------------------------------------------------------------------
impulseVDD :: DataDefinition
impulseVDD = ddNoRefs impulseV impulseVDeriv "impulseV"
 [impulseVDesc, makeRef2S assumpOT]

impulseV :: QDefinition
impulseV = mkQuantDef QP.impulseV impulseVEqn

impulseVEqn :: Expr
impulseVEqn =   (sy QPP.mass) * (sy QP.chgInVelocity)

impulseVDesc :: Sentence
impulseVDesc = foldlSent [S "An", (phrase QP.impulseV), (ch impulseV), S "occurs when a", 
  (phrase QP.force), (ch QP.force), S "acts over a body over an interval of",(phrase QP.time),
  S "Derivation of", (phrase QP.impulseV)]

impulseVDeriv :: Derivation
impulseVDeriv = (weave [impulseVDerivSentences, map E impulseVDerivEqns]) 

impulseVDerivSentences :: [Sentence]
impulseVDerivSentences = map foldlSentCol [impulseVDerivSentence1, 
 impulseVDerivSentence2, impulseVDerivSentence3]  

impulseVDerivSentence1 :: [Sentence]
impulseVDerivSentence1 = [S " Derivation of", (phrase QP.impulseV), S "-", 
  S "Newton's second law of motion states"]

impulseVDerivSentence2 :: [Sentence]
impulseVDerivSentence2 = [S "Rearranging "] 

impulseVDerivSentence3 :: [Sentence]
impulseVDerivSentence3 = [S "Integrating the right hand side "] 

impulseVDerivEqn1 :: Expr
impulseVDerivEqn1 = (sy QP.force) $= (sy QPP.mass) * (sy QP.acceleration) 
                    $= (sy QPP.mass) * (deriv(sy QP.velocity) QP.time) 

impulseVDerivEqn2 :: Expr
impulseVDerivEqn2 = (defint (eqSymb timeT)(sy time_1)(sy time_2)(sy QP.force))
                     $= sy QPP.mass * defint (eqSymb QP.velocity)(sy velo_1)(sy velo_2)1


impulseVDerivEqn3 :: Expr
impulseVDerivEqn3 = (defint (eqSymb timeT)(sy time_1)(sy time_2)(sy QP.force))
                    $= (sy QPP.mass)*(sy velo_2) - (sy QPP.mass)*(sy velo_1) 
                    $= (sy QPP.mass) * (sy QP.chgInVelocity)
                                      
impulseVDerivEqns :: [Expr]
impulseVDerivEqns = [impulseVDerivEqn1, impulseVDerivEqn2, impulseVDerivEqn3]

-----------------DD11 Relative Velocity in Collision------------------------------------------------------- 
reVelInCollDD :: DataDefinition
reVelInCollDD = ddNoRefs reVelInColl [{-- Derivation --}] "reVeInColl"
  [reVelInCollDesc]

reVelInColl :: QDefinition
reVelInColl = mkQuantDef initRelVel reVelInCollEqn

reVelInCollEqn :: Expr
reVelInCollEqn = sy velAP - sy velBP

reVelInCollDesc :: Sentence
reVelInCollDesc = foldlSent [S "In a collision, the", phrase QP.velocity, 
  S "of a", phrase CP.rigidBody,makeRef2S assumpOT, 
  S "A colliding with another", phrase CP.rigidBody,
  S "B relative to that body", ch initRelVel,
  S "is the difference between the", plural QP.velocity,
  S "of A and B at point P"]
-----------------DD13 Torque-------------------------------------------------------------------------------

-- Imported from Theories.Physics

----------------------DD14 Coefficient of Restitution--------------------------
coeffRestitutionDD :: DataDefinition
coeffRestitutionDD = ddNoRefs coeffRestitution [{-- Derivation --}] "coeffRestitution"
 [coeffRestitutionDesc]

coeffRestitution :: QDefinition
coeffRestitution = mkQuantDef QP.restitutionCoef coeffRestitutionEqn

coeffRestitutionEqn :: Expr
coeffRestitutionEqn = - sy finRelVel $.
  sy normalVect / sy initRelVel $.
  sy normalVect

coeffRestitutionDesc :: Sentence
coeffRestitutionDesc = foldlSent [S "The", phrase QP.restitutionCoef, ch QP.restitutionCoef, 
  S "is a unitless, dimensionless quantity that determines the", 
  S "elasticity of a collision between two" +:+. plural CP.rigidBody, 
  E $ sy QP.restitutionCoef $= 1, S "results in an elastic collision, while",
  E $ sy QP.restitutionCoef $< 1, S "results in an inelastic collision,",
  S "and", E $ sy QP.restitutionCoef $= 0, S "results in a totally inelastic collision"]
-----------------------DD15 Kinetic Energy--------------------------------  
kEnergyDD :: DataDefinition
kEnergyDD = ddNoRefs kEnergy [{-- Derivation --}] "kEnergy"
 [kEnergyDesc,makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI] 

kEnergy :: QDefinition
kEnergy = mkQuantDef QP.kEnergy kEnergyEqn

kEnergyEqn :: Expr
kEnergyEqn = (sy QPP.mass * sy QP.velocity $^ 2) / 2

kEnergyDesc :: Sentence
kEnergyDesc = foldlSent [S "The", phrase QP.kEnergy,
 S "of an object is the", phrase QP.energy,
 S "it possess due to its motion"]

---------------------------DD17 Potential Energy-------------------------------------------

potEnergyDD :: DataDefinition
potEnergyDD = ddNoRefs potEnergy [{-- Derivation --}] "potEnergy"
 [potEnergyDesc,makeRef2S assumpOT, makeRef2S assumpOD, makeRef2S assumpDI] 

potEnergy :: QDefinition
potEnergy = mkQuantDef QP.potEnergy potEnergyEqn

potEnergyEqn :: Expr
potEnergyEqn = (sy QPP.mass * sy QP.gravitationalAccel * sy QP.height)

potEnergyDesc :: Sentence
potEnergyDesc = foldlSent [S "The", phrase QP.potEnergy,
 S "of an object is the", phrase QP.energy,
 S "held by an object because of its", phrase QP.position, S "to other objects"]

