module Drasil.GamePhysics.DataDefs (cpDDefs, cpQDefs) where

import Drasil.GamePhysics.Unitals (initRelVel, mass_A, mass_B, mass_i,
  momtInert_A, momtInert_B, mTot, normalLen, normalVect,
  perpLen_A, perpLen_B, pos_CM, pos_i)

import Language.Drasil
import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.Physics as QP (angularAccel, 
  angularDisplacement, angularVelocity, displacement, impulseS, linearAccel, 
  linearDisplacement, linearVelocity, position, restitutionCoef, time, velocity)

----- Data Definitions -----

cpDDefs :: [QDefinition]
cpDDefs = [ctrOfMass, linDisp, linVel, linAcc, angDisp,
  angVel, angAccel, impulse]

cpQDefs :: [Block QDefinition]
cpQDefs = map (\x -> Parallel x []) cpDDefs
-- DD1 : Centre of mass --

ctrOfMass :: QDefinition
ctrOfMass = mkDataDef pos_CM ctrOfMassEqn

-- FIXME (Atomic "i") is a horrible hack
ctrOfMassEqn :: Expr
ctrOfMassEqn = (sum_all (Atomic "i") ((sy mass_i) * (sy pos_i))) / (sy mTot)

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
              phrase $ QP.time ^. term, P $ QP.time ^. symbol,
              S "also equal to the derivate of its linear",
              phrase $ QP.velocity ^. term, S "with respect to",
              phrase $ QP.time ^. term, P $ QP.time ^. symbol]
-}
linDisp :: QDefinition
linDisp = mkDataDef QP.linearDisplacement dispEqn

dispEqn :: Expr
dispEqn = deriv (apply1 QP.position QP.time) QP.time
{-
dd2descr :: Sentence
dd2descr = S "linear" +:+ (QP.displacement ^. term) +:+ S "of a" +:+
  ( CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.position ^. term) +:+ 
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD3 : Linear velocity --

{-
linVelQDef :: Sentence
linVelQDef = foldl (+:+) (EmptyS) def
  where def = [phrase $ QP.linearVelocity ^. term, S "of a",
              phrase $ CP.rigidBody ^. term, S "as a function of" ,
              phrase $ QP.time ^. term, P $ QP.time ^. symbol,
              S "also equal to the derivative of its linear",
              phrase $ QP.velocity ^. term, S "with respect to",
              phrase $ QP.time ^. term, P $ QP.time ^. symbol]
-}

linVel :: QDefinition
linVel = mkDataDef QP.linearVelocity velEqn

velEqn :: Expr
velEqn = deriv (apply1 QP.displacement QP.time) QP.time
{-
dd3descr :: Sentence
dd3descr = S "linear" +:+ (QP.velocity ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.velocity ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD4 : Linear acceleration --

linAcc :: QDefinition
linAcc = mkDataDef QP.linearAccel accelEqn

accelEqn :: Expr
accelEqn = deriv (apply1 QP.velocity QP.time) QP.time
{-
dd4descr :: Sentence
dd4descr = S "linear" +:+ (accel ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (accel ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD5 : Angular displacement --

angDisp :: QDefinition
angDisp = mkDataDef QP.angularDisplacement angDispEqn

angDispEqn :: Expr
angDispEqn = deriv (apply1 QM.orientation QP.time) QP.time
{-
dd5descr :: Sentence
dd5descr = (QP.angularDisplacement ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QM.orientation ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD6 : Angular velocity --

angVel :: QDefinition
angVel = mkDataDef QP.angularVelocity angVelEqn

angVelEqn :: Expr
angVelEqn = deriv (apply1 QP.angularDisplacement QP.time) QP.time
{-
dd6descr :: Sentence
dd6descr = ((QP.angularVelocity ^. term)) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QP.angularDisplacement ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD7 : Angular acceleration --

angAccel :: QDefinition
angAccel = mkDataDef QP.angularAccel angAccelEqn

angAccelEqn :: Expr
angAccelEqn = deriv (apply1 QP.angularVelocity QP.time) QP.time
{-
dd7descr :: Sentence
dd7descr = (QP.angularAccel ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ ((QP.angularVelocity ^. term)) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD8 : Impulse for collision response --

-- Currently a super crude implementation requiring lots of custom chunks;
-- need norms and cross products

impulse :: QDefinition
impulse = mkDataDef QP.impulseS impulseEqn

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
