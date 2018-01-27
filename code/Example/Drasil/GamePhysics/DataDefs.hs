module Drasil.GamePhysics.DataDefs (cpDDefs, cpQDefs) where

import Drasil.GamePhysics.Unitals

import Language.Drasil
import qualified Data.Drasil.Quantities.Physics as QP (restitutionCoef, time, 
  angularVelocity, angularAccel, angularDisplacement, velocity, displacement,
  position, linearAccel, linearDisplacement, linearVelocity, impulseS)
import qualified Data.Drasil.Quantities.Math as QM (orientation)
import Data.Drasil.Utils (mkDataDef)

----- Data Definitions -----

cpDDefs :: [QDefinition]
cpDDefs = [dd1CtrOfMass, dd2linDisp, dd3linVel, dd4linAcc, dd5angDisp,
  dd6angVel, dd7angAccel, dd8impulse]

cpQDefs :: [Block QDefinition]
cpQDefs = map (\x -> Parallel x []) cpDDefs
-- DD1 : Centre of mass --

dd1CtrOfMass :: QDefinition
dd1CtrOfMass = mkDataDef pos_CM ctrOfMassEqn

-- FIXME (Atomic "i") is a horrible hack
ctrOfMassEqn :: Expr
ctrOfMassEqn = (sum_all (Atomic "i") ((C mass_i) * (C pos_i))) / (C mTot)

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
dd2linDisp :: QDefinition
dd2linDisp = mkDataDef QP.linearDisplacement dispEqn

dispEqn :: Expr
dispEqn = Deriv Total (FCall (C QP.position) [C QP.time]) QP.time
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

dd3linVel :: QDefinition
dd3linVel = mkDataDef QP.linearVelocity velEqn

velEqn :: Expr
velEqn = Deriv Total (FCall (C QP.displacement) [C QP.time]) QP.time
{-
dd3descr :: Sentence
dd3descr = S "linear" +:+ (QP.velocity ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.velocity ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD4 : Linear acceleration --

dd4linAcc :: QDefinition
dd4linAcc = mkDataDef QP.linearAccel accelEqn

accelEqn :: Expr
accelEqn = Deriv Total (FCall (C QP.velocity) [C QP.time]) QP.time
{-
dd4descr :: Sentence
dd4descr = S "linear" +:+ (accel ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (accel ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD5 : Angular displacement --

dd5angDisp :: QDefinition
dd5angDisp = mkDataDef QP.angularDisplacement angDispEqn

angDispEqn :: Expr
angDispEqn = Deriv Total (FCall (C QM.orientation) [C QP.time]) QP.time
{-
dd5descr :: Sentence
dd5descr = (QP.angularDisplacement ^. term) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QM.orientation ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD6 : Angular velocity --

dd6angVel :: QDefinition
dd6angVel = mkDataDef QP.angularVelocity angVelEqn

angVelEqn :: Expr
angVelEqn = Deriv Total (FCall (C QP.angularDisplacement) [C QP.time]) QP.time
{-
dd6descr :: Sentence
dd6descr = ((QP.angularVelocity ^. term)) +:+ S "of a" +:+
  (CP.rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  P (QP.time ^. symbol) +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QP.angularDisplacement ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ P (QP.time ^. symbol)
-}
-- DD7 : Angular acceleration --

dd7angAccel :: QDefinition
dd7angAccel = mkDataDef QP.angularAccel angAccelEqn

angAccelEqn :: Expr
angAccelEqn = Deriv Total (FCall (C QP.angularVelocity) [C QP.time]) QP.time
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

dd8impulse :: QDefinition
dd8impulse = mkDataDef QP.impulseS impulseEqn

-- The last two terms in the denominator should be cross products.
impulseEqn :: Expr
impulseEqn = ((negate ((Int 1) + (C QP.restitutionCoef))) * (C initRelVel) $.
  (C normalVect)) / (((((Int 1) / (C mass_A))) + ((Int 1) / (C mass_B))) *
  ((C normalLen) $^ (Int 2)) +
  (((C perpLen_A) $^ (Int 2)) / (C momtInert_A)) +
  (((C perpLen_B) $^ (Int 2))/ (C momtInert_B)))
{-
--NOTE: Removed an extra "the" that was showing up in the output.
dd8descr :: Sentence
dd8descr = (impulseScl ^. term) +:+ S "used to determine" +:+
  (CP.collision ^. term) +:+ S "response between two" +:+ 
  irregPlur (CP.rigidBody ^. term)
-}
