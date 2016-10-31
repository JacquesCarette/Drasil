module Drasil.GamePhysics.DataDefs where

import Drasil.GamePhysics.Unitals
import Drasil.GamePhysics.Concepts

import Language.Drasil
import Data.Drasil.SI_Units

import Control.Lens ((^.))

----- Data Definitions -----

cpDDefs :: [QDefinition]
cpDDefs = [dd1CtrOfMass, dd2linDisp, dd3linVel, dd4linAcc, dd5angDisp,
    dd6angVel, dd7angAccel, dd8impulse]

-- DD1 : Centre of mass --

dd1CtrOfMass :: QDefinition
dd1CtrOfMass = fromEqn "p_CM" dd1descr (pos_CM ^. symbol) metre ctrOfMassEqn

ctrOfMassEqn :: Expr
ctrOfMassEqn = (UnaryOp (Summation Nothing
    ((C mass_i) * (C pos_i)))) / (C mTot)

dd1descr :: Sentence
dd1descr = pos_CM ^. descr

-- DD2 : Linear displacement --

dd2linDisp :: QDefinition
dd2linDisp = fromEqn "r" dd2descr (Concat [(disp ^. symbol), Atomic "(",
    (time ^. symbol), Atomic ")"]) metre dispEqn

dispEqn :: Expr
dispEqn = Deriv Total (FCall (C position) [C time]) (C time)

dd2descr :: Sentence
dd2descr = S "the linear " :+: (disp ^. descr) :+: S " of a " :+:
    S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
    S " " :+: P (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
    S "), also equal to the derivative of its linear " :+:
    (position ^. descr) :+: S " with respect to " :+: (time ^. descr) :+:
    S " " :+: P (time ^. symbol)

-- DD3 : Linear velocity --

dd3linVel :: QDefinition
dd3linVel = fromEqn "v" dd3descr (Concat [(vel ^. symbol), Atomic "(",
    (time ^. symbol), Atomic ")"]) velU velEqn

velEqn :: Expr
velEqn = Deriv Total (FCall (C disp) [C time]) (C time)

dd3descr :: Sentence
dd3descr = S "the linear " :+: (vel ^. descr) :+: S " of a " :+:
    S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
    S " " :+: P (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
    S "), also equal to the derivative of its linear " :+: (vel ^. descr) :+:
    S " with respect to " :+: (time ^. descr) :+: S " " :+:
    P (time ^. symbol)

-- DD4 : Linear acceleration --

dd4linAcc :: QDefinition
dd4linAcc = fromEqn "a" dd4descr (Concat [(accel ^. symbol), Atomic "(",
    (time ^. symbol), Atomic ")"]) accelU accelEqn

accelEqn :: Expr
accelEqn = Deriv Total (FCall (C vel) [C time]) (C time)

dd4descr :: Sentence
dd4descr = S "the linear " :+: (accel ^. descr) :+: S " of a " :+:
    S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
    S " " :+: P (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
    S "), also equal to the derivative of its linear " :+: (accel ^. descr) :+:
    S " with respect to " :+: (time ^. descr) :+: S " " :+:
    P (time ^. symbol)

-- DD5 : Angular displacement --

dd5angDisp :: QDefinition
dd5angDisp = fromEqn "theta" dd5descr (Concat [(angDisp ^. symbol),
  Atomic "(", (time ^. symbol), Atomic ")"]) radians angDispEqn

angDispEqn :: Expr
angDispEqn = Deriv Total (FCall (C orientation) [C time]) (C time)

dd5descr :: Sentence
dd5descr = S "the " :+: (angDisp ^. descr) :+: S " of a " :+:
    S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
    S " " :+: P (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
    S "), also equal to the derivative of its " :+: (orientation ^. descr) :+:
    S " with respect to " :+: (time ^. descr) :+: S " " :+:
    P (time ^. symbol)

-- DD6 : Angular velocity --

dd6angVel :: QDefinition
dd6angVel = fromEqn "omega" dd6descr (Concat [(angVel ^. symbol), Atomic "(",
    (time ^. symbol), Atomic ")"]) angVelU angVelEqn

angVelEqn :: Expr
angVelEqn = Deriv Total (FCall (C angDisp) [C time]) (C time)

dd6descr :: Sentence
dd6descr = S "the " :+: (angVel ^. descr) :+: S " of a " :+:
    S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
    S " " :+: P (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
    S "), also equal to the derivative of its " :+: (angDisp ^. descr) :+:
    S " with respect to " :+: (time ^. descr) :+: S " " :+:
    P (time ^. symbol)

-- DD7 : Angular acceleration --

dd7angAccel :: QDefinition
dd7angAccel = fromEqn "alpha" dd7descr (Concat [(angAccel ^. symbol),
    Atomic "(", (time ^. symbol), Atomic ")"]) angAccelU angAccelEqn

angAccelEqn :: Expr
angAccelEqn = Deriv Total (FCall (C angVel) [C time]) (C time)

dd7descr :: Sentence
dd7descr = S "the " :+: (angAccel ^. descr) :+: S " of a " :+:
    S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
    S " " :+: P (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
    S "), also equal to the derivative of its " :+: (angVel ^. descr) :+:
    S " with respect to " :+: (time ^. descr) :+: S " " :+:
    P (time ^. symbol)

-- DD8 : Impulse for collision response --

-- Currently a super crude implementation requiring lots of custom chunks;
-- need norms and cross products

dd8impulse :: QDefinition
dd8impulse = fromEqn "j" dd8descr lJ impulseU impulseEqn

-- The last two terms in the denominator should be cross products.
impulseEqn :: Expr
impulseEqn = ((Neg ((Int 1) + (C restCoef))) * (C initRelVel) :.
    (C normalVect)) / (((((Int 1) / (C mass_A))) + ((Int 1) / (C mass_B))) *
    ((C normalLen) :^ (Int 2)) +
    (((C perpLen_A) :^ (Int 2)) / (C momtInert_A)) +
    (((C perpLen_B) :^ (Int 2))/ (C momtInert_B)))

dd8descr :: Sentence
dd8descr = S "the " :+: (impulseScl ^. descr) :+: S " used to determine " :+:
    S (coll ^. name) :+: S " response between two " :+: S (rigidBodies ^. name)
