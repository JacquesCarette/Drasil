module Drasil.GamePhysics.Expressions where

import Language.Drasil

import Drasil.GamePhysics.Unitals (massj, velj, torquej, forcej, angAccj)
import Data.Drasil.Quantities.Physics (time, momentOfInertia,
    gravitationalAccel, angularVelocity)

transMotExpr :: PExpr
transMotExpr = sy gravitationalAccel $+ (apply1 forcej time $/ sy massj)

transMotExprDeriv1 :: (ModelExprC r, ExprC r) => r
transMotExprDeriv1 = defines (sy angAccj) $ deriv (apply1 velj time) time

rotMotExpr :: PExpr
rotMotExpr = apply1 torquej time $/ sy momentOfInertia

rotMotExprDeriv1 :: (ModelExprC r, ExprC r) => r
rotMotExprDeriv1 = defines (sy angAccj) $ deriv (apply1 angularVelocity time) time
