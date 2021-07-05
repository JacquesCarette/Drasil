module Drasil.GamePhysics.Expressions where

import Language.Drasil

import Drasil.GamePhysics.Unitals (velj, massj, forcej, accj)
import Data.Drasil.Quantities.Physics (gravitationalAccel, time)

transMotExpr :: Expr
transMotExpr = sy gravitationalAccel `addRe` (apply1 forcej time $/ sy massj)

transMotExprDeriv1 :: DisplayExpr
transMotExprDeriv1 = defines accj $ deriv (apply1 velj time) time
