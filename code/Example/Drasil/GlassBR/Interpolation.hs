module Drasil.GlassBR.Interpolation where

import Language.Drasil

interpData :: [QDefinition]
interpData = [y_interp]

lin_interp :: Expr
lin_interp = (((C y_2) - (C y_1)) :/ ((C x_2) - (C x_1))) * ((C x) - (C x_1)) + (C y_1)

y_interp :: QDefinition
y_interp = fromEqn' "y_interp" (nounPhraseSP "interpolated y value") lY lin_interp

y_2, y_1, x_2, x_1, x :: VarChunk
y_1 = makeVC "y_1"  (nounPhraseSP "y1") (sub (lY) (Atomic "1"))
y_2 = makeVC "y_2"  (nounPhraseSP "y2") (sub (lY) (Atomic "2"))
x_1 = makeVC "x_1"  (nounPhraseSP "x1") (sub (lX) (Atomic "1"))
x_2 = makeVC "x_2"  (nounPhraseSP "x2") (sub (lX) (Atomic "2"))
x   = makeVC "x"    (nounPhraseSP "x")    lX -- = params.wtnt from mainFun.py

{-

wj, wjplus1
q1 = demand on wj, q2 = demand on wjplus1 : (@ sd)
wtnt = interpolation b/w q1 and q2

q = wtnt and sd -> both are user inputs
  = charge weights, num of w curves, [list of sds]

-}
