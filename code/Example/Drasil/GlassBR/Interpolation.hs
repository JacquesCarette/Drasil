module Drasil.GlassBR.Interpolation where

import Language.Drasil
import Drasil.GlassBR.Unitals(char_weight)

interpData :: [QDefinition]
interpData = [y_interp]

y_interp :: QDefinition
y_interp = fromEqn' "y_interp" (nounPhraseSP "interpolated y value") lY lin_interp

lin_interp :: Expr
lin_interp = (((C y_2) - (C y_1)) / ((C x_2) - (C x_1))) * ((C x) - (C x_1)) + (C y_1)

--interpolation b/w q1 and q2
w_TNT :: QDefinition
w_TNT = mkDataDef eqTNTWeight w_TNTEqn

w_TNTEqn :: Expr
w_TNTEqn = (C char_weight) * (C tNT)

y_2, y_1, x_2, x_1, x :: VarChunk
y_1 = makeVC "y_1"  (nounPhraseSP "y1") (sub (lY) (Atomic "1"))
y_2 = makeVC "y_2"  (nounPhraseSP "y2") (sub (lY) (Atomic "2"))
x_1 = makeVC "x_1"  (nounPhraseSP "x1") (sub (lX) (Atomic "1"))
x_2 = makeVC "x_2"  (nounPhraseSP "x2") (sub (lX) (Atomic "2"))
x   = makeVC "x"    (nounPhraseSP "x")    lX -- = params.wtnt from mainFun.py

w_j, s_j, q_j :: VarChunk
w_j      = makeVC "w_j"      (nounPhraseSP "wj") (sub (lW) (lJ))
s_j      = makeVC "s_j"      (nounPhraseSP "sj") (sub (lS) (lJ))
q_j      = makeVC "q_j"      (nounPhraseSP "qj") (sub (lQ) (lJ))

q_1, q_2 :: VarChunk
q_1 = makeVC "q_1" (nounPhraseSP "q_1") (sub (lQ) (Atomic "1"))
q_2 = makeVC "q_2" (nounPhraseSP "q_2") (sub (lQ) (Atomic "2"))

i, k :: VarChunk
i   = makeVC "i" (nounPhraseSP "i") lI
k   = makeVC "k" (nounPhraseSP "k") lK

{-
q = wtnt and sd -> both are user inputs
  = charge weights, num of w curves, [list of sds]
-}