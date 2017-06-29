module Drasil.GlassBR.Interpolation where

import Language.Drasil

interpData :: [QDefinition]
interpData = []

{-

lin_interp ::
lin_interp x1 y1 x2 y2 x = ((y2 :- y1) :/ (x2 - x1)) :* (x :- x1) :+ y1

wj, wjplus1
q1 = demand on wj, q2 = demand on wjplus1 : (@ sd)
wtnt = interpolation b/w q1 and q2

q = wtnt and sd -> both are user inputs
  = charge weights, num of w curves, [list of sds]

-}