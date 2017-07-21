module Drasil.GlassBR.Interpolation where

import Language.Drasil
import Drasil.GlassBR.Unitals(char_weight)

y_interp :: QDefinition
y_interp = fromEqn' "y_interp" (nounPhraseSP "interpolated y value") lY lin_interp

lin_interp :: Expr
lin_interp = (((C y_2) - (C y_1)) / ((C x_2) - (C x_1))) * ((C x) - (C x_1)) + (C y_1)

y_2, y_1, x_2, x_1, x :: VarChunk
y_1  = makeVC "y_1"    (nounPhraseSP "y1")   (sub (lY) (Atomic "1"))
y_2  = makeVC "y_2"    (nounPhraseSP "y2")   (sub (lY) (Atomic "2"))
x_1  = makeVC "x_1"    (nounPhraseSP "x1")   (sub (lX) (Atomic "1"))
x_2  = makeVC "x_2"    (nounPhraseSP "x2")   (sub (lX) (Atomic "2"))
x    = makeVC "x"      (nounPhraseSP "x")    lX -- = params.wtnt from mainFun.py

i, z, z_array, y_array, x_array, y, arr :: VarChunk
i    = makeVC "i"          (nounPhraseSP "i")       lI
z    = makeVC "z"          (nounPhraseSP "z")       lZ
z_array = makeVC "z_array" (nounPhraseSP "z_array") (sub (lZ) (Atomic "array"))
y_array = makeVC "y_array" (nounPhraseSP "y_array") (sub (lY) (Atomic "array"))
x_array = makeVC "x_array" (nounPhraseSP "x_array") (sub (lX) (Atomic "array"))
y    = makeVC "y"          (nounPhraseSP "y")       lY
arr  = makeVC "arr"        (nounPhraseSP "arr")     (Atomic "arr") --FIXME: temporary variable for indInSeq?

--Python code to Expr

indInSeq :: Relation
indInSeq = (Index (C arr) (C i)) :<= (C i) :<= (Index (C arr) (C i))
--FIXME: captured constraints "arr[i] <= v and v <= arr[i+1]" correctly?

--matrixCol :: Expr -> Expr -> Expr
--matrixCol array col = matrixColHlpr (Matrix [[array]]) col []
--FIXME: implementing [] as an expression???

--matrixColHlpr :: Expr -> Expr -> [Expr] -> Expr
--matrixColHlpr a c strt = [Index a c] : strt
--Work in progress...?^
--[1, 3]
--[4, 5]

---

{-interpY-}

iVal, x_z_1, y_z_1, x_z_2, y_z_2, j, k, y_2Expr, y_1Expr, interpY, interpZ :: Expr

iVal    = FCall (indInSeq)   [C z_array, C z]
x_z_1   = FCall (matrixCol)  [C x_array, iVal]
y_z_1   = FCall (matrixCol)  [C y_array, iVal]
x_z_2   = FCall (matrixCol)  [C x_array, iVal + 1]
y_z_2   = FCall (matrixCol)  [C y_array, iVal + 1]
j       = FCall (indInSeq)   [x_z_1, C x]
k       = FCall (indInSeq)   [x_z_2, C x]
y_1Expr = FCall (lin_interp) [(Index x_z_1 j), (Index y_z_1 j), (Index x_z_1 (j+1)), (Index y_z_1 (j+1)), C x]
y_2Expr = FCall (lin_interp) [(Index x_z_2 k), (Index y_z_2 k), (Index x_z_2 (k+1)), (Index y_z_2 (k+1)), C x]
interpY = FCall (lin_interp) [(Index (C z_array) iVal), y_1Expr, (Index (C z_array) (iVal+1)), y_2Expr, C z]
interpZ = FCall (lin_interp) [y_1Expr, (Index (C z_array) (C i)), y_2Expr, (Index (C z_array) (iVal+1)), C y]
--FIXME: "for i in range(len(z_array) - 1):" implementation from 'interpZ'?