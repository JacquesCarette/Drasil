module Drasil.GlassBR.Interpolation where

import Language.Drasil
import Drasil.GlassBR.Unitals(char_weight)

--y_interp :: QDefinition
--y_interp = fromEqn' "y_interp" (nounPhraseSP "interpolated y value") lY lin_interp

v_y_2, v_y_1, v_x_2, v_x_1, v_x :: VarChunk
v_y_1  = makeVC "y_1"    (nounPhraseSP "y1")   (sub (lY) (Atomic "1"))
v_y_2  = makeVC "y_2"    (nounPhraseSP "y2")   (sub (lY) (Atomic "2"))
v_x_1  = makeVC "x_1"    (nounPhraseSP "x1")   (sub (lX) (Atomic "1"))
v_x_2  = makeVC "x_2"    (nounPhraseSP "x2")   (sub (lX) (Atomic "2"))
v_x    = makeVC "x"      (nounPhraseSP "x")    lX -- = params.wtnt from mainFun.py

v_i, v_j, v_k, v_z, v_z_array, v_y_array, v_x_array, v_y, v_arr :: VarChunk
v_v    = makeVC "v"          (nounPhraseSP "v")       lV
v_i    = makeVC "i"          (nounPhraseSP "i")       lI
v_j    = makeVC "j"          (nounPhraseSP "j")       lJ
v_k    = makeVC "k"          (nounPhraseSP "k")       lK
v_z    = makeVC "z"          (nounPhraseSP "z")       lZ
v_z_array = makeVC "z_array" (nounPhraseSP "z_array") (sub (lZ) (Atomic "array"))
v_y_array = makeVC "y_array" (nounPhraseSP "y_array") (sub (lY) (Atomic "array"))
v_x_array = makeVC "x_array" (nounPhraseSP "x_array") (sub (lX) (Atomic "array"))
v_y    = makeVC "y"          (nounPhraseSP "y")       lY
v_arr  = makeVC "arr"        (nounPhraseSP "arr")     (Atomic "arr") --FIXME: temporary variable for indInSeq?
v_x_z_1   = makeVC "x_z_1"   (nounPhraseSP "x_z_1")     (Atomic "x_z_1")
v_y_z_1   = makeVC "y_z_1"   (nounPhraseSP "y_z_1")     (Atomic "y_z_1")
v_x_z_2   = makeVC "x_z_2"   (nounPhraseSP "x_z_2")     (Atomic "x_z_2")
v_y_z_2   = makeVC "y_z_2"   (nounPhraseSP "y_z_2")     (Atomic "y_z_2")

--Python code to Expr

linInterp :: FuncDef
linInterp = funcDef "lin_interp" [v_x_1, v_y_1, v_x_2, v_y_2, v_x] Rational 
  [ FRet $ (((C v_y_2) - (C v_y_1)) / ((C v_x_2) - (C v_x_1))) * ((C v_x) - (C v_x_1)) + (C v_y_1) ]

indInSeq :: FuncDef
indInSeq = funcDef "indInSeq" [v_arr, v_v] Rational 
  [
    ffor (v_i) (C v_i :< Len (C v_arr))
      [ FCond (((Index (C v_arr) (C v_i)) :<= (C v_v)) :&& ((C v_v) :<= (Index (C v_arr) ((C v_i) + 1)))) [ FRet $ C v_i ] [] ],
    FThrow "Bound error"      
  ]
--indInSeq = (Index (C arr) (C i)) :<= (C i) :<= (Index (C arr) (C i))
--FIXME: captured constraints "arr[i] <= v and v <= arr[i+1]" correctly?

matrixCol :: FuncDef
matrixCol = funcDef "matrixCol" ([] :: [VarChunk]) Rational []
--FIXME: implementing [] as an expression???

--matrixColHlpr :: Expr -> Expr -> [Expr] -> Expr
--matrixColHlpr a c strt = [Index a c] : strt
--Work in progress...?^
--[1, 3]
--[4, 5]

---

{-interpY-}

--x_z_1, y_z_1, x_z_2, y_z_2, j, k, y_2Expr, y_1Expr, interpY, interpZ :: Expr
--x_z_1   = FCall (matrixCol)  [C x_array, C i]
--y_z_1   = FCall (matrixCol)  [C y_array, C i]
--x_z_2   = FCall (matrixCol)  [C x_array, (C i) + 1]
--y_z_2   = FCall (matrixCol)  [C y_array, (C i) + 1]
--j       = FCall (indInSeq)   [x_z_1, C x]
--k       = FCall (indInSeq)   [x_z_2, C x]
--y_1Expr = FCall (lin_interp) [(Index x_z_1 j), (Index y_z_1 j), (Index x_z_1 (j+1)), (Index y_z_1 (j+1)), C x]
--y_2Expr = FCall (lin_interp) [(Index x_z_2 k), (Index y_z_2 k), (Index x_z_2 (k+1)), (Index y_z_2 (k+1)), C x]
--interpY = FCall (lin_interp) [(Index (C z_array) iVal), y_1Expr, (Index (C z_array) (iVal+1)), y_2Expr, C z]
--interpZ = FCall (lin_interp) [y_1Expr, (Index (C z_array) (C i)), y_2Expr, (Index (C z_array) (iVal+1)), C y]
--FIXME: "for i in range(len(z_array) - 1):" implementation from 'interpZ'?

interpY :: FuncDef
interpY = funcDef "interpY" [v_x_array, v_y_array, v_z_array, v_x, v_z] Rational 
  [
    fasg v_i (FCall (asExpr indInSeq) [C v_z_array, C v_z]),
    fasg v_x_z_1 (FCall (asExpr matrixCol) [C v_x_array, C v_i]),
    fasg v_y_z_1 (FCall (asExpr matrixCol) [C v_y_array, C v_i]),
    fasg v_x_z_2 (FCall (asExpr matrixCol) [C v_x_array, (C v_i) + 1]),
    fasg v_y_z_2 (FCall (asExpr matrixCol) [C v_y_array, (C v_i) + 1]),
    FTry 
      [ fasg v_j (FCall (asExpr indInSeq) [C v_x_z_1, C v_x]),
        fasg v_k (FCall (asExpr indInSeq) [C v_x_z_2, C v_x]) ]
      [ FThrow "Interpolation of y failed" ],
    fasg v_y_1 (FCall (asExpr linInterp) [ Index (C v_x_z_1) (C v_j), 
                                           Index (C v_y_z_1) (C v_j),
                                           Index (C v_x_z_1) ((C v_j) + 1), 
                                           Index (C v_y_z_1) ((C v_j) + 1),
                                           C v_x ]),
    fasg v_y_2 (FCall (asExpr linInterp) [ Index (C v_x_z_2) (C v_k), 
                                           Index (C v_y_z_2) (C v_k),
                                           Index (C v_x_z_2) ((C v_k) + 1), 
                                           Index (C v_y_z_2) ((C v_k) + 1),
                                           C v_x ]),
    FRet (FCall (asExpr linInterp) [ Index (C v_z_array) (C v_i),
                                     C v_y_1,
                                     Index (C v_z_array) ((C v_i) + 1),
                                     C v_y_2,
                                     C v_z ] )                                  
  ]  
  
interpMod :: ModDef
interpMod = ModDef "Interpolation" [linInterp, indInSeq, interpY]


-- hack  (more so than the rest of the module!)
asExpr :: FuncDef -> Expr
asExpr (FuncDef n _ _ _) = C $ makeVC n (nounPhraseSP n) (Atomic n)