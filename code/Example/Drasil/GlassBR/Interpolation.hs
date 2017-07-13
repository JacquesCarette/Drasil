module Drasil.GlassBR.Interpolation where

import Language.Drasil
import Drasil.GlassBR.Unitals(char_weight)

interpData :: [QDefinition]
interpData = [y_interp]

y_interp :: QDefinition
y_interp = fromEqn' "y_interp" (nounPhraseSP "interpolated y value") lY lin_interp

lin_interp :: Expr
lin_interp = (((C y_2) - (C y_1)) / ((C x_2) - (C x_1))) * ((C x) - (C x_1)) + (C y_1)

y_2, y_1, x_2, x_1, x :: VarChunk
y_1  = makeVC "y_1"    (nounPhraseSP "y1")   (sub (lY) (Atomic "1"))
y_2  = makeVC "y_2"    (nounPhraseSP "y2")   (sub (lY) (Atomic "2"))
x_1  = makeVC "x_1"    (nounPhraseSP "x1")   (sub (lX) (Atomic "1"))
x_2  = makeVC "x_2"    (nounPhraseSP "x2")   (sub (lX) (Atomic "2"))
x    = makeVC "x"    	 (nounPhraseSP "x")    lX -- = params.wtnt from mainFun.py

w_j, s_j, q_j, q_1, q_2, i, v, z, z_array, y_array, x_array :: VarChunk
w_j  = makeVC "w_j"    (nounPhraseSP "wj")   (sub (lW) (lJ))
s_j  = makeVC "s_j"    (nounPhraseSP "sj")   (sub (lS) (lJ))
q_j  = makeVC "q_j"    (nounPhraseSP "qj")   (sub (lQ) (lJ))
q_1  = makeVC "q_1"    (nounPhraseSP "q_1")  (sub (lQ) (Atomic "1"))
q_2  = makeVC "q_2"    (nounPhraseSP "q_2")  (sub (lQ) (Atomic "2"))
i    = makeVC "i"      (nounPhraseSP "i")    lI
--k    = makeVC "k"      (nounPhraseSP "k")    lK
v    = makeVC "v"      (nounPhraseSP "v")    lV
z    = makeVC "z"      (nounPhraseSP "z")    lZ
z_array = makeVC "z_array" (nounPhraseSP "z_array") (sub (lZ) (Atomic "array"))
y_array = makeVC "y_array" (nounPhraseSP "y_array") (sub (lY) (Atomic "array"))
x_array = makeVC "x_array" (nounPhraseSP "x_array") (sub (lX) (Atomic "array"))

---
{-
--indInSeq :: Int -> [Double] -> Double -> Maybe _?
indInSeq (length(arr)-1) _   v = Nothing --FIXME: raise BoundError?
indInSeq n               arr v = Just
  (do
      if ((arr!!(n) <= v) && (v <= arr!!(n+1)))
        then return n --FIXME:same result as interp.py?
      indInSeq (n+1) arr v)
--input n should be 0

matrixCol :: (num t1 , Eq t1) => t1 -> ([t1] -> [t] -> a) -> t -> [a] -> [[a]]
matrixCol (length mat) _   _ currentList = return currentList
matrixCol i            mat c currentList = 
  do
    mat[i][c] ++ currentList
    matrixCol (i-1) (mat) (c) (currentList)
--start currentList = []; input i should be 0
-}

---> Python code to Expr

indInSeq :: Expr
indInSeq = (C i)
--FIXME: how to capture constraints "arr[i] <= v and v <= arr[i+1]"

matrixCol :: Expr
matrixCol = 0
--FIXME: implementing [] as an expression???

iVal, x_z_1, y_z_1, x_z_2, y_z_2, y_2Expr, y_2Expr :: Expr
interpY :: Expr

iVal = (FCall (indInSeq) [C z_array, C z])
x_z_1 = (FCall (matrixCol)[C x_array, C i]){-(C x_array, C i) -> leads to (Expr, Expr) -> error since FCall expects an Expr-}
y_z_1 = (FCall (matrixCol)[C y_array, C i])
x_z_2 = (FCall (matrixCol)[C x_array, (C i) + 1])
y_z_2 = (FCall (matrixCol)[C y_array, (C i) + 1])
j = FCall (indInSeq) [x_z_1, C x]
k = FCall (indInSeq) [x_z_2, C x]
y_1Expr = FCall (lin_interp) [x_z_1{-[C j]-},      y_z_1{-[C j]-}, x_z_1{-[C j + 1] -},       y_z_1{-[C j + 1] -}, C x]
y_2Expr = FCall (lin_interp) [x_z_2{-[k]-},        y_z_2{-[C k]-}, x_z_2{-[k + 1]-},          y_z_2{-[k + 1]-},    C x]
interpY = FCall (lin_interp) [C z_array {-!!(i)-}, y_1Expr,        C z_array{-!!(iVal + 1)-}, y_2Expr,             C z]

---

index1, index2, data_, value :: VarChunk
index1 = makeVC "index1"    (nounPhraseSP "index1")   (Atomic "index1")
index2 = makeVC "index2"    (nounPhraseSP "index2")   (Atomic "index2")
data_  = makeVC "data_"     (nounPhraseSP "data")     (Atomic "data")
value  = makeVC "value"     (nounPhraseSP "value")    (Atomic "value")

---

find_bounds :: Expr
find_bounds = 0

data1, data2, value1, value2 :: VarChunk
data1  = makeVC "data1"     (nounPhraseSP "data1")    (Atomic "data1")
data1  = makeVC "data2"     (nounPhraseSP "data2")    (Atomic "data2")
value1 = makeVC "value1"    (nounPhraseSP "value1")   (Atomic "value1")
value2 = makeVC "value2"    (nounPhraseSP "value2")   (Atomic "value2")

---

interp :: Expr
interp = 0

idx, jdx, kdx, num_interp1, num_interp2, data3 :: VarChunk
idx    = makeVC "idx"     (nounPhraseSP "idx")   (sub lI (Atomic "dx"))
jdx    = makeVC "jdx"     (nounPhraseSP "jdx")   (sub lJ (Atomic "dx"))
kdx    = makeVC "kdx"     (nounPhraseSP "kdx")   (sub lK (Atomic "dx"))