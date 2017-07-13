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

w_j, s_j, q_j, q_1, q_2, i, k :: VarChunk
w_j  = makeVC "w_j"    (nounPhraseSP "wj")   (sub (lW) (lJ))
s_j  = makeVC "s_j"    (nounPhraseSP "sj")   (sub (lS) (lJ))
q_j  = makeVC "q_j"    (nounPhraseSP "qj")   (sub (lQ) (lJ))
q_1  = makeVC "q_1"    (nounPhraseSP "q_1")  (sub (lQ) (Atomic "1"))
q_2  = makeVC "q_2"    (nounPhraseSP "q_2")  (sub (lQ) (Atomic "2"))
i    = makeVC "i"      (nounPhraseSP "i")    lI
k    = makeVC "k"      (nounPhraseSP "k")    lK
v    = makeVC "v"      (nounPhraseSP "v")    lV

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
--FIXME: implementing matrix as an expression???

--interpY :: Expr
--interpY = lin_interp (z_array[i], y_1, z_array[i+1], y_2, z)

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