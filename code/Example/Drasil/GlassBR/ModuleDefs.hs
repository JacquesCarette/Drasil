module Drasil.GlassBR.ModuleDefs where

import Language.Drasil

import Drasil.GlassBR.Unitals (plate_len, plate_width, nom_thick,
  glass_type, char_weight, tNT, sdx, sdy, sdz, pb_tol)

--from TSD.txt:

read_table :: Func
read_table = funcData "read_table" $
  [ singleLine (repeated [junk, listEntry [WithPattern] v_z_array]) ',',
    multiLine (repeated [listEntry [WithLine, WithPattern] v_x_array, 
                         listEntry [WithLine, WithPattern] v_y_array]) ','
  ]
  
readTableMod :: Mod
readTableMod = packmod "ReadTable" [read_table]

-----

--from defaultInput.txt:

inputMod :: Mod
inputMod = packmod "InputFormat" [glassInputData]

glassInputData :: Func
glassInputData = funcData "get_input" $
  [ junkLine,
    singleton plate_len, singleton plate_width, singleton nom_thick,
    junkLine,
    singleton glass_type, 
    junkLine,
    singleton char_weight, 
    junkLine, 
    singleton tNT, 
    junkLine,
    singleton sdx, singleton sdy, singleton sdz,
    junkLine,
    singleton pb_tol
  ]

  
one, two :: Symbol
one = Atomic "1"
two = Atomic "2"  

v_y_2, v_y_1, v_x_2, v_x_1, v_x :: VarChunk
v_y_1  = implVar "v_y_1"    (nounPhraseSP "y1")   (sub lY one) Real
v_y_2  = implVar "v_y_2"    (nounPhraseSP "y2")   (sub lY two) Real
v_x_1  = implVar "v_x_1"    (nounPhraseSP "x1")   (sub lX one) Real
v_x_2  = implVar "v_x_2"    (nounPhraseSP "x2")   (sub lX two) Real
v_x    = implVar "v_x"      (nounPhraseSP "x")    lX           Real -- = params.wtnt from mainFun.py

v_v, v_x_z_1, v_y_z_1, v_x_z_2, v_y_z_2, v_mat, v_col,
  v_i, v_j, v_k, v_z, v_z_array, v_y_array, v_x_array, v_y, v_arr, v_filename :: VarChunk
v_v       = implVar "v_v"          (nounPhraseSP "v")       lV  Real
v_i       = implVar "v_i"          (nounPhraseSP "i")       lI  Natural
v_j       = implVar "v_j"          (nounPhraseSP "j")       lJ  Natural
v_k       = implVar "v_k"          (nounPhraseSP "k")       (sub lK two) Natural -- k breaks things until we start using ids
                                                                          -- in codegen (after refactor end of August)
v_z       = implVar "v_z"          (nounPhraseSP "z")       lZ  Real
v_z_array = implVar "v_z_array" (nounPhraseSP "z_array") (sub lZ (Atomic "array")) (Vect Real)
v_y_array = implVar "v_y_array" (nounPhraseSP "y_array") (sub lY (Atomic "array")) (Vect $ Vect Real)
v_x_array = implVar "v_x_array" (nounPhraseSP "x_array") (sub lX (Atomic "array")) (Vect $ Vect Real)
v_y       = implVar "v_y"          (nounPhraseSP "y")       lY Real
v_arr     = implVar "v_arr"        (nounPhraseSP "arr")     (Atomic "arr") (Vect Real)--FIXME: temporary variable for indInSeq?
v_x_z_1   = implVar "v_x_z_1"   (nounPhraseSP "x_z_1")     (sub lX (sub lZ one)) (Vect Real)
v_y_z_1   = implVar "v_y_z_1"   (nounPhraseSP "y_z_1")     (sub lY (sub lZ one)) (Vect Real)
v_x_z_2   = implVar "v_x_z_2"   (nounPhraseSP "x_z_2")     (sub lX (sub lZ two)) (Vect Real)
v_y_z_2   = implVar "v_y_z_2"   (nounPhraseSP "y_z_2")     (sub lY (sub lZ two)) (Vect Real)
v_mat     = implVar "v_mat"     (nounPhraseSP "mat")       (Atomic "mat") (Vect $ Vect Real)
v_col     = implVar "v_col"     (nounPhraseSP "col")       (Atomic "col") (Vect Real)
v_filename= implVar "v_filename" (nounPhraseSP "filename") (Atomic "filename") String

linInterp :: Func
linInterp = funcDef "lin_interp" [v_x_1, v_y_1, v_x_2, v_y_2, v_x] Real 
  [ FRet $ (((sy v_y_2) - (sy v_y_1)) / ((sy v_x_2) - (sy v_x_1))) * ((sy v_x) - (sy v_x_1)) + (sy v_y_1) ]

indInSeq :: Func
indInSeq = funcDef "indInSeq" [v_arr, v_v] Natural 
  [
    ffor (v_i) (sy v_i $< (dim (sy v_arr) - 1))
      [ FCond (((idx (sy v_arr) (sy v_i)) $<= (sy v_v)) $&& ((sy v_v) $<= (idx (sy v_arr) ((sy v_i) + 1)))) [ FRet $ sy v_i ] [] ],
    FThrow "Bound error"      
  ]

matrixCol :: Func
matrixCol = funcDef "matrixCol" [v_mat, v_j] (Vect Real) 
  [
    fdec v_col (Vect Rational),
    ffor (v_i) (sy v_i $< dim (sy v_mat)) [ FAppend (sy v_col) (idx (idx (sy v_mat) (sy v_i)) (sy v_j)) ],
    FRet (sy v_col)
  ]

interpY :: Func
interpY = funcDef "interpY" [{-v_x_array, v_y_array, v_z_array,-} v_filename, v_x, v_z] Real
  [
    -- hack
  fdec v_x_array (Vect $ Vect Rational),
  fdec v_y_array (Vect $ Vect Rational),
  fdec v_z_array (Vect Rational),
  FProcCall read_table [sy v_filename, sy v_z_array, sy v_x_array, sy v_y_array],
  -- endhack
    fasg v_i (apply2 (asVC indInSeq) v_z_array v_z),
    fasg v_x_z_1 (apply2 (asVC matrixCol) v_x_array v_i),
    fasg v_y_z_1 (apply2 (asVC matrixCol) v_y_array v_i),
    fasg v_x_z_2 (apply (asExpr matrixCol) [sy v_x_array, (sy v_i) + 1]),
    fasg v_y_z_2 (apply (asExpr matrixCol) [sy v_y_array, (sy v_i) + 1]),
    FTry 
      [ fasg v_j (apply2 (asVC indInSeq) v_x_z_1 v_x),
        fasg v_k (apply2 (asVC indInSeq) v_x_z_2 v_x) ]
      [ FThrow "Interpolation of y failed" ],
    fasg v_y_1 (apply (asExpr linInterp) [ idx (sy v_x_z_1) (sy v_j), 
                                           idx (sy v_y_z_1) (sy v_j),
                                           idx (sy v_x_z_1) ((sy v_j) + 1), 
                                           idx (sy v_y_z_1) ((sy v_j) + 1),
                                           sy v_x ]),
    fasg v_y_2 (apply (asExpr linInterp) [ idx (sy v_x_z_2) (sy v_k), 
                                           idx (sy v_y_z_2) (sy v_k),
                                           idx (sy v_x_z_2) ((sy v_k) + 1), 
                                           idx (sy v_y_z_2) ((sy v_k) + 1),
                                           sy v_x ]),
    FRet (apply (asExpr linInterp) [ idx (sy v_z_array) (sy v_i),
                                     sy v_y_1,
                                     idx (sy v_z_array) ((sy v_i) + 1),
                                     sy v_y_2,
                                     sy v_z ] )                                  
  ]  
  
interpZ :: Func
interpZ = funcDef "interpZ" [{-v_x_array, v_y_array, v_z_array,-} v_filename, v_x, v_y] Real
  [
    -- hack
  fdec v_x_array (Vect $ Vect Rational),
  fdec v_y_array (Vect $ Vect Rational),
  fdec v_z_array (Vect Rational),
  FProcCall read_table [sy v_filename, sy v_z_array, sy v_x_array, sy v_y_array],
  -- endhack
    ffor v_i (sy v_i $< (dim (sy v_z_array) - 1)) 
      [
        fasg v_x_z_1 (apply2 (asVC matrixCol) v_x_array v_i),
        fasg v_y_z_1 (apply2 (asVC matrixCol) v_y_array v_i),
        fasg v_x_z_2 (apply (asExpr matrixCol) [sy v_x_array, (sy v_i) + 1]),
        fasg v_y_z_2 (apply (asExpr matrixCol) [sy v_y_array, (sy v_i) + 1]),
        FTry 
          [ fasg v_j (apply2 (asVC indInSeq) v_x_z_1 v_x),
            fasg v_k (apply2 (asVC indInSeq) v_x_z_2 v_x) ]
          [ FContinue ],
        fasg v_y_1 (apply (asExpr linInterp) [ idx (sy v_x_z_1) (sy v_j), 
                                               idx (sy v_y_z_1) (sy v_j),
                                               idx (sy v_x_z_1) ((sy v_j) + 1), 
                                               idx (sy v_y_z_1) ((sy v_j) + 1),
                                               sy v_x ]),
        fasg v_y_2 (apply (asExpr linInterp) [ idx (sy v_x_z_2) (sy v_k), 
                                               idx (sy v_y_z_2) (sy v_k),
                                               idx (sy v_x_z_2) ((sy v_k) + 1), 
                                               idx (sy v_y_z_2) ((sy v_k) + 1),
                                               sy v_x ]),
        FCond ((sy v_y_1 $<= sy v_y) $&& (sy v_y $<= sy v_y_2))
          [ FRet (apply (asExpr linInterp) [ sy v_y_1,
                                             idx (sy v_z_array) (sy v_i),
                                             sy v_y_2,
                                             idx (sy v_z_array) ((sy v_i) + 1),
                                             sy v_y ] )  
          ] []                                             
      ],
    FThrow "Interpolation of z failed"      
  ]

interpMod :: Mod
interpMod = packmod "Interpolation" $ [linInterp, indInSeq, matrixCol, interpY, interpZ]
