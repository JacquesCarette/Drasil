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
v_y_1  = makeVC'' "v_y_1"    (nounPhraseSP "y1")   (sub lY one) Real
v_y_2  = makeVC'' "v_y_2"    (nounPhraseSP "y2")   (sub lY two) Real
v_x_1  = makeVC'' "v_x_1"    (nounPhraseSP "x1")   (sub lX one) Real
v_x_2  = makeVC'' "v_x_2"    (nounPhraseSP "x2")   (sub lX two) Real
v_x    = makeVC'' "v_x"      (nounPhraseSP "x")    lX           Real -- = params.wtnt from mainFun.py

v_v, v_x_z_1, v_y_z_1, v_x_z_2, v_y_z_2, v_mat, v_col,
  v_i, v_j, v_k, v_z, v_z_array, v_y_array, v_x_array, v_y, v_arr, v_filename :: VarChunk
v_v       = makeVC'' "v_v"          (nounPhraseSP "v")       lV  Real
v_i       = makeVC'' "v_i"          (nounPhraseSP "i")       lI  Natural
v_j       = makeVC'' "v_j"          (nounPhraseSP "j")       lJ  Natural
v_k       = makeVC'' "v_k"          (nounPhraseSP "k")       (sub lK two) Natural -- k breaks things until we start using ids
                                                                          -- in codegen (after refactor end of August)
v_z       = makeVC'' "v_z"          (nounPhraseSP "z")       lZ  Real
v_z_array = makeVC'' "v_z_array" (nounPhraseSP "z_array") (sub lZ (Atomic "array")) (Vect Real)
v_y_array = makeVC'' "v_y_array" (nounPhraseSP "y_array") (sub lY (Atomic "array")) (Vect $ Vect Real)
v_x_array = makeVC'' "v_x_array" (nounPhraseSP "x_array") (sub lX (Atomic "array")) (Vect $ Vect Real)
v_y       = makeVC'' "v_y"          (nounPhraseSP "y")       lY Real
v_arr     = makeVC'' "v_arr"        (nounPhraseSP "arr")     (Atomic "arr") (Vect Real)--FIXME: temporary variable for indInSeq?
v_x_z_1   = makeVC'' "v_x_z_1"   (nounPhraseSP "x_z_1")     (sub lX (sub lZ one)) (Vect Real)
v_y_z_1   = makeVC'' "v_y_z_1"   (nounPhraseSP "y_z_1")     (sub lY (sub lZ one)) (Vect Real)
v_x_z_2   = makeVC'' "v_x_z_2"   (nounPhraseSP "x_z_2")     (sub lX (sub lZ two)) (Vect Real)
v_y_z_2   = makeVC'' "v_y_z_2"   (nounPhraseSP "y_z_2")     (sub lY (sub lZ two)) (Vect Real)
v_mat     = makeVC'' "v_mat"     (nounPhraseSP "mat")       (Atomic "mat") (Vect $ Vect Real)
v_col     = makeVC'' "v_col"     (nounPhraseSP "col")       (Atomic "col") (Vect Real)
v_filename= makeVC'' "v_filename" (nounPhraseSP "filename") (Atomic "filename") String

linInterp :: Func
linInterp = funcDef "lin_interp" [v_x_1, v_y_1, v_x_2, v_y_2, v_x] Real 
  [ FRet $ (((C v_y_2) - (C v_y_1)) / ((C v_x_2) - (C v_x_1))) * ((C v_x) - (C v_x_1)) + (C v_y_1) ]

indInSeq :: Func
indInSeq = funcDef "indInSeq" [v_arr, v_v] Natural 
  [
    ffor (v_i) (C v_i $< (dim (C v_arr) - 1))
      [ FCond (((idx (C v_arr) (C v_i)) $<= (C v_v)) $&& ((C v_v) $<= (idx (C v_arr) ((C v_i) + 1)))) [ FRet $ C v_i ] [] ],
    FThrow "Bound error"      
  ]

matrixCol :: Func
matrixCol = funcDef "matrixCol" [v_mat, v_j] (Vect Real) 
  [
    fdec v_col (Vect Rational),
    ffor (v_i) (C v_i $< dim (C v_mat)) [ FAppend (C v_col) (idx (idx (C v_mat) (C v_i)) (C v_j)) ],
    FRet (C v_col)
  ]

interpY :: Func
interpY = funcDef "interpY" [{-v_x_array, v_y_array, v_z_array,-} v_filename, v_x, v_z] Real
  [
    -- hack
  fdec v_x_array (Vect $ Vect Rational),
  fdec v_y_array (Vect $ Vect Rational),
  fdec v_z_array (Vect Rational),
  FProcCall read_table [C v_filename, C v_z_array, C v_x_array, C v_y_array],
  -- endhack
    fasg v_i (FCall (asExpr indInSeq) [C v_z_array, C v_z]),
    fasg v_x_z_1 (FCall (asExpr matrixCol) [C v_x_array, C v_i]),
    fasg v_y_z_1 (FCall (asExpr matrixCol) [C v_y_array, C v_i]),
    fasg v_x_z_2 (FCall (asExpr matrixCol) [C v_x_array, (C v_i) + 1]),
    fasg v_y_z_2 (FCall (asExpr matrixCol) [C v_y_array, (C v_i) + 1]),
    FTry 
      [ fasg v_j (FCall (asExpr indInSeq) [C v_x_z_1, C v_x]),
        fasg v_k (FCall (asExpr indInSeq) [C v_x_z_2, C v_x]) ]
      [ FThrow "Interpolation of y failed" ],
    fasg v_y_1 (FCall (asExpr linInterp) [ idx (C v_x_z_1) (C v_j), 
                                           idx (C v_y_z_1) (C v_j),
                                           idx (C v_x_z_1) ((C v_j) + 1), 
                                           idx (C v_y_z_1) ((C v_j) + 1),
                                           C v_x ]),
    fasg v_y_2 (FCall (asExpr linInterp) [ idx (C v_x_z_2) (C v_k), 
                                           idx (C v_y_z_2) (C v_k),
                                           idx (C v_x_z_2) ((C v_k) + 1), 
                                           idx (C v_y_z_2) ((C v_k) + 1),
                                           C v_x ]),
    FRet (FCall (asExpr linInterp) [ idx (C v_z_array) (C v_i),
                                     C v_y_1,
                                     idx (C v_z_array) ((C v_i) + 1),
                                     C v_y_2,
                                     C v_z ] )                                  
  ]  
  
interpZ :: Func
interpZ = funcDef "interpZ" [{-v_x_array, v_y_array, v_z_array,-} v_filename, v_x, v_y] Real
  [
    -- hack
  fdec v_x_array (Vect $ Vect Rational),
  fdec v_y_array (Vect $ Vect Rational),
  fdec v_z_array (Vect Rational),
  FProcCall read_table [C v_filename, C v_z_array, C v_x_array, C v_y_array],
  -- endhack
    ffor v_i (C v_i $< (dim (C v_z_array) - 1)) 
      [
        fasg v_x_z_1 (FCall (asExpr matrixCol) [C v_x_array, C v_i]),
        fasg v_y_z_1 (FCall (asExpr matrixCol) [C v_y_array, C v_i]),
        fasg v_x_z_2 (FCall (asExpr matrixCol) [C v_x_array, (C v_i) + 1]),
        fasg v_y_z_2 (FCall (asExpr matrixCol) [C v_y_array, (C v_i) + 1]),
        FTry 
          [ fasg v_j (FCall (asExpr indInSeq) [C v_x_z_1, C v_x]),
            fasg v_k (FCall (asExpr indInSeq) [C v_x_z_2, C v_x]) ]
          [ FContinue ],
        fasg v_y_1 (FCall (asExpr linInterp) [ idx (C v_x_z_1) (C v_j), 
                                               idx (C v_y_z_1) (C v_j),
                                               idx (C v_x_z_1) ((C v_j) + 1), 
                                               idx (C v_y_z_1) ((C v_j) + 1),
                                               C v_x ]),
        fasg v_y_2 (FCall (asExpr linInterp) [ idx (C v_x_z_2) (C v_k), 
                                               idx (C v_y_z_2) (C v_k),
                                               idx (C v_x_z_2) ((C v_k) + 1), 
                                               idx (C v_y_z_2) ((C v_k) + 1),
                                               C v_x ]),
        FCond ((C v_y_1 $<= C v_y) $&& (C v_y $<= C v_y_2))
          [ FRet (FCall (asExpr linInterp) [ C v_y_1,
                                             idx (C v_z_array) (C v_i),
                                             C v_y_2,
                                             idx (C v_z_array) ((C v_i) + 1),
                                             C v_y ] )  
          ] []                                             
      ],
    FThrow "Interpolation of z failed"      
  ]

interpMod :: Mod
interpMod = packmod "Interpolation" $ [linInterp, indInSeq, matrixCol, interpY, interpZ]
