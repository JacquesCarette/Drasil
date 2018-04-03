-- Convention used below:
-- when 'name' and 'nameCT' both appear, 'name' is the Haskell function and
-- 'nameCT' is the "Code Template" that 'name' builds.

module Drasil.GlassBR.ModuleDefs (implVars, allMods) where

import Language.Drasil hiding (a_)

import Drasil.GlassBR.Unitals (plate_len, plate_width, nom_thick,
  glass_type, char_weight, tNT, sdx, sdy, sdz, pb_tol)

allMods :: [Mod]
allMods = [readTableMod, inputMod, interpMod]

-- It's a bit odd that this has to be explicitly built here...
implVars :: [VarChunk]
implVars = [v, x_z_1, y_z_1, x_z_2, y_z_2, mat, col,
  i, j, k, z, z_vector, y_matrix, x_matrix, y, arr, filename,
  y_2, y_1, x_2, x_1, x]

--from TSD.txt:

read_table :: Func
read_table = funcData "read_table" $
  [ singleLine (repeated [junk, listEntry [WithPattern] z_vector]) ',',
    multiLine (repeated [listEntry [WithLine, WithPattern] x_matrix, 
                         listEntry [WithLine, WithPattern] y_matrix]) ','
  ]
  
readTableMod :: Mod
readTableMod = packmod "ReadTable" [read_table]

-----

--from defaultInput.txt:

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

inputMod :: Mod
inputMod = packmod "InputFormat" [glassInputData]

-----
  
one, two :: Symbol
one = Atomic "1"
two = Atomic "2"  

-- No need to be too verbose
var :: String -> Symbol -> Space -> VarChunk
var nam sym ty = implVar nam (nounPhraseSP nam) sym ty

y_2, y_1, x_2, x_1, x :: VarChunk
y_1  = var "y1"          (sub lY one) Real
y_2  = var "y2"          (sub lY two) Real
x_1  = var "x1"          (sub lX one) Real
x_2  = var "x2"          (sub lX two) Real
x    = var "x"            lX          Real -- = params.wtnt from mainFun.py

v, x_z_1, y_z_1, x_z_2, y_z_2, mat, col,
  i, j, k, z, z_vector, y_matrix, x_matrix, y, arr, filename :: VarChunk
v       = var "v"         lV                          Real
i       = var "i"         lI                          Natural
j       = var "j"         lJ                          Natural
k       = var "k"         lK                          Natural
y       = var "y"         lY                          Real
z       = var "z"         lZ                          Real
z_vector = var "z_vector" (sub lZ (Atomic "vector")) (Vect Real)
y_matrix = var "y_matrix" (sub lY (Atomic "matrix")) (Vect $ Vect Real)
x_matrix = var "x_matrix" (sub lX (Atomic "matrix")) (Vect $ Vect Real)
arr     = var "arr"       (Atomic "arr")             (Vect Real)--FIXME: temporary variable for findCT?
x_z_1   = var "x_z_1"     (sub lX (sub lZ one))      (Vect Real)
y_z_1   = var "y_z_1"     (sub lY (sub lZ one))      (Vect Real)
x_z_2   = var "x_z_2"     (sub lX (sub lZ two))      (Vect Real)
y_z_2   = var "y_z_2"     (sub lY (sub lZ two))      (Vect Real)
mat     = var "mat"       (Atomic "mat")             (Vect $ Vect Real)
col     = var "col"       (Atomic "col")             (Vect Real)
filename= var "filename"  (Atomic "filename")         String

------------------------------------------------------------------------------------------
--
-- Some semantic functions

-- Given two points (x1,y1) and (x2,y2), and an x ordinate, return
-- interpolated y on the straight line in between
interp :: (Num a, Fractional a) => (a, a) -> (a, a) -> a -> a
interp (x1,y1) (x2,y2) x_ = ((y2 - y1) / (x2 - x1)) * (x_ - x1) + y1

------------------------------------------------------------------------------------------
-- Code Template helper functions

vLook :: (HasSymbol a, HasSymbol i, Chunk a, Chunk i) => a -> i -> Expr -> Expr
vLook a i_ p = idx (sy a) (sy i_ + p)

aLook :: (HasSymbol a, HasSymbol i, HasSymbol j, Chunk a, Chunk i, Chunk j) => 
  a -> i -> j -> Expr
aLook a i_ j_ = idx (idx (sy a) (sy i_)) (sy j_)

getCol :: (HasSymbol a, HasSymbol i, Chunk a, Chunk i) => a -> i -> Expr -> Expr
getCol a_ i_ p = apply (asExpr extractColumnCT) [sy a_, sy i_ + p]

call :: Func -> [VarChunk] -> FuncStmt
call f l = FProcCall f $ map sy l

find :: (Chunk zv, Chunk z, HasSymbol zv, HasSymbol z) => zv -> z -> Expr
find zv z = apply (asExpr findCT) [sy zv, sy z]

------------------------------------------------------------------------------------------
-- Code Templates

-- Note how this one uses a semantic function in its body
linInterpCT :: Func
linInterpCT = funcDef "lin_interp" [x_1, y_1, x_2, y_2, x] Real 
  [ FRet $ interp (sy x_1, sy y_1) (sy x_2, sy y_2) (sy x) ]

findCT :: Func
findCT = funcDef "find" [arr, v] Natural 
  [
    ffor i (sy i $< (dim (sy arr) - 1))
      [ FCond ((vLook arr i 0 $<= (sy v)) $&& ((sy v) $<= vLook arr i 1))
        [ FRet $ sy i ] [] ],
    FThrow "Bound error"      
  ]

extractColumnCT :: Func
extractColumnCT = funcDef "extractColumn" [mat, j] (Vect Real) 
  [
    fdec col,
    --
    ffor i (sy i $< dim (sy mat)) 
      [ FAppend (sy col) (aLook mat i j) ],
    FRet (sy col)
  ]

interpY :: Func
interpY = funcDef "interpY" [filename, x, z] Real
  [
  -- hack
  fdec x_matrix,
  fdec y_matrix,
  fdec z_vector,
  --
  call read_table [filename, z_vector, x_matrix, y_matrix],
  -- endhack
    i     $:= (apply (asExpr findCT) [sy z_vector, sy z]),
    x_z_1 $:= getCol x_matrix i 0,
    y_z_1 $:= getCol y_matrix i 0,
    x_z_2 $:= getCol x_matrix i 1,
    y_z_2 $:= getCol y_matrix i 1,
    FTry 
      [ j $:= (apply2 (asVC findCT) x_z_1 x),
        k $:= (apply2 (asVC findCT) x_z_2 x) ]
      [ FThrow "Interpolation of y failed" ],
    y_1 $:= (apply (asExpr linInterpCT) [ vLook x_z_1 j 0,
                                        vLook y_z_1 j 0,
                                        vLook x_z_1 j 1,
                                        vLook y_z_1 j 1,
                                        sy x ]),
    y_2 $:= (apply (asExpr linInterpCT) [ vLook x_z_2 k 0,
                                        vLook y_z_2 k 0,
                                        vLook x_z_2 k 1,
                                        vLook y_z_2 k 1,
                                        sy x ]),
    FRet (apply (asExpr linInterpCT) [ vLook z_vector i 0,
                                     sy y_1,
                                     vLook z_vector i 1,
                                     sy y_2,
                                     sy z ] )                                  
  ]  
  
interpZ :: Func
interpZ = funcDef "interpZ" [filename, x, y] Real
  [
    -- hack
  fdec x_matrix,
  fdec y_matrix,
  fdec z_vector,
  --
  call read_table [filename, z_vector, x_matrix, y_matrix],
  -- endhack
    ffor i (sy i $< (dim (sy z_vector) - 1)) 
      [
        x_z_1 $:= (apply (asExpr extractColumnCT) [sy x_matrix, sy i]),
        y_z_1 $:= (apply (asExpr extractColumnCT) [sy y_matrix, sy i]),
        x_z_2 $:= (apply (asExpr extractColumnCT) [sy x_matrix, (sy i) + 1]),
        y_z_2 $:= (apply (asExpr extractColumnCT) [sy y_matrix, (sy i) + 1]),
        FTry 
          [ j $:= (apply2 (asVC findCT) x_z_1 x),
            k $:= (apply2 (asVC findCT) x_z_2 x) ]
          [ FContinue ],
        y_1 $:= (apply (asExpr linInterpCT) [ vLook x_z_1 j 0,
                                            vLook y_z_1 j 0,
                                            vLook x_z_1 j 1,
                                            vLook y_z_1 j 1,
                                            sy x ]),
        y_2 $:= (apply (asExpr linInterpCT) [ vLook x_z_2 k 0,
                                            vLook y_z_2 k 0,
                                            vLook x_z_2 k 1,
                                            vLook y_z_2 k 1,
                                            sy x ]),
        FCond ((sy y_1 $<= sy y) $&& (sy y $<= sy y_2))
          [ FRet (apply (asExpr linInterpCT) [ sy y_1,
                                             idx (sy z_vector) (sy i),
                                             sy y_2,
                                             idx (sy z_vector) ((sy i) + 1),
                                             sy y ] )  
          ] []                                             
      ],
    FThrow "Interpolation of z failed"      
  ]


interpMod :: Mod
interpMod = packmod "Interpolation" [linInterpCT, findCT, extractColumnCT, interpY, interpZ]
