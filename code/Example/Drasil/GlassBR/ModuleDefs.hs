-- Convention used below:
-- when 'name' and 'nameCT' both appear, 'name' is the Haskell function and
-- 'nameCT' is the "Code Template" that 'name' builds.

module Drasil.GlassBR.ModuleDefs (implVars, allMods) where

import Language.Drasil

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

-- Given two points (x1,y1) and (x2,y2), return the slope of the line going through them
slope :: (Num a, Fractional a) => (a, a) -> (a, a) -> a
slope (x1,y1) (x2,y2) = (y2 - y1) / (x2 - x1)

-- Given two points (x1,y1) and (x2,y2), and an x ordinate, return
-- extrapoled y on the straight line in between
onLine :: (Num a, Fractional a) => (a, a) -> (a, a) -> a -> a
onLine p1@(x1,y1) p2 x_ = 
  let m = slope p1 p2 in
  m * (x_ - x1) + y1

------------------------------------------------------------------------------------------
-- Code Template helper functions

vLook :: (HasSymbol a, HasSymbol i, HasUID a, HasUID i) => a -> i -> Expr -> Expr
vLook a i_ p = idx (sy a) (sy i_ + p)

aLook :: (HasSymbol a, HasSymbol i, HasSymbol j, HasUID a, HasUID i, HasUID j) =>
  a -> i -> j -> Expr
aLook a i_ j_ = idx (idx (sy a) (sy i_)) (sy j_)

getCol :: (HasSymbol a, HasSymbol i, HasUID a, HasUID i) => a -> i -> Expr -> Expr
getCol a_ i_ p = apply (asExpr extractColumnCT) [sy a_, sy i_ + p]

call :: Func -> [VarChunk] -> FuncStmt
call f l = FProcCall f $ map sy l

find :: (HasUID zv, HasUID z, HasSymbol zv, HasSymbol z) => zv -> z -> Expr
find zv z_ = apply (asExpr findCT) [sy zv, sy z_]

linInterp :: [Expr] -> Expr
linInterp = apply (asExpr linInterpCT)

interpOver :: (HasUID ptx, HasUID pty, HasUID ind, HasUID vv,
  HasSymbol ptx, HasSymbol pty, HasSymbol ind, HasSymbol vv) =>
  ptx -> pty -> ind -> vv -> [Expr]
interpOver ptx pty ind vv =
  [ vLook ptx ind 0, vLook pty ind 0
  , vLook ptx ind 1, vLook pty ind 1
  , sy vv ]
------------------------------------------------------------------------------------------
-- Code Templates

-- Note how this one uses a semantic function in its body
-- But it is also 'wrong' in the sense that it assumes x_1 <= x <= x_2
linInterpCT :: Func
linInterpCT = funcDef "lin_interp" [x_1, y_1, x_2, y_2, x] Real
  [ FRet $ onLine (sy x_1, sy y_1) (sy x_2, sy y_2) (sy x) ]

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
    i     $:= find z_vector z,
    x_z_1 $:= getCol x_matrix i 0,
    y_z_1 $:= getCol y_matrix i 0,
    x_z_2 $:= getCol x_matrix i 1,
    y_z_2 $:= getCol y_matrix i 1,
    FTry
      [ j $:= find x_z_1 x,
        k $:= find x_z_2 x ]
      [ FThrow "Interpolation of y failed" ],
    y_1 $:= (linInterp $ interpOver x_z_1 y_z_1 j x),
    y_2 $:= (linInterp $ interpOver x_z_2 y_z_2 k x),
    FRet $ linInterp [ vLook z_vector i 0, sy y_1, vLook z_vector i 1, sy y_2, sy z ]
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
        x_z_1 $:= getCol x_matrix i 0,
        y_z_1 $:= getCol y_matrix i 0,
        x_z_2 $:= getCol x_matrix i 1,
        y_z_2 $:= getCol y_matrix i 1,
        FTry
          [ j $:= find x_z_1 x,
            k $:= find x_z_2 x ]
          [ FContinue ],
        y_1 $:= (linInterp $ interpOver x_z_1 y_z_1 j x),
        y_2 $:= (linInterp $ interpOver x_z_2 y_z_2 k x),
        FCond ((sy y_1 $<= sy y) $&& (sy y $<= sy y_2))
          [ FRet $ linInterp [ sy y_1, vLook z_vector i 0, sy y_2, vLook z_vector i 1, sy y ]
          ] []
      ],
    FThrow "Interpolation of z failed"
  ]

interpMod :: Mod
interpMod = packmod "Interpolation" [linInterpCT, findCT, extractColumnCT, interpY, interpZ]
