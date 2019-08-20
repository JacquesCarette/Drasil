-- Convention used below:
-- when 'name' and 'nameCT' both appear, 'name' is the Haskell function and
-- 'nameCT' is the "Code Template" that 'name' builds.

module Drasil.GlassBR.ModuleDefs (allMods, implVars, interpY, interpZ) where

import Language.Drasil
import Language.Drasil.ShortHands
import Language.Drasil.Code (($:=), Func, FuncStmt(..), Mod, 
  asExpr, funcDef, fdec, ffor, funcData, quantvar, 
  multiLine, packmod, repeated, singleLine)

allMods :: [Mod]
allMods = [readTableMod, interpMod]

-- It's a bit odd that this has to be explicitly built here...
implVars :: [QuantityDict]
implVars = [v, x_z_1, y_z_1, x_z_2, y_z_2, mat, col,
  i, j, k, z, zVector, yMatrix, xMatrix, y, arr, filename,
  y_2, y_1, x_2, x_1, x]

--from TSD.txt:

readTableMod :: Mod
readTableMod = packmod "ReadTable"
  "Provides a function for reading glass ASTM data" [readTable]

readTable :: Func
readTable = funcData "read_table"
  "Reads glass ASTM data from a file with the given file name"
  [ singleLine (repeated [quantvar zVector]) ',',
    multiLine (repeated (map quantvar [xMatrix, yMatrix])) ','
  ]

-----

one, two :: Symbol
one = Integ 1
two = Integ 2

var :: String -> String -> Symbol -> Space -> QuantityDict
var nam np = implVar nam (nounPhraseSP np)

y_2, y_1, x_2, x_1, x :: QuantityDict
y_1  = var "y1" "lower y-coordinate"             (sub lY one) Real
y_2  = var "y2" "upper y-coordinate"             (sub lY two) Real
x_1  = var "x1" "lower x-coordinate"             (sub lX one) Real
x_2  = var "x2" "upper x-coordinate"             (sub lX two) Real
x    = var "x"  "x-coordinate to interpolate at" lX           Real -- = params.wtnt from mainFun.py

v, x_z_1, y_z_1, x_z_2, y_z_2, mat, col,
  i, j, k, z, zVector, yMatrix, xMatrix, y, arr, filename :: QuantityDict
i = var "i" "index" lI           Natural
j = var "j" "index" lJ           Natural
k = var "k" "index" (sub lK two) Natural     
v = var "v" "value whose index will be found" lV Real
y = var "y" "y-coordinate to interpolate at"  lY Real
z = var "z" "z-coordinate to interpolate at"  lZ Real

zVector = var "zVector" "list of z values" 
  (sub lZ (Label "vector")) (Vect Real)               
yMatrix = var "yMatrix" "lists of y values at different z values" 
  (sub lY (Label "matrix")) (Vect $ Vect Real)        
xMatrix = var "xMatrix" "lists of x values at different z values" 
  (sub lX (Label "matrix")) (Vect $ Vect Real)        
arr     = var "arr"     "array in which value should be found" 
  (Label "arr")             (Vect Real)  --FIXME: temporary variable for findCT?
x_z_1   = var "x_z_1"   "list of x values at a specific z value"    
  (sub lX (sub lZ one))      (Vect Real)
y_z_1   = var "y_z_1"   "list of y values at a specific z value"    
  (sub lY (sub lZ one))      (Vect Real)   
x_z_2   = var "x_z_2"   "list of x values at a specific z value"    
  (sub lX (sub lZ two))      (Vect Real)
y_z_2   = var "y_z_2"   "list of y values at a specific z value"   
  (sub lY (sub lZ two))      (Vect Real)
mat     = var "mat"     "matrix from which column will be extracted"     
  (Label "mat")             (Vect $ Vect Real)
col     = var "col"     "extracted column"    
  (Label "col")             (Vect Real)               
filename = var "filename" "name of file with x y and z data" 
  (Label "filename")        String

------------------------------------------------------------------------------------------
--
-- Some semantic functions

-- Given two points (x1,y1) and (x2,y2), return the slope of the line going through them
slope :: (Fractional a) => (a, a) -> (a, a) -> a
slope (x1,y1) (x2,y2) = (y2 - y1) / (x2 - x1)

-- Given two points (x1,y1) and (x2,y2), and an x ordinate, return
-- extrapoled y on the straight line in between
onLine :: (Fractional a) => (a, a) -> (a, a) -> a -> a
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

call :: Func -> [QuantityDict] -> FuncStmt
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
linInterpCT = funcDef "lin_interp" "Performs linear interpolation" 
  [x_1, y_1, x_2, y_2, x] Real (Just "y value interpolated at given x value")
  [ FRet $ onLine (sy x_1, sy y_1) (sy x_2, sy y_2) (sy x) ]

findCT :: Func
findCT = funcDef "find" 
  "Finds the array index for a value closest to the given value" 
  [arr, v] Natural (Just "index of given value in given array")
  [
    ffor i (sy i $< (dim (sy arr) - 1))
      [ FCond ((vLook arr i 0 $<= sy v) $&& (sy v $<= vLook arr i 1))
        [ FRet $ sy i ] [] ],
    FThrow "Bound error"
  ]

extractColumnCT :: Func
extractColumnCT = funcDef "extractColumn" "Extracts a column from a 2D matrix" 
  [mat, j] (Vect Real) (Just "column of the given matrix at the given index")
  [
    fdec col,
    --
    ffor i (sy i $< dim (sy mat))
      [ FAppend (sy col) (aLook mat i j) ],
    FRet (sy col)
  ]

interpY :: Func
interpY = funcDef "interpY" 
  "Linearly interpolates a y value at given x and z values" 
  [filename, x, z] Real (Just "y value interpolated at given x and z values")
  [
  -- hack
  fdec xMatrix,
  fdec yMatrix,
  fdec zVector,
  --
  call readTable [filename, zVector, xMatrix, yMatrix],
  -- endhack
    i     $:= find zVector z,
    x_z_1 $:= getCol xMatrix i 0,
    y_z_1 $:= getCol yMatrix i 0,
    x_z_2 $:= getCol xMatrix i 1,
    y_z_2 $:= getCol yMatrix i 1,
    FTry
      [ j $:= find x_z_1 x,
        k $:= find x_z_2 x ]
      [ FThrow "Interpolation of y failed" ],
    y_1 $:= linInterp (interpOver x_z_1 y_z_1 j x),
    y_2 $:= linInterp (interpOver x_z_2 y_z_2 k x),
    FRet $ linInterp [ vLook zVector i 0, sy y_1, vLook zVector i 1, sy y_2, sy z ]
  ]

interpZ :: Func
interpZ = funcDef "interpZ" 
  "Linearly interpolates a z value at given x and y values" 
  [filename, x, y] Real (Just "z value interpolated at given x and y values")
  [
    -- hack
  fdec xMatrix,
  fdec yMatrix,
  fdec zVector,
  --
  call readTable [filename, zVector, xMatrix, yMatrix],
  -- endhack
    ffor i (sy i $< (dim (sy zVector) - 1))
      [
        x_z_1 $:= getCol xMatrix i 0,
        y_z_1 $:= getCol yMatrix i 0,
        x_z_2 $:= getCol xMatrix i 1,
        y_z_2 $:= getCol yMatrix i 1,
        FTry
          [ j $:= find x_z_1 x,
            k $:= find x_z_2 x ]
          [ FContinue ],
        y_1 $:= linInterp (interpOver x_z_1 y_z_1 j x),
        y_2 $:= linInterp (interpOver x_z_2 y_z_2 k x),
        FCond ((sy y_1 $<= sy y) $&& (sy y $<= sy y_2))
          [ FRet $ linInterp [ sy y_1, vLook zVector i 0, sy y_2, vLook zVector i 1, sy y ]
          ] []
      ],
    FThrow "Interpolation of z failed"
  ]

interpMod :: Mod
interpMod = packmod "Interpolation" 
  "Provides functions for linear interpolation on three-dimensional data" 
  [linInterpCT, findCT, extractColumnCT, interpY, interpZ]
