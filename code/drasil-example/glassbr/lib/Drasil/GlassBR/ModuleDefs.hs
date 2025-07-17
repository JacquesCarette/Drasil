-- Convention used below:
-- when 'name' and 'nameCT' both appear, 'name' is the Haskell function and
-- 'nameCT' is the "Code Template" that 'name' builds.

module Drasil.GlassBR.ModuleDefs (allMods, implVars, interpY, interpZ) where

import Drasil.Code.CodeExpr (CodeExpr, LiteralC(int))
import Language.Drasil (Space(..), nounPhraseSP,
  label, sub, HasSymbol(..), HasUID, Symbol, ExprC(..), dqdNoUnit, DefinedQuantityDict, dcc)
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Code (($:=), Func, FuncStmt(..), Mod, 
  asVC, funcDef, fDecDef, ffor, funcData, quantvar, 
  multiLine, packmod, repeated, singleLine)
import qualified Drasil.GlassBR.Unitals as U
import Language.Drasil.Printers


allMods :: [Mod]
allMods = [readTableMod, interpMod]

-- It's a bit odd that this has to be explicitly built here...
implVars :: [DefinedQuantityDict]
implVars = [v, x_z_1, y_z_1, x_z_2, y_z_2, mat, col,
  i, j, k, z, zVector, yMatrix, xMatrix, y, arr, filename,
  y_2, y_1, x_2, x_1, x]

--from TSD.txt:

readTableMod :: Mod
readTableMod = packmod "ReadTable"
  "Provides a function for reading glass ASTM data" [] [readTable]

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

var :: String -> String -> String -> Symbol -> Space -> DefinedQuantityDict
var nam np desc = dqdNoUnit (dcc nam (nounPhraseSP np) desc)

y_2, y_1, x_2, x_1, x :: DefinedQuantityDict
y_1  = var "y1" "lower y-coordinate"
  "the lower y-coordinate" (sub lY one) Real
y_2  = var "y2" "upper y-coordinate"
  "the upper y-coordinate" (sub lY two) Real
x_1  = var "x1" "lower x-coordinate"
  "the lower x-coordinate" (sub lX one) Real
x_2  = var "x2" "upper x-coordinate"
  "the upper x-coordiante" (sub lX two) Real
x    = var "x"  "x-coordinate to interpolate at"
  "the x-coordinate to interpolate at" lX Real -- = params.wtnt from mainFun.py

v, x_z_1, y_z_1, x_z_2, y_z_2, mat, col,
  i, j, k, z, zVector, yMatrix, xMatrix, y, arr, filename :: DefinedQuantityDict
i = var "i" "index" "the index" lI Natural
j = var "j" "index" "the index" lJ Natural
k = var "k" "index" "the index" (sub lK two) Natural     
v = var "v" "value whose index will be found"
  "the value whose index will be found" lV Real
y = var "y" "y-coordinate to interpolate at"
  "the y-coordinate to interpolate at" lY Real
z = var "z" "z-coordinate to interpolate at"
  "the z-coordinate to interpolate at" lZ Real

zVector = var "zVector" "list of z values" 
  "the list of z values" (sub lZ (label "vector")) (Vect Real)               
yMatrix = var "yMatrix" "lists of y values at different z values" 
  "the lists of y values at different z values" (sub lY (label "matrix")) (Vect $ Vect Real)
xMatrix = var "xMatrix" "lists of x values at different z values" 
  "the lists of x values at different z values" (sub lX (label "matrix")) (Vect $ Vect Real)        
arr     = var "arr" "array in which value should be found"
  "the array in which a value should be found" (label "arr") (Vect Real)  --FIXME: temporary variable for findCT?
x_z_1   = var "x_z_1" "list of x values at a specific z value"
  "the list of x values at a specific z value" (sub lX (sub lZ one)) (Vect Real)
y_z_1   = var "y_z_1" "list of y values at a specific z value"
  "the list of y values at a specific z value" (sub lY (sub lZ one)) (Vect Real)   
x_z_2   = var "x_z_2" "list of x values at a specific z value"
  "the list of x values at a specific z value" (sub lX (sub lZ two)) (Vect Real)
y_z_2   = var "y_z_2" "list of y values at a specific z value"
  "the list of y values at a specific z value" (sub lY (sub lZ two)) (Vect Real)
mat     = var "mat" "matrix from which column will be extracted"
  "the matrix from which columns will be extracted" (label "mat") (Vect $ Vect Real)
col     = var "col" "extracted column"
  "the extracted column" (label "col") (Vect Real)
filename = var "filename" "name of file with x y and z data"
  "the name of the file with x, y, and z data" (label "filename") String

------------------------------------------------------------------------------------------
--
-- Some semantic functions

-- Given two points (x1,y1) and (x2,y2), return the slope of the line going through them
slope :: (CodeExpr, CodeExpr) -> (CodeExpr, CodeExpr) -> CodeExpr
slope (x1, y1) (x2, y2) = (y2 $- y1) $/ (x2 $- x1)

-- Given two points (x1,y1) and (x2,y2), and an x ordinate, return
-- extrapoled y on the straight line in between
onLine :: (CodeExpr, CodeExpr) -> (CodeExpr, CodeExpr) -> CodeExpr -> CodeExpr
onLine p1@(x1, y1) p2 x_ = (m $* (x_ $- x1)) $+ y1
                 where m = slope p1 p2

------------------------------------------------------------------------------------------
-- Code Template helper functions

vLook :: (HasSymbol a, HasSymbol i, HasUID a, HasUID i) => a -> i -> CodeExpr -> CodeExpr
vLook a i_ p = idx (sy a) (sy i_ $+ p)

aLook :: (HasSymbol a, HasSymbol i, HasSymbol j, HasUID a, HasUID i, HasUID j) =>
  a -> i -> j -> CodeExpr
aLook a i_ j_ = idx (idx (sy a) (sy i_)) (sy j_)

getCol :: (HasSymbol a, HasSymbol i, HasUID a, HasUID i) => a -> i -> CodeExpr -> CodeExpr
getCol a_ i_ p = apply (asVC extractColumnCT) [sy a_, sy i_ $+ p]

call :: Func -> [DefinedQuantityDict] -> FuncStmt
call f l = FVal $ apply (asVC f) $ map sy l

find :: (HasUID zv, HasUID z, HasSymbol zv, HasSymbol z) => zv -> z -> CodeExpr
find zv z_ = apply (asVC findCT) [sy zv, sy z_]

linInterp :: [CodeExpr] -> CodeExpr
linInterp = apply (asVC linInterpCT)

interpOver :: (HasUID ptx, HasUID pty, HasUID ind, HasUID vv,
  HasSymbol ptx, HasSymbol pty, HasSymbol ind, HasSymbol vv) =>
  ptx -> pty -> ind -> vv -> [CodeExpr]
interpOver ptx pty ind vv =
  [ vLook ptx ind (int 0), vLook pty ind (int 0)
  , vLook ptx ind (int 1), vLook pty ind (int 1)
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
    ffor i (dim (sy arr) $- int 1)
      [ FCond ((vLook arr i (int 0) $<= sy v) $&& (sy v $<= vLook arr i (int 1)))
        [ FRet $ sy i ] [] ],
    FThrow "Bound error"
  ]

extractColumnCT :: Func
extractColumnCT = funcDef "extractColumn" "Extracts a column from a 2D matrix" 
  [mat, j] (Vect Real) (Just "column of the given matrix at the given index")
  [
    fDecDef col (matrix [[]]),
    --
    ffor i (dim (sy mat))
      [ FAppend (sy col) (aLook mat i j) ],
    FRet (sy col)
  ]

interpY :: Func
interpY = funcDef (showHasSymbImpl U.interpY)
  "Linearly interpolates a y value at given x and z values" 
  [filename, x, z] Real (Just "y value interpolated at given x and z values")
  [
  -- hack
  fDecDef xMatrix (matrix [[]]),
  fDecDef yMatrix (matrix [[]]),
  fDecDef zVector (matrix [[]]),
  --
  call readTable [filename, zVector, xMatrix, yMatrix],
  -- endhack
    i     $:= find zVector z,
    x_z_1 $:= getCol xMatrix i (int 0),
    y_z_1 $:= getCol yMatrix i (int 0),
    x_z_2 $:= getCol xMatrix i (int 1),
    y_z_2 $:= getCol yMatrix i (int 1),
    FTry
      [ j $:= find x_z_1 x,
        k $:= find x_z_2 x ]
      [ FThrow "Interpolation of y failed" ],
    y_1 $:= linInterp (interpOver x_z_1 y_z_1 j x),
    y_2 $:= linInterp (interpOver x_z_2 y_z_2 k x),
    FRet $ linInterp [ vLook zVector i (int 0), sy y_1, vLook zVector i (int 1), sy y_2, sy z ]
  ]

interpZ :: Func
interpZ = funcDef (showHasSymbImpl U.interpZ)
  "Linearly interpolates a z value at given x and y values" 
  [filename, x, y] Real (Just "z value interpolated at given x and y values")
  [
    -- hack
  fDecDef xMatrix (matrix [[]]),
  fDecDef yMatrix (matrix [[]]),
  fDecDef zVector (matrix [[]]),
  --
  call readTable [filename, zVector, xMatrix, yMatrix],
  -- endhack
    ffor i (dim (sy zVector) $- int 1)
      [
        x_z_1 $:= getCol xMatrix i (int 0),
        y_z_1 $:= getCol yMatrix i (int 0),
        x_z_2 $:= getCol xMatrix i (int 1),
        y_z_2 $:= getCol yMatrix i (int 1),
        FTry
          [ j $:= find x_z_1 x,
            k $:= find x_z_2 x ]
          [ FContinue ],
        y_1 $:= linInterp (interpOver x_z_1 y_z_1 j x),
        y_2 $:= linInterp (interpOver x_z_2 y_z_2 k x),
        FCond ((sy y_1 $<= sy y) $&& (sy y $<= sy y_2))
          [ FRet $ linInterp [ sy y_1, vLook zVector i (int 0), sy y_2, vLook zVector i (int 1), sy y ]
          ] []
      ],
    FThrow "Interpolation of z failed"
  ]

interpMod :: Mod
interpMod = packmod "Interpolation" 
  "Provides functions for linear interpolation on three-dimensional data" []
  [linInterpCT, findCT, extractColumnCT, interpY, interpZ]
