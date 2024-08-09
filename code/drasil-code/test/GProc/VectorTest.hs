module GProc.VectorTest (vectorTest) where

import Drasil.GProc (GSProgram, SVariable, SMethod, ProcProg, ProgramSym(..),
  FileSym(..), BodySym(..), BlockSym(..), TypeSym(..), mainVar, Literal(..),
  VectorType(..), VectorDecl(..), VectorThunk(..), VectorExpression(..),
  DeclStatement(..), ControlStatement(..), ThunkAssign(..), Comparison(..),
  VariableValue(..),MethodSym(..), ModuleSym(..))

vectorTest :: ProcProg r => GSProgram r
vectorTest = prog "VectorTest" "" [fileDoc $ buildModule "VectorTest" []
  [main]]

v1 :: ProcProg r => SVariable r
v1 = mainVar "v1" (vecType double)

v2 :: ProcProg r => SVariable r
v2 = mainVar "v2" (vecType double)

x :: ProcProg r => SVariable r
x = mainVar "x" double

main :: ProcProg r => SMethod r
main = mainFunction $ body [block [vecDecDef v1 [litDouble 1, litDouble 1.5],
  vecDecDef v2 [litDouble 0, litDouble (-1)],
  thunkAssign v1 (vecAdd (vecScale (litDouble 2) (vecThunk v1)) (vecThunk v2)),
  varDec x,
  thunkAssign x (vecDot (vecThunk v1) (vecThunk v2)),
  
  assert (valueOf (mainVar "x" double) ?== litDouble (-2)) 
    (litString "Dot product of v1 and v2 should be -2.")]]
