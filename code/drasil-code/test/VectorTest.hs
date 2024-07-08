module VectorTest (vectorTest) where

import GOOL.Drasil (GSProgram, SVariable, SMethod, OOProg, ProgramSym(..),
  FileSym(..), BodySym(..), BlockSym(..), TypeSym(..), VariableSym(..),
  ScopeSym(..), Literal(..), VectorType(..), VectorDecl(..), VectorThunk(..),
  VectorExpression(..), DeclStatement(..), ThunkAssign(..), MethodSym(..),
  ModuleSym(..))

vectorTest :: OOProg r => GSProgram r
vectorTest = prog "VectorTest" "" [fileDoc $ buildModule "VectorTest" []
  [main] []]

v1 :: OOProg r => SVariable r
v1 = var "v1" (vecType double) local

v2 :: OOProg r => SVariable r
v2 = var "v2" (vecType double) local

x :: OOProg r => SVariable r
x = var "x" double local

main :: OOProg r => SMethod r
main = mainFunction $ body [block [vecDecDef v1 [litDouble 1, litDouble 1.5],
  vecDecDef v2 [litDouble 0, litDouble (-1)],
  thunkAssign v1 (vecAdd (vecScale (litDouble 2) (vecThunk v1)) (vecThunk v2)),
  varDec x,
  thunkAssign x (vecDot (vecThunk v1) (vecThunk v2))]]
