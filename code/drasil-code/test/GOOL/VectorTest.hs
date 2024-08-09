module GOOL.VectorTest (vectorTest) where

import Drasil.GOOL (GSProgram, SVariable, SMethod, OOProg, ProgramSym(..),
  FileSym(..), BodySym(..), BlockSym(..), TypeSym(..), mainVar, Literal(..),
  VectorType(..), VectorDecl(..), VectorThunk(..), VectorExpression(..),
  DeclStatement(..), ControlStatement(..), ThunkAssign(..), Comparison(..),
  VariableValue(..), MethodSym(..), ModuleSym(..))

vectorTest :: OOProg r => GSProgram r
vectorTest = prog "VectorTest" "" [fileDoc $ buildModule "VectorTest" []
  [main] []]

v1 :: OOProg r => SVariable r
v1 = mainVar "v1" (vecType double)

v2 :: OOProg r => SVariable r
v2 = mainVar "v2" (vecType double)

x :: OOProg r => SVariable r
x = mainVar "x" double

main :: OOProg r => SMethod r
main = mainFunction $ body [block [vecDecDef v1 [litDouble 1, litDouble 1.5],
  vecDecDef v2 [litDouble 0, litDouble (-1)],
  thunkAssign v1 (vecAdd (vecScale (litDouble 2) (vecThunk v1)) (vecThunk v2)),
  varDec x,
  thunkAssign x (vecDot (vecThunk v1) (vecThunk v2)),

  assert (valueOf (mainVar "x" double) ?== litDouble (-2)) 
    (litString "Dot product of v1 and v2 should be -2.")]]
