module VectorTest (vectorTest) where

import GOOL.Drasil (GSProgram, SVariable, SMethod, OOProg, ProgramSym(..),
  FileSym(..), BodySym(..), BlockSym(..), TypeSym(..), VariableSym(..),
  Literal(..), VectorType(..), VectorDecl(..), VectorThunk(..),
  VectorExpression(..), ThunkAssign(..), MethodSym(..), ModuleSym(..))

vectorTest :: OOProg r => GSProgram r
vectorTest = prog "VectorTest" [fileDoc $ buildModule "VectorTest" []
  [main] []]

v1 :: OOProg r => SVariable r
v1 = var "v1" (vecType double)

v2 :: OOProg r => SVariable r
v2 = var "v2" (vecType double)

main :: OOProg r => SMethod r
main = mainFunction $ body [block [vecDecDef v1 [litDouble 1, litDouble 1.5],
  vecDecDef v2 [litDouble 0, litDouble (-1)],
  thunkAssign v1 (vecAdd (vecScale (litDouble 2) (vecThunk v1)) (vecThunk v2))]]
