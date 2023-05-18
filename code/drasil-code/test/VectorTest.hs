module VectorTest (vectorTest) where

import GOOL.Drasil (GSProgram, SVariable, SMethod, OOProg, ProgramSym(..),
  FileSym(..), BodySym(..), BlockSym(..), TypeSym(..), DeclStatement(..),
  VectorStatement(..), setVectorized, VariableSym(..), Literal(..),
  VariableValue(..), MethodSym(..), ModuleSym(..))

vectorTest :: OOProg r => GSProgram r
vectorTest = prog "VectorTest" [fileDoc $ buildModule "VectorTest" []
  [main] []]

v1 :: OOProg r => SVariable r
v1 = var "v1" (vectorType double)

v2 :: OOProg r => SVariable r
v2 = var "v2" (vectorType double)

main :: OOProg r => SMethod r
main = mainFunction $ body [block [vectorDecDef v1 [litDouble 1, litDouble 1.5],
  vectorDecDef v2 [litDouble 0, litDouble (-1)],
  setVectorized (valueOf v1) (vectorizedAdd (vectorizedScale (litDouble 2) (vectorized (valueOf v1))) (vectorized (valueOf v2)))]]
