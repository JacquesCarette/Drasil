module VectorTest (vectorTest) where

import GOOL.Drasil (GSProgram, SVariable, SMethod, OOProg, ProgramSym(..),
  FileSym(..), BodySym(..), BlockSym(..), TypeSym(..), DeclStatement(..),
  VectorStatement(..), VariableSym(..), Literal(..), VariableValue(..),
  MethodSym(..), ModuleSym(..))

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
  vectorScale (valueOf v1) (litDouble 2),
  vectorAdd (valueOf v1) (valueOf v2)]]
