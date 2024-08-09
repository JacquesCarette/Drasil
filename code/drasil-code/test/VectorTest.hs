module VectorTest (vectorTestOO, vectorTestProc) where

import Drasil.GOOL (SVariable, SMethod, SharedProg, OOProg, BodySym(..),
  BlockSym(..), TypeSym(..), mainVar, Literal(..), VectorType(..),
  VectorDecl(..), VectorThunk(..), VectorExpression(..), DeclStatement(..),
  ControlStatement(..), BooleanExpression(..), VariableValue(..), 
  Comparison(..), List(..), ValueExpression(..), 
  ThunkAssign(..), MethodSym(..))
import qualified Drasil.GOOL as OO (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))

vectorTestOO :: OOProg r => OO.GSProgram r
vectorTestOO = OO.prog "VectorTest" "" [OO.fileDoc $ OO.buildModule
  "VectorTest" [] [main] []]

vectorTestProc :: ProcProg r => GProc.GSProgram r
vectorTestProc = GProc.prog "VectorTest" "" [GProc.fileDoc $ GProc.buildModule
  "VectorTest" [] [main]]

v1 :: SharedProg r => SVariable r
v1 = mainVar "v1" (vecType double)

v2 :: SharedProg r => SVariable r
v2 = mainVar "v2" (vecType double)

x :: SharedProg r => SVariable r
x = mainVar "x" double

main :: SharedProg r => SMethod r
main = mainFunction $ body [block [vecDecDef v1 [litDouble 1, litDouble 1.5],
  vecDecDef v2 [litDouble 0, litDouble (-1)],
  thunkAssign v1 (vecAdd (vecScale (litDouble 2) (vecThunk v1)) (vecThunk v2)),
  varDec x,
  thunkAssign x (vecDot (vecThunk v1) (vecThunk v2)),
  
  assert (valueOf (mainVar "x" double) ?== litDouble (-2)) 
    (litString "Dot product of v1 and v2 should be -2.")]]
