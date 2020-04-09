module Test.SimpleODE (simpleODE) where

import GOOL.Drasil (GSProgram, SVariable, SMethod, ProgramSym(..), FileSym(..), BodySym(..), BlockSym(..), 
  TypeSym(..), ControlBlock(..), DeclStatement(..), StatementSym(..), IOStatement(..), VariableSym(..), 
  ValueSym(..), NumericExpression(..), MethodSym(..), ModuleSym(..), ODEInfo, 
  odeInfo, ODEOptions, odeOptions, ODEMethod(RK45))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

simpleODE :: (ProgramSym repr) => GSProgram repr
simpleODE = prog "SimpleODE" [fileDoc (buildModule "SimpleODE" []
  [simpleODEMain] [])]

simpleODEMain :: (MethodSym repr) => SMethod repr
simpleODEMain = mainFunction (body [block [varDecDef odeConst (litDouble 3.5)],
  solveODE info opts,
  block [print $ valueOf odeDepVar]])

odeConst, odeDepVar, odeIndepVar :: (VariableSym repr) => SVariable repr
odeConst = var "c" double
odeDepVar = var "T" (listType double)
odeIndepVar = var "t" (listType double)

info :: (StatementSym repr) => ODEInfo repr
info = odeInfo odeIndepVar odeDepVar [odeConst] (litDouble 0.0) (litDouble 10.0) 
  (litDouble 1.0) (valueOf odeDepVar #+ valueOf odeConst)

opts :: (StatementSym repr) => ODEOptions repr
opts = odeOptions RK45 (litDouble 0.001) (litDouble 0.001) (litDouble 1.0)
