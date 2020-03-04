module Test.SimpleODE (simpleODE) where

import GOOL.Drasil (ProgramSym(..), FileSym(..), BodySym(..), BlockSym(..), 
  TypeSym(..), ControlBlockSym(..), StatementSym(..), VariableSym(..), 
  ValueSym(..), NumericExpression(..), MethodSym(..), ModuleSym(..), ODEInfo, 
  odeInfo, ODEOptions, odeOptions, ODEMethod(RK45), GS, MS, VS)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

simpleODE :: (ProgramSym repr) => GS (repr (Program repr))
simpleODE = prog "SimpleODE" [fileDoc (buildModule "SimpleODE" []
  [simpleODEMain] [])]

simpleODEMain :: (MethodSym repr) => MS (repr (Method repr))
simpleODEMain = mainFunction (body [block [varDecDef odeConst (litDouble 3.5)],
  solveODE info opts,
  block [print $ valueOf odeDepVar]])

odeConst, odeDepVar, odeIndepVar :: (VariableSym repr) =>
  VS (repr (Variable repr))
odeConst = var "c" double
odeDepVar = var "T" (listType double)
odeIndepVar = var "t" (listType double)

info :: (StatementSym repr) => ODEInfo repr
info = odeInfo odeIndepVar odeDepVar [odeConst] (litDouble 0.0) (litDouble 10.0) 
  (litDouble 1.0) (valueOf odeDepVar #+ valueOf odeConst)

opts :: (StatementSym repr) => ODEOptions repr
opts = odeOptions RK45 (litDouble 0.001) (litDouble 0.001) (litDouble 1.0)
