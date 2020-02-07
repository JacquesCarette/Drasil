module Test.SimpleODE (simpleODE) where

import GOOL.Drasil (ProgramSym(..), FileSym(..), BodySym(..), BlockSym(..), 
  TypeSym(..), ControlBlockSym(..), StatementSym(..), VariableSym(..), 
  ValueSym(..), NumericExpression(..), MethodSym(..), ModuleSym(..), ODEInfo, 
  odeInfo, ODEOptions, odeOptions, ODEMethod(RK45), GS, MS, VS)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

simpleODE :: (ProgramSym repr) => GS (repr (Program repr))
simpleODE = prog "SimpleODE" [fileDoc (buildModule "SimpleODE" [simpleODEMain] 
  [])]

simpleODEMain :: (MethodSym repr) => MS (repr (Method repr))
simpleODEMain = mainFunction (body [block [varDecDef odeConst (litFloat 3.5)],
  solveODE info opts,
  block [print $ valueOf odeDepVar]])

odeConst, odeDepVar, odeIndepVar :: (VariableSym repr) =>
  VS (repr (Variable repr))
odeConst = var "c" float
odeDepVar = var "T" (listType float)
odeIndepVar = var "t" (listType float)

info :: (StatementSym repr) => ODEInfo repr
info = odeInfo odeIndepVar odeDepVar [odeConst] (litFloat 0.0) (litFloat 10.0) 
  (litFloat 1.0) (valueOf odeDepVar #+ valueOf odeConst)

opts :: (StatementSym repr) => ODEOptions repr
opts = odeOptions RK45 (litFloat 0.001) (litFloat 0.001) (litFloat 1.0)
