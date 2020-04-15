module Test.SimpleODE (simpleODE) where

import GOOL.Drasil (GSProgram, SVariable, SMethod, ProgramSym(..), FileSym(..), BodySym(..), BlockSym(..), 
  TypeSym(..), ControlBlock(..), DeclStatement(..), IOStatement(..), VariableSym(..), 
  Literal(..), VariableValue(..), NumericExpression(..), MethodSym(..), ModuleSym(..), ODEInfo, 
  odeInfo, ODEOptions, odeOptions, ODEMethod(RK45))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

simpleODE :: (ProgramSym r) => GSProgram r
simpleODE = prog "SimpleODE" [fileDoc (buildModule "SimpleODE" []
  [simpleODEMain] [])]

simpleODEMain :: (ProgramSym r) => SMethod r
simpleODEMain = mainFunction (body [block [varDecDef odeConst (litDouble 3.5)],
  solveODE info opts,
  block [print $ valueOf odeDepVar]])

odeConst, odeDepVar, odeIndepVar :: (ProgramSym r) => SVariable r
odeConst = var "c" double
odeDepVar = var "T" (listType double)
odeIndepVar = var "t" (listType double)

info :: (ProgramSym r) => ODEInfo r
info = odeInfo odeIndepVar odeDepVar [odeConst] (litDouble 0.0) (litDouble 10.0) 
  (litDouble 1.0) (valueOf odeDepVar #+ valueOf odeConst)

opts :: (ProgramSym r) => ODEOptions r
opts = odeOptions RK45 (litDouble 0.001) (litDouble 0.001) (litDouble 1.0)
