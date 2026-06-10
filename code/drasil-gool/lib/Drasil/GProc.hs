-- | re-export smart constructors for external code writing
module Drasil.GProc (Label, GSProgram, SFile, MSBody, MSBlock, VSType,
  SVariable, SValue, VSFunction, MSStatement, MSParameter, SMethod, FSModule,
  NamedArgs, SharedProg, ProcProg, ProgramSym(..), FileSym(..), BodySym(..),
  bodyStatements, oneLiner, BlockSym(..), TypeSym(..), TypeElim(..),
  BinderSym(..), ThunkSym(..), VectorType(..), VectorDecl(..),
  VectorThunk(..), VectorExpression(..), ThunkAssign(..), StatementSym(..),
  AssignStatement(..), (&=), assignToListIndex, DeclStatement(..),
  IOStatement(..), StringStatement(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), ifNoElse, switchAsIf,
  VariableSym(..), ScopeSym(..), ScopeData, VariableElim(..), listOf, listVar,
  ValueSym(..), Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp,
  libFuncApp, exists, FunctionSym(..), Array(..), List(..), Set(..),  listSlice,
  listIndexExists, at, VisibilitySym(..),ParameterSym(..), MethodSym(..),
  ModuleSym(..), convType, ProgData(..), FileData(..), ModData(..),
  VisibilityTag(..), CodeType(..), GOOLState(..), lensMStoVS, headers, sources,
  mainMod, initialState, onStateValue, onCodeList, unCI, unJLC, jlName,
  jlVersion, unMLC, mlName, mlVersion, LoggingFor(..),
  -- TODO [Brandon Bosman, 06/09/2026]: Remove these from external interface
  getCodeType, getTypeString
  ) where

import Drasil.Shared.InterfaceCommon (Label, MSBody, MSBlock, VSFunction, VSType,
  SVariable, SValue, MSStatement, MSParameter, SMethod, NamedArgs, SharedProg,
  BodySym(..), bodyStatements, oneLiner, BlockSym(..), TypeSym(..),
  BinderSym(..), ThunkSym(..), VectorType(..), VectorDecl(..), VectorThunk(..),
  VectorExpression(..), ThunkAssign(..), StatementSym(..), AssignStatement(..),
  (&=), assignToListIndex, DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), switchAsIf, ifNoElse,
  VariableSym(..), extVar, VariableElim(..), listOf, listVar, ValueSym(..),
  Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp,
  libFuncApp, exists, Array(..), List(..), Set(..), listSlice, listIndexExists,
  at, ScopeSym(..), ParameterSym(..), MethodSym(..), VisibilitySym(..), convType,
  -- TODO [Brandon Bosman, 06/09/2026]: Remove these imports
  getCodeType, getTypeString)
import Drasil.GProc.InterfaceProc (GSProgram, SFile, FSModule, ProcProg,
  ProgramSym(..), FileSym(..), ModuleSym(..))

import Drasil.Shared.AST (FileData(..), ScopeData(..), ModData(..), ProgData(..),
  VisibilityTag(..))

import Drasil.Shared.CodeType (CodeType(..))

import Drasil.Shared.State (GOOLState(..), lensMStoVS, headers, sources, mainMod,
  initialState)

import Drasil.Shared.Helpers (onStateValue, onCodeList)

import Drasil.GProc.CodeInfoProc (unCI)

import Drasil.GProc.LanguageRenderer.JuliaRenderer (unJLC, jlName, jlVersion)
import Drasil.GProc.LanguageRenderer.MatlabRenderer (unMLC, mlName, mlVersion)
import Drasil.Shared.LanguageRenderer.LoggingFor (LoggingFor(..))
