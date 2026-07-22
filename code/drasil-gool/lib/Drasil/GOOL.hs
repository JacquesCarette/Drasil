-- | re-export smart constructors for external code writing
module Drasil.GOOL (Label, GSProgram, File, Body, Block, CS, FS, MS, VS,
  SVariable, SValue, CSStateVar, Class, Module, NamedArgs, Initializers,
  SharedProg, SharedStatement, OOProg, OOStatement, ProgramSym(..), FileSym(..),
  AttachmentSym(..), BodySym(..), bodyStatements, oneLiner, BlockSym(..),
  TypeSym(..), OOTypeSym(..), BinderSym(..), StatementSym(..),
  AssignStatement(..), (&=), DeclStatement(..), OODeclStatement(..),
  objDecNewNoParams, extObjDecNewNoParams, IOStatement(..), StringStatement(..),
  FuncAppStatement(..), OOFuncAppStatement(..), CommentStatement(..),
  initObserverList, addObserver, ControlStatement(..), ifNoElse, switchAsIf,
  VariableSym(..), ScopeSym(..), ScopeData, OOVariableSym(..), SelfSym(..),
  instanceVarSelf, VariableElim(..), ($->), listOf, listVar, ValueSym(..),
  Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  OOVariableValue, CommandLineArgs(..), NumericExpression(..),
  BooleanExpression(..), Comparison(..), ValueExpression(..),
  OOValueExpression(..), funcApp, funcAppNamedArgs, selfMethodCall, extFuncApp,
  libFuncApp, newObj, extNewObj, libNewObj, exists, objMethodCall,
  objMethodCallNamedArgs, objMethodCallMixedArgs, objMethodCallNoParams,
  classMethodCall, classMethodCallNamedArgs, classMethodCallMixedArgs,
  classMethodCallNoParams, FunctionSym, OOFunctionSym(..), ($.), selfAccess,
  GetSet(..), Reference(..), Array(..), List(..), Set(..), listSlice,
  listIndexExists, at, ObserverPattern(..), StrategyPattern(..),
  VisibilitySym(..), ParameterSym(..), MethodSym(..), OOMethodSym(..),
  privMethod, pubMethod, initializer, nonInitConstructor, StateVarSym(..),
  privDVar, pubDVar, pubSVar, ClassSym(..), ModuleSym(..), convType, convTypeOO,
  ProgData(..), FileData(..), ModData(..), TypeData(..), VisibilityTag(..),
  ParamData, CodeType(..), GOOLState(..), lensMStoVS, headers, sources, mainMod,
  initialState, onStateValue, onCodeList, unCI, unPC, unJC, unCSC, unCPPC, unSC,
  pyName, pyVersion, jName, jVersion, csName, csVersion, cppName, cppVersion,
  swiftName, swiftVersion, LoggingFor(..),
  -- TODO [Brandon Bosman, 06/09/2026]: Remove these from external interface
  TypeElim(..), getTypeString
  ) where

import Drasil.Shared.InterfaceCommon (Label, Body, Block, SVariable, SValue,
  NamedArgs, SharedProg, SharedStatement, BodySym(..), bodyStatements, oneLiner,
  BlockSym(..), TypeSym(..), BinderSym(..), StatementSym(..),
  AssignStatement(..), (&=), DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym, FuncAppStatement(..), CommentStatement(..),
  ControlStatement(..), switchAsIf, ifNoElse, VariableSym(..), extVar,
  VariableElim(..), listOf, listVar, ValueSym(..), Argument(..), Literal(..),
  MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp, libFuncApp, exists,
  Reference(..), Array(..), List(..), Set(..), listSlice, listIndexExists, at,
  ScopeSym(..), ParameterSym(..), MethodSym(..), VisibilitySym(..), convType,
  -- TODO [Brandon Bosman, 06/09/2026]: Remove these imports
  TypeElim(..), getTypeString)
import Drasil.GOOL.InterfaceGOOL (GSProgram, File, Module, Class, CSStateVar,
  Initializers, OOProg, OOStatement, ProgramSym(..), FileSym(..), ModuleSym(..),
  ClassSym(..), OOMethodSym(..), OOTypeSym(..), OOVariableSym(..), SelfSym(..),
  instanceVarSelf, ($->), AttachmentSym(..), privMethod, pubMethod, initializer,
  nonInitConstructor, StateVarSym(..), privDVar, pubDVar, pubSVar,
  OOVariableValue, OOValueExpression(..), selfMethodCall, newObj, extNewObj,
  libNewObj, OODeclStatement(..), objDecNewNoParams, extObjDecNewNoParams,
  OOFuncAppStatement(..), GetSet(..), objMethodCall, objMethodCallNamedArgs,
  objMethodCallMixedArgs, objMethodCallNoParams, classMethodCall,
  classMethodCallNamedArgs, classMethodCallMixedArgs, classMethodCallNoParams,
  OOFunctionSym(..), ($.), selfAccess, ObserverPattern(..), initObserverList,
  addObserver, StrategyPattern(..), convTypeOO)

import Drasil.Shared.AST (FileData(..), ScopeData(..), ModData(..), ProgData(..),
  TypeData(..), VisibilityTag(..), ParamData)

import Drasil.Shared.CodeType (CodeType(..))

import Drasil.Shared.State (CS, FS, MS, VS, GOOLState(..), lensMStoVS, headers,
  sources, mainMod, initialState)

import Drasil.Shared.Helpers (onStateValue, onCodeList)

import Drasil.GOOL.CodeInfoOO (unCI)

import Drasil.GOOL.LanguageRenderer.JavaRenderer (unJC, jName, jVersion)
import Drasil.GOOL.LanguageRenderer.PythonRenderer (unPC, pyName, pyVersion)
import Drasil.GOOL.LanguageRenderer.CSharpRenderer (unCSC, csName, csVersion)
import Drasil.GOOL.LanguageRenderer.CppRenderer (unCPPC, cppName, cppVersion)
import Drasil.GOOL.LanguageRenderer.SwiftRenderer (unSC, swiftName, swiftVersion)
import Drasil.Shared.LanguageRenderer.LoggingFor (LoggingFor(..))
