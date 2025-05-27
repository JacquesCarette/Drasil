-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.Common where

import Utils.Drasil (indent, stringList)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (Label, Library, Body, MSBody, VSFunction,
  VSType, Variable, SVariable, Value, SValue, MSStatement, MSParameter, SMethod,
  MixedCall, bodyStatements, oneLiner, TypeSym(infile, outfile, listInnerType),
  TypeElim(getType, getTypeString), VariableElim(variableName, variableType),
  ValueSym(valueType), Comparison(..), (&=), ControlStatement(returnStmt),
  VisibilitySym(..), MethodSym(function), funcApp, ScopeSym(Scope))
import qualified Drasil.Shared.InterfaceCommon as IC (argsList,
  TypeSym(int, bool, double, string, listType, arrayType, void), VariableSym(var),
  Literal(litTrue, litFalse, litList, litSet, litInt, litString),
  VariableValue(valueOf), StatementSym(valStmt, emptyStmt), DeclStatement(varDec,
  varDecDef, constDecDef), List(intToIndex, indexToInt), ParameterSym(param,
  pointerParam), MethodSym(mainFunction), AssignStatement(assign), ScopeSym(..))
import Drasil.GOOL.InterfaceGOOL (SFile, FSModule, SClass, CSStateVar,
  OOTypeSym(obj), PermanenceSym(..), Initializers, objMethodCallNoParams, objMethodCall)
import qualified Drasil.GOOL.InterfaceGOOL as IG (ClassSym(buildClass),
  OOVariableSym(self, objVar), OOFunctionSym(..))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  RenderBody(..), RenderType(..), RenderVariable(varFromData),
  InternalVarElim(variableBind), RenderFunction(funcFromData),
  MethodTypeSym(mType), RenderMethod(commentedFunc, mthdFromData),
  BlockCommentSym(..), ScopeElim(scopeData))
import qualified Drasil.Shared.RendererClassesCommon as S (RenderBody(multiBody),
  RenderValue(call), RenderStatement(stmt),
  InternalAssignStmt(multiAssign), InternalControlStmt(multiReturn),
  InternalListFunc(listSizeFunc, listAddFunc, listAppendFunc))
import qualified Drasil.Shared.RendererClassesCommon as RC (ImportElim(..),
  BodyElim(..), InternalTypeElim(..), InternalVarElim(variable), ValueElim(..),
  StatementElim(statement), VisibilityElim(..), MethodElim(..), FunctionElim(..))
import Drasil.Shared.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  on2StateValues, onStateList)
import Drasil.GOOL.RendererClassesOO (OORenderSym, OORenderMethod(intMethod),
  ParentSpec)
import qualified Drasil.GOOL.RendererClassesOO as S (OOMethodTypeSym(construct),
  OORenderMethod(intFunc), RenderClass(intClass, inherit),
  RenderMod(modFromData))
import qualified Drasil.GOOL.RendererClassesOO as RC (PermElim(..),
  StateVarElim(..), ClassElim(..))
import Drasil.Shared.LanguageRenderer (array', new', args, array, listSep, access,
  mathFunc, ModuleDocRenderer, FuncDocRenderer, functionDox, classDox,
  moduleDox, variableList, valueList, intValue)
import qualified Drasil.Shared.LanguageRenderer as R (self, self', module',
  print, stateVar, stateVarList, constDecDef, extVar, listAccessFunc)
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd,
  mkStateVal, mkStateVar, mkVal, mkVal)
import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (classVarCheckStatic,
  call, initStmts, docFunc, docFuncRepr, docClass, docMod, smartAdd, smartSub)
import Drasil.Shared.AST (VisibilityTag(..), ScopeTag(Global), ScopeData, sd)
import Drasil.Shared.State (FS, CS, lensFStoCS, lensFStoMS, lensCStoMS,
  lensMStoVS, lensVStoMS, currParameters, getClassName, getLangImports,
  getLibImports, getModuleImports, setClassName, setCurrMain, setMainDoc,
  useVarName, setVarScope)

import Prelude hiding (print,pi,(<>))
import Data.List (sort, intercalate)
import Control.Monad (join)
import Control.Monad.State (get, modify)
import Control.Lens ((^.))
import qualified Control.Lens as L (set)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  brackets, braces, colon, vcat, equals)
import Metadata.Drasil.DrasilMetaCall (watermark)

-- Swift and Julia --

boolRender :: String
boolRender = "Bool"

bool :: (CommonRenderSym r) => VSType r
bool = typeFromData Boolean boolRender (text boolRender)


-- Python, Java, C#, and Julia --

extVar :: (CommonRenderSym r) => Label -> Label -> VSType r -> SVariable r
extVar l n t = mkStateVar (l `access` n) t (R.extVar l n)
