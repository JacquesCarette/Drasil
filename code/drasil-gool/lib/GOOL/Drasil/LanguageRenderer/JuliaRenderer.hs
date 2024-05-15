{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Julia code is contained in this module
module GOOL.Drasil.LanguageRenderer.JuliaRenderer (
  -- * Julia Code Configuration -- defines syntax of all Julia code
  JuliaCode(..), jlName, jlVersion
) where

import Utils.Drasil (blank, indent)

-- TODO: Sort the dependencies to match the other modules
import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, Library, VSType, SVariable, SValue, 
  VSFunction, MSStatement, MixedCtorCall, OOProg, ProgramSym(..), FileSym(..),
  PermanenceSym(..), BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..),
  VariableSym(..), VariableElim(..), ValueSym(..), Argument(..), Literal(..),
  litZero, MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, selfFuncApp, extFuncApp, extNewObj,
  InternalValueExp(..), objMethodCall, FunctionSym(..), GetSet(..), List(..),
  InternalList(..), ThunkSym(..), VectorType(..), VectorDecl(..),
  VectorThunk(..), VectorExpression(..), ThunkAssign(..), StatementSym(..),
  AssignStatement(..), (&=), DeclStatement(..), IOStatement(..),
  StringStatement(..), FuncAppStatement(..), CommentStatement(..),
  ControlStatement(..), switchAsIf, StatePattern(..), ObserverPattern(..),
  StrategyPattern(..), ScopeSym(..), ParameterSym(..), MethodSym(..),
  StateVarSym(..), ClassSym(..), ModuleSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(..), ImportSym(..), 
  ImportElim, PermElim(binding), RenderBody(..), BodyElim, RenderBlock(..), 
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..), 
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind), 
  RenderValue(..), ValueElim(valuePrec), InternalGetSet(..), 
  InternalListFunc(..), RenderFunction(..), 
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..), 
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm), 
  RenderScope(..), ScopeElim, MethodTypeSym(..), RenderParam(..), 
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim, 
  StateVarElim, RenderClass(..), ClassElim, RenderMod(..), ModuleElim, 
  BlockCommentSym(..), BlockCommentElim)
import qualified GOOL.Drasil.RendererClasses as RC (import', perm, body, block, 
  type', uOp, bOp, variable, value, function, statement, scope, parameter,
  method, stateVar, class', module', blockComment')
import GOOL.Drasil.LanguageRenderer (listSep)
import qualified GOOL.Drasil.LanguageRenderer as R (body, addComments, dynamic,
  commentedMod)
import GOOL.Drasil.LanguageRenderer.Constructors ()
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (block,
  multiBlock, multiBody, fileFromData, fileDoc, listInnerType, obj)
import qualified GOOL.Drasil.LanguageRenderer.CommonPseudoOO as CP (string',
  bindingError, doxMod, funcType)
import qualified GOOL.Drasil.LanguageRenderer.Macros as M ()
import GOOL.Drasil.AST (Terminator(..), FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateMod, MethodData(..), mthd,
  updateMthd, OpData(..), ParamData(..), pd, ProgData(..), progD, TypeData(..),
  td, ValData(..), vd, VarData(..), vard, CommonThunk, pureValue, vectorize,
  vectorize2, sumComponents, commonVecIndex, commonThunkElim, commonThunkDim)
import GOOL.Drasil.Helpers (vibcat, emptyIfEmpty, toCode, toState, onCodeValue,
  onStateValue, on2CodeValues, on2StateValues, onCodeList, onStateList, on2StateWrapped)
import GOOL.Drasil.State (MS, VS, lensGStoFS, lensMStoVS, lensVStoMS, 
  revFiles, addLangImportVS, getLangImports, addLibImportVS, 
  getLibImports, addModuleImport, addModuleImportVS, getModuleImports, 
  setFileType, getClassName, setCurrMain, getClassMap, getMainDoc, useVarName,
  genLoopIndex)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)
import Control.Lens.Zoom (zoom)
import Control.Monad (join)
import Control.Monad.State (modify)
import Data.List (intercalate, sort)
import qualified Data.Map as Map (lookup)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
  vcat, colon, brackets, isEmpty, quotes)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (OptionalSpace(..))

jlExt :: String
jlExt = "jl"

newtype JuliaCode a = JLC {unJLC :: a}

instance Functor JuliaCode where
  fmap f (JLC x) = JLC (f x)

instance Applicative JuliaCode where
  pure = JLC
  (JLC f) <*> (JLC x) = JLC (f x)

instance Monad JuliaCode where
  JLC x >>= f = f x

instance OOProg JuliaCode

instance ProgramSym CodeInfo where
  type Program JuliaCode = ProgData
  prog _ _ _ = error

instance RenderSym JuliaCode

instance FileSym JuliaCode where
  type File JuliaCode = FileData
  fileDoc _ = error
  docMod = error

instance RenderFile JuliaCode where
  top _ = error
  bottom = error

  commentedMod = error

  fileFromData = error

instance ImportSym JuliaCode where
  type Import JuliaCode = Doc
  langImport _ = error
  modImport = error

instance ImportElim JuliaCode where
  import' = error

instance PermanenceSym JuliaCode where
  type Permanence JuliaCode = Doc
  static = error
  dynamic = error

instance PermElim JuliaCode where
  perm = error
  binding = error

instance BodySym JuliaCode where
  type Body JuliaCode = Doc
  body = error

  addComments _ = error

instance RenderBody JuliaCode where
  multibody = error

instance BodyElim JuliaCode where
  body = error

instance BlockSym JuliaCode where
  type Block JuliaCode = Doc
  block = error

instance RenderBlock JuliaCode where
  multiBlock = error

instance BlockElim JuliaCode where
  block = error

-- Actually implemented stuff!!!
instance TypeSym JuliaCode where
  type Type JuliaCode = TypeData
  bool = typeFromData Boolean "Bool" (text "Bool") -- TODO: merge with Swift
  int = jlIntType
  float = jlFloatType
  double = jlDoubleType
  char = jlCharType
  string = CP.string'
  infile = jlInfileType
  outfile = jlOutfileType
  listType = jlListType
  arrayType = listType -- Treat arrays and lists the same, as in Python
  listInnerType = G.listInnerType
  obj = G.obj -- Julia's not OO, but I *think* this is fine
  funcType = CP.funcType -- Julia's functions support multiple-dispatch, so we might need to revisit this
  void = jlVoidType

instance TypeElim JuliaCode where
  getType = cType . unJLC
  getTypeString = typeString . unJLC

instance RenderType JuliaCode where
  multiType ts = do
    typs <- sequence ts
    let mt = jlTuple $ map getTypeString typs
    typeFromData Void mt (text mt)
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim JuliaCode where
  type' = typeDoc . un JLC

instance UnaryOpSym JuliaCode where
  type UnaryOp JuliaCode = OpData
  notOp = C.notOp
  negateOp = G.negateOp
  sqrtOp = jlUnaryMath R.sqrt
  absOp = jlUnaryMath R.abs
  logOp = jlUnaryMath R.log10
  lnOp = jlUnaryMath R.log
  expOp = jlUnaryMath R.exp
  sinOp = jlUnaryMath R.sin
  cosOp = jlUnaryMath R.cos
  tanOp = jlUnaryMath R.tan
  asinOp = jlUnaryMath R.asin
  acosOp = jlUnaryMath R.acos
  atanOp = jlUnaryMath R.atan
  floorOp = jlUnaryMath R.floor
  ceilOp = jlUnaryMath R.ceil

instance BinaryOpSym JuliaCode where
  type BinaryOp JuliaCode = OpData
  equalOp = G.equalOp
  notEqualOp = G.notEqualOp
  greaterOp = G.greaterOp
  greaterEqualOp = G.greaterEqualOp
  lessOp = G.lessOp
  lessEqualOp = G.lessEqualOp
  plusOp = G.plusOp
  minusOp = G.minusOp
  multOp = G.multOp
  divideOp = G.divideOp
  powerOp = powerPrec jlPower
  moduloOp = G.moduloOp
  andOp = C.andOp
  orOp = C.orOp

-- Back to unimplemented methods :(
instance OpElim JuliaCode where
  uOp = error
  bOp = error
  uOpPrec = error
  bOpPrec = error

instance VariableSym JuliaCode where
  type Variable JuliaCode = VarData
  var = error
  staticVar = error
  constant = error
  extVar _ = error
  self = error
  classVar = error
  extClassVar = error
  objVar = error
  objVarSelf = error
  arrayElem _ = error

instance VariableElim JuliaCode where
  variableName = error
  variableType = error

instance InternalVarElim JuliaCode where
  variableBine = error
  variable = error

instance RenderVariable JuliaCode where
  varFromData _ _ _ _ = error

instance ValueSym JuliaCode where
  type Value JuliaCode = ValData
  valueType = error

instance Argument JuliaCode where
  pointerArg = error

-- I just had to implement this though
instance Literal JuliaCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = C.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = CP.litArray brackets
  litList = litArray

instance MathConstant JuliaCode where
  pi = mkStateVal double jlPi

-- More unimplemented :(
instance VariableValue JuliaCode where
  valueOf = error

instance CommandLineArgs JuliaCode where
  arg _ = error
  argsList = error
  argExists _ = error

instance NumericExpression JuliaCode where
  (#~) = error
  (#/^) = error
  (#|) = error
  (#+) = error
  (#-) = error
  (#*) = error
  (#/) = error -- Julia has integer division via `÷` or `div()`
  (#%) = error
  (#^) = error

  log = error
  ln = error
  exp = error
  sin = error
  cos = error
  tan = error
  csc = error
  sec = error
  cot = error
  arcsin = error
  arccos = error
  arctan = error
  floor = error
  ceil = error

instance BooleanExpression JuliaCode where
  (?!) = error
  (?&&) = error
  (?||) = error

instance Comparision JuliaCode where
  (?<) = error
  (?<=) = error
  (?>) = error
  (?>=) = error
  (?==) = error
  (?!=) = error

instance ValueExpression JuliaCode where
  inlineIf = error

  funcAppMixedArgs = error
  selfFuncAppMixedArgs = error
  extFuncAppMixedArgs = error
  libFuncAppMixedArgs = error
  newObjMixedArgs = error
  extNewObjMixedArgs _ _ _ _ = error
  libNewObjMixedArgs = error

  lambda = error

  notNull = error

instance RenderValue JuliaCode where
  inputFunc = error
  printFunc = error
  printLnFunc = error
  printFileFunc _ = error
  printFileLnFunc _ = error

  cast = error

  call _ _ _ _ _ _ = error

  valFromData _ _ _ = error

instance ValueElim JuliaCode where
  valuePrec = error
  value = error

instance InternalValueExp JuliaCode where
  objMethodCallMixedArgs' = error

instance FunctionSym JuliaCode where
  type Function JuliaCode = FuncData
  func = error
  objAccess = error

instance GetSet JuliaCode where
  get = error
  set = error

instance List JuliaCode where
  listSize = error
  listAdd = error
  listAppend = error
  listAccess = error
  listSet = error
  indexOf = error

instance InternalList JuliaCode where
  listSlice' _ _ _ _ _ = error

instance InternalGetSet JuliaCode where
  getFunc = error
  setFunc = error

instance InternalListFunc JuliaCode where
  listSizeFunc = error
  listAddFunc = error
  listAppendFunc = error
  listAccessFunc = error
  listSetFunc = error

-- convenience
jlName, jlVersion :: String
jlName = "Julia"
jlVersion = "1.10.3"

jlInt, jlFloat, jlDouble, jlChar, jlFile, jlList, jlVoid :: String
jlInt = "Int32" -- TODO: Do we use concrete or abstract types?
jlFloat = "Float32"
jlDouble = "Float64"
jlChar = "Char"
jlFile = "IOStream"
jlList = "Vector"
jlVoid = "Nothing"

jlTuple :: [String] -> String
jlTuple ts = "Tuple{" ++ intercalate listSep ts ++ "}"

-- Operators
jlUnaryMath :: (Monad r) => String -> VSOp r
jlUnaryMath = unOpPrec

jlPower :: String
jlPower = "^"

-- Constants
jlPi = "pi"

-- Comments and other stuff
jlCommentStart = text "#"

-- Type names specific to Julia (there's a lot of them)
jlIntType :: (RenderSym r) => VSType r
jlIntType = typeFromData Integer jlInt (text jlInt)

jlFloatType :: (RenderSym r) => VSType r
jlFloatType = typeFromData Float jlFloat (text jlFloat)

jlDoubleType :: (RenderSym r) => VSType r
jlDoubleType = typeFromData Double jlDouble (text jlDouble)

jlCharType :: (RenderSym r) => VSType r
jlCharType = typeFromData Char jlChar (text jlChar)

jlInfileType :: (RenderSym r) => VSType r
jlInfileType = typeFromData InFile jlFile (text jlFile)

jlOutfileType :: (RenderSym r) => VSType r
jlOutfileType = typeFromData OutFile jlFile (text jlFile)

jlListType :: (RenderSym r) => VSType r -> VSType r
jlListType t' = do
  t <- t'
  let typeName = jlList ++ "{" ++ getTypeString t ++ "}"
  typeFromData (List $ getType t) typeName (text typeName)

jlVoidType :: (RenderSym r) => VSType r
jlVoidType = typeFromData Void jlVoid (text jlVoid)