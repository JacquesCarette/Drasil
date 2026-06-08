{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

-- | The logic to render MATLAB code is contained in this module.
module Drasil.GProc.LanguageRenderer.MatlabRenderer (
  -- * MATLAB Code Configuration -- defines syntax of all MATLAB code
  MatlabCode(..), mlName, mlVersion
) where

import Drasil.Shared.InterfaceCommon (SharedProg, BodySym(..), BlockSym(..),
  TypeSym(..), TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..),
  Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), IndexTranslator(..),
  Array(..), List(..), Set(..), InternalList(..), ThunkSym(..), VectorType(..),
  VectorDecl(..), VectorThunk(..), VectorExpression(..), ThunkAssign(..),
  StatementSym(..), AssignStatement(..), DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), VisibilitySym(..), ScopeSym(..),
  ParameterSym(..), BinderSym(..), BinderElim(..), MethodSym(..))
import Drasil.GProc.InterfaceProc (ProcProg, ProgramSym(..),
  FileSym(..), ModuleSym(..))

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  ImportElim, RenderBody(..), BodyElim, RenderBlock(..), BlockElim,
  RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..),
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind),
  RenderValue(..), ValueElim(..), InternalListFunc(..), RenderFunction(..),
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..),
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm),
  RenderVisibility(..), VisibilityElim, MethodTypeSym(..), RenderParam(..),
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim,
  BlockCommentSym(..), BlockCommentElim, ScopeElim(..), InternalBinderElim(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (import', body,
  block, type', uOp, bOp, variable, function, statement, visibility, parameter,
  method, blockComment')
import Drasil.GProc.RendererClassesProc (ProcRenderSym, RenderFile(..),
  RenderMod(..), ModuleElim, ProcRenderMethod(..))
import qualified Drasil.GProc.RendererClassesProc as RC (module')
import Drasil.Shared.AST (Terminator, FileData, FuncData, ModData,
  MethodData, ParamData, ProgData, TypeData, ValData, VarData, CommonThunk)
import Drasil.Shared.State (VS)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc)

newtype MatlabCode a = MLC {unMLC :: a} deriving Functor

instance Applicative MatlabCode where
  pure = MLC
  (MLC f) <*> (MLC x) = MLC (f x)

instance Monad MatlabCode where
  MLC x >>= f = f x

instance SharedProg MatlabCode
instance ProcProg MatlabCode

instance ProgramSym MatlabCode where
  type Program MatlabCode = ProgData
  prog = undefined

instance CommonRenderSym MatlabCode
instance ProcRenderSym MatlabCode

instance FileSym MatlabCode where
  type File MatlabCode = FileData
  fileDoc = undefined
  docMod = undefined

instance RenderFile MatlabCode where
  top = undefined
  bottom = undefined
  commentedMod = undefined
  fileFromData = undefined

instance ImportSym MatlabCode where
  type Import MatlabCode = Doc
  langImport = undefined
  modImport = undefined

instance ImportElim MatlabCode where
  import' = undefined

instance BodySym MatlabCode where
  type Body MatlabCode = Doc
  body = undefined
  addComments = undefined

instance RenderBody MatlabCode where
  multiBody = undefined

instance BodyElim MatlabCode where
  body = undefined

instance BlockSym MatlabCode where
  type Block MatlabCode = Doc
  block = undefined

instance RenderBlock MatlabCode where
  multiBlock = undefined

instance BlockElim MatlabCode where
  block = undefined

instance TypeSym MatlabCode where
  bool = undefined
  int = undefined
  float = undefined
  double = undefined
  char = undefined
  string = undefined
  infile = undefined
  outfile = undefined
  listType = undefined
  setType = undefined
  arrayType = undefined
  listInnerType = undefined
  funcType = undefined
  void = undefined

instance TypeElim MatlabCode where
  getType = undefined
  getTypeString = undefined

instance RenderType MatlabCode where
  multiType = undefined
  typeFromData = undefined

instance InternalTypeElim MatlabCode where
  type' = undefined

instance UnaryOpSym MatlabCode where
  notOp = undefined
  negateOp = undefined
  sqrtOp = undefined
  absOp = undefined
  logOp = undefined
  lnOp = undefined
  expOp = undefined
  sinOp = undefined
  cosOp = undefined
  tanOp = undefined
  asinOp = undefined
  acosOp = undefined
  atanOp = undefined
  floorOp = undefined
  ceilOp = undefined

instance BinaryOpSym MatlabCode where
  equalOp = undefined
  notEqualOp = undefined
  greaterOp = undefined
  greaterEqualOp = undefined
  lessOp = undefined
  lessEqualOp = undefined
  plusOp = undefined
  minusOp = undefined
  multOp = undefined
  divideOp = undefined
  powerOp = undefined
  moduloOp = undefined
  andOp = undefined
  orOp = undefined

instance OpElim MatlabCode where
  uOp = undefined
  bOp = undefined
  uOpPrec = undefined
  bOpPrec = undefined

instance ScopeSym MatlabCode where
  global = undefined
  mainFn = undefined
  local = undefined

instance ScopeElim MatlabCode where
  scopeData = undefined

instance VariableSym MatlabCode where
  type Variable MatlabCode = VarData
  var = undefined
  constant = undefined
  extVar = undefined

instance VariableElim MatlabCode where
  variableName = undefined
  variableType = undefined

instance InternalVarElim MatlabCode where
  variableBind = undefined
  variable = undefined

instance RenderVariable MatlabCode where
  varFromData = undefined

instance ValueSym MatlabCode where
  type Value MatlabCode = ValData
  valueType = undefined

instance Argument MatlabCode where
  pointerArg = undefined

instance Literal MatlabCode where
  litTrue = undefined
  litFalse = undefined
  litChar = undefined
  litDouble = undefined
  litFloat = undefined
  litInt = undefined
  litString = undefined
  litArray = undefined
  litList = undefined
  litSet = undefined

instance MathConstant MatlabCode where
  pi = undefined

instance VariableValue MatlabCode where
  valueOf = undefined

instance CommandLineArgs MatlabCode where
  arg = undefined
  argsList = undefined
  argExists = undefined

instance NumericExpression MatlabCode where
  (#~) = undefined
  (#/^) = undefined
  (#|) = undefined
  (#+) = undefined
  (#-) = undefined
  (#*) = undefined
  (#/) = undefined
  (#%) = undefined
  (#^) = undefined
  log = undefined
  ln = undefined
  exp = undefined
  sin = undefined
  cos = undefined
  tan = undefined
  csc = undefined
  sec = undefined
  cot = undefined
  arcsin = undefined
  arccos = undefined
  arctan = undefined
  floor = undefined
  ceil = undefined

instance BooleanExpression MatlabCode where
  (?!) = undefined
  (?&&) = undefined
  (?||) = undefined

instance Comparison MatlabCode where
  (?<) = undefined
  (?<=) = undefined
  (?>) = undefined
  (?>=) = undefined
  (?==) = undefined
  (?!=) = undefined

instance ValueExpression MatlabCode where
  inlineIf = undefined
  funcAppMixedArgs = undefined
  extFuncAppMixedArgs = undefined
  libFuncAppMixedArgs = undefined
  lambda = undefined
  notNull = undefined

instance RenderValue MatlabCode where
  inputFunc = undefined
  printFunc = undefined
  printLnFunc = undefined
  printFileFunc = undefined
  printFileLnFunc = undefined
  cast = undefined
  call = undefined
  valFromData = undefined

instance ValueElim MatlabCode where
  valuePrec = undefined
  valueInt = undefined
  value = undefined

instance IndexTranslator MatlabCode where
  intToIndex = undefined
  indexToInt = undefined

instance Array MatlabCode where
  arrayElem = undefined

instance List MatlabCode where
  listSize = undefined
  listAdd = undefined
  listAppend = undefined
  listAccess = undefined
  listSet = undefined
  indexOf = undefined

instance Set MatlabCode where
  contains = undefined
  setAdd = undefined
  setRemove = undefined
  setUnion = undefined

instance InternalList MatlabCode where
  listSlice' = undefined

instance InternalListFunc MatlabCode where
  listSizeFunc = undefined
  listAddFunc = undefined
  listAppendFunc = undefined
  listAccessFunc = undefined
  listSetFunc = undefined

instance BinderSym MatlabCode where
  binder = undefined

instance BinderElim MatlabCode where
  binderName = undefined
  binderType = undefined

instance InternalBinderElim MatlabCode where
  binderElim = undefined

instance ThunkSym MatlabCode where
  type Thunk MatlabCode = CommonThunk VS

instance ThunkAssign MatlabCode where
  thunkAssign = undefined

instance VectorType MatlabCode where
  vecType = undefined

instance VectorDecl MatlabCode where
  vecDec = undefined
  vecDecDef = undefined

instance VectorThunk MatlabCode where
  vecThunk = undefined

instance VectorExpression MatlabCode where
  vecScale = undefined
  vecAdd = undefined
  vecIndex = undefined
  vecDot = undefined

instance RenderFunction MatlabCode where
  funcFromData = undefined

instance FunctionElim MatlabCode where
  functionType = undefined
  function = undefined

instance InternalAssignStmt MatlabCode where
  multiAssign = undefined

instance InternalIOStmt MatlabCode where
  printSt = undefined

instance InternalControlStmt MatlabCode where
  multiReturn = undefined

instance RenderStatement MatlabCode where
  stmt = undefined
  loopStmt = undefined
  stmtFromData = undefined

instance StatementElim MatlabCode where
  statement = undefined
  statementTerm = undefined

instance StatementSym MatlabCode where
  type Statement MatlabCode = (Doc, Terminator)
  valStmt = undefined
  emptyStmt = undefined
  multi = undefined

instance AssignStatement MatlabCode where
  assign = undefined
  (&-=) = undefined
  (&+=) = undefined
  (&++) = undefined
  (&--) = undefined

instance DeclStatement MatlabCode where
  varDec = undefined
  varDecDef = undefined
  setDec = undefined
  setDecDef = undefined
  listDec = undefined
  listDecDef = undefined
  arrayDec = undefined
  arrayDecDef = undefined
  constDecDef = undefined
  funcDecDef = undefined

instance IOStatement MatlabCode where
  print = undefined
  printLn = undefined
  printStr = undefined
  printStrLn = undefined
  printFile = undefined
  printFileLn = undefined
  printFileStr = undefined
  printFileStrLn = undefined
  getInput = undefined
  discardInput = undefined
  getFileInput = undefined
  discardFileInput = undefined
  openFileR = undefined
  openFileW = undefined
  openFileA = undefined
  closeFile = undefined
  getFileInputLine = undefined
  discardFileLine = undefined
  getFileInputAll = undefined

instance StringStatement MatlabCode where
  stringSplit = undefined
  stringListVals = undefined
  stringListLists = undefined

instance FunctionSym MatlabCode where
  type Function MatlabCode = FuncData

instance FuncAppStatement MatlabCode where
  inOutCall = undefined
  extInOutCall = undefined

instance CommentStatement MatlabCode where
  comment = undefined

instance ControlStatement MatlabCode where
  break = undefined
  continue = undefined
  returnStmt = undefined
  throw = undefined
  ifCond = undefined
  switch = undefined
  ifExists = undefined
  for = undefined
  forRange = undefined
  forEach = undefined
  while = undefined
  tryCatch = undefined
  assert = undefined

instance VisibilitySym MatlabCode where
  type Visibility MatlabCode = Doc
  private = undefined
  public = undefined

instance RenderVisibility MatlabCode where
  visibilityFromData = undefined

instance VisibilityElim MatlabCode where
  visibility = undefined

instance MethodTypeSym MatlabCode where
  type MethodType MatlabCode = TypeData
  mType = undefined

instance ParameterSym MatlabCode where
  type Parameter MatlabCode = ParamData
  param = undefined
  pointerParam = undefined

instance RenderParam MatlabCode where
  paramFromData = undefined

instance ParamElim MatlabCode where
  parameterName = undefined
  parameterType = undefined
  parameter = undefined

instance MethodSym MatlabCode where
  type Method MatlabCode = MethodData
  docMain = undefined
  function = undefined
  mainFunction = undefined
  docFunc = undefined
  inOutFunc = undefined
  docInOutFunc = undefined

instance RenderMethod MatlabCode where
  commentedFunc = undefined
  mthdFromData = undefined

instance ProcRenderMethod MatlabCode where
  intFunc = undefined

instance MethodElim MatlabCode where
  method = undefined

instance ModuleSym MatlabCode where
  type Module MatlabCode = ModData
  buildModule = undefined

instance RenderMod MatlabCode where
  modFromData = undefined
  updateModuleDoc = undefined

instance ModuleElim MatlabCode where
  module' = undefined

instance BlockCommentSym MatlabCode where
  blockComment = undefined
  docComment = undefined

instance BlockCommentElim MatlabCode where
  blockComment' = undefined

-- convenience
mlName, mlVersion :: String
mlName = "MATLAB"
mlVersion = "R2024b"
