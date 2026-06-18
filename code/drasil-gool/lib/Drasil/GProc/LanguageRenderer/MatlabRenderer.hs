{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The logic to render MATLAB code is contained in this module.
module Drasil.GProc.LanguageRenderer.MatlabRenderer (
  -- * MATLAB Code Configuration -- defines syntax of all MATLAB code
  MatlabCode(..), mlName, mlVersion
) where

import Drasil.Shared.InterfaceCommon (Label, UnRepr(..), SharedProg, BodySym(..),
  BlockSym(..), TypeSym(..), VariableSym(..), VariableElim(..), ValueSym(..),
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
  RenderType(..), UnaryOpSym(..), BinaryOpSym(..),
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind),
  RenderValue(..), ValueElim(..), InternalListFunc(..), RenderFunction(..),
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..),
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm),
  RenderVisibility(..), VisibilityElim, MethodTypeSym(..), RenderParam(..),
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim,
  BlockCommentSym(..), BlockCommentElim, ScopeElim(..), InternalBinderElim(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (import', body,
  block, uOp, bOp, variable, function, statement, visibility, parameter,
  method, blockComment')
import Drasil.GProc.RendererClassesProc (ProcRenderSym, RenderFile(..),
  RenderMod(..), ModuleElim, ProcRenderMethod(..))
import qualified Drasil.GProc.RendererClassesProc as RC (module')
import qualified Drasil.GProc.LanguageRenderer.AbstractProc as A (function)
import qualified Drasil.Shared.LanguageRenderer as R (commentedItem,
  parameterList)
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  comment, param, docFunc)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (mainBody,
  functionDoc, docInOutFunc')
import Drasil.Shared.AST (Terminator, FileData, FuncData, ModData,
  MethodData, mthd, updateMthd, ParamData, paramVar, paramDoc, pd, ProgData,
  TypeData, ValData, VarData, CommonThunk, mthdDoc, modDoc)
import Drasil.Shared.Helpers (toCode, toState, onCodeValue, onStateValue,
  on2CodeValues, on2StateValues)
import Drasil.Shared.State (VS, lensMStoVS)

import Control.Lens.Zoom (zoom)

import Drasil.FileHandling.Legacy (indent)
import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, text, (<>), (<+>), vcat, hcat,
  parens, brackets, equals, punctuate)

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

instance UnRepr MatlabCode inner where
  unRepr = unMLC

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
  import' = unMLC

instance BodySym MatlabCode where
  type Body MatlabCode = Doc
  body = undefined
  addComments = undefined

instance RenderBody MatlabCode where
  multiBody = undefined

instance BodyElim MatlabCode where
  body = unMLC

instance BlockSym MatlabCode where
  type Block MatlabCode = Doc
  block = undefined

instance RenderBlock MatlabCode where
  multiBlock = undefined

instance BlockElim MatlabCode where
  block = unMLC

instance TypeSym MatlabCode where
  bool = undefined
  int = undefined
  float = undefined
  double = undefined
  char = undefined
  string = undefined
  infile = undefined
  outfile = undefined
  referenceType = id -- Ignore reference types in "high-level" langauges for now; later on think about using boxed/unboxed types
  listType = undefined
  setType = undefined
  arrayType = undefined
  innerType = undefined
  funcType = undefined
  void = undefined

instance RenderType MatlabCode where
  multiType = undefined

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
  scopeData = unMLC

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
  arrayLength = undefined
  arrayCopy = undefined

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
  comment = G.comment mlCmtStart

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
  private = toCode empty
  public = toCode empty

instance RenderVisibility MatlabCode where
  visibilityFromData = undefined

instance VisibilityElim MatlabCode where
  visibility = unMLC

instance MethodTypeSym MatlabCode where
  type MethodType MatlabCode = TypeData
  mType = zoom lensMStoVS

instance ParameterSym MatlabCode where
  type Parameter MatlabCode = ParamData
  -- A MATLAB parameter is just the variable name.
  param = G.param mlParam
  pointerParam = param

instance RenderParam MatlabCode where
  paramFromData v' d = do
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)

instance ParamElim MatlabCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unMLC

instance MethodSym MatlabCode where
  type Method MatlabCode = MethodData
  docMain = mainFunction
  function = A.function
  mainFunction = CP.mainBody
  docFunc = G.docFunc CP.functionDoc

  -- MATLAB returns values through the output list in the header
  -- (function [outs] = name(ins)), not through a return statement, so we build
  -- the method directly instead of reusing the shared inOutFunc machinery.
  inOutFunc n _ ins outs both b = do
    pms  <- mapM param (both ++ ins)
    rets <- mapM (zoom lensMStoVS) (both ++ outs)
    bod  <- b
    pure $ toCode $ mthd $ mlFuncDoc n (map RC.variable rets) pms (RC.body bod)
  docInOutFunc n s = CP.docInOutFunc' CP.functionDoc (inOutFunc n s)

instance RenderMethod MatlabCode where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)
  mthdFromData _ d = toState $ toCode $ mthd d

instance ProcRenderMethod MatlabCode where
  intFunc _ n _ _ ps b = do
    pms <- sequence ps
    bod <- b
    pure $ toCode $ mthd $ mlFuncDoc n [] pms (RC.body bod)

instance MethodElim MatlabCode where
  method = mthdDoc . unMLC

instance ModuleSym MatlabCode where
  type Module MatlabCode = ModData
  buildModule = undefined

instance RenderMod MatlabCode where
  modFromData = undefined
  updateModuleDoc = undefined

instance ModuleElim MatlabCode where
  module' = modDoc . unMLC

instance BlockCommentSym MatlabCode where
  blockComment = undefined
  docComment = undefined

instance BlockCommentElim MatlabCode where
  blockComment' = undefined

-- | A MATLAB parameter renders as just the variable name.
mlParam :: MatlabCode (Variable MatlabCode) -> Doc
mlParam = RC.variable

-- | Renders a MATLAB function: @function [outs] = name(ins) ... end@.
--   With no outputs the @[outs] =@ part is dropped; with a single output the
--   brackets are dropped (@function out = name(ins)@).
mlFuncDoc :: (CommonRenderSym r) => Label -> [Doc] -> [r (Parameter r)] -> Doc
  -> Doc
mlFuncDoc n outs pms bod =
  vcat [text "function" <+> retDoc <> text n <> parens (R.parameterList pms),
        indent bod,
        text "end"]
  where retDoc = case outs of
          []  -> empty
          [o] -> o <+> equals <> text " "
          os  -> brackets (hcat (punctuate (text ", ") os)) <+> equals <> text " "

-- convenience
mlName, mlVersion :: String
mlName = "MATLAB"
mlVersion = "R2024b"

-- Comments
mlCmtStart :: Doc
mlCmtStart = text "%"
