{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The logic to render MATLAB code is contained in this module.
module Drasil.GProc.LanguageRenderer.MatlabRenderer (
  -- * MATLAB Code Configuration -- defines syntax of all MATLAB code
  MatlabCode(..), mlName, mlVersion
) where

import Drasil.Shared.InterfaceCommon (Label, VSType, UnRepr(..), SharedProg, BodySym(..),
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
  block, uOp, bOp, variable, value, function, statement, visibility, parameter,
  method, blockComment')
import Drasil.GProc.RendererClassesProc (ProcRenderSym, RenderFile(..),
  RenderMod(..), ModuleElim, ProcRenderMethod(..))
import qualified Drasil.GProc.LanguageRenderer.AbstractProc as A (fileDoc,
  docMod, fileFromData, buildModule, modFromData, function)
import qualified Drasil.Shared.LanguageRenderer as R (commentedMod,
  commentedItem, parameterList, body, addComments, docCmt, multiStmt, sqrt,
  abs, log10, log, exp, sin, cos, tan, asin, acos, atan, floor, ceil)
import qualified Drasil.GProc.RendererClassesProc as RC (module')
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  comment, param, docFunc, var, multiBody, block, multiBlock, stmt, loopStmt,
  negateOp, plusOp, minusOp, multOp, divideOp, equalOp, greaterOp,
  greaterEqualOp, lessOp, lessEqualOp, csc, sec, cot, valueOf, litDouble,
  litInt, litString, valStmt, emptyStmt, assign)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (mainBody,
  functionDoc, docInOutFunc')
import qualified Drasil.Shared.LanguageRenderer.CLike as C (andOp, orOp,
  litTrue, litFalse)
import qualified Drasil.Shared.LanguageRenderer.Common as CS (varDecDef)
import Drasil.Shared.AST (Terminator(..), FileType(Combined), FileData, fileD,
  FuncData, ModData, md, updateMod, MethodData, mthd, updateMthd, ParamData,
  paramVar, paramDoc, pd, ProgData, TypeData, cType, ValData, vd, val, valPrec,
  valInt, opDoc, opPrec, VarData, varName,
  varType, varBind, varDoc, vard, CommonThunk, progD, mthdDoc, modDoc)
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.LanguageRenderer.Constructors (typeFromData, unOpPrec,
  powerPrec, unExpr, unExpr', binExpr)
import Drasil.Shared.Helpers (toCode, toState, onCodeValue, onStateValue,
  onCodeList, onStateList, on2CodeValues, on2StateValues)
import Drasil.Shared.State (VS, lensGStoFS, lensMStoVS, revFiles, setFileType,
  getMainDoc)

import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)

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
  prog n st files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n st) fs

instance CommonRenderSym MatlabCode
instance ProcRenderSym MatlabCode

instance UnRepr MatlabCode inner where
  unRepr = unMLC

instance FileSym MatlabCode where
  type File MatlabCode = FileData
  fileDoc m = do
    modify (setFileType Combined)
    A.fileDoc mlExt m
  docMod = A.docMod mlExt

instance RenderFile MatlabCode where
  top _ = toCode empty
  bottom = toCode empty
  commentedMod = on2StateValues (on2CodeValues R.commentedMod)
  fileFromData = A.fileFromData (onCodeValue . fileD)

instance ImportSym MatlabCode where
  type Import MatlabCode = Doc
  langImport = undefined
  modImport = undefined

instance ImportElim MatlabCode where
  import' = unMLC

instance BodySym MatlabCode where
  type Body MatlabCode = Doc
  body = onStateList (onCodeList R.body)
  addComments s = onStateValue (onCodeValue (R.addComments s mlCmtStart))

instance RenderBody MatlabCode where
  multiBody = G.multiBody

instance BodyElim MatlabCode where
  body = unMLC

instance BlockSym MatlabCode where
  type Block MatlabCode = Doc
  block = G.block

instance RenderBlock MatlabCode where
  multiBlock = G.multiBlock

instance BlockElim MatlabCode where
  block = unMLC

instance TypeSym MatlabCode where
  bool = mlTy Boolean "logical"
  int = mlTy Integer "int"
  float = mlTy Float "double"
  double = mlTy Double "double"
  char = mlTy Char "char"
  string = mlTy String "string"
  infile = mlTy InFile "file"
  outfile = mlTy OutFile "file"
  listType = undefined
  setType = undefined
  arrayType = undefined
  listInnerType = undefined
  funcType = undefined
  void = mlTy Void "void"

instance RenderType MatlabCode where
  multiType = undefined

instance UnaryOpSym MatlabCode where
  notOp = undefined
  negateOp = G.negateOp
  sqrtOp = unOpPrec R.sqrt
  absOp = unOpPrec R.abs
  logOp = unOpPrec R.log10
  lnOp = unOpPrec R.log
  expOp = unOpPrec R.exp
  sinOp = unOpPrec R.sin
  cosOp = unOpPrec R.cos
  tanOp = unOpPrec R.tan
  asinOp = unOpPrec R.asin
  acosOp = unOpPrec R.acos
  atanOp = unOpPrec R.atan
  floorOp = unOpPrec R.floor
  ceilOp = unOpPrec R.ceil

instance BinaryOpSym MatlabCode where
  equalOp = G.equalOp
  notEqualOp = undefined
  greaterOp = G.greaterOp
  greaterEqualOp = G.greaterEqualOp
  lessOp = G.lessOp
  lessEqualOp = G.lessEqualOp
  plusOp = G.plusOp
  minusOp = G.minusOp
  multOp = G.multOp
  divideOp = G.divideOp
  powerOp = powerPrec "^"
  moduloOp = undefined
  andOp = C.andOp
  orOp = C.orOp

instance OpElim MatlabCode where
  uOp = opDoc . unMLC
  bOp = opDoc . unMLC
  uOpPrec = opPrec . unMLC
  bOpPrec = opPrec . unMLC

instance ScopeSym MatlabCode where
  global = undefined
  mainFn = undefined
  local = undefined

instance ScopeElim MatlabCode where
  scopeData = unMLC

instance VariableSym MatlabCode where
  type Variable MatlabCode = VarData
  var = G.var
  constant = var
  extVar = undefined

instance VariableElim MatlabCode where
  variableName = varName . unMLC
  variableType = onCodeValue varType

instance InternalVarElim MatlabCode where
  variableBind = varBind . unMLC
  variable = varDoc . unMLC

instance RenderVariable MatlabCode where
  varFromData b n t' d = do
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym MatlabCode where
  type Value MatlabCode = ValData
  valueType = undefined

instance Argument MatlabCode where
  pointerArg = undefined

instance Literal MatlabCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = undefined
  litDouble = G.litDouble
  litFloat = undefined
  litInt = G.litInt
  litString = G.litString
  litArray = undefined
  litList = undefined
  litSet = undefined

instance MathConstant MatlabCode where
  pi = undefined

instance VariableValue MatlabCode where
  valueOf = G.valueOf

instance CommandLineArgs MatlabCode where
  arg = undefined
  argsList = undefined
  argExists = undefined

instance NumericExpression MatlabCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) = binExpr divideOp
  (#%) = undefined
  (#^) = binExpr powerOp
  log = unExpr logOp
  ln = unExpr lnOp
  exp = unExpr expOp
  sin = unExpr sinOp
  cos = unExpr cosOp
  tan = unExpr tanOp
  csc = G.csc
  sec = G.sec
  cot = G.cot
  arcsin = unExpr asinOp
  arccos = unExpr acosOp
  arctan = unExpr atanOp
  floor = unExpr floorOp
  ceil = unExpr ceilOp

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
  valFromData p i t' d = do
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim MatlabCode where
  valuePrec = valPrec . unMLC
  valueInt = valInt . unMLC
  value = val . unMLC

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
  stmt = G.stmt
  loopStmt = G.loopStmt
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim MatlabCode where
  statement = fst . unMLC
  statementTerm = snd . unMLC

instance StatementSym MatlabCode where
  type Statement MatlabCode = (Doc, Terminator)
  valStmt = G.valStmt Semi
  emptyStmt = G.emptyStmt
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement MatlabCode where
  assign = G.assign Semi
  (&-=) = undefined
  (&+=) = undefined
  (&++) = undefined
  (&--) = undefined

instance DeclStatement MatlabCode where
  varDec v scp = CS.varDecDef v scp Nothing
  varDecDef v scp e = CS.varDecDef v scp (Just e)
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
  -- MATLAB has no `return <expr>`: a function returns by assigning its named
  -- output, so a return becomes `result = <value>;`.
  returnStmt v' = do
    v <- zoom lensMStoVS v'
    stmtFromData (text mlRet <+> equals <+> RC.value v) Semi
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
  intFunc _ n _ t ps b = do
    pms <- sequence ps
    tp  <- t
    bod <- b
    -- A function with a non-void return type declares the output variable
    -- 'result' in its header (function result = name(...)); returnStmt then
    -- assigns to it. Void functions declare no output.
    let outs = [text mlRet | cType (unMLC tp) /= Void]
    pure $ toCode $ mthd $ mlFuncDoc n outs pms (RC.body bod)

instance MethodElim MatlabCode where
  method = mthdDoc . unMLC

instance ModuleSym MatlabCode where
  type Module MatlabCode = ModData
  -- MATLAB single-file first pass: no module wrapper and no import statements
  -- The main script body goes first, then the local function definitions.
  buildModule n _ = A.buildModule n getMainDoc (pure empty)

instance RenderMod MatlabCode where
  modFromData n = A.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim MatlabCode where
  module' = modDoc . unMLC

instance BlockCommentSym MatlabCode where
  blockComment lns = toCode $ R.docCmt lns mlBlockCmtStart mlBlockCmtEnd
  docComment = onStateValue (\lns -> toCode $
    R.docCmt lns mlBlockCmtStart mlBlockCmtEnd)

instance BlockCommentElim MatlabCode where
  blockComment' = unMLC

-- convenience
mlName, mlVersion :: String
mlName = "MATLAB"
mlVersion = "R2024b"

-- | MATLAB source file extension.
mlExt :: String
mlExt = "m"

-- | Name of the implicit output variable a value-returning function assigns to.
mlRet :: String
mlRet = "result"

-- | Makes a MATLAB type. Only the 'CodeType' tag is used (internally); the
--   name is never written out, since MATLAB code has no type annotations.
mlTy :: CodeType -> String -> VSType MatlabCode
mlTy c s = typeFromData c s (text s)

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

-- Comments
mlCmtStart :: Doc
mlCmtStart = text "%"

-- | MATLAB block-comment delimiters.
mlBlockCmtStart, mlBlockCmtEnd :: Doc
mlBlockCmtStart = text "%{"
mlBlockCmtEnd = text "%}"
