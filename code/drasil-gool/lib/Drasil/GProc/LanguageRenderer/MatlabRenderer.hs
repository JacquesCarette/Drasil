{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

-- | The logic to render MATLAB code is contained in this module
module Drasil.GProc.LanguageRenderer.MatlabRenderer (
  -- * MATLAB Code Configuration -- defines syntax of all MATLAB code
  MatlabCode(..), mlName, mlVersion
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (SharedProg, Label, VSType, SValue, litZero,
  SVariable, MSStatement, MSBlock, SMethod, BodySym(..), BlockSym(..),
  TypeSym(..), TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..),
  Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), funcApp, extFuncApp, IndexTranslator(..),
  Array(..), List(..), Set(..), InternalList(..), ThunkSym(..), VectorType(..),
  VectorDecl(..), VectorThunk(..), VectorExpression(..), ThunkAssign(..),
  StatementSym(..), AssignStatement(..), DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), VisibilitySym(..), ScopeSym(..),
  ParameterSym(..), BinderSym(..), BinderElim(..), MethodSym(..), (&=),
  switchAsIf, convScope)
import Drasil.GProc.InterfaceProc (ProcProg, FSModule, ProgramSym(..),
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
import qualified Drasil.Shared.RendererClassesCommon as RC (import', body, block,
  type', uOp, bOp, variable, value, function, statement, visibility, parameter,
  method, blockComment')
import Drasil.GProc.RendererClassesProc (ProcRenderSym, RenderFile(..),
  RenderMod(..), ModuleElim, ProcRenderMethod(..))
import qualified Drasil.GProc.RendererClassesProc as RC (module')
import Drasil.Shared.LanguageRenderer (printLabel, listSep, listSep',
  parameterList, forLabel, inLabel, tryLabel, catchLabel,
  valueList, binderList)
import qualified Drasil.Shared.LanguageRenderer as R (sqrt, abs, log10, log,
  exp, sin, cos, tan, asin, acos, atan, floor, ceil, multiStmt, body,
  addComments, blockCmt, docCmt, commentedMod, listSetFunc, commentedItem,
  break, continue, constDec', assign, subAssign, addAssign)
import Drasil.Shared.LanguageRenderer.Constructors (mkVal, mkStateVal, VSOp,
  unOpPrec, powerPrec, unExpr, unExpr', binExpr, multPrec, typeUnExpr,
  typeBinExpr, mkStmt, mkStmtNoEnd)
import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (OptionalSpace(..))
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  block, multiBlock, litChar, litDouble, litInt, litString, valueOf, negateOp,
  equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp,
  minusOp, multOp, divideOp, moduloOp, call, funcAppMixedArgs, lambda,
  listAccess, listSet, tryCatch, csc, multiBody, sec, cot, stmt, loopStmt,
  emptyStmt, print, comment, valStmt, returnStmt, param, docFunc, throw, arg,
  argsList, ifCond, smartAdd, local, var, smartSub)

import qualified Drasil.Shared.LanguageRenderer.Common as CS

import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (
  listDec, listDecDef,
  notNull, functionDoc, listAdd,
  listAppend, intToIndex', indexToInt', inOutFunc, docInOutFunc', forLoopError,
  openFileR', openFileW', openFileA', multiReturn, multiAssign,
  inOutCall, mainBody, argExists, litSet)

import qualified Drasil.Shared.LanguageRenderer.CLike as C (litTrue, litFalse,
  notOp, andOp, orOp, inlineIf, while)

import qualified Drasil.GProc.LanguageRenderer.AbstractProc as A (fileDoc,
  fileFromData, buildModule, docMod, modFromData, listInnerType, arrayElem,
  funcDecDef, function)
import qualified Drasil.Shared.LanguageRenderer.Macros as M (increment1,
  decrement1, ifExists, stringListVals, stringListLists, arrayDecAsList)
import Drasil.Shared.AST (Terminator(..), FileType(..), FileData(..), fileD,
  FuncData(..), ModData(..), md, updateMod, MethodData(..), mthd, OpData(..),
  ParamData(..), ProgData(..), TypeData(..), td, ValData(..), vd, VarData(..),
  vard, CommonThunk, progD, fd, pd, updateMthd, commonThunkDim, commonThunkElim,
  vectorize, vectorize2, commonVecIndex, sumComponents, pureValue, ScopeTag(..),
  ScopeData(..), sd, BinderD(..), bindFormD)
import Drasil.Shared.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  on2CodeValues, on2StateValues, onCodeList, onStateList, emptyIfEmpty)
import Drasil.Shared.State (VS, lensGStoFS, revFiles, setFileType, lensMStoVS,
  getModuleImports, addModuleImportVS, getLangImports, getLibImports,
  addLibImportVS, useVarName, getMainDoc, genLoopIndex, genVarNameIf,
  setVarScope, getVarScope)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe, isNothing)
import Data.Functor ((<&>))
import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)
import Data.List (intercalate, sort)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), empty, brackets, vcat,
  quotes, doubleQuotes, parens, equals, colon)
import qualified Text.PrettyPrint.HughesPJ as D (float)

mlExt :: String
mlExt = "m"

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
  langImport n = let modName = text n
    in toCode $ importLabel <+> modName
  modImport n = let modName = text n
                    fileName = text $ n ++ '.' : mlExt
    in toCode $ vcat [includeLabel <> parens (doubleQuotes fileName),
                      importLabel <+> text "." <> modName]

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
  bool = CS.bool
  int = mlIntType
  float = mlFloatType
  double = mlDoubleType
  char = mlCharType
  string = mlStringType
  infile = mlInfileType
  outfile = mlOutfileType
  listType = mlListType
  setType = mlSetType
  arrayType = listType -- Treat arrays and lists the same, as in Python
  listInnerType = A.listInnerType
  funcType = CS.funcType
  void = mlVoidType

instance TypeElim MatlabCode where
  getType = cType . unMLC
  getTypeString v = let tp = typeString $ unMLC v in
    case cType $ unMLC v of
      (Object _) -> error mlClassError
      _ -> tp

instance RenderType MatlabCode where
  multiType ts = do
    typs <- sequence ts
    let mt = mlTuple $ map getTypeString typs
    typeFromData Void mt (text mt)
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim MatlabCode where
  type' v = let t = typeDoc $ unMLC v in
    case cType $ unMLC v of
      (Object _) -> t <> error mlClassError
      _ -> t

instance UnaryOpSym MatlabCode where
  notOp = C.notOp
  negateOp = G.negateOp
  sqrtOp = mlUnaryMath R.sqrt
  absOp = mlUnaryMath R.abs
  logOp = mlUnaryMath R.log10
  lnOp = mlUnaryMath R.log
  expOp = mlUnaryMath R.exp
  sinOp = mlUnaryMath R.sin
  cosOp = mlUnaryMath R.cos
  tanOp = mlUnaryMath R.tan
  asinOp = mlUnaryMath R.asin
  acosOp = mlUnaryMath R.acos
  atanOp = mlUnaryMath R.atan
  floorOp = mlUnaryMath R.floor
  ceilOp = mlUnaryMath R.ceil

instance BinaryOpSym MatlabCode where
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
  powerOp = powerPrec mlPower
  moduloOp = G.moduloOp
  andOp = C.andOp
  orOp = C.orOp

instance OpElim MatlabCode where
  uOp = opDoc . unMLC
  bOp = opDoc . unMLC
  uOpPrec = opPrec . unMLC
  bOpPrec = opPrec . unMLC

instance ScopeSym MatlabCode where
  global = toCode $ sd Global
  mainFn = global
  local = G.local

instance ScopeElim MatlabCode where
  scopeData = unMLC

instance VariableSym MatlabCode where
  type Variable MatlabCode = VarData
  var = G.var
  constant = var
  extVar l n t = modify (addModuleImportVS l) >> CS.extVar l n t

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
  valueType v = valType <$> v

instance Argument MatlabCode where
  pointerArg = id

instance Literal MatlabCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = mlLitFloat
  litInt = G.litInt
  litString = G.litString
  litArray = litList
  litList = mlLitList
  litSet = CP.litSet (text "Set" <>) (parens . brackets)

instance MathConstant MatlabCode where
  pi :: SValue MatlabCode
  pi = mkStateVal double mlPi

instance VariableValue MatlabCode where
  valueOf = G.valueOf

instance CommandLineArgs MatlabCode where
  arg n = G.arg (litInt $ n+1) argsList
  argsList = G.argsList mlArgs
  argExists = CP.argExists

instance NumericExpression MatlabCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) v1' v2' = do -- Julia has integer division via `÷` or `div()`
    v1 <- v1'
    v2 <- v2'
    let mlDivision Integer Integer = binExpr (multPrec mlIntDiv)
        mlDivision _ _ = binExpr divideOp
    mlDivision (getType $ valueType v1) (getType $ valueType v2)
        (pure v1) (pure v2)
  (#%) = binExpr moduloOp
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
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

instance Comparison MatlabCode where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression MatlabCode where
  inlineIf = C.inlineIf

  funcAppMixedArgs = G.funcAppMixedArgs
  extFuncAppMixedArgs l n t ps ns = do
    modify (addModuleImportVS l)
    CS.extFuncAppMixedArgs l n t ps ns
  libFuncAppMixedArgs l n t ps ns = do
    modify (addLibImportVS l)
    CS.extFuncAppMixedArgs l n t ps ns

  lambda = G.lambda mlLambda

  notNull = CP.notNull mlNull

instance RenderValue MatlabCode where
  inputFunc = mkStateVal string (mlReadLine <> parens empty)
  printFunc = mkStateVal void mlPrintFunc
  printLnFunc = mkStateVal void mlPrintLnFunc
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty

  cast = mlCast

  call = G.call mlNamedArgSep

  valFromData p i t' d = do
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim MatlabCode where
  valuePrec = valPrec . unMLC
  valueInt = valInt . unMLC
  value = val . unMLC

instance IndexTranslator MatlabCode where
  intToIndex = CP.intToIndex'
  indexToInt = CP.indexToInt'

instance Array MatlabCode where
  arrayElem = A.arrayElem

instance List MatlabCode where
  listSize = CS.listSize
  listAdd = CP.listAdd
  listAppend = CP.listAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = mlIndexOf

instance Set MatlabCode where
  contains s e = funcApp "in" bool [e, s]
  setAdd s e = funcApp "push!" void [s, e]
  setRemove s e = funcApp "delete!" void [s, e]
  setUnion a b = funcApp "union!" void [a, b]

instance InternalList MatlabCode where
  listSlice' b e s vn vo = mlListSlice vn vo b e (fromMaybe (litInt 1) s)

instance InternalListFunc MatlabCode where
  listSizeFunc l = do
    f <- funcApp mlListSize int [l]
    funcFromData (RC.value f) int
  listAddFunc l i v = do
    f <- funcApp mlListAdd void [l, i, v]
    funcFromData (RC.value f) void
  listAppendFunc l v = do
    f <- funcApp mlListAppend void [l, v]
    funcFromData (RC.value f) void
  listAccessFunc = CS.listAccessFunc
  listSetFunc = CS.listSetFunc R.listSetFunc

instance BinderSym MatlabCode where
  binder nm tp = onCodeValue (bindFormD nm) <$> tp

instance BinderElim MatlabCode where
  binderName = bindName . unMLC
  binderType = onCodeValue bindType

instance InternalBinderElim MatlabCode where
  binderElim = text . bindName . unMLC

instance ThunkSym MatlabCode where
  type Thunk MatlabCode = CommonThunk VS

instance ThunkAssign MatlabCode where
  thunkAssign v t = do
    iName <- genLoopIndex
    let
      i = var iName int
      dim = fmap pure $ t >>= commonThunkDim (fmap unMLC . listSize . fmap pure) . unMLC
      loopInit = zoom lensMStoVS (fmap unMLC t) >>= commonThunkElim
        (const emptyStmt) (const $ assign v $ litZero $ fmap variableType v)
      loopBody = zoom lensMStoVS (fmap unMLC t) >>= commonThunkElim
        (valStmt . listSet (valueOf v) (valueOf i) . vecIndex (valueOf i) . pure . pure)
        ((v &+=) . vecIndex (valueOf i) . pure . pure)
    multi [loopInit,
      forRange i (litInt 0) dim (litInt 1) $ body [block [loopBody]]]

instance VectorType MatlabCode where
  vecType = listType

instance VectorDecl MatlabCode where
  vecDec = listDec
  vecDecDef = listDecDef

instance VectorThunk MatlabCode where
  vecThunk = pure . pure . pureValue . fmap unMLC . valueOf

instance VectorExpression MatlabCode where
  vecScale k = fmap $ fmap $ vectorize (fmap unMLC . (k #*) . fmap pure)
  vecAdd = liftA2 $ liftA2 $ vectorize2 (\v1 v2 -> fmap unMLC $ fmap pure v1 #+ fmap pure v2)
  vecIndex i = (>>= fmap pure . commonVecIndex (fmap unMLC . flip listAccess i . fmap pure) . unMLC)
  vecDot = liftA2 $ liftA2 $ fmap sumComponents <$> vectorize2 (\v1 v2 -> fmap unMLC $ fmap pure v1 #* fmap pure v2)

instance RenderFunction MatlabCode where
  funcFromData d = onStateValue $ onCodeValue (`fd` d)

instance FunctionElim MatlabCode where
  functionType = onCodeValue fType
  function = funcDoc . unMLC

instance InternalAssignStmt MatlabCode where
  multiAssign = CP.multiAssign id

instance InternalIOStmt MatlabCode where
  printSt = mlPrint

instance InternalControlStmt MatlabCode where
  multiReturn = CP.multiReturn id

instance RenderStatement MatlabCode where
  stmt = G.stmt
  loopStmt = G.loopStmt
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim MatlabCode where
  statement = fst . unMLC
  statementTerm = snd . unMLC

instance StatementSym MatlabCode where
  type Statement MatlabCode = (Doc, Terminator)
  valStmt = G.valStmt Empty
  emptyStmt = G.emptyStmt
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement MatlabCode where
  assign = mlAssign
  (&-=) = mlSubAssign
  (&+=) = mlIncrement
  (&++) = M.increment1
  (&--) = M.decrement1

instance DeclStatement MatlabCode where
  varDec v scp = CS.varDecDef v scp Nothing
  varDecDef v scp e = CS.varDecDef v scp (Just e)
  setDec = varDec
  setDecDef = varDecDef
  listDec _ = CP.listDec
  listDecDef = CP.listDecDef
  arrayDec = M.arrayDecAsList
  arrayDecDef = listDecDef
  constDecDef = mlConstDecDef
  funcDecDef = A.funcDecDef

instance IOStatement MatlabCode where
  print      = mlOut False Nothing printFunc
  printLn    = mlOut True  Nothing printLnFunc
  printStr   = mlOut False Nothing printFunc   . litString
  printStrLn = mlOut True  Nothing printLnFunc . litString

  printFile f      = mlOut False (Just f) printFunc
  printFileLn f    = mlOut True (Just f) printLnFunc
  printFileStr f   = printFile   f . litString
  printFileStrLn f = printFileLn f . litString

  getInput = mlInput inputFunc
  discardInput = valStmt inputFunc
  getFileInput f = mlInput (readLine f)
  discardFileInput f = valStmt (readLine f)
  openFileR f n = f &= CP.openFileR' n
  openFileW f n = f &= CP.openFileW' n
  openFileA f n = f &= CP.openFileA' n
  closeFile f = valStmt $ funcApp mlCloseFunc void [f]
  getFileInputLine = getFileInput
  discardFileLine = discardFileInput
  getFileInputAll f v = v &= readLines f

instance StringStatement MatlabCode where
  stringSplit d vnew s = vnew &= funcApp mlSplit (listType string) [s, litString [d]]
  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FunctionSym MatlabCode where
  type Function MatlabCode = FuncData

instance FuncAppStatement MatlabCode where
  inOutCall = CP.inOutCall funcApp
  extInOutCall m = CP.inOutCall (extFuncApp m)

instance CommentStatement MatlabCode where
  comment = G.comment mlCmtStart

instance ControlStatement MatlabCode where
  break = mkStmtNoEnd R.break
  continue = mkStmtNoEnd R.continue
  returnStmt = G.returnStmt Empty
  throw = G.throw mlThrow Empty
  ifCond = G.ifCond id empty mlSpace elseIfLabel empty mlEnd
  switch = switchAsIf
  ifExists = M.ifExists
  for _ _ _ _ = error $ CP.forLoopError mlName
  forRange i initv finalv stepv = forEach i (mlRange initv finalv stepv)
  forEach = CS.forEach' mlForEach
  while = C.while id empty mlEnd
  tryCatch = G.tryCatch mlTryCatch
  assert condition errorMessage = do
    cond <- zoom lensMStoVS condition
    errMsg <- zoom lensMStoVS errorMessage
    mkStmtNoEnd (mlAssert cond errMsg)

instance VisibilitySym MatlabCode where
  type Visibility MatlabCode = Doc

  private = toCode empty -- Julia doesn't have private/public members
  public = toCode empty

instance RenderVisibility MatlabCode where
  visibilityFromData _ = toCode

instance VisibilityElim MatlabCode where
  visibility = unMLC

instance MethodTypeSym MatlabCode where
  type MethodType MatlabCode = TypeData

  mType = zoom lensMStoVS

instance ParameterSym MatlabCode where
  type Parameter MatlabCode = ParamData

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

  inOutFunc n s = CP.inOutFunc (function n s)
  docInOutFunc n s = CP.docInOutFunc' CP.functionDoc (inOutFunc n s)

instance RenderMethod MatlabCode where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)
  mthdFromData _ d = toState $ toCode $ mthd d

instance ProcRenderMethod MatlabCode where
  intFunc _ n _ _ ps b = do
    pms <- sequence ps
    toCode . mthd . mlIntFunc n pms <$> b

instance MethodElim MatlabCode where
  method = mthdDoc . unMLC

instance ModuleSym MatlabCode where
  type Module MatlabCode = ModData
  buildModule n is fs = mlModContents n is fs <&>
    updateModuleDoc (\m -> emptyIfEmpty m (vibcat [mlModStart n, m, mlEnd]))

instance RenderMod MatlabCode where
  modFromData n = A.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim MatlabCode where
  module' = modDoc . unMLC

instance BlockCommentSym MatlabCode where
  blockComment lns = toCode $ R.blockCmt lns mlBlockCmtStart mlBlockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns mlDocCmtStart
    mlDocCmtEnd)

instance BlockCommentElim MatlabCode where
  blockComment' = unMLC

-- convenience
mlName, mlVersion :: String
mlName = "MATLAB"
mlVersion = "R2024b"

-- Concrete versions of each Julia datatype
mlIntConc, mlFloatConc, mlDoubleConc, mlCharConc, mlStringConc, mlListConc,
  mlSetConc, mlFile, mlVoid :: String
mlIntConc = "Int64"
mlFloatConc = "Float32"
mlDoubleConc = "Float64"
mlCharConc = "Char"
mlStringConc = "String"
mlListConc = "Array"
mlSetConc = "Set"
mlFile = "IOStream"
mlVoid = "Nothing"

mlClassError :: String
mlClassError = "Classes are not supported in MATLAB"

-- The only consistent way of creating floats is by casting
mlLitFloat :: (CommonRenderSym r) => Float -> SValue r
mlLitFloat f = mkStateVal float (text mlFloatConc <> parens (D.float f))

mlLitList :: (CommonRenderSym r) => VSType r -> [SValue r] -> SValue r
mlLitList t' es = do
  t <- t'
  let lt' = listType t'
  elems <- sequence es
  let typeDec = if null es then RC.type' t else empty
  mkStateVal lt' (typeDec <> brackets (valueList elems))

mlCast :: (CommonRenderSym r) => VSType r -> SValue r -> SValue r
mlCast t' v' = do
  t <- t'
  v <- v'
  let vTp = getType $ valueType v
      tTp = getType t
      vDoc = RC.value v
      tDoc = RC.type' t
      mlCast' :: CodeType -> CodeType -> Doc -> Doc -> Doc
      -- Converting string to char
      mlCast' String Char vDoc' _ = text "only" <> parens vDoc'
      -- Converting string to something else
      mlCast' String _    vDoc' tDoc' = text "parse" <> parens (tDoc' <> listSep' <+> vDoc')
      -- Converting non-string to char
      mlCast' _      Char vDoc' _ = text "only" <> parens (text "string" <> parens vDoc')
      -- Converting something to string
      mlCast' _      String vDoc' _ = text "string" <> parens vDoc'
      -- Converting non-string to non-string
      mlCast' _      _    vDoc' tDoc' = tDoc' <> parens vDoc'
  mkVal t (mlCast' vTp tTp vDoc tDoc)

mlAssign :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
mlAssign vr' v' = do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  scpData <- getVarScope (variableName vr) -- Need to do global declarations
  mkStmtNoEnd $ mlGlobalDec scpData <+> R.assign vr v

mlSubAssign :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
mlSubAssign vr' v' = do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  scpData <- getVarScope (variableName vr) -- Need to do global declarations
  mkStmtNoEnd $ mlGlobalDec scpData <+> R.subAssign vr v

mlIncrement :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
mlIncrement vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  scpData <- getVarScope (variableName vr) -- Need to do global declarations
  mkStmt $ mlGlobalDec scpData <+> R.addAssign vr v

mlGlobalDec :: ScopeData -> Doc
mlGlobalDec scp = if scopeTag scp == Global then mlGlobal else empty

mlGlobal :: Doc
mlGlobal = text "global"

mlConstDecDef :: (CommonRenderSym r) => SVariable r -> r ScopeData -> SValue r
  -> MSStatement r
mlConstDecDef v' scp def' = do
  let scpData = scopeData scp
  v <- zoom lensMStoVS v'
  def <- zoom lensMStoVS def'
  modify $ useVarName $ variableName v
  modify $ setVarScope (variableName v) scpData
  let decDoc = if scopeTag scpData == Global then R.constDec' else empty
  mkStmt $ decDoc <+> RC.variable v <+> equals <+> RC.value def

-- List API
mlListSize, mlListAdd, mlListAppend, mlListAbsdex :: Label
mlListSize   = "length"
mlListAdd    = "insert!"
mlListAppend = "append!"
mlListAbsdex = "findfirst"

mlIndexOf :: (SharedProg r) => SValue r -> SValue r -> SValue r
mlIndexOf l v = do
  v' <- v
  let t = toCode $ valueType v'
  indexToInt $ funcApp
    mlListAbsdex t [lambda [binder "x" t] (valueOf (var "x" t) ?== v), l]

-- List slicing in Julia.  See HelloWorld.ml to see the full suite of
-- possible outputs of this function.
mlListSlice :: (CommonRenderSym r) => SVariable r -> SValue r ->
  Maybe (SValue r) -> Maybe (SValue r) -> SValue r -> MSBlock r
mlListSlice vn vo beg end step = do

  vnew <- zoom lensMStoVS vn
  scpData <- getVarScope $ variableName vnew
  let scp = convScope scpData

  stepV <- zoom lensMStoVS step

  let mbStepV = valueInt stepV
  bName <- genVarNameIf (isNothing beg && isNothing mbStepV) "begIdx"
  eName <- genVarNameIf (isNothing mbStepV) "endIdx"

  let begVar = var bName int
      endVar = var eName int

      (setBeg, begVal) = case (beg, mbStepV) of
        -- If we have a value for beg, just use it
        (Just b, _)        -> (emptyStmt, intToIndex b)
        -- If we don't have a value for `beg` but we do for `step`, use `begin` or `end`
        (Nothing, Just s)  -> (emptyStmt,
          if s > 0 then mkStateVal int mlBegin else mkStateVal int mlEnd)
        -- Otherwise, generate an if-statement to calculate `beg` at runtime
        (Nothing, Nothing) -> (varDecDef begVar scp $
          inlineIf (step ?> litInt 0) (litInt 1) (listSize vo),
          valueOf begVar)

      -- Similar to `begVal`, but if we're given a value, we have to either
      -- do nothing or add 2 based on the sign of `step`, because `end` needs
      -- to be inclusive
      (setEnd, endVal) = case (end, mbStepV) of
        (Just e, Just s)  -> (emptyStmt,
          if s > 0 then e else e `G.smartAdd` litInt 2)
        (Just e, Nothing) -> (varDecDef endVar scp $
          inlineIf (step ?> litInt 0) e (e `G.smartAdd` litInt 2),
          valueOf endVar)
        (Nothing, Just s) -> (emptyStmt,
          if s > 0 then mkStateVal int mlEnd else mkStateVal int mlBegin)
        (Nothing, Nothing) -> (varDecDef endVar scp $
          inlineIf (step ?> litInt 0) (listSize vo) (litInt 1), valueOf endVar)

      setToSlice = mlListSlice' vn vo begVal endVal step mbStepV

  block [
      setBeg,
      setEnd,
      setToSlice
    ]

mlListSlice' :: (CommonRenderSym r) => SVariable r -> SValue r -> SValue r ->
  SValue r -> SValue r -> Maybe Integer -> MSStatement r
mlListSlice' vn vo beg end step mStep = do
  vold  <- zoom lensMStoVS vo
  beg'  <- zoom lensMStoVS beg
  end'  <- zoom lensMStoVS end
  step' <- zoom lensMStoVS step
  let stepDoc = case mStep of
        (Just 1) -> empty
        _        -> colon <> RC.value step'
      theSlice = mkStateVal void (RC.value vold <> brackets (RC.value beg' <> stepDoc <> colon <> RC.value end'))
  vn &= theSlice

-- Other functionality
mlRange :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
mlRange initv finalv stepv = do
  t <- listType int
  iv <- initv
  sv <- stepv
  fv <- finalv `G.smartSub` litInt 1
  mkVal t (RC.value iv <> colon <> RC.value sv <> colon <> RC.value fv)

mlSplit :: String
mlSplit = "split"

mlPrintFunc, mlPrintLnFunc :: Doc
mlPrintFunc = text printLabel
mlPrintLnFunc = text "println"

mlParseFunc :: Label
mlParseFunc = "parse"

mlType, arrow, mlNamedArgSep :: Doc
mlType = colon <> colon
arrow = text "->"
mlNamedArgSep = equals

mlTuple :: [String] -> String
mlTuple ts = "Tuple{" ++ intercalate listSep ts ++ "}"

-- Operators
mlUnaryMath :: (Monad r) => String -> VSOp r
mlUnaryMath = unOpPrec

mlPower, mlIntDiv :: String
mlPower = "^"
mlIntDiv = "÷"

-- Constants
mlPi :: Doc
mlPi = text "pi"

-- Comments
mlCmtStart, mlBlockCmtStart, mlBlockCmtEnd, mlDocCmtStart, mlDocCmtEnd :: Doc
mlCmtStart      = text "#"
mlBlockCmtStart = text "#="
mlBlockCmtEnd   = text "=#"
mlDocCmtStart   = text "\"\"\""
mlDocCmtEnd     = text "\"\"\""

-- Control structures

mlSpace :: OptionalSpace
mlSpace = OSpace {oSpace = empty}

-- | Creates a for-each loop in Julia
mlForEach :: (CommonRenderSym r) => r (Variable r) -> r (Value r) -> r (Body r) -> Doc
mlForEach i lstVar b = vcat [
  forLabel <+> RC.variable i <+> inLabel <+> RC.value lstVar,
  indent $ RC.body b,
  mlEnd]

-- | Creates the contents of a module in Julia
mlModContents :: Label -> [Label] -> [SMethod MatlabCode] ->
  FSModule MatlabCode
mlModContents n is = A.buildModule n (do
  lis <- getLangImports
  libis <- getLibImports
  mis <- getModuleImports
  pure $ vibcat [
    vcat (map (RC.import' . li) lis),
    vcat (map (RC.import' . li) (sort $ is ++ libis)),
    vcat (map (RC.import' . mi) mis)])
  (do getMainDoc)
  where mi, li :: Label -> MatlabCode (Import MatlabCode)
        mi = modImport
        li = langImport

-- Functions
-- | Creates a function.  n is function name, pms is list of parameters, and
--   bod is body.
mlIntFunc :: (CommonRenderSym r) => Label -> [r (Parameter r)] ->
  r (Body r) -> Doc
mlIntFunc n pms bod = do
  vcat [mlFunc <+> text n <> parens (parameterList pms),
        indent $ RC.body bod,
        mlEnd]

mlLambda :: (CommonRenderSym r) => [r BinderD] -> r (Value r) -> Doc
mlLambda ps ex = binderList ps <+> arrow <+> RC.value ex

-- Exceptions
mlThrow :: (CommonRenderSym r) => r (Value r) -> Doc
mlThrow errMsg = mlThrowLabel <> parens (RC.value errMsg)

mlTryCatch :: (CommonRenderSym r) => r (Body r) -> r (Body r) -> Doc
mlTryCatch tryB catchB = vcat [
  tryLabel,
  indent $ RC.body tryB,
  catchLabel <+> mlException,
  indent $ RC.body catchB,
  mlEnd]

mlException :: Doc
mlException = text "ErrorException"

includeLabel, importLabel :: Doc
includeLabel = text "include"
importLabel = text "import"

-- Assertions
mlAssert :: (CommonRenderSym r) => r (Value r) -> r (Value r) -> Doc
mlAssert condition errorMessage = vcat [
  text "@assert" <+> RC.value condition <+> RC.value errorMessage
  ]

mlMod, elseIfLabel, mlFunc, mlBegin, mlEnd, mlThrowLabel :: Doc
mlMod        = text "module"
elseIfLabel  = text "elseif"
mlFunc       = text "function"
mlBegin      = text "begin"
mlEnd        = text "end"
mlThrowLabel = text "error" -- TODO: this hints at an underdeveloped exception system

mlParam :: (CommonRenderSym r) => r (Variable r) -> Doc
mlParam v = RC.variable v <> mlType <> RC.type' (variableType v)

-- Type names specific to Julia (there's a lot of them)
mlIntType :: (CommonRenderSym r) => VSType r
mlIntType = typeFromData Integer mlIntConc (text mlIntConc)

mlFloatType :: (CommonRenderSym r) => VSType r
mlFloatType = typeFromData Float mlFloatConc (text mlFloatConc)

mlDoubleType :: (CommonRenderSym r) => VSType r
mlDoubleType = typeFromData Double mlDoubleConc (text mlDoubleConc)

mlCharType :: (CommonRenderSym r) => VSType r
mlCharType = typeFromData Char mlCharConc (text mlCharConc)

mlStringType :: (CommonRenderSym r) => VSType r
mlStringType = typeFromData String mlStringConc (text mlStringConc)

mlInfileType :: (CommonRenderSym r) => VSType r
mlInfileType = typeFromData InFile mlFile (text mlFile)

mlOutfileType :: (CommonRenderSym r) => VSType r
mlOutfileType = typeFromData OutFile mlFile (text mlFile)

mlListType :: (CommonRenderSym r) => VSType r -> VSType r
mlListType t' = do
  t <- t'
  let typeName = mlListConc ++ "{" ++ getTypeString t ++ "}"
  typeFromData (List $ getType t) typeName (text typeName)

mlSetType :: (CommonRenderSym r) => VSType r -> VSType r
mlSetType t' = do
  t <- t'
  let typeName = mlSetConc ++ "{" ++ getTypeString t ++ "}"
  typeFromData (Set $ getType t) typeName (text typeName)

mlVoidType :: (CommonRenderSym r) => VSType r
mlVoidType = typeFromData Void mlVoid (text mlVoid)

mlNull :: Label
mlNull = "nothing"

-- Modules
-- | Creates the text for the start of a module.
--   n is the name of the module.
mlModStart :: Label -> Doc
mlModStart n = mlMod <+> text n

-- IO
mlPrint :: Bool -> Maybe (SValue MatlabCode) -> SValue MatlabCode ->
  SValue MatlabCode -> MSStatement MatlabCode
-- Printing to console
mlPrint _ f' p' v' = do
  f <- zoom lensMStoVS $ fromMaybe (mkStateVal void empty) f' -- The file to print to
  prf <- zoom lensMStoVS p' -- The print function to use
  v <- zoom lensMStoVS v' -- The value to print
  let fl = emptyIfEmpty (RC.value f) $ RC.value f <> listSep'
  mkStmtNoEnd $ RC.value prf <> parens (fl <> RC.value v)

-- mlPrint can handle lists, so don't use G.print for lists
mlOut :: (CommonRenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r ->
  MSStatement r
mlOut newLn f printFn v = zoom lensMStoVS v >>= mlOut' . getType . valueType
  where mlOut' (List _) = printSt newLn f printFn v
        mlOut' _ = G.print newLn f printFn v

mlInput :: SValue MatlabCode -> SVariable MatlabCode -> MSStatement MatlabCode
mlInput inSrc v = v &= (v >>= mlInput' . getType . variableType)
  where mlInput' Integer = mlParse mlIntConc int inSrc
        mlInput' Float = mlParse mlFloatConc float inSrc
        mlInput' Double = mlParse mlDoubleConc double inSrc
        mlInput' Boolean = mlParse CS.boolRender bool inSrc
        mlInput' String = inSrc
        mlInput' Char = mlParse mlCharConc char inSrc
        mlInput' _ = error "Attempt to read a value of unreadable type"

readLine, readLines :: (CommonRenderSym r) => SValue r -> SValue r
readLine f = funcApp mlReadLineFunc string [f]
readLines f = funcApp mlReadLinesFunc (listType string) [f]

mlReadLine :: Doc
mlReadLine = text mlReadLineFunc

mlReadLineFunc, mlReadLinesFunc, mlCloseFunc :: Label
mlReadLineFunc = "readline"
mlReadLinesFunc = "readlines"
mlCloseFunc = "close"

mlArgs :: Label
mlArgs = "ARGS"

mlParse :: (CommonRenderSym r) => Label -> VSType r -> SValue r -> SValue r
mlParse tl tp v = let
  typeLabel = mkStateVal void (text tl)
  in funcApp mlParseFunc tp [typeLabel, v]
