{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

-- | The logic to render Julia code is contained in this module
module Drasil.GOOL.LanguageRenderer.JuliaRenderer (
  -- * Julia Code Configuration -- defines syntax of all Julia code
  JuliaCode(..), jlName, jlVersion
) where

import Utils.Drasil (indent)

import Drasil.GOOL.CodeType (CodeType(..))
import Drasil.GOOL.InterfaceCommon (SharedProg, Label, VSType, SValue, litZero,
  SVariable, MSStatement, MSBlock, SMethod, BodySym(..), BlockSym(..),
  TypeSym(..), TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..),
  Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), funcApp, extFuncApp, List(..), Set(..),
  InternalList(..), ThunkSym(..), VectorType(..), VectorDecl(..),
  VectorThunk(..), VectorExpression(..), ThunkAssign(..), StatementSym(..),
  AssignStatement(..), DeclStatement(..), IOStatement(..), StringStatement(..),
  FunctionSym(..), FuncAppStatement(..), CommentStatement(..),
  ControlStatement(..), VisibilitySym(..), ScopeSym(..), ParameterSym(..),
  MethodSym(..), (&=), switchAsIf, convScope)
import Drasil.GOOL.InterfaceProc (ProcProg, FSModule, ProgramSym(..),
  FileSym(..), ModuleSym(..))

import Drasil.GOOL.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  ImportElim, RenderBody(..), BodyElim, RenderBlock(..), BlockElim,
  RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..),
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind),
  RenderValue(..), ValueElim(..), InternalListFunc(..), RenderFunction(..),
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..),
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm),
  RenderVisibility(..), VisibilityElim, MethodTypeSym(..), RenderParam(..),
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim,
  BlockCommentSym(..), BlockCommentElim, ScopeElim(..))
import qualified Drasil.GOOL.RendererClassesCommon as RC (import', body, block,
  type', uOp, bOp, variable, value, function, statement, visibility, parameter,
  method, blockComment')
import Drasil.GOOL.RendererClassesProc (ProcRenderSym, RenderFile(..),
  RenderMod(..), ModuleElim, ProcRenderMethod(..))
import qualified Drasil.GOOL.RendererClassesProc as RC (module')
import Drasil.GOOL.LanguageRenderer (printLabel, listSep, listSep',
  variableList, parameterList, forLabel, inLabel, tryLabel, catchLabel,
  valueList)
import qualified Drasil.GOOL.LanguageRenderer as R (sqrt, abs, log10, log,
  exp, sin, cos, tan, asin, acos, atan, floor, ceil, multiStmt, body,
  addComments, blockCmt, docCmt, commentedMod, listSetFunc, commentedItem,
  break, continue, constDec', assign, subAssign, addAssign)
import Drasil.GOOL.LanguageRenderer.Constructors (mkVal, mkStateVal, VSOp,
  unOpPrec, powerPrec, unExpr, unExpr', binExpr, multPrec, typeUnExpr,
  typeBinExpr, mkStmt, mkStmtNoEnd)
import Drasil.GOOL.LanguageRenderer.LanguagePolymorphic (OptionalSpace(..))
import qualified Drasil.GOOL.LanguageRenderer.LanguagePolymorphic as G (
  block, multiBlock, litChar, litDouble, litInt, litString, valueOf, negateOp,
  equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp,
  minusOp, multOp, divideOp, moduloOp, call, funcAppMixedArgs, lambda,
  listAccess, listSet, tryCatch, csc, multiBody, sec, cot, stmt, loopStmt,
  emptyStmt, print, comment, valStmt, returnStmt, param, docFunc, throw, arg,
  argsList, ifCond, smartAdd, local, var)
import qualified Drasil.GOOL.LanguageRenderer.CommonPseudoOO as CP (bool,
  boolRender, extVar, funcType, listDec, listDecDef, listAccessFunc,
  listSetFunc, notNull, extFuncAppMixedArgs, functionDoc, listSize, listAdd,
  listAppend, intToIndex', indexToInt', inOutFunc, docInOutFunc', forLoopError,
  varDecDef, openFileR', openFileW', openFileA', multiReturn, multiAssign,
  inOutCall, mainBody, argExists, forEach', litSet)
import qualified Drasil.GOOL.LanguageRenderer.CLike as C (litTrue, litFalse,
  notOp, andOp, orOp, inlineIf, while)
import qualified Drasil.GOOL.LanguageRenderer.AbstractProc as A (fileDoc,
  fileFromData, buildModule, docMod, modFromData, listInnerType, arrayElem,
  funcDecDef, function)
import qualified Drasil.GOOL.LanguageRenderer.Macros as M (increment1,
  decrement1, ifExists, stringListVals, stringListLists)
import Drasil.GOOL.AST (Terminator(..), FileType(..), FileData(..), fileD,
  FuncData(..), ModData(..), md, updateMod, MethodData(..), mthd, OpData(..),
  ParamData(..), ProgData(..), TypeData(..), td, ValData(..), vd, VarData(..),
  vard, CommonThunk, progD, fd, pd, updateMthd, commonThunkDim, commonThunkElim,
  vectorize, vectorize2, commonVecIndex, sumComponents, pureValue, ScopeTag(..),
  ScopeData(..), sd)
import Drasil.GOOL.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  on2CodeValues, on2StateValues, onCodeList, onStateList, emptyIfEmpty)
import Drasil.GOOL.State (VS, lensGStoFS, revFiles, setFileType, lensMStoVS,
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

jlExt :: String
jlExt = "jl"

newtype JuliaCode a = JLC {unJLC :: a} deriving Functor

instance Applicative JuliaCode where
  pure = JLC
  (JLC f) <*> (JLC x) = JLC (f x)

instance Monad JuliaCode where
  JLC x >>= f = f x

instance SharedProg JuliaCode
instance ProcProg JuliaCode

instance ProgramSym JuliaCode where
  type Program JuliaCode = ProgData
  prog n st files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n st) fs

instance CommonRenderSym JuliaCode
instance ProcRenderSym JuliaCode

instance FileSym JuliaCode where
  type File JuliaCode = FileData
  fileDoc m = do
    modify (setFileType Combined)
    A.fileDoc jlExt m
  docMod = A.docMod jlExt

instance RenderFile JuliaCode where
  top _ = toCode empty
  bottom = toCode empty

  commentedMod = on2StateValues (on2CodeValues R.commentedMod)

  fileFromData = A.fileFromData (onCodeValue . fileD)

instance ImportSym JuliaCode where
  type Import JuliaCode = Doc
  langImport n = let modName = text n
    in toCode $ importLabel <+> modName
  modImport n = let modName = text n
                    fileName = text $ n ++ '.' : jlExt
    in toCode $ vcat [includeLabel <> parens (doubleQuotes fileName),
                      importLabel <+> text "." <> modName]

instance ImportElim JuliaCode where
  import' = unJLC

instance BodySym JuliaCode where
  type Body JuliaCode = Doc
  body = onStateList (onCodeList R.body)

  addComments s = onStateValue (onCodeValue (R.addComments s jlCmtStart))

instance RenderBody JuliaCode where
  multiBody = G.multiBody

instance BodyElim JuliaCode where
  body = unJLC

instance BlockSym JuliaCode where
  type Block JuliaCode = Doc
  block = G.block

instance RenderBlock JuliaCode where
  multiBlock = G.multiBlock

instance BlockElim JuliaCode where
  block = unJLC

instance TypeSym JuliaCode where
  type Type JuliaCode = TypeData
  bool = CP.bool
  int = jlIntType
  float = jlFloatType
  double = jlDoubleType
  char = jlCharType
  string = jlStringType
  infile = jlInfileType
  outfile = jlOutfileType
  listType = jlListType
  setType = jlSetType
  arrayType = listType -- Treat arrays and lists the same, as in Python
  listInnerType = A.listInnerType
  funcType = CP.funcType
  void = jlVoidType

instance TypeElim JuliaCode where
  getType = cType . unJLC
  getTypeString v = let tp = typeString $ unJLC v in
    case cType $ unJLC v of
      (Object _) -> error jlClassError
      _ -> tp

instance RenderType JuliaCode where
  multiType ts = do
    typs <- sequence ts
    let mt = jlTuple $ map getTypeString typs
    typeFromData Void mt (text mt)
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim JuliaCode where
  type' v = let t = typeDoc $ unJLC v in
    case cType $ unJLC v of
      (Object _) -> t <> error jlClassError
      _ -> t

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

instance OpElim JuliaCode where
  uOp = opDoc . unJLC
  bOp = opDoc . unJLC
  uOpPrec = opPrec . unJLC
  bOpPrec = opPrec . unJLC

instance ScopeSym JuliaCode where
  type Scope JuliaCode = ScopeData
  global = toCode $ sd Global
  mainFn = global
  local = G.local

instance ScopeElim JuliaCode where
  scopeData = unJLC

instance VariableSym JuliaCode where
  type Variable JuliaCode = VarData
  var = G.var
  constant = var
  extVar l n t = modify (addModuleImportVS l) >> CP.extVar l n t
  arrayElem i = A.arrayElem (litInt i)

instance VariableElim JuliaCode where
  variableName = varName . unJLC
  variableType = onCodeValue varType

instance InternalVarElim JuliaCode where
  variableBind = varBind . unJLC
  variable = varDoc . unJLC

instance RenderVariable JuliaCode where
  varFromData b n t' d = do
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym JuliaCode where
  type Value JuliaCode = ValData
  valueType = onCodeValue valType

instance Argument JuliaCode where
  pointerArg = id

instance Literal JuliaCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = jlLitFloat
  litInt = G.litInt
  litString = G.litString
  litArray = litList
  litList = jlLitList
  litSet = CP.litSet (text "Set" <>) (parens . brackets)

instance MathConstant JuliaCode where
  pi :: SValue JuliaCode
  pi = mkStateVal double jlPi

instance VariableValue JuliaCode where
  valueOf = G.valueOf

instance CommandLineArgs JuliaCode where
  arg n = G.arg (litInt $ n+1) argsList
  argsList = G.argsList jlArgs
  argExists = CP.argExists

instance NumericExpression JuliaCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) v1' v2' = do -- Julia has integer division via `÷` or `div()`
    v1 <- v1'
    v2 <- v2'
    let jlDivision Integer Integer = binExpr (multPrec jlIntDiv)
        jlDivision _ _ = binExpr divideOp
    jlDivision (getType $ valueType v1) (getType $ valueType v2)
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

instance BooleanExpression JuliaCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

instance Comparison JuliaCode where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression JuliaCode where
  inlineIf = C.inlineIf

  funcAppMixedArgs = G.funcAppMixedArgs
  extFuncAppMixedArgs l n t ps ns = do
    modify (addModuleImportVS l)
    CP.extFuncAppMixedArgs l n t ps ns
  libFuncAppMixedArgs l n t ps ns = do
    modify (addLibImportVS l)
    CP.extFuncAppMixedArgs l n t ps ns

  lambda = G.lambda jlLambda

  notNull = CP.notNull jlNull

instance RenderValue JuliaCode where
  inputFunc = mkStateVal string (jlReadLine <> parens empty)
  printFunc = mkStateVal void jlPrintFunc
  printLnFunc = mkStateVal void jlPrintLnFunc
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty

  cast = jlCast

  call = G.call jlNamedArgSep

  valFromData p i t' d = do
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim JuliaCode where
  valuePrec = valPrec . unJLC
  valueInt = valInt . unJLC
  value = val . unJLC

instance List JuliaCode where
  intToIndex = CP.intToIndex'
  indexToInt = CP.indexToInt'
  listSize = CP.listSize
  listAdd = CP.listAdd
  listAppend = CP.listAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = jlIndexOf

instance Set JuliaCode where
  contains s e = funcApp "in" bool [e, s]
  setAdd s e = funcApp "push!" void [s, e]
  setRemove s e = funcApp "delete!" void [s, e]
  setUnion a b = funcApp "union" void [a, b]

instance InternalList JuliaCode where
  listSlice' b e s vn vo = jlListSlice vn vo b e (fromMaybe (litInt 1) s)

instance InternalListFunc JuliaCode where
  listSizeFunc l = do
    f <- funcApp jlListSize int [l]
    funcFromData (RC.value f) int
  listAddFunc l i v = do
    f <- funcApp jlListAdd void [l, i, v]
    funcFromData (RC.value f) void
  listAppendFunc l v = do
    f <- funcApp jlListAppend void [l, v]
    funcFromData (RC.value f) void
  listAccessFunc = CP.listAccessFunc
  listSetFunc = CP.listSetFunc R.listSetFunc

instance ThunkSym JuliaCode where
  type Thunk JuliaCode = CommonThunk VS

instance ThunkAssign JuliaCode where
  thunkAssign v t = do
    iName <- genLoopIndex
    let
      i = var iName int
      dim = fmap pure $ t >>= commonThunkDim (fmap unJLC . (\l -> listSize l #- litInt 1) . fmap pure) . unJLC
      loopInit = zoom lensMStoVS (fmap unJLC t) >>= commonThunkElim
        (const emptyStmt) (const $ assign v $ litZero $ fmap variableType v)
      loopBody = zoom lensMStoVS (fmap unJLC t) >>= commonThunkElim
        (valStmt . listSet (valueOf v) (valueOf i) . vecIndex (valueOf i) . pure . pure)
        ((v &+=) . vecIndex (valueOf i) . pure . pure)
    multi [loopInit,
      forRange i (litInt 0) dim (litInt 1) $ body [block [loopBody]]]

instance VectorType JuliaCode where
  vecType = listType

instance VectorDecl JuliaCode where
  vecDec = listDec
  vecDecDef = listDecDef

instance VectorThunk JuliaCode where
  vecThunk = pure . pure . pureValue . fmap unJLC . valueOf

instance VectorExpression JuliaCode where
  vecScale k = fmap $ fmap $ vectorize (fmap unJLC . (k #*) . fmap pure)
  vecAdd = liftA2 $ liftA2 $ vectorize2 (\v1 v2 -> fmap unJLC $ fmap pure v1 #+ fmap pure v2)
  vecIndex i = (>>= fmap pure . commonVecIndex (fmap unJLC . flip listAccess i . fmap pure) . unJLC)
  vecDot = liftA2 $ liftA2 $ fmap sumComponents <$> vectorize2 (\v1 v2 -> fmap unJLC $ fmap pure v1 #* fmap pure v2)

instance RenderFunction JuliaCode where
  funcFromData d = onStateValue $ onCodeValue (`fd` d)

instance FunctionElim JuliaCode where
  functionType = onCodeValue fType
  function = funcDoc . unJLC

instance InternalAssignStmt JuliaCode where
  multiAssign = CP.multiAssign id

instance InternalIOStmt JuliaCode where
  printSt = jlPrint

instance InternalControlStmt JuliaCode where
  multiReturn = CP.multiReturn id

instance RenderStatement JuliaCode where
  stmt = G.stmt
  loopStmt = G.loopStmt
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim JuliaCode where
  statement = fst . unJLC
  statementTerm = snd . unJLC

instance StatementSym JuliaCode where
  type Statement JuliaCode = (Doc, Terminator)
  valStmt = G.valStmt Empty
  emptyStmt = G.emptyStmt
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement JuliaCode where
  assign = jlAssign
  (&-=) = jlSubAssign
  (&+=) = jlIncrement
  (&++) = M.increment1
  (&--) = M.decrement1

instance DeclStatement JuliaCode where
  varDec v scp = CP.varDecDef v scp Nothing
  varDecDef v scp e = CP.varDecDef v scp (Just e)
  setDec = varDec
  setDecDef = varDecDef
  listDec _ = CP.listDec
  listDecDef = CP.listDecDef
  arrayDec = listDec
  arrayDecDef = listDecDef
  constDecDef = jlConstDecDef
  funcDecDef = A.funcDecDef

instance IOStatement JuliaCode where
  print      = jlOut False Nothing printFunc
  printLn    = jlOut True  Nothing printLnFunc
  printStr   = jlOut False Nothing printFunc   . litString
  printStrLn = jlOut True  Nothing printLnFunc . litString

  printFile f      = jlOut False (Just f) printFunc
  printFileLn f    = jlOut True (Just f) printLnFunc
  printFileStr f   = printFile   f . litString
  printFileStrLn f = printFileLn f . litString

  getInput = jlInput inputFunc
  discardInput = valStmt inputFunc
  getFileInput f = jlInput (readLine f)
  discardFileInput f = valStmt (readLine f)
  openFileR f n = f &= CP.openFileR' n
  openFileW f n = f &= CP.openFileW' n
  openFileA f n = f &= CP.openFileA' n
  closeFile f = valStmt $ funcApp jlCloseFunc void [f]
  getFileInputLine = getFileInput
  discardFileLine = discardFileInput
  getFileInputAll f v = v &= readLines f

instance StringStatement JuliaCode where
  stringSplit d vnew s = vnew &= funcApp jlSplit (listType string) [s, litString [d]]
  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FunctionSym JuliaCode where
  type Function JuliaCode = FuncData

instance FuncAppStatement JuliaCode where
  inOutCall = CP.inOutCall funcApp
  extInOutCall m = CP.inOutCall (extFuncApp m)

instance CommentStatement JuliaCode where
  comment = G.comment jlCmtStart

instance ControlStatement JuliaCode where
  break = mkStmtNoEnd R.break
  continue = mkStmtNoEnd R.continue
  returnStmt = G.returnStmt Empty
  throw = G.throw jlThrow Empty
  ifCond = G.ifCond id empty jlSpace elseIfLabel empty jlEnd
  switch = switchAsIf
  ifExists = M.ifExists
  for _ _ _ _ = error $ CP.forLoopError jlName
  forRange i initv finalv stepv = forEach i (jlRange initv finalv stepv)
  forEach = CP.forEach' jlForEach
  while = C.while id empty jlEnd
  tryCatch = G.tryCatch jlTryCatch
  assert condition errorMessage = do
    cond <- zoom lensMStoVS condition
    errMsg <- zoom lensMStoVS errorMessage
    mkStmtNoEnd (jlAssert cond errMsg)

instance VisibilitySym JuliaCode where
  type Visibility JuliaCode = Doc

  private = toCode empty -- Julia doesn't have private/public members
  public = toCode empty

instance RenderVisibility JuliaCode where
  visibilityFromData _ = toCode

instance VisibilityElim JuliaCode where
  visibility = unJLC

instance MethodTypeSym JuliaCode where
  type MethodType JuliaCode = TypeData

  mType = zoom lensMStoVS

instance ParameterSym JuliaCode where
  type Parameter JuliaCode = ParamData

  param = G.param jlParam
  pointerParam = param

instance RenderParam JuliaCode where
  paramFromData v' d = do
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)

instance ParamElim JuliaCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unJLC

instance MethodSym JuliaCode where
  type Method JuliaCode = MethodData
  docMain = mainFunction
  function = A.function
  mainFunction = CP.mainBody
  docFunc = G.docFunc CP.functionDoc

  inOutFunc n s = CP.inOutFunc (function n s)
  docInOutFunc n s = CP.docInOutFunc' CP.functionDoc (inOutFunc n s)

instance RenderMethod JuliaCode where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)
  mthdFromData _ d = toState $ toCode $ mthd d

instance ProcRenderMethod JuliaCode where
  intFunc _ n _ _ ps b = do
    pms <- sequence ps
    toCode . mthd . jlIntFunc n pms <$> b

instance MethodElim JuliaCode where
  method = mthdDoc . unJLC

instance ModuleSym JuliaCode where
  type Module JuliaCode = ModData
  buildModule n is fs = jlModContents n is fs <&>
    updateModuleDoc (\m -> emptyIfEmpty m (vibcat [jlModStart n, m, jlEnd]))

instance RenderMod JuliaCode where
  modFromData n = A.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim JuliaCode where
  module' = modDoc . unJLC

instance BlockCommentSym JuliaCode where
  type BlockComment JuliaCode = Doc

  blockComment lns = toCode $ R.blockCmt lns jlBlockCmtStart jlBlockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns jlDocCmtStart
    jlDocCmtEnd)

instance BlockCommentElim JuliaCode where
  blockComment' = unJLC

-- convenience
jlName, jlVersion :: String
jlName = "Julia"
jlVersion = "1.10.3"

-- Concrete versions of each Julia datatype
jlIntConc, jlFloatConc, jlDoubleConc, jlCharConc, jlStringConc, jlListConc,
  jlSetConc, jlFile, jlVoid :: String
jlIntConc = "Int64"
jlFloatConc = "Float32"
jlDoubleConc = "Float64"
jlCharConc = "Char"
jlStringConc = "String"
jlListConc = "Array"
jlSetConc = "Set"
jlFile = "IOStream"
jlVoid = "Nothing"

jlClassError :: String
jlClassError = "Classes are not supported in Julia"

-- The only consistent way of creating floats is by casting
jlLitFloat :: (CommonRenderSym r) => Float -> SValue r
jlLitFloat f = mkStateVal float (text jlFloatConc <> parens (D.float f))

jlLitList :: (CommonRenderSym r) => VSType r -> [SValue r] -> SValue r
jlLitList t' es = do
  t <- t'
  let lt' = listType t'
  elems <- sequence es
  let typeDec = if null es then RC.type' t else empty
  mkStateVal lt' (typeDec <> brackets (valueList elems))

jlCast :: (CommonRenderSym r) => VSType r -> SValue r -> SValue r
jlCast t' v' = do
  t <- t'
  v <- v'
  let vTp = getType $ valueType v
      tTp = getType t
      vDoc = RC.value v
      tDoc = RC.type' t
      jlCast' :: CodeType -> CodeType -> Doc -> Doc -> Doc
      -- Converting string to char
      jlCast' String Char vDoc' _ = text "only" <> parens vDoc'
      -- Converting string to something else
      jlCast' String _    vDoc' tDoc' = text "parse" <> parens (tDoc' <> listSep' <+> vDoc')
      -- Converting non-string to char
      jlCast' _      Char vDoc' _ = text "only" <> parens (text "string" <> parens vDoc')
      -- Converting something to string
      jlCast' _      String vDoc' _ = text "string" <> parens vDoc'
      -- Converting non-string to non-string
      jlCast' _      _    vDoc' tDoc' = tDoc' <> parens vDoc'
  mkVal t (jlCast' vTp tTp vDoc tDoc)

jlAssign :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
jlAssign vr' v' = do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  scpData <- getVarScope (variableName vr) -- Need to do global declarations
  mkStmtNoEnd $ jlGlobalDec scpData <+> R.assign vr v

jlSubAssign :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
jlSubAssign vr' v' = do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  scpData <- getVarScope (variableName vr) -- Need to do global declarations
  mkStmtNoEnd $ jlGlobalDec scpData <+> R.subAssign vr v

jlIncrement :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
jlIncrement vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  scpData <- getVarScope (variableName vr) -- Need to do global declarations
  mkStmt $ jlGlobalDec scpData <+> R.addAssign vr v

jlGlobalDec :: ScopeData -> Doc
jlGlobalDec scp = if scopeTag scp == Global then jlGlobal else empty

jlGlobal :: Doc
jlGlobal = text "global"

jlConstDecDef :: (CommonRenderSym r) => SVariable r -> r (Scope r) -> SValue r
  -> MSStatement r
jlConstDecDef v' scp def' = do
  let scpData = scopeData scp
  v <- zoom lensMStoVS v'
  def <- zoom lensMStoVS def'
  modify $ useVarName $ variableName v
  modify $ setVarScope (variableName v) scpData
  let decDoc = if scopeTag scpData == Global then R.constDec' else empty
  mkStmt $ decDoc <+> RC.variable v <+> equals <+> RC.value def

-- List API
jlListSize, jlListAdd, jlListAppend, jlListAbsdex :: Label
jlListSize   = "length"
jlListAdd    = "insert!"
jlListAppend = "append!"
jlListAbsdex = "findfirst"

jlIndexOf :: (SharedProg r) => SValue r -> SValue r -> SValue r
jlIndexOf l v = do
  v' <- v
  let t = toCode $ valueType v'
  indexToInt $ funcApp
    jlListAbsdex t [lambda [var "x" t] (valueOf (var "x" t) ?== v), l]

-- List slicing in Julia.  See HelloWorld.jl to see the full suite of
-- possible outputs of this function.
jlListSlice :: (CommonRenderSym r) => SVariable r -> SValue r ->
  Maybe (SValue r) -> Maybe (SValue r) -> SValue r -> MSBlock r
jlListSlice vn vo beg end step = do

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
          if s > 0 then mkStateVal int jlBegin else mkStateVal int jlEnd)
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
          if s > 0 then mkStateVal int jlEnd else mkStateVal int jlBegin)
        (Nothing, Nothing) -> (varDecDef endVar scp $
          inlineIf (step ?> litInt 0) (listSize vo) (litInt 1), valueOf endVar)

      setToSlice = jlListSlice' vn vo begVal endVal step mbStepV

  block [
      setBeg,
      setEnd,
      setToSlice
    ]

jlListSlice' :: (CommonRenderSym r) => SVariable r -> SValue r -> SValue r ->
  SValue r -> SValue r -> Maybe Integer -> MSStatement r
jlListSlice' vn vo beg end step mStep = do
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
jlRange :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
jlRange initv finalv stepv = do
  t <- listType int
  iv <- initv
  sv <- stepv
  fv <- finalv
  mkVal t (RC.value iv <> colon <> RC.value sv <> colon <> RC.value fv)

jlSplit :: String
jlSplit = "split"

jlPrintFunc, jlPrintLnFunc :: Doc
jlPrintFunc = text printLabel
jlPrintLnFunc = text "println"

jlParseFunc :: Label
jlParseFunc = "parse"

jlType, arrow, jlNamedArgSep :: Doc
jlType = colon <> colon
arrow = text "->"
jlNamedArgSep = equals

jlTuple :: [String] -> String
jlTuple ts = "Tuple{" ++ intercalate listSep ts ++ "}"

-- Operators
jlUnaryMath :: (Monad r) => String -> VSOp r
jlUnaryMath = unOpPrec

jlPower, jlIntDiv :: String
jlPower = "^"
jlIntDiv = "÷"

-- Constants
jlPi :: Doc
jlPi = text "pi"

-- Comments
jlCmtStart, jlBlockCmtStart, jlBlockCmtEnd, jlDocCmtStart, jlDocCmtEnd :: Doc
jlCmtStart      = text "#"
jlBlockCmtStart = text "#="
jlBlockCmtEnd   = text "=#"
jlDocCmtStart   = text "\"\"\""
jlDocCmtEnd     = text "\"\"\""

-- Control structures

jlSpace :: OptionalSpace
jlSpace = OSpace {oSpace = empty}

-- | Creates a for-each loop in Julia
jlForEach :: (CommonRenderSym r) => r (Variable r) -> r (Value r) -> r (Body r) -> Doc
jlForEach i lstVar b = vcat [
  forLabel <+> RC.variable i <+> inLabel <+> RC.value lstVar,
  indent $ RC.body b,
  jlEnd]

-- | Creates the contents of a module in Julia
jlModContents :: Label -> [Label] -> [SMethod JuliaCode] ->
  FSModule JuliaCode
jlModContents n is = A.buildModule n (do
  lis <- getLangImports
  libis <- getLibImports
  mis <- getModuleImports
  pure $ vibcat [
    vcat (map (RC.import' . li) lis),
    vcat (map (RC.import' . li) (sort $ is ++ libis)),
    vcat (map (RC.import' . mi) mis)])
  (do getMainDoc)
  where mi, li :: Label -> JuliaCode (Import JuliaCode)
        mi = modImport
        li = langImport

-- Functions
-- | Creates a function.  n is function name, pms is list of parameters, and
--   bod is body.
jlIntFunc :: (CommonRenderSym r) => Label -> [r (Parameter r)] ->
  r (Body r) -> Doc
jlIntFunc n pms bod = do
  vcat [jlFunc <+> text n <> parens (parameterList pms),
        indent $ RC.body bod,
        jlEnd]

jlLambda :: (CommonRenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
jlLambda ps ex = variableList ps <+> arrow <+> RC.value ex

-- Exceptions
jlThrow :: (CommonRenderSym r) => r (Value r) -> Doc
jlThrow errMsg = jlThrowLabel <> parens (RC.value errMsg)

jlTryCatch :: (CommonRenderSym r) => r (Body r) -> r (Body r) -> Doc
jlTryCatch tryB catchB = vcat [
  tryLabel,
  indent $ RC.body tryB,
  catchLabel <+> jlException,
  indent $ RC.body catchB,
  jlEnd]

jlException :: Doc
jlException = text "ErrorException"

includeLabel, importLabel :: Doc
includeLabel = text "include"
importLabel = text "import"

-- Assertions
jlAssert :: (CommonRenderSym r) => r (Value r) -> r (Value r) -> Doc
jlAssert condition errorMessage = vcat [
  text "@assert" <+> RC.value condition <+> RC.value errorMessage
  ]

jlMod, elseIfLabel, jlFunc, jlBegin, jlEnd, jlThrowLabel :: Doc
jlMod        = text "module"
elseIfLabel  = text "elseif"
jlFunc       = text "function"
jlBegin      = text "begin"
jlEnd        = text "end"
jlThrowLabel = text "error" -- TODO: this hints at an underdeveloped exception system

jlParam :: (CommonRenderSym r) => r (Variable r) -> Doc
jlParam v = RC.variable v <> jlType <> RC.type' (variableType v)

-- Type names specific to Julia (there's a lot of them)
jlIntType :: (CommonRenderSym r) => VSType r
jlIntType = typeFromData Integer jlIntConc (text jlIntConc)

jlFloatType :: (CommonRenderSym r) => VSType r
jlFloatType = typeFromData Float jlFloatConc (text jlFloatConc)

jlDoubleType :: (CommonRenderSym r) => VSType r
jlDoubleType = typeFromData Double jlDoubleConc (text jlDoubleConc)

jlCharType :: (CommonRenderSym r) => VSType r
jlCharType = typeFromData Char jlCharConc (text jlCharConc)

jlStringType :: (CommonRenderSym r) => VSType r
jlStringType = typeFromData String jlStringConc (text jlStringConc)

jlInfileType :: (CommonRenderSym r) => VSType r
jlInfileType = typeFromData InFile jlFile (text jlFile)

jlOutfileType :: (CommonRenderSym r) => VSType r
jlOutfileType = typeFromData OutFile jlFile (text jlFile)

jlListType :: (CommonRenderSym r) => VSType r -> VSType r
jlListType t' = do
  t <- t'
  let typeName = jlListConc ++ "{" ++ getTypeString t ++ "}"
  typeFromData (List $ getType t) typeName (text typeName)

jlSetType :: (CommonRenderSym r) => VSType r -> VSType r
jlSetType t' = do
  t <- t'
  let typeName = jlSetConc ++ "{" ++ getTypeString t ++ "}"
  typeFromData (Set $ getType t) typeName (text typeName)

jlVoidType :: (CommonRenderSym r) => VSType r
jlVoidType = typeFromData Void jlVoid (text jlVoid)

jlNull :: Label
jlNull = "nothing"

-- Modules
-- | Creates the text for the start of a module.
--   n is the name of the module.
jlModStart :: Label -> Doc
jlModStart n = jlMod <+> text n

-- IO
jlPrint :: Bool -> Maybe (SValue JuliaCode) -> SValue JuliaCode ->
  SValue JuliaCode -> MSStatement JuliaCode
-- Printing to console
jlPrint _ f' p' v' = do
  f <- zoom lensMStoVS $ fromMaybe (mkStateVal void empty) f' -- The file to print to
  prf <- zoom lensMStoVS p' -- The print function to use
  v <- zoom lensMStoVS v' -- The value to print
  let fl = emptyIfEmpty (RC.value f) $ RC.value f <> listSep'
  mkStmtNoEnd $ RC.value prf <> parens (fl <> RC.value v)

-- jlPrint can handle lists, so don't use G.print for lists
jlOut :: (CommonRenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r ->
  MSStatement r
jlOut newLn f printFn v = zoom lensMStoVS v >>= jlOut' . getType . valueType
  where jlOut' (List _) = printSt newLn f printFn v
        jlOut' _ = G.print newLn f printFn v

jlInput :: SValue JuliaCode -> SVariable JuliaCode -> MSStatement JuliaCode
jlInput inSrc v = v &= (v >>= jlInput' . getType . variableType)
  where jlInput' Integer = jlParse jlIntConc int inSrc
        jlInput' Float = jlParse jlFloatConc float inSrc
        jlInput' Double = jlParse jlDoubleConc double inSrc
        jlInput' Boolean = jlParse CP.boolRender bool inSrc
        jlInput' String = inSrc
        jlInput' Char = jlParse jlCharConc char inSrc
        jlInput' _ = error "Attempt to read a value of unreadable type"

readLine, readLines :: (CommonRenderSym r) => SValue r -> SValue r
readLine f = funcApp jlReadLineFunc string [f]
readLines f = funcApp jlReadLinesFunc (listType string) [f]

jlReadLine :: Doc
jlReadLine = text jlReadLineFunc

jlReadLineFunc, jlReadLinesFunc, jlCloseFunc :: Label
jlReadLineFunc = "readline"
jlReadLinesFunc = "readlines"
jlCloseFunc = "close"

jlArgs :: Label
jlArgs = "ARGS"

jlParse :: (CommonRenderSym r) => Label -> VSType r -> SValue r -> SValue r
jlParse tl tp v = let
  typeLabel = mkStateVal void (text tl)
  in funcApp jlParseFunc tp [typeLabel, v]
