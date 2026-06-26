{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The logic to render Julia code is contained in this module
module Drasil.GProc.LanguageRenderer.JuliaRenderer (
  -- * Julia Code Configuration -- defines syntax of all Julia code
  JuliaCode(..), jlName, jlVersion
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (UnRepr(..), SharedProg, Label, VSType,
  SValue, SVariable, MSStatement, MSBlock, SMethod, BodySym(..), BlockSym(..),
  TypeSym(..), getCodeType, getTypeString, VariableSym(..), VariableElim(..),
  ValueSym(..), Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), funcApp, extFuncApp, IndexTranslator(..),
  Reference(..), Array(..), List(..), Set(..), InternalList(..),
  StatementSym(..), AssignStatement(..), DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), VisibilitySym(..), ScopeSym(..),
  ParameterSym(..), BinderSym(..), BinderElim(..), MethodSym(..), (&=),
  switchAsIf, convScope)
import Drasil.GProc.InterfaceProc (ProcProg, FSModule, ProgramSym(..),
  FileSym(..), ModuleSym(..))

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  ImportElim, RenderBody(..), BodyElim, RenderBlock(..), BlockElim,
  RenderType(..), UnaryOpSym(..), BinaryOpSym(..), OpElim(uOpPrec, bOpPrec),
  RenderVariable(..), InternalVarElim(variableBind), RenderValue(..),
  ValueElim(..), InternalListFunc(..), RenderFunction(..),
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..),
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm),
  RenderVisibility(..), VisibilityElim, MethodTypeSym(..), RenderParam(..),
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim,
  BlockCommentSym(..), BlockCommentElim, ScopeElim(..), InternalBinderElim(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (import', body, block,
  uOp, bOp, variable, value, function, statement, visibility, parameter, method,
  blockComment')
import Drasil.GProc.RendererClassesProc (ProcRenderSym, RenderFile(..),
  RenderMod(..), ModuleElim, ProcRenderMethod(..))
import qualified Drasil.GProc.RendererClassesProc as RC (module')
import Drasil.Shared.LanguageRenderer (printLabel, listSep, listSep',
  parameterList, forLabel, inLabel, tryLabel, catchLabel,
  valueList, binderList)
import qualified Drasil.Shared.LanguageRenderer as R (sqrt, abs, log10, log,
  exp, sin, cos, tan, asin, acos, atan, floor, ceil, multiStmt, body,
  addComments, blockCmt, docCmt, commentedMod, commentedItem, break, continue,
  constDec', assign, subAssign, addAssign)
import Drasil.Shared.LanguageRenderer.Constructors (mkVal, mkStateVal, VSOp,
  unOpPrec, powerPrec, unExpr, unExpr', binExpr, multPrec, typeUnExpr,
  typeBinExpr, mkStmtNoEnd, typeFromData)
import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (OptionalSpace(..))
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  block, multiBlock, litChar, litDouble, litInt, litString, valueOf, negateOp,
  equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp,
  minusOp, multOp, divideOp, moduloOp, call, funcAppMixedArgs, lambda,
  listAccess, tryCatch, csc, multiBody, sec, cot, stmt, loopStmt, emptyStmt,
  print, comment, valStmt, returnStmt, param, docFunc, throw, arg, argsList,
  ifCond, smartAdd, local, var, smartSub)
import Drasil.GProc.Renderers (renderType)

import qualified Drasil.Shared.LanguageRenderer.Common as CS

import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (listDec,
  listDecDef, listSet, notNull, functionDoc, intToIndex', indexToInt', inOutFunc,
  docInOutFunc', forLoopError, openFileR', openFileW', openFileA', multiReturn,
  multiAssign, inOutCall, mainBody, argExists, litSet)

import qualified Drasil.Shared.LanguageRenderer.CLike as C (litTrue, litFalse,
  notOp, andOp, orOp, inlineIf, while)

import qualified Drasil.GProc.LanguageRenderer.AbstractProc as A (fileDoc,
  fileFromData, buildModule, docMod, modFromData, listAppend, listAdd,
  innerType, arrayElem, funcDecDef, function)
import qualified Drasil.Shared.LanguageRenderer.Macros as M (increment1,
  decrement1, ifExists, stringListVals, stringListLists, arrayDecAsList)
import Drasil.Shared.AST (Terminator(..), FileType(..), FileData(..), fileD,
  FuncData(..), ModData(..), md, updateMod, MethodData(..), mthd, OpData(..),
  ParamData(..), ProgData(..), TypeData(..), ValData(..), vd, VarData(..),
  vard, progD, fd, pd, updateMthd, ScopeTag(..), ScopeData(..), sd, BinderD(..),
  bindFormD)
import Drasil.Shared.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  on2CodeValues, on2StateValues, onCodeList, onStateList, emptyIfEmpty)
import Drasil.Shared.State (lensGStoFS, revFiles, setFileType, lensMStoVS,
  getModuleImports, addModuleImportVS, getLangImports, getLibImports,
  addLibImportVS, useVarName, getMainDoc, genVarNameIf, setVarScope, getVarScope)

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

instance UnRepr JuliaCode inner where
  unRepr = unJLC

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
  bool = CS.bool
  int = jlIntType
  float = jlFloatType
  double = jlDoubleType
  char = jlCharType
  string = jlStringType
  infile = jlInfileType
  outfile = jlOutfileType
  referenceType = id -- Ignore reference types in "high-level" langauges for now; later on think about using boxed/unboxed types
  listType = jlListType
  setType = jlSetType
  arrayType = listType -- Treat arrays and lists the same, as in Python
  innerType = A.innerType
  funcType = CS.funcType
  void = jlVoidType

instance RenderType JuliaCode where
  multiType ts = do
    typs <- sequence ts
    let mt = jlTuple $ map getTypeString typs
    typeFromData Void mt (text mt)

instance UnaryOpSym JuliaCode where
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
  global = toCode $ sd Global
  mainFn = global
  local = G.local

instance ScopeElim JuliaCode where
  scopeData = unJLC

instance VariableSym JuliaCode where
  type Variable JuliaCode = VarData
  var = G.var
  constant = var
  extVar l n t = modify (addModuleImportVS l) >> CS.extVar l n t

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
  valueType v = valType <$> v

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
    jlDivision (getCodeType $ valueType v1) (getCodeType $ valueType v2)
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
    CS.extFuncAppMixedArgs l n t ps ns
  libFuncAppMixedArgs l n t ps ns = do
    modify (addLibImportVS l)
    CS.extFuncAppMixedArgs l n t ps ns

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

instance IndexTranslator JuliaCode where
  intToIndex = CP.intToIndex'
  indexToInt = CP.indexToInt'

instance Reference JuliaCode where
  makeRef = id
  maybeDeref = id

instance Array JuliaCode where
  arrayElem = A.arrayElem
  arrayLength = listSize
  arrayCopy arr = let
    arrTp = onStateValue valueType arr
    in funcApp "copy" arrTp [arr]

instance List JuliaCode where
  listSize = CS.listSize jlListSize
  listAdd = A.listAdd jlListAdd
  listAppend = A.listAppend jlListAppend
  listAccess = G.listAccess
  listSet = CP.listSet
  indexOf = jlIndexOf

instance Set JuliaCode where
  contains s e = funcApp "in" bool [e, s]
  setAdd s e = funcApp "push!" void [s, e]
  setRemove s e = funcApp "delete!" void [s, e]
  setUnion a b = funcApp "union!" void [a, b]

instance InternalList JuliaCode where
  listSlice' b e s vn vo = jlListSlice vn vo b e (fromMaybe (litInt 1) s)

instance InternalListFunc JuliaCode where
  listAccessFunc = CS.listAccessFunc

instance BinderSym JuliaCode where
  binder nm tp = onCodeValue (bindFormD nm) <$> tp

instance BinderElim JuliaCode where
  binderName = bindName . unJLC
  binderType = onCodeValue bindType

instance InternalBinderElim JuliaCode where
  binderElim = text . bindName . unJLC

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
  varDec v scp = CS.varDecDef v scp Nothing
  varDecDef v scp e = CS.varDecDef v scp (Just e)
  setDec = varDec
  setDecDef = varDecDef
  listDec _ = CP.listDec
  listDecDef = CP.listDecDef
  arrayDec = M.arrayDecAsList
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
  forEach = CS.forEach' jlForEach
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

-- The only consistent way of creating floats is by casting
jlLitFloat :: (CommonRenderSym r) => Float -> SValue r
jlLitFloat f = mkStateVal float (text jlFloatConc <> parens (D.float f))

jlLitList :: VSType JuliaCode -> [SValue JuliaCode] -> SValue JuliaCode
jlLitList t' es = do
  t <- t'
  let lt' = listType t'
  elems <- sequence es
  let typeDec = if null es then renderType t else empty
  mkStateVal lt' (typeDec <> brackets (valueList elems))

jlCast :: VSType JuliaCode -> SValue JuliaCode -> SValue JuliaCode
jlCast t' v' = do
  t <- t'
  v <- v'
  let vTp = getCodeType $ valueType v
      tTp = getCodeType t
      vDoc = RC.value v
      tDoc = renderType t
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
  mkStmtNoEnd $ jlGlobalDec scpData <+> R.addAssign vr v

jlGlobalDec :: ScopeData -> Doc
jlGlobalDec scp = if scopeTag scp == Global then jlGlobal else empty

jlGlobal :: Doc
jlGlobal = text "global"

jlConstDecDef :: (CommonRenderSym r) => SVariable r -> r ScopeData -> SValue r
  -> MSStatement r
jlConstDecDef v' scp def' = do
  let scpData = scopeData scp
  v <- zoom lensMStoVS v'
  def <- zoom lensMStoVS def'
  modify $ useVarName $ variableName v
  modify $ setVarScope (variableName v) scpData
  let decDoc = if scopeTag scpData == Global then R.constDec' else empty
  mkStmtNoEnd $ decDoc <+> RC.variable v <+> equals <+> RC.value def

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
    jlListAbsdex t [lambda [binder "x" t] (valueOf (var "x" t) ?== v), l]

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
  fv <- finalv `G.smartSub` litInt 1
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

jlLambda :: (CommonRenderSym r) => [r BinderD] -> r (Value r) -> Doc
jlLambda ps ex = binderList ps <+> arrow <+> RC.value ex

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

jlParam :: JuliaCode (Variable JuliaCode) -> Doc
jlParam v = RC.variable v <> jlType <> renderType (variableType v)

-- Type names specific to Julia (there's a lot of them)
jlIntType :: (Monad r) => VSType r
jlIntType = typeFromData Integer jlIntConc (text jlIntConc)

jlFloatType :: (Monad r) => VSType r
jlFloatType = typeFromData Float jlFloatConc (text jlFloatConc)

jlDoubleType :: (Monad r) => VSType r
jlDoubleType = typeFromData Double jlDoubleConc (text jlDoubleConc)

jlCharType :: (Monad r) => VSType r
jlCharType = typeFromData Char jlCharConc (text jlCharConc)

jlStringType :: (Monad r) => VSType r
jlStringType = typeFromData String jlStringConc (text jlStringConc)

jlInfileType :: (Monad r) => VSType r
jlInfileType = typeFromData InFile jlFile (text jlFile)

jlOutfileType :: (Monad r) => VSType r
jlOutfileType = typeFromData OutFile jlFile (text jlFile)

jlListType :: (Monad r, UnRepr r TypeData) => VSType r -> VSType r
jlListType t' = do
  t <- t'
  let typeName = jlListConc ++ "{" ++ getTypeString t ++ "}"
  typeFromData (List $ getCodeType t) typeName (text typeName)

jlSetType :: (Monad r, UnRepr r TypeData) => VSType r -> VSType r
jlSetType t' = do
  t <- t'
  let typeName = jlSetConc ++ "{" ++ getTypeString t ++ "}"
  typeFromData (Set $ getCodeType t) typeName (text typeName)

jlVoidType :: (Monad r) => VSType r
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
jlOut :: (CommonRenderSym r, UnRepr r TypeData) => Bool -> Maybe (SValue r) ->
  SValue r -> SValue r -> MSStatement r
jlOut newLn f printFn v = zoom lensMStoVS v >>= jlOut' . getCodeType . valueType
  where jlOut' (List _) = printSt newLn f printFn v
        jlOut' _ = G.print newLn f printFn v

jlInput :: SValue JuliaCode -> SVariable JuliaCode -> MSStatement JuliaCode
jlInput inSrc v = v &= (v >>= jlInput' . getCodeType . variableType)
  where jlInput' Integer = jlParse jlIntConc int inSrc
        jlInput' Float = jlParse jlFloatConc float inSrc
        jlInput' Double = jlParse jlDoubleConc double inSrc
        jlInput' Boolean = jlParse CS.boolRender bool inSrc
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
