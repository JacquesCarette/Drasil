{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Swift code is contained in this module
module Drasil.GOOL.LanguageRenderer.SwiftRenderer (
  -- * Swift Code Configuration -- defines syntax of all Swift code
  SwiftCode(..), swiftName, swiftVersion
) where

import Utils.Drasil (indent)

import Drasil.GOOL.CodeType (CodeType(..))
import Drasil.GOOL.InterfaceCommon (SharedProg, Label, MSBody, MSBlock, VSType,
  SVariable, SValue, MSStatement, MSParameter, SMethod, BodySym(..), oneLiner,
  bodyStatements, BlockSym(..), TypeSym(..), TypeElim(..), VariableSym(..),
  VisibilitySym(..), VariableElim(..), ValueSym(..), Argument(..), Literal(..),
  litZero, MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp, List(..), Set(..),
  listSlice, InternalList(..), ThunkSym(..), VectorType(..), VectorDecl(..),
  VectorThunk(..), VectorExpression(..), ThunkAssign(..), StatementSym(..),
  AssignStatement(..), (&=), DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), ScopeSym(..), ParameterSym(..),
  MethodSym(..), convScope)
import Drasil.GOOL.InterfaceGOOL (OOProg, ProgramSym(..), FileSym(..),
  ModuleSym(..), ClassSym(..), OOTypeSym(..), OOVariableSym(..),
  StateVarSym(..), PermanenceSym(..), OOValueSym, OOVariableValue,
  OOValueExpression(..), selfFuncApp, newObj, InternalValueExp(..),
  objMethodCall, objMethodCallNamedArgs, objMethodCallNoParams,
  OOFunctionSym(..), ($.), GetSet(..), OODeclStatement(..),
  OOFuncAppStatement(..), ObserverPattern(..), StrategyPattern(..),
  OOMethodSym(..), Initializers,
  convTypeOO)
import Drasil.GOOL.RendererClassesCommon (MSMthdType, CommonRenderSym,
  ImportSym(..), ImportElim, RenderBody(..), BodyElim, RenderBlock(..),
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..),
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind),
  RenderValue(..), ValueElim(valuePrec, valueInt), InternalListFunc(..),
  RenderFunction(..), FunctionElim(functionType), InternalAssignStmt(..),
  InternalIOStmt(..), InternalControlStmt(..), RenderStatement(..),
  StatementElim(statementTerm), RenderVisibility(..), VisibilityElim,
  MethodTypeSym(..), RenderParam(..), ParamElim(parameterName, parameterType),
  RenderMethod(..), MethodElim, BlockCommentSym(..), BlockCommentElim,
  ScopeElim(..))
import qualified Drasil.GOOL.RendererClassesCommon as RC (import', body, block,
  type', uOp, bOp, variable, value, function, statement, visibility, parameter,
  method, blockComment')
import Drasil.GOOL.RendererClassesOO (OORenderSym, RenderFile(..),
  PermElim(binding), InternalGetSet(..), OOMethodTypeSym(..),
  OORenderMethod(..), StateVarElim, RenderClass(..), ClassElim, RenderMod(..),
  ModuleElim)
import qualified Drasil.GOOL.RendererClassesOO as RC (perm, stateVar,
  class', module')
import Drasil.GOOL.LanguageRenderer (dot, blockCmtStart, blockCmtEnd,
  docCmtStart, bodyStart, bodyEnd, commentStart, elseIfLabel, forLabel,
  inLabel, tryLabel, catchLabel, throwLabel, throwsLabel, importLabel, listSep',
  printLabel, listSep, piLabel, access, tuple, ClassDocRenderer, parameterList)
import qualified Drasil.GOOL.LanguageRenderer as R (sqrt, abs, log10, log, exp,
  sin, cos, tan, asin, acos, atan, floor, ceil, pow, class', multiStmt, body,
  classVar, func, listSetFunc, castObj, static, dynamic, break, continue,
  private, blockCmt, docCmt, addComments, commentedMod, commentedItem)
import Drasil.GOOL.LanguageRenderer.Constructors (mkStmtNoEnd, mkStateVal,
  mkVal, VSOp, unOpPrec, powerPrec, unExpr, unExpr', typeUnExpr, binExpr,
  binExpr', typeBinExpr)
import qualified Drasil.GOOL.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, listInnerType, obj, csc, sec, cot, negateOp,
  equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp,
  minusOp, multOp, divideOp, moduloOp, var, staticVar, objVar, arrayElem,
  litChar, litDouble, litInt, litString, valueOf, arg, argsList, objAccess,
  objMethodCall, call, funcAppMixedArgs, selfFuncAppMixedArgs, newObjMixedArgs,
  lambda, func, get, set, listAdd, listAppend, listAccess, listSet, getFunc,
  setFunc, listAppendFunc, stmt, loopStmt, emptyStmt, assign, subAssign,
  increment, objDecNew, print, returnStmt, valStmt, comment, throw, ifCond,
  tryCatch, construct, param, method, getMethod, setMethod, initStmts,
  function, docFunc, buildClass, implementingClass, docClass, commentedClass,
  modFromData, fileDoc, fileFromData, defaultOptSpace, local)
import qualified Drasil.GOOL.LanguageRenderer.CommonPseudoOO as CP (classVar, 
  objVarSelf, intClass, buildModule, docMod', contains, bindingError, extFuncAppMixedArgs, 
  notNull, listDecDef, destructorError, stateVarDef, constVar, litArray, 
  listSetFunc, extraClass, listAccessFunc, doubleRender, double, openFileR, 
  openFileW, self, multiAssign, multiReturn, listDec, funcDecDef, 
  inOutCall, forLoopError, mainBody, inOutFunc, docInOutFunc', bool, float, 
  stringRender', string', inherit, implements, functionDoc, intToIndex,
  indexToInt, forEach', global)
import qualified Drasil.GOOL.LanguageRenderer.CLike as C (notOp, andOp, orOp, inOp, 
  litTrue, litFalse, inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, 
  listSize, varDecDef, setDecDef, extObjDecNew, switch, while)
import qualified Drasil.GOOL.LanguageRenderer.Macros as M (ifExists, decrement1,
  increment1, runStrategy, stringListVals, stringListLists, notifyObservers',
  makeSetterVal)
import Drasil.GOOL.AST (Terminator(..), VisibilityTag(..), qualName, FileType(..),
  FileData(..), fileD, FuncData(..), fd, ModData(..), md, updateMod,
  MethodData(..), mthd, updateMthd, OpData(..), ParamData(..), pd, ProgData(..),
  progD, TypeData(..), td, ValData(..), vd, Binding(..), VarData(..), vard,
  CommonThunk, pureValue, vectorize, vectorize2, sumComponents, commonVecIndex,
  commonThunkElim, commonThunkDim, ScopeData)
import Drasil.GOOL.Helpers (hicat, emptyIfNull, toCode, toState, onCodeValue,
  onStateValue, on2CodeValues, on2StateValues, onCodeList, onStateList)
import Drasil.GOOL.State (MS, VS, lensGStoFS, lensFStoCS, lensFStoMS,
  lensCStoVS, lensMStoFS, lensMStoVS, lensVStoFS, revFiles, addLangImportVS,
  getLangImports, getLibImports, setFileType, getClassName, setModuleName,
  getModuleName, getCurrMain, getMethodExcMap, getMainDoc, setThrowUsed,
  getThrowUsed, setErrorDefined, getErrorDefined, incrementLine, incrementWord,
  getLineIndex, getWordIndex, resetIndices, useVarName, genLoopIndex,
  genVarNameIf, setVarScope, getVarScope)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)
import Data.Composition ((.:))
import Data.List (intercalate, sort)
import Data.Map (findWithDefault)
import Data.Maybe (fromMaybe, isNothing)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
  vcat, lbrace, rbrace, braces, brackets, colon, space, doubleQuotes)
import qualified Text.PrettyPrint.HughesPJ as D (float)

swiftExt :: String
swiftExt = "swift"

newtype SwiftCode a = SC {unSC :: a} deriving Eq

instance Functor SwiftCode where
  fmap f (SC x) = SC (f x)

instance Applicative SwiftCode where
  pure = SC
  (SC f) <*> (SC x) = SC (f x)

instance Monad SwiftCode where
  SC x >>= f = f x

instance SharedProg SwiftCode
instance OOProg SwiftCode

instance ProgramSym SwiftCode where
  type Program SwiftCode = ProgData
  prog n st files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n st) fs

instance CommonRenderSym SwiftCode
instance OORenderSym SwiftCode

instance FileSym SwiftCode where
  type File SwiftCode = FileData
  fileDoc m = do
    modify (setFileType Combined)
    G.fileDoc swiftExt top bottom m

  docMod = CP.docMod' swiftExt

instance RenderFile SwiftCode where
  top _ = toCode empty
  bottom = toCode empty

  commentedMod = on2StateValues (on2CodeValues R.commentedMod)

  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym SwiftCode where
  type Import SwiftCode = Doc
  langImport n = toCode $ importLabel <+> text n
  modImport = langImport

instance ImportElim SwiftCode where
  import' = unSC

instance PermanenceSym SwiftCode where
  type Permanence SwiftCode = Doc
  static = toCode R.static
  dynamic = toCode R.dynamic

instance PermElim SwiftCode where
  perm = unSC
  binding = error $ CP.bindingError swiftName

instance BodySym SwiftCode where
  type Body SwiftCode = Doc
  body = onStateList (onCodeList R.body)

  addComments s = onStateValue (onCodeValue (R.addComments s commentStart))

instance RenderBody SwiftCode where
  multiBody = G.multiBody

instance BodyElim SwiftCode where
  body = unSC

instance BlockSym SwiftCode where
  type Block SwiftCode = Doc
  block = G.block

instance RenderBlock SwiftCode where
  multiBlock = G.multiBlock

instance BlockElim SwiftCode where
  block = unSC

instance TypeSym SwiftCode where
  type Type SwiftCode = TypeData
  bool = CP.bool
  int = swiftIntType
  float = CP.float
  double = CP.double
  char = swiftCharType
  string = CP.string'
  infile = swiftFileType
  outfile = swiftFileHdlType
  listType = swiftListType
  arrayType = listType -- For now, treating arrays and lists the same, like we do for Python
  setType = listType
  listInnerType = G.listInnerType
  funcType = swiftFuncType
  void = swiftVoidType

instance OOTypeSym SwiftCode where
  obj = G.obj

instance TypeElim SwiftCode where
  getType = cType . unSC
  getTypeString = typeString . unSC

instance RenderType SwiftCode where
  multiType ts = do
    typs <- sequence ts
    let mt = tuple $ map getTypeString typs
    typeFromData Void mt (text mt)
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim SwiftCode where
  type' = typeDoc . unSC

instance UnaryOpSym SwiftCode where
  type UnaryOp SwiftCode = OpData
  notOp = C.notOp
  negateOp = G.negateOp
  sqrtOp = swiftUnaryMath R.sqrt
  absOp = swiftUnaryMath R.abs
  logOp = swiftUnaryMath R.log10
  lnOp = swiftUnaryMath R.log
  expOp = swiftUnaryMath R.exp
  sinOp = swiftUnaryMath R.sin
  cosOp = swiftUnaryMath R.cos
  tanOp = swiftUnaryMath R.tan
  asinOp = swiftUnaryMath R.asin
  acosOp = swiftUnaryMath R.acos
  atanOp = swiftUnaryMath R.atan
  floorOp = swiftUnaryMath R.floor
  ceilOp = swiftUnaryMath R.ceil

instance BinaryOpSym SwiftCode where
  type BinaryOp SwiftCode = OpData
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
  powerOp = addMathImport $ powerPrec R.pow
  moduloOp = G.moduloOp
  andOp = C.andOp
  orOp = C.orOp
  inOp = C.inOp

instance OpElim SwiftCode where
  uOp = opDoc . unSC
  bOp = opDoc . unSC
  uOpPrec = opPrec . unSC
  bOpPrec = opPrec . unSC

instance ScopeSym SwiftCode where
  type Scope SwiftCode = ScopeData
  global = CP.global
  mainFn = global
  local = G.local

instance ScopeElim SwiftCode where
  scopeData = unSC

instance VariableSym SwiftCode where
  type Variable SwiftCode = VarData
  var         = G.var
  constant    = var
  extVar _    = var
  arrayElem i = G.arrayElem (litInt i)

instance OOVariableSym SwiftCode where
  staticVar' _ = G.staticVar
  self = CP.self
  classVar = CP.classVar R.classVar
  extClassVar = classVar
  objVar = G.objVar
  objVarSelf = CP.objVarSelf

instance VariableElim SwiftCode where
  variableName = varName . unSC
  variableType = onCodeValue varType

instance InternalVarElim SwiftCode where
  variableBind = varBind . unSC
  variable = varDoc . unSC

instance RenderVariable SwiftCode where
  varFromData b n t' d = do
    t <- t'
    pure $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym SwiftCode where
  type Value SwiftCode = ValData
  valueType = onCodeValue valType

instance OOValueSym SwiftCode

instance Argument SwiftCode where
  pointerArg = swiftArgVal

instance Literal SwiftCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar doubleQuotes
  litDouble = G.litDouble
  litFloat = swiftLitFloat
  litInt = G.litInt
  litString = G.litString
  litArray = CP.litArray brackets
  litSet = litArray
  litList = litArray

instance MathConstant SwiftCode where
  pi = mkStateVal double swiftPi

instance VariableValue SwiftCode where
  valueOf = G.valueOf

instance OOVariableValue SwiftCode

instance CommandLineArgs SwiftCode where
  arg n = G.arg (litInt n) argsList
  argsList = G.argsList (swiftCommLine `access` swiftArgs)
  argExists i = listSize argsList ?> litInt (fromIntegral i)

instance NumericExpression SwiftCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = swiftNumBinExpr (binExpr plusOp)
  (#-) = swiftNumBinExpr (binExpr minusOp)
  (#*) = swiftNumBinExpr (binExpr multOp)
  (#/) = swiftNumBinExpr (binExpr divideOp)
  (#%) = binExpr moduloOp
  (#^) v1' v2' = do
    v1 <- v1'
    v2 <- v2'
    let swiftPower Integer Integer b e = cast int $ binExpr' powerOp
          (cast double b) (cast double e)
        swiftPower _ _ b e = binExpr' powerOp b e
    swiftPower (getType $ valueType v1) (getType $ valueType v2) (pure v1)
      (pure v2)

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

instance BooleanExpression SwiftCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool
  isin = typeBinExpr inOp bool
  
instance Comparison SwiftCode where
  (?<) = swiftNumBinExpr (typeBinExpr lessOp bool)
  (?<=) = swiftNumBinExpr (typeBinExpr lessEqualOp bool)
  (?>) = swiftNumBinExpr (typeBinExpr greaterOp bool)
  (?>=) = swiftNumBinExpr (typeBinExpr greaterEqualOp bool)
  (?==) = swiftNumBinExpr (typeBinExpr equalOp bool)
  (?!=) = swiftNumBinExpr (typeBinExpr notEqualOp bool)

instance ValueExpression SwiftCode where
  inlineIf = C.inlineIf

  funcAppMixedArgs = G.funcAppMixedArgs
  extFuncAppMixedArgs = CP.extFuncAppMixedArgs
  libFuncAppMixedArgs = C.libFuncAppMixedArgs

  lambda = G.lambda swiftLambda

  notNull = CP.notNull swiftNil

instance OOValueExpression SwiftCode where
  selfFuncAppMixedArgs = G.selfFuncAppMixedArgs dot self
  newObjMixedArgs = G.newObjMixedArgs ""
  extNewObjMixedArgs m tp vs ns = do
    t <- tp
    call (Just m) Nothing (getTypeString t) (pure t) vs ns
  libNewObjMixedArgs = C.libNewObjMixedArgs

instance RenderValue SwiftCode where
  inputFunc = mkStateVal string empty
  printFunc = mkStateVal void empty
  printLnFunc = mkStateVal void empty
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty

  cast = swiftCast

  call l o n t as ns = do
    mn <- zoom lensVStoFS getModuleName
    mem <- getMethodExcMap
        -- If function being called throws exceptions, need to wrap call in try
    let f = if null $ findWithDefault [] (maybe (qualName mn n) (`qualName` n)
          l) mem then id else swiftTryVal
    f $ G.call swiftNamedArgSep Nothing o n t as ns

  valFromData p i t' d = do
    t <- t'
    pure $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim SwiftCode where
  valuePrec = valPrec . unSC
  valueInt = valInt . unSC
  value = val . unSC

instance InternalValueExp SwiftCode where
  objMethodCallMixedArgs' = G.objMethodCall

instance FunctionSym SwiftCode where
  type Function SwiftCode = FuncData

instance OOFunctionSym SwiftCode where
  func = G.func
  objAccess = G.objAccess

instance GetSet SwiftCode where
  get = G.get
  set = G.set

instance List SwiftCode where
  intToIndex = CP.intToIndex
  indexToInt = CP.indexToInt
  listSize = C.listSize
  listAdd = G.listAdd
  listAppend = G.listAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = swiftIndexOf

instance Set SwiftCode where
  contains = CP.contains swiftContains

instance InternalList SwiftCode where
  listSlice' b e s vn vo = swiftListSlice vn vo b e (fromMaybe (litInt 1) s)

instance InternalGetSet SwiftCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

instance InternalListFunc SwiftCode where
  listSizeFunc _ = funcFromData (R.func swiftListSize) int
  listAddFunc _ i v = do
    f <- swiftListAddFunc i v
    funcFromData (R.func (RC.value f)) (pure $ valueType f)
  listAppendFunc _ = G.listAppendFunc swiftListAppend
  listAccessFunc = CP.listAccessFunc
  listSetFunc = CP.listSetFunc R.listSetFunc

instance ThunkSym SwiftCode where
  type Thunk SwiftCode = CommonThunk VS

instance ThunkAssign SwiftCode where
  thunkAssign v t = do
    iName <- genLoopIndex
    let
      i = var iName int
      dim = fmap pure $ t >>= commonThunkDim (fmap unSC . listSize . fmap pure) . unSC
      loopInit = zoom lensMStoVS (fmap unSC t) >>= commonThunkElim
        (const emptyStmt) (const $ assign v $ litZero $ fmap variableType v)
      loopBody = zoom lensMStoVS (fmap unSC t) >>= commonThunkElim
        (valStmt . listSet (valueOf v) (valueOf i) . vecIndex (valueOf i) . pure . pure)
        ((v &+=) . vecIndex (valueOf i) . pure . pure)
    multi [loopInit,
      forRange i (litInt 0) dim (litInt 1) $ body [block [loopBody]]]

instance VectorType SwiftCode where
  vecType = listType

instance VectorDecl SwiftCode where
  vecDec = listDec
  vecDecDef = listDecDef

instance VectorThunk SwiftCode where
  vecThunk = pure . pure . pureValue . fmap unSC . valueOf

instance VectorExpression SwiftCode where
  vecScale k = fmap $ fmap $ vectorize (fmap unSC . (k #*) . fmap pure)
  vecAdd = liftA2 $ liftA2 $ vectorize2 (\v1 v2 -> fmap unSC $ fmap pure v1 #+ fmap pure v2)
  vecIndex i = (>>= fmap pure . commonVecIndex (fmap unSC . flip listAccess i . fmap pure) . unSC)
  vecDot = liftA2 $ liftA2 $ fmap sumComponents <$> vectorize2 (\v1 v2 -> fmap unSC $ fmap pure v1 #* fmap pure v2)

instance RenderFunction SwiftCode where
  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance FunctionElim SwiftCode where
  functionType = onCodeValue fType
  function = funcDoc . unSC

instance InternalAssignStmt SwiftCode where
  multiAssign = CP.multiAssign parens

instance InternalIOStmt SwiftCode where
  printSt = swiftPrint

instance InternalControlStmt SwiftCode where
  multiReturn = CP.multiReturn parens

instance RenderStatement SwiftCode where
  stmt = G.stmt
  loopStmt = G.loopStmt
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim SwiftCode where
  statement = fst . unSC
  statementTerm = snd . unSC

instance StatementSym SwiftCode where
  type Statement SwiftCode = (Doc, Terminator)
  valStmt = G.valStmt Empty
  emptyStmt = G.emptyStmt
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement SwiftCode where
  assign = G.assign Empty
  (&-=) = G.subAssign Empty
  (&+=) = G.increment
  (&++) = M.increment1
  (&--) = M.decrement1

instance DeclStatement SwiftCode where
  varDec = swiftVarDec swiftVar
  varDecDef = C.varDecDef Empty
  setDecDef = C.setDecDef Empty
  setDec = swiftSetDec swiftConst
  listDec _ = CP.listDec
  listDecDef = CP.listDecDef
  arrayDec = listDec
  arrayDecDef = listDecDef
  constDecDef vr scp vl' = do
    vdec <- swiftVarDec swiftConst vr scp
    vl <- zoom lensMStoVS vl'
    mkStmtNoEnd $ RC.statement vdec <+> equals <+> RC.value vl
  funcDecDef = CP.funcDecDef

instance OODeclStatement SwiftCode where
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = C.extObjDecNew

instance IOStatement SwiftCode where
  print      = swiftOut False Nothing printFunc
  printLn    = swiftOut True  Nothing printLnFunc
  printStr   = swiftOut False Nothing printFunc   . litString
  printStrLn = swiftOut True  Nothing printLnFunc . litString

  printFile f      = swiftOut False (Just f) (printFileFunc f)
  printFileLn f    = swiftOut True  (Just f) (printFileLnFunc f)
  printFileStr f   = swiftOut False (Just f) (printFileFunc f)   . litString
  printFileStrLn f = swiftOut True  (Just f) (printFileLnFunc f) . litString

  getInput v = v &= swiftInput v swiftReadLineFunc
  discardInput = valStmt swiftReadLineFunc
  getFileInput _ v = do
    wi <- getWordIndex
    li <- getLineIndex
    modify incrementWord
    v &= swiftInput v
      (listAccess (listAccess swiftContentsVal (litInt li)) (litInt wi))
  discardFileInput _ = modify incrementWord >> emptyStmt

  openFileR v pth = do
    v' <- zoom lensMStoVS v
    scpData <- getVarScope $ variableName v'
    let scp = convScope scpData
    multi [CP.openFileR swiftOpenFile v pth,
      varDec swiftContentsVar scp, swiftReadFile swiftContentsVar (valueOf v)]
  openFileW = swiftOpenFileWA False
  openFileA = swiftOpenFileWA True
  closeFile = swiftCloseFile

  getFileInputLine _ v = do
    v' <- zoom lensMStoVS v
    scpData <- getVarScope $ variableName v'
    let scp = convScope scpData
    wi <- getWordIndex
    li <- getLineIndex
    modify incrementLine
    slc <- listSlice swiftLineVar (listAccess swiftContentsVal (litInt li))
      (Just $ litInt wi) Nothing Nothing
    multi [varDec swiftLineVar scp, mkStmtNoEnd $ RC.block slc,
      v &= swiftJoinedFunc ' ' swiftLineVal]
  discardFileLine _ = modify incrementLine >> emptyStmt
  getFileInputAll _ v = do
    li <- getLineIndex
    let l = var "l" (listType string)
    slc <- listSlice swiftContentsVar swiftContentsVal 
      (Just $ litInt (li+1)) Nothing Nothing
    multi [mkStmtNoEnd $ RC.block slc,
      v &= swiftMapFunc swiftContentsVal
        (lambda [l] (swiftJoinedFunc ' ' (valueOf l)))]

instance StringStatement SwiftCode where
  stringSplit d vnew s = vnew &= swiftSplitFunc d s

  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FuncAppStatement SwiftCode where
  inOutCall = CP.inOutCall funcApp
  extInOutCall m = CP.inOutCall (extFuncApp m)

instance OOFuncAppStatement SwiftCode where
  selfInOutCall = CP.inOutCall selfFuncApp

instance CommentStatement SwiftCode where
  comment = G.comment commentStart

instance ControlStatement SwiftCode where
  break = mkStmtNoEnd R.break
  continue = mkStmtNoEnd R.continue

  returnStmt = G.returnStmt Empty

  throw msg = do
    modify setThrowUsed
    G.throw swiftThrowDoc Empty msg

  ifCond = G.ifCond id bodyStart G.defaultOptSpace elseIfLabel bodyEnd empty
  switch = C.switch (space <>) emptyStmt

  ifExists = M.ifExists

  for _ _ _ _ = error $ CP.forLoopError swiftName
  forRange i initv finalv stepv = forEach i (swiftStrideFunc initv finalv stepv)
  forEach = CP.forEach' swiftForEach
  while = C.while id bodyStart bodyEnd

  tryCatch = G.tryCatch swiftTryCatch

  assert condition errorMessage = do
    cond <- zoom lensMStoVS condition
    errMsg <- zoom lensMStoVS errorMessage
    mkStmtNoEnd (swiftAssert cond errMsg)

instance ObserverPattern SwiftCode where
  notifyObservers = M.notifyObservers'

instance StrategyPattern SwiftCode where
  runStrategy = M.runStrategy

instance VisibilitySym SwiftCode where
  type Visibility SwiftCode = Doc
  private = toCode R.private
  public = toCode empty

instance RenderVisibility SwiftCode where
  visibilityFromData _ = toCode
  
instance VisibilityElim SwiftCode where
  visibility = unSC

instance MethodTypeSym SwiftCode where
  type MethodType SwiftCode = TypeData
  mType = zoom lensMStoVS
  
instance OOMethodTypeSym SwiftCode where
  construct = G.construct

instance ParameterSym SwiftCode where
  type Parameter SwiftCode = ParamData
  param = G.param (swiftParam empty)
  pointerParam = G.param (swiftParam swiftInOut)

instance RenderParam SwiftCode where
  paramFromData v' d = do
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)

instance ParamElim SwiftCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unSC

instance MethodSym SwiftCode where
  type Method SwiftCode = MethodData
  docMain = mainFunction
  function = G.function
  mainFunction = CP.mainBody
  docFunc = G.docFunc CP.functionDoc

  inOutFunc n s = CP.inOutFunc (function n s)

  docInOutFunc n s = CP.docInOutFunc' CP.functionDoc (inOutFunc n s)

instance OOMethodSym SwiftCode where
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor = swiftConstructor

  inOutMethod n s p = CP.inOutFunc (method n s p)
  docInOutMethod n s p = CP.docInOutFunc' CP.functionDoc (inOutMethod n s p)

instance RenderMethod SwiftCode where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)

  mthdFromData _ d = toState $ toCode $ mthd d

instance OORenderMethod SwiftCode where
  intMethod _ = swiftMethod
  intFunc _ n s _ = swiftMethod n s dynamic
  destructor _ = error $ CP.destructorError swiftName

instance MethodElim SwiftCode where
  method = mthdDoc . unSC

instance StateVarSym SwiftCode where
  type StateVar SwiftCode = Doc
  stateVar s p vr = do
    v <- zoom lensCStoVS vr
    stateVarDef s p vr (typeDfltVal $ getType $ variableType v)
  stateVarDef = CP.stateVarDef
  constVar = CP.constVar (RC.perm (static :: SwiftCode (Permanence SwiftCode)))

instance StateVarElim SwiftCode where
  stateVar = unSC

instance ClassSym SwiftCode where
  type Class SwiftCode = Doc
  buildClass = G.buildClass
  extraClass = CP.extraClass
  implementingClass = G.implementingClass

  docClass = G.docClass swiftClassDoc

instance RenderClass SwiftCode where
  intClass = CP.intClass R.class'

  inherit = CP.inherit
  implements = CP.implements

  commentedClass = G.commentedClass

instance ClassElim SwiftCode where
  class' = unSC

instance ModuleSym SwiftCode where
  type Module SwiftCode = ModData
  buildModule n is fs cs = do
    modify (setModuleName n) -- This needs to be set before the functions/
                             -- classes are evaluated. CP.buildModule will 
                             -- reset it to the proper name.
    fns <- mapM (zoom lensFStoMS) fs
    cls <- mapM (zoom lensFStoCS) cs
    mn <- getCurrMain
    let modName = if mn then swiftMain else n
    CP.buildModule modName (do
      lis <- getLangImports
      libis <- getLibImports
      pure $ vcat $ map (RC.import' . 
          (langImport :: Label -> SwiftCode (Import SwiftCode))) 
          (sort $ lis ++ is ++ libis)) 
      (zoom lensFStoMS swiftStringError) getMainDoc 
        (map pure fns) (map pure cls)
  
instance RenderMod SwiftCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim SwiftCode where
  module' = modDoc . unSC

instance BlockCommentSym SwiftCode where
  type BlockComment SwiftCode = Doc
  blockComment lns = toCode $ R.blockCmt lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns docCmtStart
    blockCmtEnd)

instance BlockCommentElim SwiftCode where
  blockComment' = unSC

addMathImport :: VS a -> VS a
addMathImport = (>>) $ modify (addLangImportVS swiftMath)

addFoundationImport :: VS a -> VS a
addFoundationImport = (>>) $ modify (addLangImportVS swiftFoundation)

swiftName, swiftVersion :: String
swiftName = "Swift"
swiftVersion = "5.2.4"

swiftUnwrapVal :: (CommonRenderSym r) => SValue r -> SValue r
swiftUnwrapVal v' = do
  v <- v'
  mkVal (valueType v) (RC.value v <> swiftUnwrap')

swiftTryVal :: (CommonRenderSym r) => SValue r -> SValue r
swiftTryVal v' = do
  v <- v'
  mkVal (valueType v) (tryLabel <+> RC.value v)

swiftArgVal :: (CommonRenderSym r) => SValue r -> SValue r
swiftArgVal v' = do
  v <- v'
  mkVal (valueType v) (swiftInOutArg <> RC.value v)

-- Putting "gool" in these names to avoid name conflicts
-- The `local` is a hack, but Swift doesn't care about scope
-- and I don't want to change the IOStatement API just for this
swiftContentsVar, swiftLineVar :: SVariable SwiftCode
swiftContentsVar = var "goolContents" (listType $ listType string)
swiftLineVar = var "goolLine" (listType string)

swiftContentsVal, swiftLineVal :: SValue SwiftCode
swiftContentsVal = valueOf swiftContentsVar
swiftLineVal = valueOf swiftLineVar

swiftIntType :: (CommonRenderSym r) => VSType r
swiftIntType = typeFromData Integer swiftInt (text swiftInt)

swiftCharType :: (CommonRenderSym r) => VSType r
swiftCharType = typeFromData Char swiftChar (text swiftChar)

swiftFileType :: (CommonRenderSym r) => VSType r
swiftFileType = addFoundationImport $ typeFromData InFile swiftURL
  (text swiftURL)

swiftFileHdlType :: (CommonRenderSym r) => VSType r
swiftFileHdlType = addFoundationImport $ typeFromData OutFile swiftFileHdl
  (text swiftFileHdl)

swiftListType :: (CommonRenderSym r) => VSType r -> VSType r
swiftListType t' = do
  t <- t'
  typeFromData (List $ getType t) ("[" ++ getTypeString t ++ "]")
    (brackets $ RC.type' t)


swiftFuncType :: (CommonRenderSym r) => [VSType r] -> VSType r -> VSType r
swiftFuncType ps r = do
  pts <- sequence ps
  rt <- r
  typeFromData (Func (map getType pts) (getType rt))
    ("(" ++ intercalate listSep (map getTypeString pts) ++ ")" ++ " " ++
      swiftRetType ++ " " ++ getTypeString rt)
    (parens (hicat listSep' $ map RC.type' pts) <+> swiftRetType' <+>
      RC.type' rt)

swiftVoidType :: (CommonRenderSym r) => VSType r
swiftVoidType = typeFromData Void swiftVoid (text swiftVoid)

swiftPi, swiftListSize, swiftFirst, swiftDesc, swiftUTF8, swiftVar, swiftConst,
  swiftDo, swiftFunc, swiftCtorName, swiftExtension, swiftInOut, swiftError,
  swiftDocDir, swiftUserMask, swiftInOutArg, swiftNamedArgSep, swiftTypeSpec,
  swiftConforms, swiftNoLabel, swiftRetType', swiftUnwrap' :: Doc
swiftPi = text $ CP.doubleRender `access` piLabel
swiftListSize = text "count"
swiftFirst = text "first"
swiftDesc = text "description"
swiftUTF8 = text "utf8"
swiftVar = text "var"
swiftConst = text "let"
swiftDo = text "do"
swiftFunc = text "func"
swiftCtorName = text "init"
swiftExtension = text "extension"
swiftInOut = text "inout"
swiftError = text "Error"
swiftDocDir = text $ "" `access` "documentDirectory"
swiftUserMask = text $ "" `access` "userDomainMask"
swiftNamedArgSep = colon <> space
swiftInOutArg = text "&"
swiftTypeSpec = colon
swiftConforms = colon
swiftNoLabel = text "_"
swiftRetType' = text swiftRetType
swiftUnwrap' = text swiftUnwrap

swiftMain, swiftFoundation, swiftMath, swiftNil, swiftInt, swiftChar,
  swiftURL, swiftFileHdl, swiftRetType, swiftVoid, swiftCommLine,
  swiftSearchDir, swiftPathMask, swiftArgs, swiftWrite, swiftIndex,
  swiftStride, swiftMap, swiftListAdd, swiftListAppend, swiftReadLine,
  swiftSeekEnd, swiftClose, swiftJoined, swiftAppendPath, swiftUrls, swiftSplit,
  swiftData, swiftEncoding, swiftOf, swiftFrom, swiftTo, swiftBy, swiftAt,
  swiftTerm, swiftFor, swiftIn, swiftContentsOf, swiftWriteTo, swiftSep,
  swiftSepBy, swiftUnwrap, swiftContains, swiftSet :: String
swiftMain = "main"
swiftFoundation = "Foundation"
swiftMath = swiftFoundation
swiftNil = "nil"
swiftInt = "Int"
swiftChar = "Character"
swiftURL = "URL"
swiftFileHdl = "FileHandle"
swiftRetType = "->"
swiftVoid = "Void"
swiftCommLine = "CommandLine"
swiftSearchDir = "SearchPathDirectory"
swiftPathMask = "SearchPathDomainMask"
swiftArgs = "arguments"
swiftWrite = "write"
swiftIndex = "firstIndex"
swiftStride = "stride"
swiftMap = "map"
swiftListAdd = "insert"
swiftListAppend = "append"
swiftReadLine = "readLine"
swiftSeekEnd = "seekToEnd"
swiftClose = "close"
swiftJoined = "joined"
swiftAppendPath = "appendingPathComponent"
swiftUrls = "FileManager" `access` "default" `access` "urls"
swiftSplit = "components"
swiftData = "Data"
swiftEncoding = "Encoding"
swiftOf = "of"
swiftFrom = "from"
swiftTo = "to"
swiftBy = "by"
swiftAt = "at"
swiftTerm = "terminator"
swiftFor = "for"
swiftIn = "in"
swiftContentsOf = "contentsOf"
swiftWriteTo = "forWritingTo"
swiftSep = "separator"
swiftSepBy = "separatedBy"
swiftUnwrap = "!"
swiftContains = "contains"
swiftSet = "Set"

swiftUnaryMath :: (Monad r) => String -> VSOp r
swiftUnaryMath = addMathImport . unOpPrec

swiftNumBinExpr :: (CommonRenderSym r) => (SValue r -> SValue r -> SValue r) ->
  SValue r -> SValue r -> SValue r
swiftNumBinExpr f v1' v2' = do
  v1 <- v1'
  v2 <- v2'
  let exprT t1 t2 = if t1 == t2 then f (pure v1) (pure v2) else exprT' t1 t2
      exprT' Double _ = f (pure v1) (cast double $ pure v2)
      exprT' _ Double = f (cast double $ pure v1) (pure v2)
      exprT' Float _  = f (pure v1) (cast float $ pure v2)
      exprT' _ Float  = f (cast float $ pure v1) (pure v2)
      exprT' _ _      = f (pure v1) (pure v2)
  exprT (getType $ valueType v1) (getType $ valueType v2)

swiftLitFloat :: (CommonRenderSym r) => Float -> SValue r
swiftLitFloat = mkStateVal float . D.float

swiftLambda :: (CommonRenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
swiftLambda ps ex = braces $ parens (hicat listSep'
  (zipWith (\n t -> n <> swiftTypeSpec <+> t)
    (map RC.variable ps)
    (map (RC.type' . variableType) ps)))
  <+> swiftRetType' <+> RC.type' (valueType ex) <+> inLabel <+> RC.value ex

swiftReadableTypes :: [CodeType]
swiftReadableTypes = [Integer, Double, Float, Boolean, Char]

swiftCast :: (CommonRenderSym r) => VSType r -> SValue r -> SValue r
swiftCast t' v' = do
  t <- t'
  v <- v'
  let unwrap = if getType t `elem` swiftReadableTypes &&
        getType (valueType v) == String then swiftUnwrapVal else id
  unwrap $ mkStateVal (pure t) (R.castObj (RC.type' t) (RC.value v))

swiftIndexFunc :: (OORenderSym r) => SValue r -> SValue r -> SValue r
swiftIndexFunc l v' = do
  v <- v'
  let t = pure $ valueType v
      ofArg = var swiftOf t
  objMethodCallNamedArgs int l swiftIndex [(ofArg, pure v)]

swiftStrideFunc :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r
  -> SValue r
swiftStrideFunc beg end step = let t = listType int 
                                   fromArg = var swiftFrom int
                                   toArg = var swiftTo int
                                   byArg = var swiftBy int
  in cast t (funcAppNamedArgs swiftStride t 
    [(fromArg, beg), (toArg, end), (byArg, step)])

swiftMapFunc :: (OORenderSym r) => SValue r -> SValue r -> SValue r
swiftMapFunc lst f = objMethodCall (onStateValue valueType lst) lst swiftMap [f]

swiftListAddFunc :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r
swiftListAddFunc i v = let atArg = var swiftAt int
  in funcAppMixedArgs swiftListAdd (listType $ onStateValue valueType v) 
    [v] [(atArg, i)]

swiftWriteFunc :: (OORenderSym r) => SValue r -> SValue r -> SValue r
swiftWriteFunc v f = let contentsArg = var swiftContentsOf (obj swiftData)
  in swiftTryVal $ objMethodCallNamedArgs void f swiftWrite 
    [(contentsArg, newObj (obj swiftData) [v $. funcFromData (R.func swiftUTF8) 
    (obj swiftEncoding)])]

swiftReadLineFunc :: (CommonRenderSym r) => SValue r
swiftReadLineFunc = swiftUnwrapVal $ funcApp swiftReadLine string []

swiftReadFileFunc :: (CommonRenderSym r) => SValue r -> SValue r
swiftReadFileFunc v = let contentsArg = var swiftContentsOf infile
  in swiftTryVal $ funcAppNamedArgs CP.stringRender' string [(contentsArg, v)]

swiftSplitFunc :: (OORenderSym r) => Char -> SValue r -> SValue r
swiftSplitFunc d s = let sepArg = var swiftSepBy char
  in objMethodCallNamedArgs (listType string) s swiftSplit [(sepArg, litChar d)]

swiftJoinedFunc :: (OORenderSym r) => Char -> SValue r -> SValue r
swiftJoinedFunc d s = let sepArg = var swiftSep char
  in objMethodCallNamedArgs string s swiftJoined [(sepArg, litChar d)]

swiftIndexOf :: (OORenderSym r) => SValue r -> SValue r -> SValue r
swiftIndexOf = swiftUnwrapVal .: swiftIndexFunc

-- | Swift's syntactic sugar for list slicing.
swiftListSlice :: (OORenderSym r) => SVariable r -> SValue r ->
  Maybe (SValue r) -> Maybe (SValue r) -> SValue r -> MSBlock r
swiftListSlice vn vo beg end step = do

  vnew <- zoom lensMStoVS vn
  scpData <- getVarScope $ variableName vnew
  let scp = convScope scpData

  stepV <- zoom lensMStoVS step
  let mbStepV = valueInt stepV

  -- Generate fresh variable names if required
  begName <- genVarNameIf (isNothing beg && isNothing mbStepV) "begIdx"
  endName <- genVarNameIf (isNothing end && isNothing mbStepV) "endIdx"

  let (setBeg, begVal) = M.makeSetterVal begName step mbStepV beg (litInt 0)    (listSize vo #- litInt 1) scp
      (setEnd, endVal) = M.makeSetterVal endName step mbStepV end (listSize vo) (litInt (-1)) scp
      
      i = var "i" int
      setToSlice = vn &= swiftMapFunc (swiftStrideFunc begVal endVal step) (lambda [i] (listAccess vo (valueOf i)))
  block [
      setBeg,
      setEnd,
      setToSlice
    ]

swiftPrint :: Bool -> Maybe (SValue SwiftCode) -> SValue SwiftCode ->
  SValue SwiftCode -> MSStatement SwiftCode
swiftPrint newLn Nothing _ v = do
  let s = litString "" :: SValue SwiftCode
      nl = [(var swiftTerm string, s) | not newLn]
  valStmt $ funcAppMixedArgs printLabel void [v] nl
swiftPrint newLn (Just f) _ v' = do
  v <- zoom lensMStoVS v'
  let valToPrint (List _) = pure v $. funcFromData (R.func swiftDesc) string
      valToPrint String = pure v
      valToPrint _ = cast string (pure v)
      prNewLn = if newLn then valStmt (swiftWriteFunc (litString "\\n") f)
        else emptyStmt
  tryCatch (bodyStatements
    [valStmt $ swiftWriteFunc (valToPrint $ getType $ valueType v) f, prNewLn])
    (oneLiner $ throw "Error printing to file.")

-- swiftPrint can handle lists, so don't use G.print for lists.
swiftOut :: (CommonRenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r
  -> MSStatement r
swiftOut newLn f printFn v = zoom lensMStoVS v >>= swOut . getType . valueType
  where swOut (List _) = printSt newLn f printFn v
        swOut _ = G.print newLn f printFn v

swiftInput :: SVariable SwiftCode -> SValue SwiftCode -> SValue SwiftCode
swiftInput vr vl = do
  vr' <- vr
  let swiftInput' String = vl
      swiftInput' ct
        | ct `elem` swiftReadableTypes = cast (pure $ variableType vr') vl
        | otherwise = error "Attempt to read value of unreadable type"
  swiftInput' (getType $ variableType vr')

swiftOpenFile :: (OORenderSym r) => SValue r -> VSType r -> SValue r
swiftOpenFile n t = let forArg = var swiftFor (obj swiftSearchDir)
                        dirVal = mkStateVal (obj swiftSearchDir) swiftDocDir
                        inArg = var swiftIn (obj swiftPathMask)
                        maskVal = mkStateVal (obj swiftPathMask) swiftUserMask
  in objMethodCall t (swiftUnwrapVal $
    funcAppNamedArgs swiftUrls (listType t) [(forArg, dirVal), (inArg, maskVal)]
    $. funcFromData (R.func swiftFirst) t) swiftAppendPath [n]

swiftOpenFileHdl :: (OORenderSym r) => SValue r -> VSType r -> SValue r
swiftOpenFileHdl n t = let forWritingArg = var swiftWriteTo swiftFileType
  in swiftTryVal $ funcAppNamedArgs swiftFileHdl outfile 
    [(forWritingArg, swiftOpenFile n t)]

swiftOpenFileWA :: (OORenderSym r) => Bool -> SVariable r -> SValue r ->
  MSStatement r
swiftOpenFileWA app f' n' = tryCatch
    (bodyStatements [CP.openFileW (\f n _ -> swiftOpenFileHdl f n) f' n',
      if app
        then valStmt $ swiftTryVal $ objMethodCallNoParams void (valueOf f')
          swiftSeekEnd
        else emptyStmt])
    -- It's important for the catch case to throw, or else the swift compiler
    -- will have no guarantees that the file variable has been initialized.
    (oneLiner $ throw "Error opening file.")

swiftCloseFile :: (OORenderSym r) => SValue r -> MSStatement r
swiftCloseFile f' = do
  f <- zoom lensMStoVS f'
  -- How I've currently implemented file-reading, files don't need to be 
  -- "closed", so InFile case is (correctly) just an empty stmt
  let swClose InFile = modify resetIndices >> emptyStmt
      swClose OutFile = tryCatch (oneLiner $ valStmt $ swiftTryVal $
          objMethodCallNoParams void (pure f) swiftClose)
        (oneLiner $ throw "Error closing file.")
      swClose _ = error "closeFile called on non-file-typed value"
  swClose (getType $ valueType f)

swiftReadFile :: (OORenderSym r) => SVariable r -> SValue r -> MSStatement r
swiftReadFile v f = let l = var "l" string
  in tryCatch 
  (oneLiner $ v &= swiftMapFunc (swiftSplitFunc '\n' $ swiftReadFileFunc f) 
    (lambda [l] (swiftSplitFunc ' ' (valueOf l))))
  (oneLiner $ throw "Error reading from file.")

swiftVarDec :: Doc -> SVariable SwiftCode -> SwiftCode (Scope SwiftCode)
  -> MSStatement SwiftCode
swiftVarDec dec v' scp = do
  v <- zoom lensMStoVS v'
  modify $ useVarName (variableName v)
  modify $ setVarScope (variableName v) (scopeData scp)
  let bind Static = static :: SwiftCode (Permanence SwiftCode)
      bind Dynamic = dynamic :: SwiftCode (Permanence SwiftCode)
      p = bind $ variableBind v
  mkStmtNoEnd (RC.perm p <+> dec <+> RC.variable v <> swiftTypeSpec
    <+> RC.type' (variableType v))

swiftSetDec :: Doc -> SVariable SwiftCode -> MSStatement SwiftCode
swiftSetDec dec v' = do
  v <- zoom lensMStoVS v'
  modify $ useVarName (variableName v)
  let bind Static = static :: SwiftCode (Permanence SwiftCode)
      bind Dynamic = dynamic :: SwiftCode (Permanence SwiftCode)
      p = bind $ variableBind v
  mkStmtNoEnd (RC.perm p <+> dec <+> RC.variable v <> swiftTypeSpec
    <+> text (swiftSet ++ (replaceBrackets $ getTypeString (variableType v))))

replaceBrackets :: String -> String
replaceBrackets str = "<" ++ (init . tail) str ++ ">"

swiftThrowDoc :: (CommonRenderSym r) => r (Value r) -> Doc
swiftThrowDoc errMsg = throwLabel <+> RC.value errMsg

swiftForEach :: (CommonRenderSym r) => r (Variable r) -> r (Value r) -> r (Body r) -> Doc
swiftForEach i lstVar b = vcat [
  forLabel <+> RC.variable i <+> inLabel <+> RC.value lstVar <+> bodyStart,
  indent $ RC.body b,
  bodyEnd]

swiftTryCatch :: (CommonRenderSym r) => r (Body r) -> r (Body r) -> Doc
swiftTryCatch tb cb = vcat [
  swiftDo <+> lbrace,
  indent $ RC.body tb,
  rbrace <+> catchLabel <+> lbrace,
  indent $ RC.body cb,
  rbrace]

swiftAssert :: (CommonRenderSym r) => r (Value r) -> r (Value r) -> Doc
swiftAssert condition errorMessage = vcat [
  text "assert(" <+> RC.value condition <+> text "," <+> RC.value errorMessage <> text ")"
  ]

swiftParam :: (CommonRenderSym r) => Doc -> r (Variable r) -> Doc
swiftParam io v = swiftNoLabel <+> RC.variable v <> swiftTypeSpec <+> io
  <+> RC.type' (variableType v)

swiftMethod :: Label -> SwiftCode (Visibility SwiftCode) -> 
  SwiftCode (Permanence SwiftCode) -> MSMthdType SwiftCode -> 
  [MSParameter SwiftCode] -> MSBody SwiftCode -> SMethod SwiftCode
swiftMethod n s p t ps b = do
  tp <- t
  pms <- sequence ps
  bod <- b
  mem <- zoom lensMStoVS getMethodExcMap
  mn <- zoom lensMStoFS getModuleName
  let excs = findWithDefault [] (qualName mn n) mem
  mthdFromData Pub (vcat [
    RC.visibility s <+> RC.perm p <+> swiftFunc <+> text n <> 
      parens (parameterList pms) <+> emptyIfNull excs throwsLabel <+> 
      swiftRetType' <+> RC.type' tp <+> bodyStart,
    indent $ RC.body bod,
    bodyEnd])

swiftConstructor :: (OORenderSym r) => [MSParameter r] -> Initializers r ->
  MSBody r -> SMethod r
swiftConstructor ps is b = do
  pms <- sequence ps
  bod <- multiBody [G.initStmts is, b]
  mem <- zoom lensMStoVS getMethodExcMap
  mn <- zoom lensMStoFS getModuleName
  cn <- getClassName
  let excs = findWithDefault [] (qualName mn cn) mem
  mthdFromData Pub (vcat [
    swiftCtorName <> parens (parameterList pms) <+>
      emptyIfNull excs throwsLabel <+> bodyStart,
    indent $ RC.body bod,
    bodyEnd])

-- If the program uses throw, then generate code that extends Strings with the 
-- Error protocol. This line only needs to be generated once for the entire 
-- program
swiftStringError :: MS Doc
swiftStringError = do
  tu <- getThrowUsed
  errdef <- getErrorDefined
  str <- zoom lensMStoVS (string :: VSType SwiftCode)
  if tu && not errdef then do
    modify setErrorDefined
    pure (swiftExtension <+> RC.type' str <> swiftConforms <+> swiftError <+> bodyStart <> bodyEnd)
  else pure empty

swiftClassDoc :: ClassDocRenderer
swiftClassDoc desc = [desc | not (null desc)]

typeDfltVal :: (OORenderSym r) => CodeType -> SValue r
typeDfltVal Boolean = litFalse
typeDfltVal Integer = litInt 0
typeDfltVal Float = litFloat 0.0
typeDfltVal Double = litDouble 0.0
typeDfltVal Char = litChar ' '
typeDfltVal String = litString ""
typeDfltVal (List t) = litList (convTypeOO t) []
typeDfltVal (Array t) = litArray (convTypeOO t) []
typeDfltVal (Set t) = litSet (convTypeOO t) []
typeDfltVal _ = error "Attempt to get default value for type with none."
