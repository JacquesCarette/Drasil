{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python code is contained in this module
module Drasil.GOOL.LanguageRenderer.PythonRenderer (
  -- * Python Code Configuration -- defines syntax of all Python code
  PythonCode(..), pyName, pyVersion
) where

import Utils.Drasil (blank, indent)

import Drasil.GOOL.CodeType (CodeType(..))
import Drasil.GOOL.InterfaceCommon (SharedProg, Label, Library, VSType,
  VSFunction, SVariable, SValue, MSStatement, MixedCtorCall, BodySym(..),
  BlockSym(..), TypeSym(..), TypeElim(..), VariableSym(..), VisibilitySym(..),
  VariableElim(..), ValueSym(..), Argument(..), Literal(..), litZero,
  MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, extFuncApp, List(..), Set(..), InternalList(..),
  ThunkSym(..), VectorType(..), VectorDecl(..), VectorThunk(..),
  VectorExpression(..), ThunkAssign(..), StatementSym(..), AssignStatement(..),
  (&=), DeclStatement(..), IOStatement(..), StringStatement(..),
  FunctionSym(..), FuncAppStatement(..), CommentStatement(..),
  ControlStatement(..), switchAsIf, ScopeSym(..), ParameterSym(..),
  MethodSym(..))
import Drasil.GOOL.InterfaceGOOL (OOProg, ProgramSym(..), FileSym(..),
  ModuleSym(..), ClassSym(..), OOTypeSym(..), OOVariableSym(..),
  StateVarSym(..), PermanenceSym(..), OOValueSym, OOVariableValue,
  InternalValueExp(..), extNewObj, objMethodCall, OOFunctionSym(..), GetSet(..),
  OOValueExpression(..), selfFuncApp, OODeclStatement(..),
  OOFuncAppStatement(..), ObserverPattern(..), StrategyPattern(..),
  OOMethodSym(..))
import Drasil.GOOL.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  ImportElim, RenderBody(..), BodyElim, RenderBlock(..),
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
import qualified Drasil.GOOL.RendererClassesOO as RC (perm, stateVar, class',
  module')
import Drasil.GOOL.LanguageRenderer (classDec, dot, ifLabel, elseLabel, 
  forLabel, inLabel, whileLabel, tryLabel, importLabel, exceptionObj', listSep',
  argv, printLabel, listSep, piLabel, access, functionDox, variableList, 
  parameterList)
import qualified Drasil.GOOL.LanguageRenderer as R (sqrt, fabs, log10, 
  log, exp, sin, cos, tan, asin, acos, atan, floor, ceil, multiStmt, body, 
  classVar, listSetFunc, castObj, dynamic, break, continue, addComments, 
  commentedMod, commentedItem, var)
import Drasil.GOOL.LanguageRenderer.Constructors (mkStmtNoEnd, mkStateVal, 
  mkVal, mkStateVar, VSOp, unOpPrec, powerPrec, multPrec, andPrec, orPrec, inPrec, 
  unExpr, unExpr', typeUnExpr, binExpr, typeBinExpr, mkStaticVar)
import qualified Drasil.GOOL.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, listInnerType, obj, negateOp, csc, sec, cot,
  equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp,
  minusOp, multOp, divideOp, moduloOp, var, staticVar, objVar, arrayElem,
  litChar, litDouble, litInt, litString, valueOf, arg, argsList, objAccess,
  objMethodCall, call, funcAppMixedArgs, selfFuncAppMixedArgs, newObjMixedArgs,
  lambda, func, get, set, listAdd, listAppend, listAccess, listSet, getFunc,
  setFunc, listAppendFunc, stmt, loopStmt, emptyStmt, assign, subAssign,
  setFunc, listAppendFunc, stmt, loopStmt, emptyStmt, assign, subAssign,
  increment, objDecNew, print, closeFile, returnStmt, valStmt, comment, throw,
  ifCond, tryCatch, construct, param, method, getMethod, setMethod, function,
  buildClass, implementingClass, commentedClass, modFromData, fileDoc,
  fileFromData, local)
import qualified Drasil.GOOL.LanguageRenderer.CommonPseudoOO as CP (int,
  constructor, doxFunc, doxClass, doxMod, extVar, classVar, objVarSelf,
  extFuncAppMixedArgs, indexOf, contains, listAddFunc, discardFileLine, intClass, 
  funcType, buildModule, bindingError, notNull, listDecDef, destructorError, 
  stateVarDef, constVar, litArray, litSet, listSetFunc, extraClass, listAccessFunc, 
  multiAssign, multiReturn, listDec, funcDecDef, inOutCall, forLoopError, 
  mainBody, inOutFunc, docInOutFunc', listSize, setDecDef, setDec, intToIndex, indexToInt,
  varDecDef, openFileR', openFileW', openFileA', argExists, forEach', global)
import qualified Drasil.GOOL.LanguageRenderer.Macros as M (ifExists, 
  decrement1, increment1, runStrategy, stringListVals, stringListLists, 
  notifyObservers')
import Drasil.GOOL.AST (Terminator(..), FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateMod, MethodData(..), mthd,
  updateMthd, OpData(..), ParamData(..), pd, ProgData(..), progD, TypeData(..),
  td, ValData(..), vd, VarData(..), vard, CommonThunk, pureValue, vectorize,
  vectorize2, sumComponents, commonVecIndex, commonThunkElim, commonThunkDim,
  ScopeData)
import Drasil.GOOL.Helpers (vibcat, emptyIfEmpty, toCode, toState, onCodeValue,
  onStateValue, on2CodeValues, on2StateValues, onCodeList, onStateList,
  on2StateWrapped)
import Drasil.GOOL.State (MS, VS, lensGStoFS, lensMStoVS, lensVStoMS, revFiles,
  addLangImportVS, getLangImports, addLibImportVS, getLibImports, addModuleImport,
  addModuleImportVS, getModuleImports, setFileType, getClassName, setCurrMain,
  getClassMap, getMainDoc, genLoopIndex, varNameAvailable)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe)
import Control.Lens.Zoom (zoom)
import Control.Monad (join)
import Control.Monad.State (modify)
import Data.List (intercalate, sort)
import Data.Char (toUpper, isUpper, isLower)
import qualified Data.Map as Map (lookup)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
  vcat, colon, brackets, isEmpty, quotes, comma, braces)
import Drasil.GOOL.LanguageRenderer.LanguagePolymorphic (OptionalSpace(..))

pyExt :: String
pyExt = "py"

newtype PythonCode a = PC {unPC :: a}

instance Functor PythonCode where
  fmap f (PC x) = PC (f x)

instance Applicative PythonCode where
  pure = PC
  (PC f) <*> (PC x) = PC (f x)

instance Monad PythonCode where
  PC x >>= f = f x

instance SharedProg PythonCode
instance OOProg PythonCode

instance ProgramSym PythonCode where
  type Program PythonCode = ProgData 
  prog n st files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n st) fs

instance CommonRenderSym PythonCode
instance OORenderSym PythonCode

instance FileSym PythonCode where
  type File PythonCode = FileData
  fileDoc m = do
    modify (setFileType Combined)
    G.fileDoc pyExt top bottom m

  docMod = CP.doxMod pyExt

instance RenderFile PythonCode where
  top _ = toCode empty
  bottom = toCode empty
  
  commentedMod = on2StateValues (on2CodeValues R.commentedMod)

  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym PythonCode where
  type Import PythonCode = Doc
  langImport n = toCode $ importLabel <+> text n
  modImport = langImport

instance ImportElim PythonCode where
  import' = unPC

instance PermanenceSym PythonCode where
  type Permanence PythonCode = Doc
  static = toCode empty
  dynamic = toCode R.dynamic

instance PermElim PythonCode where
  perm = unPC
  binding = error $ CP.bindingError pyName

instance BodySym PythonCode where
  type Body PythonCode = Doc
  body = onStateList (onCodeList R.body)

  addComments s = onStateValue (onCodeValue (R.addComments s pyCommentStart))

instance RenderBody PythonCode where
  multiBody = G.multiBody 

instance BodyElim PythonCode where
  body = unPC

instance BlockSym PythonCode where
  type Block PythonCode = Doc
  block = G.block

instance RenderBlock PythonCode where
  multiBlock = G.multiBlock

instance BlockElim PythonCode where
  block = unPC

instance TypeSym PythonCode where
  type Type PythonCode = TypeData
  bool = typeFromData Boolean "" empty
  int = CP.int
  float = error pyFloatError
  double = typeFromData Double pyDouble (text pyDouble)
  char = typeFromData Char "" empty
  string = pyStringType
  infile = typeFromData InFile "" empty
  outfile = typeFromData OutFile "" empty
  listType t' = t' >>=(\t -> typeFromData (List (getType t)) "" empty)
  arrayType = listType
  listInnerType = G.listInnerType
  funcType = CP.funcType
  void = typeFromData Void pyVoid (text pyVoid)

instance OOTypeSym PythonCode where
  obj = G.obj

instance TypeElim PythonCode where
  getType = cType . unPC
  getTypeString = typeString . unPC

instance RenderType PythonCode where
  multiType _ = typeFromData Void "" empty
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim PythonCode where
  type' = typeDoc . unPC

instance UnaryOpSym PythonCode where
  type UnaryOp PythonCode = OpData
  notOp = pyNotOp
  negateOp = G.negateOp
  sqrtOp = pySqrtOp
  absOp = pyAbsOp
  logOp = pyLogOp
  lnOp = pyLnOp
  expOp = pyExpOp
  sinOp = pySinOp
  cosOp = pyCosOp
  tanOp = pyTanOp
  asinOp = pyAsinOp
  acosOp = pyAcosOp
  atanOp = pyAtanOp
  floorOp = pyFloorOp 
  ceilOp = pyCeilOp

instance BinaryOpSym PythonCode where
  type BinaryOp PythonCode = OpData
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
  powerOp = powerPrec pyPower
  moduloOp = G.moduloOp
  andOp = andPrec pyAnd
  orOp = orPrec pyOr
  inOp = inPrec pyIn

instance OpElim PythonCode where
  uOp = opDoc . unPC
  bOp = opDoc . unPC
  uOpPrec = opPrec . unPC
  bOpPrec = opPrec . unPC

instance ScopeSym PythonCode where
  type Scope PythonCode = ScopeData
  global = CP.global
  mainFn = global
  local = G.local

instance ScopeElim PythonCode where
  scopeData = unPC

instance VariableSym PythonCode where
  type Variable PythonCode = VarData
  var          = G.var
  constant n   = var $ toConstName n
  extVar l n t = modify (addModuleImportVS l) >> CP.extVar l n t
  arrayElem i  = G.arrayElem (litInt i)

instance OOVariableSym PythonCode where
  staticVar' c n t = if c then mkStaticVar n t (R.var (toConstName n))
                          else G.staticVar n t
  self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar pySelf (obj l) (text pySelf))
  classVar = CP.classVar R.classVar
  extClassVar c v = join $ on2StateValues (\t cm -> maybe id ((>>) . modify . 
    addModuleImportVS) (Map.lookup (getTypeString t) cm) $ 
    CP.classVar pyClassVar (toState t) v) c getClassMap
  objVar = G.objVar
  objVarSelf = CP.objVarSelf

instance VariableElim PythonCode where
  variableName = varName . unPC
  variableType = onCodeValue varType

instance InternalVarElim PythonCode where
  variableBind = varBind . unPC
  variable = varDoc . unPC

instance RenderVariable PythonCode where
  varFromData b n t' d = do 
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym PythonCode where
  type Value PythonCode = ValData
  valueType = onCodeValue valType

instance OOValueSym PythonCode

instance Argument PythonCode where
  pointerArg = id

instance Literal PythonCode where
  litTrue = mkStateVal bool pyTrue
  litFalse = mkStateVal bool pyFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = error pyFloatError
  litInt = G.litInt
  litString = G.litString
  litArray = CP.litArray brackets
  litSet = CP.litSet braces
  litList = litArray

instance MathConstant PythonCode where
  pi = addmathImport $ mkStateVal double pyPi

instance VariableValue PythonCode where
  valueOf = G.valueOf

instance OOVariableValue PythonCode

instance CommandLineArgs PythonCode where
  arg n = G.arg (litInt $ n+1) argsList
  argsList = do
    modify (addLangImportVS pySys)
    G.argsList $ pySys `access` argv
  argExists = CP.argExists

instance NumericExpression PythonCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) v1' v2' = do
    v1 <- v1'
    v2 <- v2'
    let pyDivision Integer Integer = binExpr (multPrec pyIntDiv)
        pyDivision _ _ = binExpr divideOp
    pyDivision (getType $ valueType v1) (getType $ valueType v2) (pure v1) 
      (pure v2)
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

instance BooleanExpression PythonCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool
  isin = typeBinExpr inOp bool

instance Comparison PythonCode where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression PythonCode where
  inlineIf = pyInlineIf

  funcAppMixedArgs = G.funcAppMixedArgs
  extFuncAppMixedArgs l n t ps ns = do
    modify (addModuleImportVS l)
    CP.extFuncAppMixedArgs l n t ps ns
  libFuncAppMixedArgs l n t ps ns = do
    modify (addLibImportVS l)
    CP.extFuncAppMixedArgs l n t ps ns

  lambda = G.lambda pyLambda

  notNull = CP.notNull pyNull

instance OOValueExpression PythonCode where
  selfFuncAppMixedArgs = G.selfFuncAppMixedArgs dot self
  newObjMixedArgs = G.newObjMixedArgs ""
  extNewObjMixedArgs l tp ps ns = do
    modify (addModuleImportVS l)
    pyExtNewObjMixedArgs l tp ps ns
  libNewObjMixedArgs l tp ps ns = do
    modify (addLibImportVS l)
    pyExtNewObjMixedArgs l tp ps ns

instance RenderValue PythonCode where
  inputFunc = mkStateVal string pyInputFunc
  printFunc = mkStateVal void pyPrintFunc
  printLnFunc = mkStateVal void empty
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty
  
  cast = on2StateWrapped (\t v-> mkVal t . R.castObj (RC.type' t) 
    $ RC.value v)
  
  call = G.call pyNamedArgSep

  valFromData p i t' d = do 
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim PythonCode where
  valuePrec = valPrec . unPC
  valueInt = valInt . unPC
  value = val . unPC

instance InternalValueExp PythonCode where
  objMethodCallMixedArgs' = G.objMethodCall

instance FunctionSym PythonCode where
  type Function PythonCode = FuncData

instance OOFunctionSym PythonCode where
  func = G.func
  objAccess = G.objAccess

instance GetSet PythonCode where
  get = G.get
  set = G.set

instance List PythonCode where
  intToIndex = CP.intToIndex
  indexToInt = CP.indexToInt
  listSize = CP.listSize
  listAdd = G.listAdd
  listAppend = G.listAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = CP.indexOf pyIndex

instance Set PythonCode where
  --fromList =
  contains = CP.contains pyIn

instance InternalList PythonCode where
  listSlice' b e s vn vo = pyListSlice vn vo (getVal b) (getVal e) (getVal s)
    where getVal = fromMaybe (mkStateVal void empty)

instance InternalGetSet PythonCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

instance InternalListFunc PythonCode where
  listSizeFunc l = do
    f <- funcApp pyListSize int [l]
    funcFromData (RC.value f) int
  listAddFunc _ = CP.listAddFunc pyInsert
  listAppendFunc _ = G.listAppendFunc pyAppendFunc
  listAccessFunc = CP.listAccessFunc
  listSetFunc = CP.listSetFunc R.listSetFunc

instance ThunkSym PythonCode where
  type Thunk PythonCode = CommonThunk VS

instance ThunkAssign PythonCode where
  thunkAssign v t = do
    iName <- genLoopIndex
    let
      i = var iName int
      dim = fmap pure $ t >>= commonThunkDim (fmap unPC . listSize . fmap pure) . unPC
      loopInit = zoom lensMStoVS (fmap unPC t) >>= commonThunkElim
        (const emptyStmt) (const $ assign v $ litZero $ fmap variableType v)
      loopBody = zoom lensMStoVS (fmap unPC t) >>= commonThunkElim
        (valStmt . listSet (valueOf v) (valueOf i) . vecIndex (valueOf i) . pure . pure)
        ((v &+=) . vecIndex (valueOf i) . pure . pure)
    multi [loopInit,
      forRange i (litInt 0) dim (litInt 1) $ body [block [loopBody]]]

instance VectorType PythonCode where
  vecType = listType

instance VectorDecl PythonCode where
  vecDec = listDec
  vecDecDef = listDecDef

instance VectorThunk PythonCode where
  vecThunk = pure . pure . pureValue . fmap unPC . valueOf

instance VectorExpression PythonCode where
  vecScale k = fmap $ fmap $ vectorize (fmap unPC . (k #*) . fmap pure)
  vecAdd = liftA2 $ liftA2 $ vectorize2 (\v1 v2 -> fmap unPC $ fmap pure v1 #+ fmap pure v2)
  vecIndex i = (>>= fmap pure . commonVecIndex (fmap unPC . flip listAccess i . fmap pure) . unPC)
  vecDot = liftA2 $ liftA2 $ fmap sumComponents <$> vectorize2 (\v1 v2 -> fmap unPC $ fmap pure v1 #* fmap pure v2)

instance RenderFunction PythonCode where
  funcFromData d = onStateValue (onCodeValue (`fd` d))
  
instance FunctionElim PythonCode where
  functionType = onCodeValue fType
  function = funcDoc . unPC

instance InternalAssignStmt PythonCode where
  multiAssign = CP.multiAssign id

instance InternalIOStmt PythonCode where
  printSt = pyPrint

instance InternalControlStmt PythonCode where
  multiReturn = CP.multiReturn id

instance RenderStatement PythonCode where
  stmt = G.stmt
  loopStmt = G.loopStmt
  
  emptyStmt = G.emptyStmt

  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim PythonCode where
  statement = fst . unPC
  statementTerm = snd . unPC

instance StatementSym PythonCode where
  -- Terminator determines how statements end
  type Statement PythonCode = (Doc, Terminator)
  valStmt = G.valStmt Empty
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement PythonCode where
  assign = G.assign Empty
  (&-=) = G.subAssign Empty
  (&+=) = G.increment
  (&++) = M.increment1
  (&--) = M.decrement1

instance DeclStatement PythonCode where
  varDec v scp = CP.varDecDef v scp Nothing
  varDecDef v scp e = CP.varDecDef v scp (Just e)
  listDec _ = CP.listDec
  setDecDef = CP.setDecDef
  listDecDef = CP.listDecDef
  arrayDec = listDec
  arrayDecDef = listDecDef
  constDecDef v scp e = do
    v' <- zoom lensMStoVS v
    let n = toConstName $ variableName v'
        newConst = constant n (pure (variableType v'))
    available <- varNameAvailable n
    if available
      then varDecDef newConst scp e
      else error "Cannot safely capitalize constant."
  funcDecDef = CP.funcDecDef

instance OODeclStatement PythonCode where
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew lib v scp vs = do
    modify (addModuleImport lib)
    varDecDef v scp (extNewObj lib (onStateValue variableType v) vs)

instance IOStatement PythonCode where
  print      = pyOut False Nothing printFunc
  printLn    = pyOut True  Nothing printFunc
  printStr   = print   . litString
  printStrLn = printLn . litString

  printFile f      = pyOut False (Just f) printFunc
  printFileLn f    = pyOut True  (Just f) printFunc
  printFileStr f   = printFile f   . litString
  printFileStrLn f = printFileLn f . litString

  getInput = pyInput inputFunc
  discardInput = valStmt inputFunc
  getFileInput f = pyInput (readline f)
  discardFileInput f = valStmt (readline f)

  openFileR f n = f &= CP.openFileR' n
  openFileW f n = f &= CP.openFileW' n
  openFileA f n = f &= CP.openFileA' n
  closeFile = G.closeFile pyClose

  getFileInputLine = getFileInput
  discardFileLine = CP.discardFileLine pyReadline
  getFileInputAll f v = v &= readlines f
  
instance StringStatement PythonCode where
  stringSplit d vnew s = assign vnew (objAccess s (splitFunc d))

  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FuncAppStatement PythonCode where
  inOutCall = CP.inOutCall funcApp
  extInOutCall m = CP.inOutCall (extFuncApp m)

instance OOFuncAppStatement PythonCode where
  selfInOutCall = CP.inOutCall selfFuncApp

instance CommentStatement PythonCode where
  comment = G.comment pyCommentStart

instance ControlStatement PythonCode where
  break = mkStmtNoEnd R.break
  continue = mkStmtNoEnd R.continue

  returnStmt = G.returnStmt Empty

  throw = G.throw pyThrow Empty

  ifCond = G.ifCond id pyBodyStart pySpace pyElseIf pyBodyEnd empty
  switch = switchAsIf

  ifExists = M.ifExists

  for _ _ _ _ = error $ CP.forLoopError pyName
  forRange i initv finalv stepv = forEach i (range initv finalv stepv)
  forEach = CP.forEach' pyForEach
  while v' b' = do 
    v <- zoom lensMStoVS v'
    b <- b'
    mkStmtNoEnd (pyWhile v b)

  tryCatch = G.tryCatch pyTryCatch

  assert condition errorMessage = do
      cond <- zoom lensMStoVS condition
      errMsg <- zoom lensMStoVS errorMessage
      mkStmtNoEnd (pyAssert cond errMsg)

instance ObserverPattern PythonCode where
  notifyObservers = M.notifyObservers'

instance StrategyPattern PythonCode where
  runStrategy = M.runStrategy

instance VisibilitySym PythonCode where
  type Visibility PythonCode = Doc
  private = toCode empty
  public = toCode empty

instance RenderVisibility PythonCode where
  visibilityFromData _ = toCode

instance VisibilityElim PythonCode where
  visibility = unPC

instance MethodTypeSym PythonCode where
  type MethodType PythonCode = TypeData
  mType = zoom lensMStoVS
  
instance OOMethodTypeSym PythonCode where
  construct = G.construct

instance ParameterSym PythonCode where
  type Parameter PythonCode = ParamData
  param = G.param RC.variable
  pointerParam = param

instance RenderParam PythonCode where
  paramFromData v' d = do 
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)
  
instance ParamElim PythonCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unPC

instance MethodSym PythonCode where
  type Method PythonCode = MethodData
  docMain = mainFunction
  function = G.function
  mainFunction = CP.mainBody
  docFunc = CP.doxFunc

  inOutFunc n s = CP.inOutFunc (function n s)
  docInOutFunc n s = CP.docInOutFunc' functionDox (inOutFunc n s)

instance OOMethodSym PythonCode where
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor = CP.constructor initName

  inOutMethod n s p = CP.inOutFunc (method n s p)
  docInOutMethod n s p = CP.docInOutFunc' functionDox (inOutMethod n s p)

instance RenderMethod PythonCode where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m 
    (onStateValue (onCodeValue R.commentedItem) cmt)
    
  mthdFromData _ d = toState $ toCode $ mthd d

instance OORenderMethod PythonCode where
  intMethod m n _ _ _ ps b = do
    modify (if m then setCurrMain else id)
    sl <- zoom lensMStoVS self
    pms <- sequence ps
    toCode . mthd . pyMethod n sl pms <$> b
  intFunc m n _ _ _ ps b = do
    modify (if m then setCurrMain else id)
    bd <- b
    pms <- sequence ps
    pure $ toCode $ mthd $ pyFunction n pms bd
  destructor _ = error $ CP.destructorError pyName
  
instance MethodElim PythonCode where
  method = mthdDoc . unPC

instance StateVarSym PythonCode where
  type StateVar PythonCode = Doc
  stateVar _ _ _ = toState (toCode empty)
  stateVarDef = CP.stateVarDef
  constVar = CP.constVar (RC.perm 
    (static :: PythonCode (Permanence PythonCode)))
  
instance StateVarElim PythonCode where
  stateVar = unPC

instance ClassSym PythonCode where
  type Class PythonCode = Doc
  buildClass par sVars cstrs = if length cstrs <= 1 
                                  then G.buildClass par sVars cstrs
                                  else error pyMultCstrsError
  extraClass n par sVars cstrs = if 
                                  length cstrs <= 1
                                    then CP.extraClass n par sVars cstrs
                                    else error pyMultCstrsError
  implementingClass n iNms sVars cstrs = if 
                                  length cstrs <= 1
                                    then G.implementingClass n iNms sVars cstrs
                                    else error pyMultCstrsError

  docClass = CP.doxClass

instance RenderClass PythonCode where
  intClass = CP.intClass pyClass

  inherit n = toCode $ maybe empty (parens . text) n
  implements is = toCode $ parens (text $ intercalate listSep is)

  commentedClass = G.commentedClass
  
instance ClassElim PythonCode where
  class' = unPC

instance ModuleSym PythonCode where
  type Module PythonCode = ModData
  buildModule n is = CP.buildModule n (do
    lis <- getLangImports
    libis <- getLibImports
    mis <- getModuleImports
    pure $ vibcat [
      vcat (map (RC.import' . 
        (langImport :: Label -> PythonCode (Import PythonCode))) lis),
      vcat (map (RC.import' . 
        (langImport :: Label -> PythonCode (Import PythonCode))) (sort $ is ++ 
        libis)),
      vcat (map (RC.import' . 
        (modImport :: Label -> PythonCode (Import PythonCode))) mis)]) 
    (pure empty) getMainDoc

instance RenderMod PythonCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)
  
instance ModuleElim PythonCode where
  module' = modDoc . unPC

instance BlockCommentSym PythonCode where
  type BlockComment PythonCode = Doc
  blockComment lns = toCode $ pyBlockComment lns pyCommentStart
  docComment = onStateValue (\lns -> toCode $ pyDocComment lns pyDocCommentStart
    pyCommentStart)

instance BlockCommentElim PythonCode where
  blockComment' = unPC

-- convenience
initName :: Label
initName = "__init__"

pyName, pyVersion :: String
pyName = "Python"
pyVersion = "3.5.1"

pyInt, pyDouble, pyString, pyVoid :: String
pyInt = "int"
pyDouble = "float"
pyString = "str"
pyVoid = "NoneType"

pyFloatError :: String
pyFloatError = "Floats unavailable in Python, use Doubles instead"

pyPower, pyAnd, pyOr, pyIntDiv :: String
pyPower = "**"
pyAnd = "and"
pyOr = "or"
pyIntDiv = "//"

pySelf, pyNull :: String
pySelf = "self"
pyNull = "None"

pyNull' :: Doc
pyNull' = text pyNull

pyTrue, pyFalse :: Doc
pyTrue = text "True"
pyFalse = text "False"

pyPi :: Doc
pyPi = text $ pyMath `access` piLabel

pySys :: String
pySys = "sys"

pyInputFunc, pyPrintFunc :: Doc
pyInputFunc = text "input()" -- raw_input() for < Python 3.0
pyPrintFunc = text printLabel

pyListSize, pyIndex, pyInsert, pyAppendFunc, pyReadline, pyReadlines, pyClose, 
  pySplit, pyRange, pyRstrip, pyMath :: String
pyListSize = "len"
pyIndex = "index"
pyInsert = "insert"
pyAppendFunc = "append"
pyReadline = "readline"
pyReadlines = "readlines"
pyClose = "close"
pySplit = "split"
pyRange = "range"
pyRstrip = "rstrip"
pyMath = "math"
pyIn = "__contains__"
pySetAdd = "Add"

pyDef, pyLambdaDec, pyElseIf, pyRaise, pyExcept :: Doc
pyDef = text "def"
pyLambdaDec = text "lambda"
pyElseIf = text "elif"
pyRaise = text "raise"
pyExcept = text "except"

pyBodyStart, pyBodyEnd, pyCommentStart, pyDocCommentStart, pyNamedArgSep :: Doc
pyBodyStart = colon
pyBodyEnd = empty
pyCommentStart = text "#"
pyDocCommentStart = pyCommentStart <> pyCommentStart
pyNamedArgSep = equals

pySpace :: OptionalSpace
pySpace = OSpace {oSpace = empty}

pyNotOp :: (Monad r) => VSOp r
pyNotOp = unOpPrec "not"

pySqrtOp :: (Monad r) => VSOp r
pySqrtOp = mathFunc R.sqrt

pyAbsOp :: (Monad r) => VSOp r
pyAbsOp = mathFunc R.fabs

pyLogOp :: (Monad r) => VSOp r
pyLogOp = mathFunc R.log10

pyLnOp :: (Monad r) => VSOp r
pyLnOp = mathFunc R.log

pyExpOp :: (Monad r) => VSOp r
pyExpOp = mathFunc R.exp

pySinOp :: (Monad r) => VSOp r
pySinOp = mathFunc R.sin

pyCosOp :: (Monad r) => VSOp r
pyCosOp = mathFunc R.cos

pyTanOp :: (Monad r) => VSOp r
pyTanOp = mathFunc R.tan

pyAsinOp :: (Monad r) => VSOp r
pyAsinOp = mathFunc R.asin

pyAcosOp :: (Monad r) => VSOp r
pyAcosOp = mathFunc R.acos

pyAtanOp :: (Monad r) => VSOp r
pyAtanOp = mathFunc R.atan

pyFloorOp :: (Monad r) => VSOp r
pyFloorOp = mathFunc R.floor

pyCeilOp :: (Monad r) => VSOp r
pyCeilOp = mathFunc R.ceil

addmathImport :: VS a -> VS a
addmathImport = (>>) $ modify (addLangImportVS pyMath)

mathFunc :: (Monad r) => String -> VSOp r
mathFunc = addmathImport . unOpPrec . access pyMath 

splitFunc :: (OORenderSym r) => Char -> VSFunction r
splitFunc d = func pySplit (listType string) [litString [d]]

readline, readlines :: (OORenderSym r) => SValue r -> SValue r
readline f = objMethodCall string f pyReadline []
readlines f = objMethodCall (listType string) f pyReadlines []

readInt, readDouble, readString :: (OORenderSym r) => SValue r -> SValue r
readInt inSrc = funcApp pyInt int [inSrc]
readDouble inSrc = funcApp pyDouble double [inSrc]
readString inSrc = objMethodCall string inSrc pyRstrip []

range :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
range initv finalv stepv = funcApp pyRange (listType int) [initv, finalv, stepv]

pyClassVar :: Doc -> Doc -> Doc
pyClassVar c v = c <> dot <> c <> dot <> v

pyInlineIf :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
pyInlineIf c' v1' v2' = do 
  c <- c'
  v1 <- v1'
  v2 <- v2'
  valFromData (valuePrec c) (valueInt c) (toState $ valueType v1) 
    (RC.value v1 <+> ifLabel <+> RC.value c <+> elseLabel <+> RC.value v2)

pyLambda :: (CommonRenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
pyLambda ps ex = pyLambdaDec <+> variableList ps <> colon <+> RC.value ex

pyStringType :: (CommonRenderSym r) => VSType r
pyStringType = typeFromData String pyString (text pyString)

pyExtNewObjMixedArgs :: (CommonRenderSym r) => Library -> MixedCtorCall r
pyExtNewObjMixedArgs l tp vs ns = tp >>= (\t -> call (Just l) Nothing 
  (getTypeString t) (pure t) vs ns)

pyPrint :: Bool -> Maybe (SValue PythonCode) -> SValue PythonCode -> 
  SValue PythonCode -> MSStatement PythonCode
pyPrint newLn f' p' v' = do
    f <- zoom lensMStoVS $ fromMaybe (mkStateVal void empty) f'
    prf <- zoom lensMStoVS p'
    v <- zoom lensMStoVS v'
    s <- zoom lensMStoVS (litString "" :: SValue PythonCode)
    let nl = if newLn then empty else listSep' <> text "end" <> equals <> 
               RC.value s
        fl = emptyIfEmpty (RC.value f) $ listSep' <> text "file" <> equals 
               <> RC.value f
    mkStmtNoEnd $ RC.value prf <> parens (RC.value v <> nl <> fl)

pyOut :: (CommonRenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r -> 
  MSStatement r
pyOut newLn f printFn v = zoom lensMStoVS v >>= pyOut' . getType . valueType
  where pyOut' (List _) = printSt newLn f printFn v
        pyOut' _ = G.print newLn f printFn v

pyInput :: SValue PythonCode -> SVariable PythonCode -> MSStatement PythonCode
pyInput inSrc v = v &= (v >>= pyInput' . getType . variableType)
  where pyInput' Integer = readInt inSrc
        pyInput' Float = readDouble inSrc
        pyInput' Double = readDouble inSrc
        pyInput' Boolean = inSrc ?!= litString "0"
        pyInput' String = readString inSrc
        pyInput' Char = inSrc
        pyInput' _ = error "Attempt to read a value of unreadable type"

pyThrow :: (CommonRenderSym r) => r (Value r) -> Doc
pyThrow errMsg = pyRaise <+> exceptionObj' <> parens (RC.value errMsg)

pyForEach :: (CommonRenderSym r) => r (Variable r) -> r (Value r) -> r (Body r) -> Doc
pyForEach i lstVar b = vcat [
  forLabel <+> RC.variable i <+> inLabel <+> RC.value lstVar <> colon,
  indent $ RC.body b]

pyWhile :: (CommonRenderSym r) => r (Value r) -> r (Body r) -> Doc
pyWhile v b = vcat [
  whileLabel <+> RC.value v <> colon,
  indent $ RC.body b]

pyTryCatch :: (CommonRenderSym r) => r (Body r) -> r (Body r) -> Doc
pyTryCatch tryB catchB = vcat [
  tryLabel <> colon,
  indent $ RC.body tryB,
  pyExcept <+> exceptionObj' <> colon,
  indent $ RC.body catchB]

pyAssert :: (CommonRenderSym r) => r (Value r) -> r (Value r) -> Doc
pyAssert condition message = text "assert" <+> RC.value condition <> comma <+> RC.value message

pyListSlice :: (CommonRenderSym r, Monad r) => SVariable r -> SValue r -> SValue r -> 
  SValue r -> SValue r -> MS (r Doc)
pyListSlice vn vo beg end step = zoom lensMStoVS $ do
  vnew <- vn
  vold <- vo
  b <- beg
  e <- end
  s <- step
  pure $ toCode $ RC.variable vnew <+> equals <+> RC.value vold <> 
    brackets (RC.value b <> colon <> RC.value e <> colon <> RC.value s)

pyMethod :: (CommonRenderSym r) => Label -> r (Variable r) -> [r (Parameter r)] ->
  r (Body r) -> Doc
pyMethod n slf ps b = vcat [
  pyDef <+> text n <> parens (RC.variable slf <> oneParam <> pms) <> colon,
  indent bodyD]
      where pms = parameterList ps
            oneParam = emptyIfEmpty pms listSep'
            bodyD | isEmpty (RC.body b) = pyNull'
                  | otherwise = RC.body b

pyFunction :: (CommonRenderSym r) => Label -> [r (Parameter r)] -> r (Body r) -> Doc
pyFunction n ps b = vcat [
  pyDef <+> text n <> parens (parameterList ps) <> colon,
  indent bodyD]
  where bodyD | isEmpty (RC.body b) = pyNull'
              | otherwise = RC.body b

pyClass :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
pyClass n pn s vs fs = vcat [
  s <+> classDec <+> text n <> pn <> colon,
  indent funcSec]
  where funcSec | isEmpty (vs <> fs) = pyNull'
                | isEmpty vs = fs
                | isEmpty fs = vs
                | otherwise = vcat [vs, blank, fs]

pyMultCstrsError :: String
pyMultCstrsError = "Python classes cannot have multiple constructors"

pyBlockComment :: [String] -> Doc -> Doc
pyBlockComment lns cmt = vcat $ map ((<+>) cmt . text) lns

pyDocComment :: [String] -> Doc -> Doc -> Doc
pyDocComment [] _ _ = empty
pyDocComment (l:lns) start mid = vcat $ start <+> text l : map ((<+>) mid . 
  text) lns

toConstName :: String -> String
toConstName (s:s':ss) = if isLower s && isUpper s'
                          then toUpper s : '_' : s' : toConstName ss
                          else toUpper s : toConstName (s' : ss)
toConstName (s:ss)    = toUpper s : toConstName ss
toConstName ""        = ""