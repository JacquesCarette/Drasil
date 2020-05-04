{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python code is contained in this module
module GOOL.Drasil.LanguageRenderer.PythonRenderer (
  -- * Python Code Configuration -- defines syntax of all Python code
  PythonCode(..)
) where

import Utils.Drasil (blank, indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, Library, MSBody, VSType, SVariable, 
  SValue, VSFunction, MSStatement, MSParameter, SMethod, MixedCtorCall, OOProg, 
  ProgramSym(..), FileSym(..), PermanenceSym(..), BodySym(..), bodyStatements, 
  oneLiner, BlockSym(..), TypeSym(..), TypeElim(..), ControlBlock(..), 
  VariableSym(..), VariableElim(..), listOf, ValueSym(..), Literal(..), 
  MathConstant(..), VariableValue(..), CommandLineArgs(..), 
  NumericExpression(..), BooleanExpression(..), Comparison(..), 
  ValueExpression(..), funcApp, selfFuncApp, extFuncApp, extNewObj, 
  InternalValueExp(..), objMethodCall, objMethodCallNoParams, FunctionSym(..), 
  ($.), GetSet(..), List(..), InternalList(..), at, Iterator(..), 
  StatementSym(..), AssignStatement(..), (&=), DeclStatement(..), 
  IOStatement(..), StringStatement(..), FuncAppStatement(..), 
  CommentStatement(..), observerListName, ControlStatement(..), switchAsIf, 
  StatePattern(..), ObserverPattern(..), StrategyPattern(..), ScopeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..),
  ODEInfo(..), ODEOptions(..), ODEMethod(..))
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(..), ImportSym(..), 
  ImportElim, PermElim(binding), RenderBody(..), BodyElim, RenderBlock(..), 
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..), 
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind), 
  RenderValue(..), ValueElim(valuePrec), InternalGetSet(..), 
  InternalListFunc(..), InternalIterator(..), RenderFunction(..), 
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..), 
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm), 
  RenderScope(..), ScopeElim, MethodTypeSym(..), RenderParam(..), 
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim, 
  StateVarElim, RenderClass(..), ClassElim, RenderMod(..), ModuleElim, 
  BlockCommentSym(..), BlockCommentElim)
import qualified GOOL.Drasil.RendererClasses as RC (import', perm, body, block, 
  type', uOp, bOp, variable, value, function, statement, scope, parameter,
  method, stateVar, class', module', blockComment')
import GOOL.Drasil.LanguageRenderer (classDec, dot, ifLabel, elseLabel, 
  forLabel, inLabel, whileLabel, tryLabel, argv, exceptionObj, listSep, access, 
  valueList, variableList, parameterList, surroundBody)
import qualified GOOL.Drasil.LanguageRenderer as R (multiStmt, body, 
  multiAssign, return', classVar, listSetFunc, castObj, dynamic, break, 
  continue, addComments, commentedMod, commentedItem)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmtNoEnd, mkStateVal, 
  mkVal, mkStateVar, VSOp, unOpPrec, powerPrec, multPrec, andPrec, orPrec, 
  unExpr, unExpr', typeUnExpr, binExpr, typeBinExpr)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, int, listInnerType, obj, funcType, negateOp, 
  csc, sec, cot, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, 
  lessEqualOp, plusOp, minusOp, multOp, divideOp, moduloOp, var, staticVar,
  arrayElem, litChar, litDouble, litInt, litString, valueOf, arg, argsList, 
  objAccess, objMethodCall, call, funcAppMixedArgs, selfFuncAppMixedArgs, 
  newObjMixedArgs, lambda, func, get, set, listAdd, listAppend, iterBegin, 
  iterEnd, listAccess, listSet, getFunc, setFunc, listAppendFunc, stmt, 
  loopStmt, emptyStmt, assign, increment, objDecNew, print, closeFile, 
  returnStmt, valStmt, comment, throw, ifCond, tryCatch, construct, param, 
  method, getMethod, setMethod, constructor, function, docFunc, buildClass, 
  implementingClass, docClass, commentedClass, modFromData, fileDoc, docMod, 
  fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (docFuncRepr)
import qualified GOOL.Drasil.LanguageRenderer.CommonPseudoOO as CP (
  bindingError, extVar, classVar, objVarSelf, iterVar, extFuncAppMixedArgs, 
  indexOf, listAddFunc,  iterBeginError, iterEndError, listDecDef, 
  discardFileLine, destructorError, stateVarDef, constVar, intClass, objVar, 
  listSetFunc, listAccessFunc, buildModule)
import qualified GOOL.Drasil.LanguageRenderer.Macros as M (ifExists, decrement, 
  decrement1, increment1, runStrategy, stringListVals, stringListLists,
  observerIndex, checkState)
import GOOL.Drasil.AST (Terminator(..), FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateMod, MethodData(..), mthd, 
  updateMthd, OpData(..), ParamData(..), pd, ProgData(..), progD, TypeData(..), 
  td, ValData(..), vd, VarData(..), vard)
import GOOL.Drasil.Helpers (vibcat, emptyIfEmpty, toCode, toState, onCodeValue,
  onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues,
  onCodeList, onStateList, on2StateLists)
import GOOL.Drasil.State (MS, VS, lensGStoFS, lensMStoVS, lensVStoMS, revFiles,
  addLangImportVS, getLangImports, addLibImport, addLibImportVS, getLibImports, 
  addModuleImport, addModuleImportVS, getModuleImports, setFileType, 
  getClassName, setCurrMain, getClassMap, setMainDoc, getMainDoc)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (modify)
import Data.List (intercalate, sort)
import qualified Data.Map as Map (lookup)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
  vcat, colon, brackets, isEmpty)

pyExt :: String
pyExt = "py"

newtype PythonCode a = PC {unPC :: a}

instance Functor PythonCode where
  fmap f (PC x) = PC (f x)

instance Applicative PythonCode where
  pure = PC
  (PC f) <*> (PC x) = PC (f x)

instance Monad PythonCode where
  return = PC
  PC x >>= f = f x

instance OOProg PythonCode

instance ProgramSym PythonCode where
  type Program PythonCode = ProgData 
  prog n files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    return $ onCodeList (progD n) fs

instance RenderSym PythonCode

instance FileSym PythonCode where
  type File PythonCode = FileData
  fileDoc m = do
    modify (setFileType Combined)
    G.fileDoc pyExt top bottom m

  docMod = G.docMod pyExt

instance RenderFile PythonCode where
  top _ = toCode empty
  bottom = toCode empty
  
  commentedMod = on2StateValues (on2CodeValues R.commentedMod)

  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym PythonCode where
  type Import PythonCode = Doc
  langImport n = toCode $ pyImport <+> text n
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
  bool = toState $ typeFromData Boolean "" empty
  int = G.int
  float = error pyFloatError
  double = toState $ typeFromData Double pyDouble (text pyDouble)
  char = toState $ typeFromData Char "" empty
  string = pyStringType
  infile = toState $ typeFromData File "" empty
  outfile = toState $ typeFromData File "" empty
  listType = onStateValue (\t -> typeFromData (List (getType t)) "" empty)
  arrayType = listType
  listInnerType = G.listInnerType
  obj = G.obj
  funcType = G.funcType
  iterator t = t
  void = toState $ typeFromData Void pyVoid (text pyVoid)

instance TypeElim PythonCode where
  getType = cType . unPC
  getTypeString = typeString . unPC

instance RenderType PythonCode where
  typeFromData t s d = toCode $ td t s d

instance InternalTypeElim PythonCode where
  type' = typeDoc . unPC

instance ControlBlock PythonCode where
  solveODE info opts = modify (addLibImport odeLib) >> multiBlock [
    block [
      r &= objMethodCall odeT (extNewObj odeLib odeT 
      [lambda [iv, dv] (ode info)]) 
        "set_integrator" (pyODEMethod (solveMethod opts) ++
          [absTol opts >>= (mkStateVal double . (text "atol=" <>) . RC.value),
          relTol opts >>= (mkStateVal double . (text "rtol=" <>) . RC.value)]),
      valStmt $ objMethodCall odeT rVal "set_initial_value" [initVal info]],
    block [
      listDecDef iv [tInit info],
      listDecDef dv [initVal info],
      while (objMethodCallNoParams bool rVal "successful" ?&& 
        r_t ?< tFinal info) (bodyStatements [
          valStmt $ objMethodCall odeT rVal "integrate" [r_t #+ stepSize opts],
          valStmt $ listAppend (valueOf iv) r_t,
          valStmt $ listAppend (valueOf dv) (listAccess r_y $ litInt 0)
        ])
     ]
   ]
   where odeLib = "scipy.integrate"
         iv = indepVar info
         dv = depVar info
         odeT = obj "ode"
         r = var "r" odeT
         rVal = valueOf r
         r_t = valueOf $ objVar r (var "t" $ listInnerType $ onStateValue 
           variableType iv)
         r_y = valueOf $ objVar r (var "y" $ onStateValue variableType dv)

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

instance OpElim PythonCode where
  uOp = opDoc . unPC
  bOp = opDoc . unPC
  uOpPrec = opPrec . unPC
  bOpPrec = opPrec . unPC

instance VariableSym PythonCode where
  type Variable PythonCode = VarData
  var = G.var
  staticVar = G.staticVar
  const = var
  extVar l n t = modify (addModuleImportVS l) >> CP.extVar l n t
  self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar pySelf (obj l) (text pySelf))
  classVar = CP.classVar R.classVar
  extClassVar c v = join $ on2StateValues (\t cm -> maybe id ((>>) . modify . 
    addModuleImportVS) (Map.lookup (getTypeString t) cm) $ 
    CP.classVar pyClassVar (toState t) v) c getClassMap
  objVar = CP.objVar
  objVarSelf = CP.objVarSelf
  arrayElem i = G.arrayElem (litInt i)
  iterVar = CP.iterVar

instance VariableElim PythonCode where
  variableName = varName . unPC
  variableType = onCodeValue varType

instance InternalVarElim PythonCode where
  variableBind = varBind . unPC
  variable = varDoc . unPC

instance RenderVariable PythonCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym PythonCode where
  type Value PythonCode = ValData
  valueType = onCodeValue valType

instance Literal PythonCode where
  litTrue = mkStateVal bool pyTrue
  litFalse = mkStateVal bool pyFalse
  litChar = G.litChar
  litDouble = G.litDouble
  litFloat = error pyFloatError
  litInt = G.litInt
  litString = G.litString
  litArray t es = sequence es >>= (\elems -> mkStateVal (arrayType t) 
    (brackets $ valueList elems))
  litList = litArray

instance MathConstant PythonCode where
  pi = addmathImport $ mkStateVal double pyPi

instance VariableValue PythonCode where
  valueOf = G.valueOf

instance CommandLineArgs PythonCode where
  arg n = G.arg (litInt $ n+1) argsList
  argsList = do
    modify (addLangImportVS pySys)
    G.argsList $ pySys `access` argv
  argExists i = listSize argsList ?> litInt (fromIntegral $ i+1)

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
    pyDivision (getType $ valueType v1) (getType $ valueType v2) (return v1) 
      (return v2)
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
  selfFuncAppMixedArgs = G.selfFuncAppMixedArgs dot self
  extFuncAppMixedArgs l n t ps ns = do
    modify (addModuleImportVS l)
    CP.extFuncAppMixedArgs l n t ps ns
  libFuncAppMixedArgs l n t ps ns = do
    modify (addLibImportVS l)
    CP.extFuncAppMixedArgs l n t ps ns
  newObjMixedArgs = G.newObjMixedArgs ""
  extNewObjMixedArgs l tp ps ns = do
    modify (addModuleImportVS l)
    pyExtNewObjMixedArgs l tp ps ns
  libNewObjMixedArgs l tp ps ns = do
    modify (addLibImportVS l)
    pyExtNewObjMixedArgs l tp ps ns

  lambda = G.lambda pyLambda

  notNull v = v ?!= valueOf (var pyNull void)

instance RenderValue PythonCode where
  inputFunc = mkStateVal string pyInputFunc
  printFunc = mkStateVal void pyPrintFunc
  printLnFunc = mkStateVal void empty
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty
  
  cast = on2StateValues (\t -> mkVal t . R.castObj (RC.type' t) . RC.value)

  call = G.call pyNamedArgSep

  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance ValueElim PythonCode where
  valuePrec = valPrec . unPC
  value = val . unPC

instance InternalValueExp PythonCode where
  objMethodCallMixedArgs' = G.objMethodCall

instance FunctionSym PythonCode where
  type Function PythonCode = FuncData
  func = G.func
  objAccess = G.objAccess

instance GetSet PythonCode where
  get = G.get
  set = G.set

instance List PythonCode where
  listSize = on2StateValues (\f v -> mkVal (functionType f) 
    (pyListSize (RC.value v) (RC.function f))) listSizeFunc
  listAdd = G.listAdd
  listAppend = G.listAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = CP.indexOf pyIndex

instance InternalList PythonCode where
  listSlice' b e s vn vo = pyListSlice vn vo (getVal b) (getVal e) (getVal s)
    where getVal = fromMaybe (mkStateVal void empty)

instance Iterator PythonCode where
  iterBegin = G.iterBegin
  iterEnd = G.iterEnd

instance InternalGetSet PythonCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

instance InternalListFunc PythonCode where
  listSizeFunc = funcFromData pyListSizeFunc int
  listAddFunc _ = CP.listAddFunc pyInsert
  listAppendFunc = G.listAppendFunc pyAppendFunc
  listAccessFunc = CP.listAccessFunc
  listSetFunc = CP.listSetFunc R.listSetFunc

instance InternalIterator PythonCode where
  iterBeginFunc _ = error $ CP.iterBeginError pyName
  iterEndFunc _ = error $ CP.iterEndError pyName

instance RenderFunction PythonCode where
  funcFromData d = onStateValue (onCodeValue (`fd` d))
  
instance FunctionElim PythonCode where
  functionType = onCodeValue fType
  function = funcDoc . unPC

instance InternalAssignStmt PythonCode where
  multiAssign vars vals = zoom lensMStoVS $ on2StateLists (\vrs vls -> 
    mkStmtNoEnd (R.multiAssign vrs vls)) vars vals

instance InternalIOStmt PythonCode where
  printSt = pyPrint

instance InternalControlStmt PythonCode where
  multiReturn [] = error "Attempt to write return statement with no return variables"
  multiReturn vs = zoom lensMStoVS $ onStateList (mkStmtNoEnd . R.return') vs

instance RenderStatement PythonCode where
  stmt = G.stmt
  loopStmt = G.loopStmt
  
  emptyStmt = G.emptyStmt

  stmtFromData d t = toCode (d, t)

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
  (&-=) = M.decrement
  (&+=) = G.increment
  (&++) = M.increment1
  (&--) = M.decrement1

instance DeclStatement PythonCode where
  varDec _ = toState $ mkStmtNoEnd empty
  varDecDef = assign
  listDec _ v = v &= litList (onStateValue variableType v) []
  listDecDef = CP.listDecDef
  arrayDec = listDec
  arrayDecDef = listDecDef
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew lib v vs = do
    modify (addModuleImport lib)
    varDecDef v (extNewObj lib (onStateValue variableType v) vs)
  constDecDef = varDecDef
  funcDecDef v ps r = do
    vr <- zoom lensMStoVS v
    f <- function (variableName vr) private dynamic (return $ variableType vr) 
      (map param ps) (oneLiner $ returnStmt r)
    return $ mkStmtNoEnd $ RC.method f

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

  openFileR f n = f &= openRead n
  openFileW f n = f &= openWrite n
  openFileA f n = f &= openAppend n
  closeFile = G.closeFile pyClose

  getFileInputLine = getFileInput
  discardFileLine = CP.discardFileLine pyReadline
  getFileInputAll f v = v &= readlines f
  
instance StringStatement PythonCode where
  stringSplit d vnew s = assign vnew (objAccess s (splitFunc d))

  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FuncAppStatement PythonCode where
  inOutCall = pyInOutCall funcApp
  selfInOutCall = pyInOutCall selfFuncApp
  extInOutCall m = pyInOutCall (extFuncApp m)

instance CommentStatement PythonCode where
  comment = G.comment pyCommentStart

instance ControlStatement PythonCode where
  break = toState $ mkStmtNoEnd R.break
  continue = toState $ mkStmtNoEnd R.continue

  returnStmt = G.returnStmt Empty

  throw = G.throw pyThrow Empty

  ifCond = G.ifCond pyBodyStart pyElseIf pyBodyEnd
  switch = switchAsIf

  ifExists = M.ifExists

  for _ _ _ _ = error $ "Classic for loops not available in Python, please " ++
    "use forRange, forEach, or while instead"
  forRange i initv finalv stepv = forEach i (range initv finalv stepv)
  forEach i' v' b' = do
    i <- zoom lensMStoVS i'
    v <- zoom lensMStoVS v'
    mkStmtNoEnd . pyForEach i v <$> b'
  while v' = on2StateValues (\v b -> mkStmtNoEnd (pyWhile v b)) 
    (zoom lensMStoVS v')

  tryCatch = G.tryCatch pyTryCatch

instance StatePattern PythonCode where 
  checkState = M.checkState

instance ObserverPattern PythonCode where
  notifyObservers f t = forRange M.observerIndex initv (listSize obsList) 
    (litInt 1) notify
    where obsList = valueOf $ observerListName `listOf` t
          initv = litInt 0
          notify = oneLiner $ valStmt $ at obsList (valueOf M.observerIndex) $. f

instance StrategyPattern PythonCode where
  runStrategy = M.runStrategy

instance ScopeSym PythonCode where
  type Scope PythonCode = Doc
  private = toCode empty
  public = toCode empty

instance RenderScope PythonCode where
  scopeFromData _ = toCode

instance ScopeElim PythonCode where
  scope = unPC

instance MethodTypeSym PythonCode where
  type MethodType PythonCode = TypeData
  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym PythonCode where
  type Parameter PythonCode = ParamData
  param = G.param RC.variable
  pointerParam = param

instance RenderParam PythonCode where
  paramFromData v d = on2CodeValues pd v (toCode d)
  
instance ParamElim PythonCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unPC

instance MethodSym PythonCode where
  type Method PythonCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor = G.constructor initName

  docMain = mainFunction

  function = G.function
  mainFunction b = do
    modify setCurrMain
    bod <- b
    modify (setMainDoc $ RC.body bod)
    return $ toCode $ mthd empty

  docFunc = G.docFunc

  inOutMethod n = pyInOut (method n)

  docInOutMethod n = pyDocInOut (inOutMethod n)

  inOutFunc n = pyInOut (function n)

  docInOutFunc n = pyDocInOut (inOutFunc n)

instance RenderMethod PythonCode where
  intMethod m n _ _ _ ps b = do
    modify (if m then setCurrMain else id)
    sl <- zoom lensMStoVS self
    pms <- sequence ps
    toCode . mthd . pyMethod n sl pms <$> b
  intFunc m n _ _ _ ps b = do
    modify (if m then setCurrMain else id)
    bd <- b
    pms <- sequence ps
    return $ toCode $ mthd $ pyFunction n pms bd
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m 
    (onStateValue (onCodeValue R.commentedItem) cmt)
    
  destructor _ = error $ CP.destructorError pyName
  
instance MethodElim PythonCode where
  method = mthdDoc . unPC

instance StateVarSym PythonCode where
  type StateVar PythonCode = Doc
  stateVar _ _ _ = toState (toCode empty)
  stateVarDef _ = CP.stateVarDef
  constVar _ = CP.constVar (RC.perm 
    (static :: PythonCode (Permanence PythonCode)))
  
instance StateVarElim PythonCode where
  stateVar = unPC

instance ClassSym PythonCode where
  type Class PythonCode = Doc
  buildClass = G.buildClass
  extraClass = buildClass
  implementingClass = G.implementingClass

  docClass = G.docClass

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
    return $ vibcat [
      vcat (map (RC.import' . 
        (langImport :: Label -> PythonCode (Import PythonCode))) lis),
      vcat (map (RC.import' . 
        (langImport :: Label -> PythonCode (Import PythonCode))) (sort $ is ++ 
        libis)),
      vcat (map (RC.import' . 
        (modImport :: Label -> PythonCode (Import PythonCode))) mis)]) 
    getMainDoc

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

pyName :: String
pyName = "Python"

pyImport :: Doc
pyImport = text "import"

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

pyTrue, pyFalse :: Doc
pyTrue = text "True"
pyFalse = text "False"

pyPi :: Doc
pyPi = text $ pyMath `access` "pi"

pySys :: String
pySys = "sys"

pyInputFunc, pyPrintFunc, pyListSizeFunc :: Doc
pyInputFunc = text "input()" -- raw_input() for < Python 3.0
pyPrintFunc = text "print"
pyListSizeFunc = text "len"

pyIndex, pyInsert, pyAppendFunc, pyReadline, pyReadlines, pyOpen, pyClose, 
  pyRead, pyWrite, pyAppend, pySplit, pyRange, pyRstrip, pyMath :: String
pyIndex = "index"
pyInsert = "insert"
pyAppendFunc = "append"
pyReadline = "readline"
pyReadlines = "readlines"
pyOpen = "open"
pyClose = "close"
pyRead = "r"
pyWrite = "w"
pyAppend = "a"
pySplit = "split"
pyRange = "range"
pyRstrip = "rstrip"
pyMath = "math"

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

pyODEMethod :: ODEMethod -> [SValue PythonCode]
pyODEMethod RK45 = [litString "dopri5"]
pyODEMethod BDF = [litString "vode", 
  (litString "bdf" :: SValue PythonCode) >>= 
  (mkStateVal string . (text "method=" <>) . RC.value)]
pyODEMethod Adams = [litString "vode", 
  (litString "adams" :: SValue PythonCode) >>= 
  (mkStateVal string . (text "method=" <>) . RC.value)]

pyNotOp :: (Monad r) => VSOp r
pyNotOp = unOpPrec "not"

pySqrtOp :: (Monad r) => VSOp r
pySqrtOp = mathFunc "sqrt"

pyAbsOp :: (Monad r) => VSOp r
pyAbsOp = mathFunc "fabs"

pyLogOp :: (Monad r) => VSOp r
pyLogOp = mathFunc "log10"

pyLnOp :: (Monad r) => VSOp r
pyLnOp = mathFunc "log"

pyExpOp :: (Monad r) => VSOp r
pyExpOp = mathFunc "exp"

pySinOp :: (Monad r) => VSOp r
pySinOp = mathFunc "sin"

pyCosOp :: (Monad r) => VSOp r
pyCosOp = mathFunc "cos"

pyTanOp :: (Monad r) => VSOp r
pyTanOp = mathFunc "tan"

pyAsinOp :: (Monad r) => VSOp r
pyAsinOp = mathFunc "asin"

pyAcosOp :: (Monad r) => VSOp r
pyAcosOp = mathFunc "acos"

pyAtanOp :: (Monad r) => VSOp r
pyAtanOp = mathFunc "atan"

pyFloorOp :: (Monad r) => VSOp r
pyFloorOp = mathFunc "floor"

pyCeilOp :: (Monad r) => VSOp r
pyCeilOp = mathFunc "ceil"

addmathImport :: VS a -> VS a
addmathImport = (>>) $ modify (addLangImportVS pyMath)

mathFunc :: (Monad r) => String -> VSOp r
mathFunc = addmathImport . unOpPrec . access pyMath 

splitFunc :: (RenderSym r) => Char -> VSFunction r
splitFunc d = func pySplit (listType string) [litString [d]]

openRead, openWrite, openAppend :: (RenderSym r) => SValue r -> SValue r
openRead n = funcApp pyOpen infile [n, litString pyRead]
openWrite n = funcApp pyOpen outfile [n, litString pyWrite]
openAppend n = funcApp pyOpen outfile [n, litString pyAppend]

readline, readlines :: (RenderSym r) => SValue r -> SValue r
readline f = objMethodCall string f pyReadline []
readlines f = objMethodCall (listType string) f pyReadlines []

readInt, readDouble, readString :: (RenderSym r) => SValue r -> SValue r
readInt inSrc = funcApp pyInt int [inSrc]
readDouble inSrc = funcApp pyDouble double [inSrc]
readString inSrc = objMethodCall string inSrc pyRstrip []

range :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
range initv finalv stepv = funcApp pyRange (listType int) [initv, finalv, stepv]

pyClassVar :: Doc -> Doc -> Doc
pyClassVar c v = c <> dot <> c <> dot <> v

pyInlineIf :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
pyInlineIf = on3StateValues (\c v1 v2 -> valFromData (valuePrec c) 
  (valueType v1) (RC.value v1 <+> ifLabel <+> RC.value c <+> elseLabel <+> 
  RC.value v2))

pyLambda :: (RenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
pyLambda ps ex = pyLambdaDec <+> variableList ps <> colon <+> RC.value ex

pyListSize :: Doc -> Doc -> Doc
pyListSize v f = f <> parens v

pyStringType :: (RenderSym r) => VSType r
pyStringType = toState $ typeFromData String pyString (text pyString)

pyExtNewObjMixedArgs :: (RenderSym r) => Library -> MixedCtorCall r
pyExtNewObjMixedArgs l tp vs ns = tp >>= (\t -> call (Just l) Nothing 
  (getTypeString t) (return t) vs ns)

pyPrint :: Bool -> Maybe (SValue PythonCode) -> SValue PythonCode -> 
  SValue PythonCode -> MSStatement PythonCode
pyPrint newLn f' p' v' = zoom lensMStoVS $ do
    f <- fromMaybe (mkStateVal void empty) f'
    prf <- p'
    v <- v'
    s <- litString "" :: SValue PythonCode
    let nl = if newLn then empty else text listSep <> text "end" <> equals <> 
               RC.value s
        fl = emptyIfEmpty (RC.value f) $ text listSep <> text "file" <> equals 
               <> RC.value f
    return $ mkStmtNoEnd $ RC.value prf <> parens (RC.value v <> nl <> fl)

pyOut :: (RenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r -> 
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

pyThrow :: (RenderSym r) => r (Value r) -> Doc
pyThrow errMsg = pyRaise <+> text exceptionObj <> parens (RC.value errMsg)

pyForEach :: (RenderSym r) => r (Variable r) -> r (Value r) -> r (Body r) -> Doc
pyForEach i lstVar b = vcat [
  forLabel <+> RC.variable i <+> inLabel <+> RC.value lstVar <> colon,
  indent $ RC.body b]

pyWhile :: (RenderSym r) => r (Value r) -> r (Body r) -> Doc
pyWhile v b = vcat [
  whileLabel <+> RC.value v <> colon,
  indent $ RC.body b]

pyTryCatch :: (RenderSym r) => r (Body r) -> r (Body r) -> Doc
pyTryCatch tryB catchB = vcat [
  tryLabel <+> colon,
  indent $ RC.body tryB,
  pyExcept <+> text exceptionObj <+> colon,
  indent $ RC.body catchB]

pyListSlice :: (RenderSym r, Monad r) => SVariable r -> SValue r -> SValue r -> 
  SValue r -> SValue r -> MS (r Doc)
pyListSlice vn vo beg end step = zoom lensMStoVS $ do
  vnew <- vn
  vold <- vo
  b <- beg
  e <- end
  s <- step
  return $ toCode $ RC.variable vnew <+> equals <+> RC.value vold <> 
    brackets (RC.value b <> colon <> RC.value e <> colon <> RC.value s)

pyMethod :: (RenderSym r) => Label -> r (Variable r) -> [r (Parameter r)] ->
  r (Body r) -> Doc
pyMethod n slf ps b = vcat [
  pyDef <+> text n <> parens (RC.variable slf <> oneParam <> pms) <> colon,
  indent bodyD]
      where pms = parameterList ps
            oneParam = emptyIfEmpty pms $ text listSep
            bodyD | isEmpty (RC.body b) = text pyNull
                  | otherwise = RC.body b

pyFunction :: (RenderSym r) => Label -> [r (Parameter r)] -> r (Body r) -> Doc
pyFunction n ps b = vcat [
  pyDef <+> text n <> parens (parameterList ps) <> colon,
  indent bodyD]
  where bodyD | isEmpty (RC.body b) = text pyNull
              | otherwise = RC.body b

pyClass :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
pyClass n pn s vs fs = vcat [
  s <+> classDec <+> text n <> pn <> colon,
  indent funcSec]
  where funcSec | isEmpty (vs <> fs) = text pyNull
                | isEmpty vs = fs
                | isEmpty fs = vs
                | otherwise = vcat [vs, blank, fs]

pyInOutCall :: (Label -> VSType PythonCode -> [SValue PythonCode] -> 
  SValue PythonCode) -> Label -> [SValue PythonCode] -> [SVariable PythonCode] 
  -> [SVariable PythonCode] -> MSStatement PythonCode
pyInOutCall f n ins [] [] = valStmt $ f n void ins
pyInOutCall f n ins outs both = multiAssign rets [f n void (map valueOf both ++ 
  ins)]
  where rets = both ++ outs

pyBlockComment :: [String] -> Doc -> Doc
pyBlockComment lns cmt = vcat $ map ((<+>) cmt . text) lns

pyDocComment :: [String] -> Doc -> Doc -> Doc
pyDocComment [] _ _ = empty
pyDocComment (l:lns) start mid = vcat $ start <+> text l : map ((<+>) mid . 
  text) lns

pyInOut :: (PythonCode (Scope PythonCode) -> PythonCode (Permanence PythonCode) 
    -> VSType PythonCode -> [MSParameter PythonCode] -> MSBody PythonCode -> 
    SMethod PythonCode)
  -> PythonCode (Scope PythonCode) -> PythonCode (Permanence PythonCode) -> 
  [SVariable PythonCode] -> [SVariable PythonCode] -> [SVariable PythonCode] -> 
  MSBody PythonCode -> SMethod PythonCode
pyInOut f s p ins [] [] b = f s p void (map param ins) b
pyInOut f s p ins outs both b = f s p void (map param $ both ++ ins) 
  (on3StateValues (on3CodeValues surroundBody) (multi $ map varDec outs) b 
  (multiReturn $ map valueOf rets))
  where rets = both ++ outs

pyDocInOut :: (RenderSym r) => (r (Scope r) -> r (Permanence r) 
    -> [SVariable r] -> [SVariable r] -> [SVariable r] -> MSBody r -> SMethod r)
  -> r (Scope r) -> r (Permanence r) -> String -> [(String, SVariable r)] -> 
  [(String, SVariable r)] -> [(String, SVariable r)] -> MSBody r -> SMethod r
pyDocInOut f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is)
  (map fst $ bs ++ os) (f s p (map snd is) (map snd os) (map snd bs) b)
