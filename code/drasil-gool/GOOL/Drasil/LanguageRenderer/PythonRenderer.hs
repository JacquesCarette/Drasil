{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python code is contained in this module
module GOOL.Drasil.LanguageRenderer.PythonRenderer (
  -- * Python Code Configuration -- defines syntax of all Python code
  PythonCode(..)
) where

import Utils.Drasil (blank, indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym, FileSym(..),
  InternalFile(..), KeywordSym(..), ImportSym(..), PermanenceSym(..), 
  InternalPerm(..), BodySym(..), InternalBody(..), BlockSym(..), 
  InternalBlock(..), ControlBlockSym(..), TypeSym(..), InternalType(..), 
  UnaryOpSym(..), BinaryOpSym(..), InternalOp(..), VariableSym(..), 
  InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  InternalSelector(..), objMethodCall, objMethodCallNoParams, FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), InternalParam(..), MethodSym(..), 
  InternalMethod(..), 
  StateVarSym(..), InternalStateVar(..), ClassSym(..), InternalClass(..), 
  ModuleSym(..), InternalMod(..), BlockCommentSym(..), ODEInfo(..), 
  ODEOptions(..), ODEMethod(..))
import GOOL.Drasil.LanguageRenderer (enumElementsDocD', multiStateDocD, 
  bodyDocD, outDoc, destructorError, multiAssignDoc, returnDocD, mkStNoEnd,
  breakDocD, continueDocD, mkStateVal, mkVal, mkStateVar, classVarDocD, 
  newObjDocD', listSetFuncDocD, castObjDocD, dynamicDocD, bindingError, 
  classDec, dot, forLabel, inLabel, observerListName, commentedItem, 
  addCommentsDocD, commentedModD, docFuncRepr, valueList, variableList,
  parameterList, surroundBody)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  oneLiner, multiBody, block, multiBlock, int, float, listInnerType, obj, 
  enumType, funcType, runStrategy, notOp', negateOp, sqrtOp', absOp', expOp', 
  sinOp', cosOp', tanOp', asinOp', acosOp', atanOp', equalOp, notEqualOp, 
  greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp, minusOp, multOp, 
  divideOp, moduloOp, var, staticVar, extVar, enumVar, classVar, objVar, 
  objVarSelf, listVar, listOf, iterVar, litChar, litFloat, litInt, litString, 
  valueOf, arg, enumElement, argsList, objAccess, objMethodCall, 
  objMethodCallNoParams, selfAccess, listIndexExists, indexOf, funcApp, 
  selfFuncApp, extFuncApp, newObj, lambda, func, get, set, listAdd, listAppend, 
  iterBegin, iterEnd, listAccess, listSet, getFunc, setFunc, listAddFunc, 
  listAppendFunc, iterBeginError, iterEndError, listAccessFunc, listSetFunc, 
  state, loopState, emptyState, assign, assignToListIndex, decrement, 
  increment', increment1', decrement1, objDecNew, objDecNewNoParams, closeFile, 
  discardFileLine, stringListVals, stringListLists, returnState, valState, 
  comment, throw, initState, changeState, initObserverList, addObserver, ifCond,
  ifNoElse, switchAsIf, ifExists, tryCatch, checkState, construct, param, 
  method, getMethod, setMethod, privMethod, pubMethod, constructor, function, 
  docFunc, stateVarDef, constVar, privMVar, pubMVar, pubGVar, buildClass, 
  privClass, pubClass, docClass, commentedClass, buildModule, modFromData, 
  fileDoc, docMod, fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (unOpPrec, unExpr, 
  unExpr', typeUnExpr, powerPrec, multPrec, andPrec, orPrec, binExpr, 
  typeBinExpr, addmathImport)
import GOOL.Drasil.Data (Terminator(..), ScopeTag(..), FileType(..), 
  FileData(..), fileD, FuncData(..), fd, ModData(..), md, updateModDoc, 
  MethodData(..), mthd, updateMthdDoc, OpData(..), od, ParamData(..), pd, 
  ProgData(..), progD, TypeData(..), td, ValData(..), vd, VarData(..), vard)
import GOOL.Drasil.Helpers (vibcat, emptyIfEmpty, toCode, toState, onCodeValue,
  onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues,
  onCodeList, onStateList, on2StateLists, on1CodeValue1List, on1StateValue1List)
import GOOL.Drasil.State (MS, VS, lensGStoFS, lensMStoVS, lensVStoMS, 
  addLangImportVS, getLangImports, 
  addLibImport, getLibImports, addModuleImport, addModuleImportVS, getModuleImports, setClassName, 
  getClassName, setCurrMain, getClassMap)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (modify)
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

instance ProgramSym PythonCode where
  type Program PythonCode = ProgData 
  prog n = onStateList (onCodeList (progD n)) . map (zoom lensGStoFS)

instance RenderSym PythonCode

instance FileSym PythonCode where
  type RenderFile PythonCode = FileData
  fileDoc = G.fileDoc Combined pyExt top bottom

  docMod = G.docMod

  commentedMod cmt m = on2StateValues (on2CodeValues commentedModD) m cmt

instance InternalFile PythonCode where
  top _ = toCode empty
  bottom = toCode empty

  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym PythonCode where
  type Keyword PythonCode = Doc
  endStatement = toCode empty
  endStatementLoop = toCode empty

  inherit n = toCode $ parens (text n)

  list _ = toCode empty

  blockStart = toCode colon
  blockEnd = toCode empty

  ifBodyStart = blockStart
  elseIf = toCode $ text "elif"
  
  iterForEachLabel = toCode forLabel
  iterInLabel = toCode inLabel

  commentStart = toCode $ text "#"
  blockCommentStart = toCode empty
  blockCommentEnd = toCode empty
  docCommentStart = toCode $ text "##"
  docCommentEnd = toCode empty

  keyDoc = unPC

instance ImportSym PythonCode where
  type Import PythonCode = Doc
  langImport n = toCode $ text $ "import " ++ n
  modImport = langImport

  importDoc = unPC

instance PermanenceSym PythonCode where
  type Permanence PythonCode = Doc
  static_ = toCode empty
  dynamic_ = toCode dynamicDocD

instance InternalPerm PythonCode where
  permDoc = unPC
  binding = error $ bindingError pyName

instance BodySym PythonCode where
  type Body PythonCode = Doc
  body = onStateList (onCodeList bodyDocD)
  bodyStatements = block
  oneLiner = G.oneLiner

  addComments s = onStateValue (on2CodeValues (addCommentsDocD s) commentStart)

instance InternalBody PythonCode where
  bodyDoc = unPC
  docBody = onStateValue toCode
  multiBody = G.multiBody 

instance BlockSym PythonCode where
  type Block PythonCode = Doc
  block = G.block endStatement

instance InternalBlock PythonCode where
  blockDoc = unPC
  docBlock = onStateValue toCode
  multiBlock = G.multiBlock

instance TypeSym PythonCode where
  type Type PythonCode = TypeData
  bool = toState $ typeFromData Boolean "" empty
  int = G.int
  float = G.float
  char = toState $ typeFromData Char "" empty
  string = pyStringType
  infile = toState $ typeFromData File "" empty
  outfile = toState $ typeFromData File "" empty
  listType _ = onStateValue (\t -> typeFromData (List (getType t)) "[]" 
    (brackets empty))
  listInnerType = G.listInnerType
  obj = G.obj
  enumType = G.enumType
  funcType = G.funcType
  iterator t = t
  void = toState $ typeFromData Void "NoneType" (text "NoneType")

  getType = cType . unPC
  getTypeString = typeString . unPC
  getTypeDoc = typeDoc . unPC

instance InternalType PythonCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym PythonCode where
  runStrategy = G.runStrategy

  listSlice' b e s vnew vold = docBlock $ zoom lensMStoVS $ pyListSlice vnew 
    vold (getVal b) (getVal e) (getVal s)
    where getVal = fromMaybe (mkStateVal void empty)

  solveODE info opts = modify (addLibImport odeLib) >> multiBlock [
    block [
      r &= objMethodCall odeT (extNewObj odeLib odeT 
      [lambda [iv, dv] (ode info)]) 
        "set_integrator" (pyODEMethod (solveMethod opts) ++
          [absTol opts >>= (mkStateVal float . (text "atol=" <>) . valueDoc),
          relTol opts >>= (mkStateVal float . (text "rtol=" <>) . valueDoc)]),
      valState $ objMethodCall odeT rVal "set_initial_value" [initVal info]],
    block [
      listDecDef iv [tInit info],
      listDecDef dv [initVal info],
      while (objMethodCallNoParams bool rVal "successful" ?&& 
        r_t ?< tFinal info) (bodyStatements [
          valState $ objMethodCall odeT rVal "integrate" [r_t #+ stepSize opts],
          valState $ listAppend (valueOf iv) r_t,
          valState $ listAppend (valueOf dv) (listAccess r_y $ litInt 0)
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
  notOp = G.notOp'
  negateOp = G.negateOp
  sqrtOp = G.sqrtOp'
  absOp = G.absOp'
  logOp = pyLogOp
  lnOp = pyLnOp
  expOp = G.expOp'
  sinOp = G.sinOp'
  cosOp = G.cosOp'
  tanOp = G.tanOp'
  asinOp = G.asinOp'
  acosOp = G.acosOp'
  atanOp = G.atanOp'
  floorOp = addmathImport $ unOpPrec "math.floor"
  ceilOp = addmathImport $ unOpPrec "math.ceil"

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
  powerOp = powerPrec "**"
  moduloOp = G.moduloOp
  andOp = andPrec "and"
  orOp = orPrec "or"

instance InternalOp PythonCode where
  uOpDoc = opDoc . unPC
  bOpDoc = opDoc . unPC
  uOpPrec = opPrec . unPC
  bOpPrec = opPrec . unPC
  
  uOpFromData p d = toState $ toCode $ od p d
  bOpFromData p d = toState $ toCode $ od p d

instance VariableSym PythonCode where
  type Variable PythonCode = VarData
  var = G.var
  staticVar = G.staticVar
  const = var
  extVar l n t = modify (addModuleImportVS l) >> G.extVar l n t
  self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar "self" (obj l) (text "self"))
  enumVar = G.enumVar
  classVar = G.classVar classVarDocD
  extClassVar c v = join $ on2StateValues (\t cm -> maybe id ((>>) . modify . 
    addModuleImportVS) (Map.lookup (getTypeString t) cm) $ 
    G.classVar pyClassVar (toState t) v) c getClassMap
  objVar = G.objVar
  objVarSelf = G.objVarSelf
  listVar = G.listVar
  listOf = G.listOf
  iterVar = G.iterVar

  ($->) = objVar

  variableBind = varBind . unPC
  variableName = varName . unPC
  variableType = onCodeValue varType
  variableDoc = varDoc . unPC

instance InternalVariable PythonCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym PythonCode where
  type Value PythonCode = ValData
  litTrue = mkStateVal bool (text "True")
  litFalse = mkStateVal bool (text "False")
  litChar = G.litChar
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString

  pi = addmathImport $ mkStateVal float (text "math.pi")

  ($:) = enumElement

  valueOf = G.valueOf
  arg n = G.arg (litInt $ n+1) argsList
  enumElement = G.enumElement
  argsList = modify (addLangImportVS "sys") >> G.argsList "sys.argv"

  valueType = onCodeValue valType
  valueDoc = valDoc . unPC

instance NumericExpression PythonCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) v1' v2' = join $ on2StateValues (\v1 v2 -> pyDivision (getType $ 
    valueType v1) (getType $ valueType v2) v1' v2') v1' v2'
    where pyDivision Integer Integer = binExpr (multPrec "//")
          pyDivision _ _ = binExpr divideOp
  (#%) = binExpr moduloOp
  (#^) = binExpr powerOp

  log = unExpr logOp
  ln = unExpr lnOp
  exp = unExpr expOp
  sin = unExpr sinOp
  cos = unExpr cosOp
  tan = unExpr tanOp
  csc v = litFloat 1.0 #/ sin v
  sec v = litFloat 1.0 #/ cos v
  cot v = litFloat 1.0 #/ tan v
  arcsin = unExpr asinOp
  arccos = unExpr acosOp
  arctan = unExpr atanOp
  floor = unExpr floorOp
  ceil = unExpr ceilOp

instance BooleanExpression PythonCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression PythonCode where
  inlineIf = pyInlineIf
  funcApp = G.funcApp
  selfFuncApp = G.selfFuncApp self
  extFuncApp l n t ps = modify (addModuleImportVS l) >> G.extFuncApp l n t ps
  newObj = G.newObj newObjDocD'
  extNewObj l tp ps = modify (addModuleImportVS l) >> on1StateValue1List 
    (\t -> mkVal t . pyExtStateObj l (getTypeDoc t) . valueList) tp ps

  lambda = G.lambda pyLambda

  exists v = v ?!= valueOf (var "None" void)
  notNull = exists

instance InternalValue PythonCode where
  inputFunc = mkStateVal string (text "input()") -- raw_input() for < Python 3.0
  printFunc = mkStateVal void (text "print")
  printLnFunc = mkStateVal void empty
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty
  
  cast = on2StateValues (\t -> mkVal t . castObjDocD (getTypeDoc t) . valueDoc)

  valuePrec = valPrec . unPC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector PythonCode where
  objAccess = G.objAccess
  ($.) = objAccess 

  selfAccess = G.selfAccess

  listIndexExists = G.listIndexExists
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf = G.indexOf "index"

instance InternalSelector PythonCode where
  objMethodCall' = G.objMethodCall
  objMethodCallNoParams' = G.objMethodCallNoParams

instance FunctionSym PythonCode where
  type Function PythonCode = FuncData
  func = G.func

  get = G.get
  set = G.set

  listSize = on2StateValues (\f v -> mkVal (functionType f) 
    (pyListSize (valueDoc v) (functionDoc f))) listSizeFunc
  listAdd = G.listAdd
  listAppend = G.listAppend

  iterBegin = G.iterBegin
  iterEnd = G.iterEnd

instance SelectorFunction PythonCode where
  listAccess = G.listAccess
  listSet = G.listSet
  at = listAccess

instance InternalFunction PythonCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

  listSizeFunc = funcFromData (text "len") int
  listAddFunc _ = G.listAddFunc "insert"
  listAppendFunc = G.listAppendFunc "append"

  iterBeginFunc _ = error $ G.iterBeginError pyName
  iterEndFunc _ = error $ G.iterEndError pyName

  listAccessFunc = G.listAccessFunc
  listSetFunc = G.listSetFunc listSetFuncDocD

  functionType = onCodeValue fType
  functionDoc = funcDoc . unPC

  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance InternalStatement PythonCode where
  printSt nl f p v = zoom lensMStoVS $ on3StateValues (\f' p' v' -> mkStNoEnd $ 
    pyPrint nl p' v' f') (fromMaybe (mkStateVal void empty) f) p v

  state = G.state
  loopState = G.loopState
  
  emptyState = G.emptyState
  statementDoc = fst . unPC
  statementTerm = snd . unPC

  stateFromData d t = toCode (d, t)

instance StatementSym PythonCode where
  -- Terminator determines how statements end
  type Statement PythonCode = (Doc, Terminator)
  assign = G.assign Empty
  assignToListIndex = G.assignToListIndex
  multiAssign vars vals = zoom lensMStoVS $ on2StateLists (\vrs vls -> 
    mkStNoEnd (multiAssignDoc vrs vls)) vars vals
  (&=) = assign
  (&-=) = G.decrement
  (&+=) = G.increment'
  (&++) = G.increment1'
  (&~-) = G.decrement1

  varDec _ = toState $ mkStNoEnd empty
  varDecDef = assign
  listDec _ v = zoom lensMStoVS $ onStateValue (mkStNoEnd . pyListDec) v
  listDecDef v' vs' = zoom lensMStoVS $ on1StateValue1List (\v vs -> mkStNoEnd 
    $ pyListDecDef v vs) v' vs'
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew lib v vs = modify (addModuleImport lib) >> varDecDef v 
    (extNewObj lib (onStateValue variableType v) vs)
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams lib v = modify (addModuleImport lib) >> varDecDef v 
    (extNewObj lib (onStateValue variableType v) [])
  constDecDef = varDecDef
  funcDecDef v ps r = onStateValue (mkStNoEnd . methodDoc) (zoom lensMStoVS v 
    >>= (\vr -> function (variableName vr) private dynamic_ 
    (toState $ variableType vr) (map param ps) (oneLiner $ returnState r)))

  print = pyOut False Nothing printFunc
  printLn = pyOut True Nothing printFunc
  printStr = print . litString
  printStrLn = printLn . litString

  printFile f = pyOut False (Just f) printFunc
  printFileLn f = pyOut True (Just f) printFunc
  printFileStr f = printFile f . litString
  printFileStrLn f = printFileLn f . litString

  getInput = pyInput inputFunc
  discardInput = valState inputFunc
  getFileInput f = pyInput (objMethodCall string f "readline" [])
  discardFileInput f = valState (objMethodCall string f "readline" [])

  openFileR f n = f &= funcApp "open" infile [n, litString "r"]
  openFileW f n = f &= funcApp "open" outfile [n, litString "w"]
  openFileA f n = f &= funcApp "open" outfile [n, litString "a"]
  closeFile = G.closeFile "close"

  getFileInputLine = getFileInput
  discardFileLine = G.discardFileLine "readline"
  stringSplit d vnew s = assign vnew (objAccess s (func "split" 
    (listType static_ string) [litString [d]]))  

  stringListVals = G.stringListVals
  stringListLists = G.stringListLists

  break = toState $ mkStNoEnd breakDocD
  continue = toState $ mkStNoEnd continueDocD

  returnState = G.returnState Empty
  multiReturn [] = error "Attempt to write return statement with no return variables"
  multiReturn vs = zoom lensMStoVS $ onStateList (mkStNoEnd . returnDocD) vs

  valState = G.valState Empty

  comment = G.comment commentStart

  free v = v &= valueOf (var "None" void)

  throw = G.throw pyThrow Empty

  initState = G.initState
  changeState = G.changeState

  initObserverList = G.initObserverList
  addObserver = G.addObserver

  inOutCall = pyInOutCall funcApp
  selfInOutCall = pyInOutCall selfFuncApp
  extInOutCall m = pyInOutCall (extFuncApp m)

  multi = onStateList (on1CodeValue1List multiStateDocD endStatement)

instance ControlStatementSym PythonCode where
  ifCond = G.ifCond ifBodyStart elseIf blockEnd
  ifNoElse = G.ifNoElse
  switch = switchAsIf
  switchAsIf = G.switchAsIf

  ifExists = G.ifExists

  for _ _ _ _ = error $ "Classic for loops not available in Python, please " ++
    "use forRange, forEach, or while instead"
  forRange it initval finalval step bod = (\i initv finalv stepv b -> mkStNoEnd 
    (pyForRange iterInLabel i initv finalv stepv b)) <$> zoom lensMStoVS it <*> 
    zoom lensMStoVS initval <*> zoom lensMStoVS finalval <*> 
    zoom lensMStoVS step <*> bod
  forEach i' v' = on3StateValues (\i v b -> mkStNoEnd (pyForEach 
    iterForEachLabel iterInLabel i v b)) 
    (zoom lensMStoVS i') (zoom lensMStoVS v')
  while v' = on2StateValues (\v b -> mkStNoEnd (pyWhile v b)) 
    (zoom lensMStoVS v')

  tryCatch = G.tryCatch pyTryCatch

  checkState = G.checkState
  notifyObservers f t = forRange index initv (listSize obsList) 
    (litInt 1) notify
    where obsList = valueOf $ observerListName `listOf` t
          index = var "observerIndex" int
          initv = litInt 0
          notify = oneLiner $ valState $ at obsList (valueOf index) $. f

  getFileInputAll f v = v &= objMethodCall (listType static_ string) f
    "readlines" []

instance ScopeSym PythonCode where
  type Scope PythonCode = Doc
  private = toCode empty
  public = toCode empty

instance InternalScope PythonCode where
  scopeDoc = unPC

instance MethodTypeSym PythonCode where
  type MethodType PythonCode = TypeData
  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym PythonCode where
  type Parameter PythonCode = ParamData
  param = G.param variableDoc
  pointerParam = param

instance InternalParam PythonCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameterDoc = paramDoc . unPC
  paramFromData v d = on2CodeValues pd v (toCode d)

instance MethodSym PythonCode where
  type Method PythonCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor = G.constructor initName
  destructor _ = error $ destructorError pyName

  docMain = mainFunction

  function = G.function
  mainFunction b = modify setCurrMain >> onStateValue (onCodeValue mthd) b

  docFunc = G.docFunc

  inOutMethod n = pyInOut (method n)

  docInOutMethod n = pyDocInOut (inOutMethod n)

  inOutFunc n = pyInOut (function n)

  docInOutFunc n = pyDocInOut (inOutFunc n)

instance InternalMethod PythonCode where
  intMethod m n _ _ _ ps b = modify (if m then setCurrMain else id) >> 
    on3StateValues (\sl pms bd -> methodFromData Pub $ pyMethod n sl pms bd) 
    (zoom lensMStoVS self) (sequence ps) b 
  intFunc m n _ _ _ ps b = modify (if m then setCurrMain else id) >>
    on1StateValue1List (\bd pms -> methodFromData Pub $ pyFunction n pms bd) 
    b ps
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthdDoc) m 
    (onStateValue (onCodeValue commentedItem) cmt)

  methodDoc = mthdDoc . unPC
  methodFromData _ = toCode . mthd

instance StateVarSym PythonCode where
  type StateVar PythonCode = Doc
  stateVar _ _ _ = toState (toCode empty)
  stateVarDef _ = G.stateVarDef
  constVar _ = G.constVar (permDoc 
    (static_ :: PythonCode (Permanence PythonCode)))
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance InternalStateVar PythonCode where
  stateVarDoc = unPC
  stateVarFromData = onStateValue toCode

instance ClassSym PythonCode where
  type Class PythonCode = Doc
  buildClass = G.buildClass pyClass inherit
  enum n es s = modify (setClassName n) >> classFromData (toState $ pyClass n 
    empty (scopeDoc s) (enumElementsDocD' es) empty)
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass = G.commentedClass

instance InternalClass PythonCode where
  classDoc = unPC
  classFromData = onStateValue toCode

instance ModuleSym PythonCode where
  type Module PythonCode = ModData
  buildModule n = G.buildModule n (on3StateValues (\lis libis mis -> vibcat [
    vcat (map (importDoc . 
      (langImport :: Label -> PythonCode (Import PythonCode))) lis),
    vcat (map (importDoc . 
      (langImport :: Label -> PythonCode (Import PythonCode))) libis),
    vcat (map (importDoc . 
      (modImport :: Label -> PythonCode (Import PythonCode))) mis)]) 
    getLangImports getLibImports getModuleImports)

instance InternalMod PythonCode where
  moduleDoc = modDoc . unPC
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateModDoc f)

instance BlockCommentSym PythonCode where
  type BlockComment PythonCode = Doc
  blockComment lns = onCodeValue (pyBlockComment lns) commentStart
  docComment = onStateValue (\lns -> on2CodeValues (pyDocComment lns) 
    docCommentStart commentStart)

  blockCommentDoc = unPC

-- convenience
initName :: Label
initName = "__init__"

pyName :: String
pyName = "Python"

pyODEMethod :: ODEMethod -> [VS (PythonCode (Value PythonCode))]
pyODEMethod RK45 = [litString "dopri5"]
pyODEMethod BDF = [litString "vode", 
  (litString "bdf" :: VS (PythonCode (Value PythonCode))) >>= 
  (mkStateVal string . (text "method=" <>) . valueDoc)]
pyODEMethod Adams = [litString "vode", 
  (litString "adams" :: VS (PythonCode (Value PythonCode))) >>= 
  (mkStateVal string . (text "method=" <>) . valueDoc)]

pyLogOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
pyLogOp = addmathImport $ unOpPrec "math.log10"

pyLnOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
pyLnOp = addmathImport $ unOpPrec "math.log"

pyClassVar :: Doc -> Doc -> Doc
pyClassVar c v = c <> dot <> c <> dot <> v

pyExtStateObj :: Label -> Doc -> Doc -> Doc
pyExtStateObj l t vs = text l <> dot <> t <> parens vs

pyInlineIf :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))
pyInlineIf = on3StateValues (\c v1 v2 -> valFromData (valuePrec c) 
  (valueType v1) (valueDoc v1 <+> text "if" <+> valueDoc c <+> text "else" <+> 
  valueDoc v2))

pyLambda :: (RenderSym repr) => [repr (Variable repr)] -> repr (Value repr) -> 
  Doc
pyLambda ps ex = text "lambda" <+> variableList ps <> colon <+> valueDoc ex

pyListSize :: Doc -> Doc -> Doc
pyListSize v f = f <> parens v

pyStringType :: (RenderSym repr) => VS (repr (Type repr))
pyStringType = toState $ typeFromData String "str" (text "str")

pyListDec :: (RenderSym repr) => repr (Variable repr) -> Doc
pyListDec v = variableDoc v <+> equals <+> getTypeDoc (variableType v)

pyListDecDef :: (RenderSym repr) => repr (Variable repr) -> [repr (Value repr)] 
  -> Doc
pyListDecDef v vs = variableDoc v <+> equals <+> brackets (valueList vs)

pyPrint :: (RenderSym repr) => Bool -> repr (Value repr) -> repr (Value repr) 
  -> repr (Value repr) -> Doc
pyPrint newLn prf v f = valueDoc prf <> parens (valueDoc v <> nl <> fl)
  where nl = if newLn then empty else text ", end=''"
        fl = emptyIfEmpty (valueDoc f) $ text ", file=" <> valueDoc f

pyOut :: (RenderSym repr) => Bool -> Maybe (VS (repr (Value repr))) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> MS (repr (Statement repr))
pyOut newLn f printFn v = zoom lensMStoVS v >>= pyOut' . getType . valueType
  where pyOut' (List _) = printSt newLn f printFn v
        pyOut' _ = outDoc newLn f printFn v

pyInput :: VS (PythonCode (Value PythonCode)) ->
  VS (PythonCode (Variable PythonCode)) -> 
  MS (PythonCode (Statement PythonCode))
pyInput inSrc v = v &= (v >>= pyInput' . getType . variableType)
  where pyInput' Integer = funcApp "int" int [inSrc]
        pyInput' Float = funcApp "float" float [inSrc]
        pyInput' Boolean = inSrc ?!= litString "0"
        pyInput' String = objMethodCall string inSrc "rstrip" []
        pyInput' Char = inSrc
        pyInput' _ = error "Attempt to read a value of unreadable type"

pyThrow :: (RenderSym repr) => repr (Value repr) -> Doc
pyThrow errMsg = text "raise" <+> text "Exception" <> parens (valueDoc errMsg)

pyForRange :: (RenderSym repr) => repr (Keyword repr) -> repr (Variable repr)
  -> repr (Value repr) -> repr (Value repr) -> repr (Value repr) -> 
  repr (Body repr) -> Doc
pyForRange inLbl i initv finalv stepv b = vcat [
  forLabel <+> variableDoc i <+> keyDoc inLbl <+> text "range" <> parens 
    (valueDoc initv <> text ", " <> valueDoc finalv <> text ", " <> valueDoc 
    stepv) <> colon,
  indent $ bodyDoc b]

pyForEach :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Variable repr) -> repr (Value repr) -> repr (Body repr) -> Doc
pyForEach forEachLabel inLbl i lstVar b = vcat [
  keyDoc forEachLabel <+> variableDoc i <+> keyDoc inLbl <+> valueDoc lstVar <> 
    colon,
  indent $ bodyDoc b]

pyWhile :: (RenderSym repr) => repr (Value repr) -> repr (Body repr) -> Doc
pyWhile v b = vcat [
  text "while" <+> valueDoc v <> colon,
  indent $ bodyDoc b]

pyTryCatch :: (RenderSym repr) => repr (Body repr) -> repr (Body repr) -> Doc
pyTryCatch tryB catchB = vcat [
  text "try" <+> colon,
  indent $ bodyDoc tryB,
  text "except" <+> text "Exception" <+> colon,
  indent $ bodyDoc catchB]

pyListSlice :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS Doc
pyListSlice vn vo beg end step = (\vnew vold b e s -> variableDoc vnew <+> 
  equals <+> valueDoc vold <> brackets (valueDoc b <> colon <> valueDoc e <> 
  colon <> valueDoc s)) <$> vn <*> vo <*> beg <*> end <*> step

pyMethod :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> Doc
pyMethod n slf ps b = vcat [
  text "def" <+> text n <> parens (variableDoc slf <> oneParam <> pms) <> colon,
  indent bodyD]
      where pms = parameterList ps
            oneParam = emptyIfEmpty pms $ text ", "
            bodyD | isEmpty (bodyDoc b) = text "None"
                  | otherwise = bodyDoc b

pyFunction :: (RenderSym repr) => Label -> [repr (Parameter repr)] -> 
  repr (Body repr) -> Doc
pyFunction n ps b = vcat [
  text "def" <+> text n <> parens (parameterList ps) <> colon,
  indent bodyD]
  where bodyD | isEmpty (bodyDoc b) = text "None"
              | otherwise = bodyDoc b

pyClass :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
pyClass n pn s vs fs = vcat [
  s <+> classDec <+> text n <> pn <> colon,
  indent funcSec]
  where funcSec | isEmpty (vs <> fs) = text "None"
                | isEmpty vs = fs
                | isEmpty fs = vs
                | otherwise = vcat [vs, blank, fs]

pyInOutCall :: (Label -> VS (PythonCode (Type PythonCode)) -> 
  [VS (PythonCode (Value PythonCode))] -> VS (PythonCode (Value PythonCode))) 
  -> Label -> [VS (PythonCode (Value PythonCode))] -> 
  [VS (PythonCode (Variable PythonCode))] -> 
  [VS (PythonCode (Variable PythonCode))] -> 
  MS (PythonCode (Statement PythonCode))
pyInOutCall f n ins [] [] = valState $ f n void ins
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
    -> VS (PythonCode (Type PythonCode)) -> 
    [MS (PythonCode (Parameter PythonCode))] 
    -> MS (PythonCode (Body PythonCode)) -> MS (PythonCode (Method PythonCode)))
  -> PythonCode (Scope PythonCode) -> PythonCode (Permanence PythonCode) -> 
  [VS (PythonCode (Variable PythonCode))] -> 
  [VS (PythonCode (Variable PythonCode))] -> 
  [VS (PythonCode (Variable PythonCode))] -> MS (PythonCode (Body PythonCode)) 
  -> MS (PythonCode (Method PythonCode))
pyInOut f s p ins [] [] b = f s p void (map param ins) b
pyInOut f s p ins outs both b = f s p void (map param $ both ++ ins) 
  (on3StateValues (on3CodeValues surroundBody) (multi $ map varDec outs) b 
  (multiReturn $ map valueOf rets))
  where rets = both ++ outs

pyDocInOut :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr) 
    -> [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
    [VS (repr (Variable repr))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, VS (repr (Variable repr)))] -> [(String, VS (repr (Variable repr)))]
  -> [(String, VS (repr (Variable repr)))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))
pyDocInOut f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is)
  (map fst $ bs ++ os) (f s p (map snd is) (map snd os) (map snd bs) b)
  