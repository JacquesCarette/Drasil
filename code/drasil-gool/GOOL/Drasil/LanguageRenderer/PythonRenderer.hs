{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python code is contained in this module
module GOOL.Drasil.LanguageRenderer.PythonRenderer (
  -- * Python Code Configuration -- defines syntax of all Python code
  PythonCode(..)
) where

import Utils.Drasil (blank, indent)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym(..), 
  InternalFile(..), KeywordSym(..), PermanenceSym(..), InternalPerm(..), 
  BodySym(..), BlockSym(..), InternalBlock(..), ControlBlockSym(..), 
  TypeSym(..), InternalType(..), UnaryOpSym(..), BinaryOpSym(..), 
  InternalOp(..), VariableSym(..), InternalVariable(..), ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  InternalValue(..), Selector(..), FunctionSym(..), SelectorFunction(..), 
  InternalFunction(..), InternalStatement(..), StatementSym(..), 
  ControlStatementSym(..), ScopeSym(..), InternalScope(..), MethodTypeSym(..), 
  ParameterSym(..), InternalParam(..), MethodSym(..), InternalMethod(..), 
  StateVarSym(..), InternalStateVar(..), ClassSym(..), InternalClass(..), 
  ModuleSym(..), InternalMod(..), BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (enumElementsDocD', multiStateDocD, 
  bodyDocD, outDoc, destructorError,
  multiAssignDoc, returnDocD, mkStNoEnd, stringListVals', stringListLists', 
  breakDocD, continueDocD, unOpPrec, notOpDocD', 
  negateOpDocD, sqrtOpDocD', absOpDocD', expOpDocD', sinOpDocD', cosOpDocD', 
  tanOpDocD', asinOpDocD', acosOpDocD', atanOpDocD', unExpr, unExpr', 
  typeUnExpr, powerPrec, multPrec, andPrec, orPrec, equalOpDocD, notEqualOpDocD,
  greaterOpDocD, greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, 
  minusOpDocD, multOpDocD, divideOpDocD, moduloOpDocD, binExpr, typeBinExpr, 
  mkVal, mkVar, litCharD, litFloatD, litIntD, litStringD, classVarDocD, 
  newObjDocD', varD, staticVarD, extVarD, enumVarD, classVarD, objVarD, 
  objVarSelfD, listVarD, listOfD, iterVarD, valueOfD, argD, enumElementD, 
  argsListD, objAccessD, objMethodCallD, objMethodCallNoParamsD, selfAccessD, 
  listIndexExistsD, indexOfD, funcAppD, selfFuncAppD, extFuncAppD, newObjD, 
  listSetFuncDocD, castObjDocD, funcD, getD, setD, listAddD, listAppendD, 
  iterBeginD, iterEndD, listAccessD, listSetD, getFuncD, setFuncD, listAddFuncD,
  listAppendFuncD, iterBeginError, iterEndError, listAccessFuncD, listSetFuncD, 
  dynamicDocD, bindingError, classDec, dot, forLabel, inLabel, observerListName,
  commentedItem, addCommentsDocD, commentedModD, docFuncRepr, valueList, 
  parameterList, surroundBody, filterOutObjs)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  oneLiner, block, int, float, listInnerType, obj, enumType, runStrategy, 
  state, loopState, emptyState, assign, assignToListIndex, decrement, 
  increment', increment1', decrement1, objDecNew, objDecNewNoParams, closeFile, 
  discardFileLine, returnState, valState, comment, throw, initState, 
  changeState, initObserverList, addObserver, ifCond, ifNoElse, switchAsIf, 
  ifExists, tryCatch, checkState, construct, param, method, getMethod, 
  setMethod, privMethod, pubMethod, constructor, function, docFunc, stateVarDef,
  constVar, privMVar, pubMVar, pubGVar, buildClass, privClass, pubClass, 
  docClass, commentedClass, buildModule, modFromData, fileDoc, docMod, 
  fileFromData)
import GOOL.Drasil.Data (Terminator(..), ScopeTag(..), FileType(..), 
  FileData(..), fileD, FuncData(..), fd, ModData(..), md, updateModDoc, 
  MethodData(..), mthd, updateMthdDoc, OpData(..), ParamData(..), pd, 
  ProgData(..), progD, TypeData(..), td, ValData(..), vd, VarData(..), vard)
import GOOL.Drasil.Helpers (emptyIfEmpty, toCode, toState, onCodeValue,
  onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues,
  onCodeList, onStateList, on1CodeValue1List, on1StateValue1List)
import GOOL.Drasil.State (GS, MS, lensGStoFS, lensMStoGS, setCurrMain)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad.State (modify)
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

instance RenderSym PythonCode where
  type RenderFile PythonCode = FileData
  -- temporary evalState until I add more state
  fileDoc = G.fileDoc Combined pyExt top bottom

  docMod = G.docMod

  commentedMod cmt m = on2StateValues (on2CodeValues commentedModD) m cmt

instance InternalFile PythonCode where
  top _ = toCode pytop
  bottom = toCode empty

  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym PythonCode where
  type Keyword PythonCode = Doc
  endStatement = toCode empty
  endStatementLoop = toCode empty

  include n = toCode $ pyInclude n
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

  bodyDoc = unPC

instance BlockSym PythonCode where
  type Block PythonCode = Doc
  block = G.block endStatement

instance InternalBlock PythonCode where
  blockDoc = unPC
  docBlock = onStateValue toCode

instance TypeSym PythonCode where
  type Type PythonCode = TypeData
  bool = typeFromData Boolean "" empty
  int = G.int
  float = G.float
  char = typeFromData Char "" empty
  string = pyStringType
  infile = typeFromData File "" empty
  outfile = typeFromData File "" empty
  listType _ t = typeFromData (List (getType t)) "[]" (brackets empty)
  listInnerType = G.listInnerType
  obj = G.obj
  enumType = G.enumType
  iterator t = t
  void = typeFromData Void "NoneType" (text "NoneType")

  getType = cType . unPC
  getTypeString = typeString . unPC
  getTypeDoc = typeDoc . unPC

instance InternalType PythonCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym PythonCode where
  runStrategy = G.runStrategy

  listSlice vnew vold b e s = toState $ toCode $ pyListSlice vnew vold (getVal  
    b) (getVal e) (getVal s)
    where getVal = fromMaybe (mkVal void empty)

instance UnaryOpSym PythonCode where
  type UnaryOp PythonCode = OpData
  notOp = toCode notOpDocD'
  negateOp = toCode negateOpDocD
  sqrtOp = toCode sqrtOpDocD'
  absOp = toCode absOpDocD'
  logOp = toCode pyLogOp
  lnOp = toCode pyLnOp
  expOp = toCode expOpDocD'
  sinOp = toCode sinOpDocD'
  cosOp = toCode cosOpDocD'
  tanOp = toCode tanOpDocD'
  asinOp = toCode asinOpDocD'
  acosOp = toCode acosOpDocD'
  atanOp = toCode atanOpDocD'
  floorOp = toCode $ unOpPrec "math.floor"
  ceilOp = toCode $ unOpPrec "math.ceil"

instance BinaryOpSym PythonCode where
  type BinaryOp PythonCode = OpData
  equalOp = toCode equalOpDocD
  notEqualOp = toCode notEqualOpDocD
  greaterOp = toCode greaterOpDocD
  greaterEqualOp = toCode greaterEqualOpDocD
  lessOp = toCode lessOpDocD
  lessEqualOp = toCode lessEqualOpDocD
  plusOp = toCode plusOpDocD
  minusOp = toCode minusOpDocD
  multOp = toCode multOpDocD
  divideOp = toCode divideOpDocD
  powerOp = toCode $ powerPrec "**"
  moduloOp = toCode moduloOpDocD
  andOp = toCode $ andPrec "and"
  orOp = toCode $ orPrec "or"

instance InternalOp PythonCode where
  uOpDoc = opDoc . unPC
  bOpDoc = opDoc . unPC
  uOpPrec = opPrec . unPC
  bOpPrec = opPrec . unPC

instance VariableSym PythonCode where
  type Variable PythonCode = VarData
  var = varD
  staticVar = staticVarD
  const = var
  extVar = extVarD
  self l = mkVar "self" (obj l) (text "self")
  enumVar = enumVarD
  classVar = classVarD classVarDocD
  extClassVar = classVarD pyClassVar
  objVar = objVarD
  objVarSelf = objVarSelfD
  listVar = listVarD
  listOf = listOfD
  iterVar = iterVarD

  ($->) = objVar

  variableBind = varBind . unPC
  variableName = varName . unPC
  variableType = onCodeValue varType
  variableDoc = varDoc . unPC

instance InternalVariable PythonCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym PythonCode where
  type Value PythonCode = ValData
  litTrue = mkVal bool (text "True")
  litFalse = mkVal bool (text "False")
  litChar = litCharD
  litFloat = litFloatD
  litInt = litIntD
  litString = litStringD

  pi = mkVal float (text "math.pi")

  ($:) = enumElement

  valueOf = valueOfD
  arg n = argD (litInt $ n+1) argsList
  enumElement = enumElementD
  argsList = argsListD "sys.argv"

  valueType = onCodeValue valType
  valueDoc = valDoc . unPC

instance NumericExpression PythonCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) v1 v2 = pyDivision (getType $ valueType v1) (getType $ valueType v2) 
    where pyDivision Integer Integer = binExpr (toCode $ multPrec "//") v1 v2
          pyDivision _ _ = binExpr divideOp v1 v2
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
  funcApp = funcAppD
  selfFuncApp c = selfFuncAppD (self c)
  extFuncApp = extFuncAppD
  newObj = newObjD newObjDocD'
  extNewObj l t vs = mkVal t (pyExtStateObj l (getTypeDoc t) (valueList vs))

  exists v = v ?!= valueOf (var "None" void)
  notNull = exists

instance InternalValue PythonCode where
  inputFunc = mkVal string (text "input()")  -- raw_input() for < Python 3.0
  printFunc = mkVal void (text "print")
  printLnFunc = mkVal void empty
  printFileFunc _ = mkVal void empty
  printFileLnFunc _ = mkVal void empty
  
  cast t v = mkVal t $ castObjDocD (getTypeDoc t) (valueDoc v)

  valuePrec = valPrec . unPC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector PythonCode where
  objAccess = objAccessD
  ($.) = objAccess 

  objMethodCall = objMethodCallD
  objMethodCallNoParams = objMethodCallNoParamsD

  selfAccess = selfAccessD

  listIndexExists = listIndexExistsD
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf = indexOfD "index"

instance FunctionSym PythonCode where
  type Function PythonCode = FuncData
  func = funcD

  get = getD
  set = setD

  listSize v = mkVal (functionType listSizeFunc) 
    (pyListSize (valueDoc v) (functionDoc (listSizeFunc :: PythonCode (Function PythonCode))))
  listAdd = listAddD
  listAppend = listAppendD

  iterBegin = iterBeginD
  iterEnd = iterEndD

instance SelectorFunction PythonCode where
  listAccess = listAccessD
  listSet = listSetD
  at = listAccess

instance InternalFunction PythonCode where
  getFunc = getFuncD
  setFunc = setFuncD

  listSizeFunc = funcFromData int (text "len")
  listAddFunc _ = listAddFuncD "insert"
  listAppendFunc = listAppendFuncD "append"

  iterBeginFunc _ = error $ iterBeginError pyName
  iterEndFunc _ = error $ iterEndError pyName

  listAccessFunc = listAccessFuncD 
  listSetFunc = listSetFuncD listSetFuncDocD

  functionType = onCodeValue funcType
  functionDoc = funcDoc . unPC

  funcFromData t d = on2CodeValues fd t (toCode d)

instance InternalStatement PythonCode where
  printSt nl p v f = toState $ mkStNoEnd $ pyPrint nl p v (fromMaybe (mkVal 
    void empty) f)

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
  multiAssign vrs vls = toState $ mkStNoEnd $ multiAssignDoc vrs vls
  (&=) = assign
  (&-=) = G.decrement
  (&+=) = G.increment'
  (&++) = G.increment1'
  (&~-) = G.decrement1

  varDec _ = toState $ mkStNoEnd empty
  varDecDef = assign
  listDec _ v = toState $ mkStNoEnd $ pyListDec v
  listDecDef v vs = toState $ mkStNoEnd $ pyListDecDef v vs
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew lib v vs = varDecDef v (extNewObj lib (variableType v) vs)
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams lib v = varDecDef v (extNewObj lib (variableType v) [])
  constDecDef = varDecDef

  print v = pyOut False printFunc v Nothing
  printLn v = pyOut True printFunc v Nothing
  printStr s = print (litString s)
  printStrLn s = printLn (litString s)

  printFile f v = pyOut False printFunc v (Just f)
  printFileLn f v = pyOut True printFunc v (Just f)
  printFileStr f s = printFile f (litString s)
  printFileStrLn f s = printFileLn f (litString s)

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

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = toState $ mkStNoEnd breakDocD
  continue = toState $ mkStNoEnd continueDocD

  returnState = G.returnState Empty
  multiReturn [] = error "Attempt to write return statement with no return variables"
  multiReturn vs = toState $ mkStNoEnd $ returnDocD vs

  valState = G.valState Empty

  comment = G.comment commentStart

  free v = v &= valueOf (var "None" void)

  throw = G.throw pyThrow Empty

  initState = G.initState
  changeState = G.changeState

  initObserverList = G.initObserverList
  addObserver = G.addObserver

  inOutCall = pyInOutCall funcApp
  selfInOutCall c = pyInOutCall (selfFuncApp c)
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
  forRange i initv finalv stepv = onStateValue (mkStNoEnd . pyForRange i 
    iterInLabel initv finalv stepv)
  forEach e v = onStateValue (mkStNoEnd . pyForEach e iterForEachLabel 
    iterInLabel v)
  while v = onStateValue (mkStNoEnd . pyWhile v)

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
  mType t = t
  construct = toCode . G.construct

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
  destructor _ _ = error $ destructorError pyName

  docMain = mainFunction

  function = G.function
  mainFunction b = modify setCurrMain >> onStateValue (onCodeValue mthd) 
    (zoom lensMStoGS b)

  docFunc = G.docFunc

  inOutMethod n c = pyInOut (method n c)

  docInOutMethod n c = pyDocInOut (inOutMethod n c)

  inOutFunc n = pyInOut (function n)

  docInOutFunc n = pyDocInOut (inOutFunc n)

instance InternalMethod PythonCode where
  intMethod m n l _ _ _ ps b = modify (if m then setCurrMain else id) >> 
    on1StateValue1List (\bd pms -> methodFromData Pub $ pyMethod n (self l) pms 
    bd) (zoom lensMStoGS b) ps
  intFunc m n _ _ _ ps b = modify (if m then setCurrMain else id) >>
    on1StateValue1List (\bd pms -> methodFromData Pub $ pyFunction n pms bd) 
    (zoom lensMStoGS b) ps
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
  enum n es s = classFromData (toState $ pyClass n empty (scopeDoc s)
    (enumElementsDocD' es) empty)
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass = G.commentedClass

instance InternalClass PythonCode where
  classDoc = unPC
  classFromData = onStateValue toCode

instance ModuleSym PythonCode where
  type Module PythonCode = ModData
  buildModule n ls = G.buildModule n (map include ls)

instance InternalMod PythonCode where
  moduleDoc = modDoc . unPC
  modFromData n = G.modFromData n (\d m -> toCode $ md n m d)
  updateModuleDoc f = onCodeValue (updateModDoc f)

instance BlockCommentSym PythonCode where
  type BlockComment PythonCode = Doc
  blockComment lns = onCodeValue (pyBlockComment lns) commentStart
  docComment = onStateValue (\lns -> on2CodeValues (pyDocComment lns) 
    docCommentStart commentStart)

  blockCommentDoc = unPC

-- convenience
imp, initName :: Label
imp = "import"
initName = "__init__"

pyName :: String
pyName = "Python"

pytop :: Doc 
pytop = vcat [   -- There are also imports from the libraries supplied by module. These will be handled by module.
  text imp <+> text "sys",
  text imp <+> text "math"] 

pyInclude :: Label -> Doc
pyInclude n = text imp <+> text n

pyLogOp :: OpData
pyLogOp = unOpPrec "math.log10"

pyLnOp :: OpData
pyLnOp = unOpPrec "math.log"

pyClassVar :: Doc -> Doc -> Doc
pyClassVar c v = c <> dot <> c <> dot <> v

pyExtStateObj :: Label -> Doc -> Doc -> Doc
pyExtStateObj l t vs = text l <> dot <> t <> parens vs

pyInlineIf :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr)
pyInlineIf c v1 v2 = valFromData (valuePrec c) (valueType v1) 
  (valueDoc v1 <+> text "if" <+> valueDoc c <+> text "else" <+> valueDoc v2)

pyListSize :: Doc -> Doc -> Doc
pyListSize v f = f <> parens v

pyStringType :: (RenderSym repr) => repr (Type repr)
pyStringType = typeFromData String "str" (text "str")

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

pyOut :: (RenderSym repr) => Bool -> repr (Value repr) -> repr (Value repr) 
  -> Maybe (repr (Value repr)) -> GS (repr (Statement repr))
pyOut newLn printFn v f = pyOut' (getType $ valueType v)
  where pyOut' (List _) = printSt newLn printFn v f
        pyOut' _ = outDoc newLn printFn v f

pyInput :: PythonCode (Value PythonCode) -> PythonCode (Variable PythonCode) -> 
  GS (PythonCode (Statement PythonCode))
pyInput inSrc v = v &= pyInput' (getType $ variableType v)
  where pyInput' Integer = funcApp "int" int [inSrc]
        pyInput' Float = funcApp "float" float [inSrc]
        pyInput' Boolean = inSrc ?!= litString "0"
        pyInput' String = objMethodCall string inSrc "rstrip" []
        pyInput' Char = inSrc
        pyInput' _ = error "Attempt to read a value of unreadable type"

pyThrow :: (RenderSym repr) => repr (Value repr) -> Doc
pyThrow errMsg = text "raise" <+> text "Exception" <> parens (valueDoc errMsg)

pyForRange :: (RenderSym repr) => repr (Variable repr) -> repr (Keyword repr) 
  -> repr (Value repr) -> repr (Value repr) -> repr (Value repr) -> 
  repr (Body repr) -> Doc
pyForRange i inLbl initv finalv stepv b = vcat [
  forLabel <+> variableDoc i <+> keyDoc inLbl <+> text "range" <> parens 
    (valueDoc initv <> text ", " <> valueDoc finalv <> text ", " <> valueDoc 
    stepv) <> colon,
  indent $ bodyDoc b]

pyForEach :: (RenderSym repr) => repr (Variable repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> repr (Value repr) -> repr (Body repr) -> Doc
pyForEach i forEachLabel inLbl lstVar b = vcat [
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

pyListSlice :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr) -> repr (Value repr) -> Doc
pyListSlice vnew vold b e s = variableDoc vnew <+> equals <+> valueDoc vold <> 
  brackets (valueDoc b <> colon <> valueDoc e <> colon <> valueDoc s)

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

pyInOutCall :: (Label -> PythonCode (Type PythonCode) -> 
  [PythonCode (Value PythonCode)] -> PythonCode (Value PythonCode)) -> Label -> 
  [PythonCode (Value PythonCode)] -> [PythonCode (Variable PythonCode)] -> 
  [PythonCode (Variable PythonCode)] -> GS (PythonCode (Statement PythonCode))
pyInOutCall f n ins [] [] = valState $ f n void ins
pyInOutCall f n ins outs both = if null rets then valState (f n void (map 
  valueOf both ++ ins)) else multiAssign (filterOutObjs both ++ outs) 
  [f n void (map valueOf both ++ ins)]
  where rets = filterOutObjs both ++ outs

pyBlockComment :: [String] -> Doc -> Doc
pyBlockComment lns cmt = vcat $ map ((<+>) cmt . text) lns

pyDocComment :: [String] -> Doc -> Doc -> Doc
pyDocComment [] _ _ = empty
pyDocComment (l:lns) start mid = vcat $ start <+> text l : map ((<+>) mid . 
  text) lns

pyInOut :: (PythonCode (Scope PythonCode) -> PythonCode (Permanence PythonCode) 
    -> PythonCode (Type PythonCode) -> [MS (PythonCode (Parameter PythonCode))] 
    -> GS (PythonCode (Body PythonCode)) -> MS (PythonCode (Method PythonCode)))
  -> PythonCode (Scope PythonCode) -> PythonCode (Permanence PythonCode) -> 
  [PythonCode (Variable PythonCode)] -> [PythonCode (Variable PythonCode)] -> 
  [PythonCode (Variable PythonCode)] -> GS (PythonCode (Body PythonCode)) -> 
  MS (PythonCode (Method PythonCode))
pyInOut f s p ins [] [] b = f s p void (map param ins) b
pyInOut f s p ins outs both b = f s p void (map param $ both ++ ins) 
  (if null rets then b else on3StateValues (on3CodeValues surroundBody) 
  (multi $ map varDec outs) b (multiReturn $ map valueOf rets))
  where rets = filterOutObjs both ++ outs

pyDocInOut :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr) 
    -> [repr (Variable repr)] -> [repr (Variable repr)] -> 
    [repr (Variable repr)] -> GS (repr (Body repr)) -> MS (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, repr (Variable repr))] -> [(String, repr (Variable repr))] -> 
  [(String, repr (Variable repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
pyDocInOut f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is)
  (map fst $ bRets ++ os) (f s p (map snd is) (map snd os) (map snd bs) b)
  where bRets = filter (not . isObject . getType . variableType . snd) bs
