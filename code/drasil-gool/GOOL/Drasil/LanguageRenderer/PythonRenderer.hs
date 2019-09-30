{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python code is contained in this module
module GOOL.Drasil.LanguageRenderer.PythonRenderer (
  -- * Python Code Configuration -- defines syntax of all Python code
  PythonCode(..)
) where

import Utils.Drasil (blank, indent)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label,
  ProgramSym(..), RenderSym(..), InternalFile(..),
  KeywordSym(..), PermanenceSym(..), BodySym(..), BlockSym(..), 
  ControlBlockSym(..), TypeSym(..), UnaryOpSym(..), BinaryOpSym(..), 
  VariableSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), InternalValue(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..), 
  ParameterSym(..), MethodSym(..), InternalMethod(..), StateVarSym(..), 
  ClassSym(..), ModuleSym(..), BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (addExt, fileDoc', enumElementsDocD', 
  multiStateDocD, blockDocD, bodyDocD, oneLinerD, outDoc, intTypeDocD, 
  floatTypeDocD, typeDocD, enumTypeDocD, listInnerTypeD, constructDocD, 
  paramListDocD, mkParam, methodListDocD, stateVarListDocD, ifCondDocD, 
  runStrategyD, assignDocD, multiAssignDoc, plusEqualsDocD', plusPlusDocD', 
  statementDocD, returnDocD, commentDocD, mkStNoEnd, stringListVals', 
  stringListLists', printStD, stateD, loopStateD, emptyStateD, assignD, assignToListIndexD, decrementD, decrement1D, closeFileD, discardFileLineD,unOpPrec, notOpDocD', 
  negateOpDocD, sqrtOpDocD', absOpDocD',
  expOpDocD', sinOpDocD', cosOpDocD', tanOpDocD', asinOpDocD', acosOpDocD', 
  atanOpDocD', unExpr, unExpr', typeUnExpr, powerPrec, multPrec, andPrec, 
  orPrec, equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, 
  lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, 
  divideOpDocD, moduloOpDocD, binExpr, typeBinExpr, mkVal, mkVar, litCharD, 
  litFloatD, litIntD, litStringD, classVarD, newObjDocD', varD, staticVarD, 
  extVarD, enumVarD, classVarD, objVarD, objVarSelfD, listVarD, listOfD, 
  iterVarD, valueOfD, argD, enumElementD, argsListD, objAccessD, objMethodCallD,
  objMethodCallNoParamsD, selfAccessD, listIndexExistsD, indexOfD, funcAppD, 
  extFuncAppD, newObjD, listSetFuncDocD, castObjDocD, funcD, getD, setD, 
  listAddD, listAppendD, iterBeginD, iterEndD, listAccessD, listSetD, getFuncD, 
  setFuncD, listAddFuncD, listAppendFuncD, iterBeginError, iterEndError, 
  listAccessFuncD, listSetFuncD, breakDocD, continueDocD, dynamicDocD, classDec,
  dot, forLabel, inLabel, observerListName, commentedItem, addCommentsDocD, 
  classDoc, moduleDoc, commentedModD, docFuncRepr, valList, surroundBody, 
  getterName, setterName, filterOutObjs)
import GOOL.Drasil.Data (Terminator(..), FileData(..), file, FuncData(..), fd, 
  ModData(..), md, updateModDoc, MethodData(..), mthd, OpData(..), 
  ParamData(..), ProgData(..), progD, TypeData(..), td, ValData(..), vd,
  VarData(..), vard)
import GOOL.Drasil.Helpers (vibcat, 
  emptyIfEmpty, liftA4, liftA5, liftA6, liftList, lift1List, lift2Lists, 
  lift4Pair, liftPair, liftPairFst, checkParams)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($+$), parens, empty,
  equals, vcat, colon, brackets, isEmpty)

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
  prog n = liftList (progD n)

instance RenderSym PythonCode where
  type RenderFile PythonCode = FileData
  fileDoc code = liftA2 file (fmap (addExt pyExt . name) code) (liftA2 
    updateModDoc (liftA2 emptyIfEmpty (fmap modDoc code) $ liftA3 fileDoc' 
    (top code) (fmap modDoc code) bottom) code)

  docMod d a dt m = commentedMod (docComment $ moduleDoc d a dt $ filePath 
    (unPC m)) m

  commentedMod = liftA2 commentedModD

instance InternalFile PythonCode where
  top _ = return pytop
  bottom = return empty

instance KeywordSym PythonCode where
  type Keyword PythonCode = Doc
  endStatement = return empty
  endStatementLoop = return empty

  include n = return $ pyInclude n
  inherit = return empty

  list _ = return empty

  blockStart = return colon
  blockEnd = return empty

  ifBodyStart = blockStart
  elseIf = return $ text "elif"
  
  iterForEachLabel = return forLabel
  iterInLabel = return inLabel

  commentStart = return $ text "#"
  blockCommentStart = return empty
  blockCommentEnd = return empty
  docCommentStart = return $ text "##"
  docCommentEnd = return empty

instance PermanenceSym PythonCode where
  type Permanence PythonCode = Doc
  static_ = return empty
  dynamic_ = return dynamicDocD

instance BodySym PythonCode where
  type Body PythonCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner = oneLinerD

  addComments s = liftA2 (addCommentsDocD s) commentStart

  bodyDoc = unPC

instance BlockSym PythonCode where
  type Block PythonCode = Doc
  block sts = lift1List blockDocD endStatement (map (fmap fst . state) sts)

  docBlock = return

instance TypeSym PythonCode where
  type Type PythonCode = TypeData
  bool = return $ td Boolean "" empty
  int = return intTypeDocD
  float = return floatTypeDocD
  char = return $ td Char "" empty
  string = return pyStringType
  infile = return $ td File "" empty
  outfile = return $ td File "" empty
  listType _ t = return $ td (List (getType t)) "[]" (brackets empty)
  listInnerType = listInnerTypeD
  obj t = return $ typeDocD t
  enumType t = return $ enumTypeDocD t
  iterator t = t
  void = return $ td Void "NoneType" (text "NoneType")

  getType = cType . unPC
  getTypeString = typeString . unPC
  getTypeDoc = typeDoc . unPC

instance ControlBlockSym PythonCode where
  runStrategy = runStrategyD

  listSlice vnew vold b e s = liftA5 pyListSlice vnew vold (getVal b) 
    (getVal e) (getVal s)
    where getVal = fromMaybe (liftA2 mkVal void (return empty))

instance UnaryOpSym PythonCode where
  type UnaryOp PythonCode = OpData
  notOp = return notOpDocD'
  negateOp = return negateOpDocD
  sqrtOp = return sqrtOpDocD'
  absOp = return absOpDocD'
  logOp = return pyLogOp
  lnOp = return pyLnOp
  expOp = return expOpDocD'
  sinOp = return sinOpDocD'
  cosOp = return cosOpDocD'
  tanOp = return tanOpDocD'
  asinOp = return asinOpDocD'
  acosOp = return acosOpDocD'
  atanOp = return atanOpDocD'
  floorOp = return $ unOpPrec "math.floor"
  ceilOp = return $ unOpPrec "math.ceil"

instance BinaryOpSym PythonCode where
  type BinaryOp PythonCode = OpData
  equalOp = return equalOpDocD
  notEqualOp = return notEqualOpDocD
  greaterOp = return greaterOpDocD
  greaterEqualOp = return greaterEqualOpDocD
  lessOp = return lessOpDocD
  lessEqualOp = return lessEqualOpDocD
  plusOp = return plusOpDocD
  minusOp = return minusOpDocD
  multOp = return multOpDocD
  divideOp = return divideOpDocD
  powerOp = return $ powerPrec "**"
  moduloOp = return moduloOpDocD
  andOp = return $ andPrec "and"
  orOp = return $ orPrec "or"

instance VariableSym PythonCode where
  type Variable PythonCode = VarData
  var = varD
  staticVar = staticVarD
  const = var
  extVar = extVarD
  self l = liftA2 (mkVar "self") (obj l) (return $ text "self")
  enumVar = enumVarD
  classVar = classVarD pyClassVar
  objVar = objVarD
  objVarSelf = objVarSelfD
  listVar = listVarD
  listOf = listOfD
  iterVar = iterVarD

  ($->) = objVar

  variableBind = varBind . unPC
  variableName = varName . unPC
  variableType = fmap varType
  variableDoc = varDoc . unPC

  varFromData b n t d = liftA2 (vard b n) t (return d)

instance ValueSym PythonCode where
  type Value PythonCode = ValData
  litTrue = liftA2 mkVal bool (return $ text "True")
  litFalse = liftA2 mkVal bool (return $ text "False")
  litChar = litCharD
  litFloat = litFloatD
  litInt = litIntD
  litString = litStringD

  ($:) = enumElement

  valueOf = valueOfD
  arg n = argD (litInt $ n+1) argsList
  enumElement = enumElementD
  argsList = argsListD "sys.argv"

  valueType = fmap valType
  valueDoc = valDoc . unPC

instance NumericExpression PythonCode where
  (#~) = liftA2 unExpr' negateOp
  (#/^) = liftA2 unExpr sqrtOp
  (#|) = liftA2 unExpr absOp
  (#+) = liftA3 binExpr plusOp
  (#-) = liftA3 binExpr minusOp
  (#*) = liftA3 binExpr multOp
  (#/) v1 v2 = pyDivision (getType $ valueType v1) (getType $ valueType v2) 
    where pyDivision Integer Integer = liftA2 (binExpr (multPrec "//")) v1 v2
          pyDivision _ _ = liftA3 binExpr divideOp v1 v2
  (#%) = liftA3 binExpr moduloOp
  (#^) = liftA3 binExpr powerOp

  log = liftA2 unExpr logOp
  ln = liftA2 unExpr lnOp
  exp = liftA2 unExpr expOp
  sin = liftA2 unExpr sinOp
  cos = liftA2 unExpr cosOp
  tan = liftA2 unExpr tanOp
  csc v = litFloat 1.0 #/ sin v
  sec v = litFloat 1.0 #/ cos v
  cot v = litFloat 1.0 #/ tan v
  arcsin = liftA2 unExpr asinOp
  arccos = liftA2 unExpr acosOp
  arctan = liftA2 unExpr atanOp
  floor = liftA2 unExpr floorOp
  ceil = liftA2 unExpr ceilOp

instance BooleanExpression PythonCode where
  (?!) = liftA3 typeUnExpr notOp bool
  (?&&) = liftA4 typeBinExpr andOp bool
  (?||) = liftA4 typeBinExpr orOp bool

  (?<) = liftA4 typeBinExpr lessOp bool
  (?<=) = liftA4 typeBinExpr lessEqualOp bool
  (?>) = liftA4 typeBinExpr greaterOp bool
  (?>=) = liftA4 typeBinExpr greaterEqualOp bool
  (?==) = liftA4 typeBinExpr equalOp bool
  (?!=) = liftA4 typeBinExpr notEqualOp bool

instance ValueExpression PythonCode where
  inlineIf = liftA3 pyInlineIf
  funcApp = funcAppD
  selfFuncApp = funcApp
  extFuncApp = extFuncAppD
  newObj = newObjD newObjDocD'
  extNewObj l t vs = liftA2 mkVal t (liftA2 (pyExtStateObj l) t (liftList 
    valList vs))

  exists v = v ?!= valueOf (var "None" void)
  notNull = exists

instance InternalValue PythonCode where
  inputFunc = liftA2 mkVal string (return $ text "input()")  -- raw_input() for < Python 3.0
  printFunc = liftA2 mkVal void (return $ text "print")
  printLnFunc = liftA2 mkVal void (return empty)
  printFileFunc _ = liftA2 mkVal void (return empty)
  printFileLnFunc _ = liftA2 mkVal void (return empty)
  
  cast t v = liftA2 mkVal t $ liftA2 castObjDocD (fmap typeDoc t) v

  valFromData p t d = liftA2 (vd p) t (return d)

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

  listSize v = liftA2 mkVal (fmap funcType listSizeFunc) 
    (liftA2 pyListSize v listSizeFunc)
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

  listSizeFunc = liftA2 fd int (return $ text "len")
  listAddFunc _ = listAddFuncD "insert"
  listAppendFunc = listAppendFuncD "append"

  iterBeginFunc _ = error $ iterBeginError pyName
  iterEndFunc _ = error $ iterEndError pyName

  listAccessFunc = listAccessFuncD 
  listSetFunc = listSetFuncD listSetFuncDocD

  functionType = fmap funcType
  functionDoc = funcDoc . unPC

  funcFromData t d = liftA2 fd t (return d)

instance InternalStatement PythonCode where
  printSt nl p v f = mkStNoEnd <$> liftA3 (pyPrint nl) p v 
    (fromMaybe (liftA2 mkVal void (return empty)) f)

  state = stateD
  loopState = loopStateD
  
  emptyState = emptyStateD
  statementDoc = fst . unPC
  statementTerm = snd . unPC

  stateFromData d t = return (d, t)

instance StatementSym PythonCode where
  -- Terminator determines how statements end
  type Statement PythonCode = (Doc, Terminator)
  assign = assignD Empty
  assignToListIndex = assignToListIndexD
  multiAssign vrs vls = mkStNoEnd <$> lift2Lists multiAssignDoc vrs vls
  (&=) = assign
  (&-=) = decrementD
  (&+=) vr vl = mkStNoEnd <$> liftA3 plusEqualsDocD' vr plusOp vl
  (&++) v = mkStNoEnd <$> liftA2 plusPlusDocD' v plusOp
  (&~-) = decrement1D

  varDec _ = return (mkStNoEnd empty)
  varDecDef = assign
  listDec _ v = mkStNoEnd <$> fmap pyListDec v
  listDecDef v vs = mkStNoEnd <$> liftA2 pyListDecDef v (liftList valList vs)
  objDecDef = varDecDef
  objDecNew v vs = varDecDef v (newObj (variableType v) vs)
  extObjDecNew lib v vs = varDecDef v (extNewObj lib (variableType v) vs)
  objDecNewNoParams v = varDecDef v (newObj (variableType v) [])
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
  closeFile = closeFileD "close"

  getFileInputLine = getFileInput
  discardFileLine = discardFileLineD "readline"
  stringSplit d vnew s = assign vnew (objAccess s (func "split" 
    (listType static_ string) [litString [d]]))  

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = return (mkStNoEnd breakDocD)
  continue = return (mkStNoEnd continueDocD)

  returnState v = mkStNoEnd <$> liftList returnDocD [v]
  multiReturn [] = error "Attempt to write return statement with no return variables"
  multiReturn vs = mkStNoEnd <$> liftList returnDocD vs

  valState v = mkStNoEnd <$> fmap valDoc v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free v = v &= valueOf (var "None" void)

  throw errMsg = mkStNoEnd <$> fmap pyThrow (litString errMsg)

  initState fsmName initialState = varDecDef (var fsmName string) 
    (litString initialState)
  changeState fsmName toState = var fsmName string &= litString toState

  initObserverList t = listDecDef (var observerListName t)
  addObserver o = valState $ listAdd obsList lastelem o
    where obsList = valueOf $ observerListName `listOf` valueType o
          lastelem = listSize obsList

  inOutCall = pyInOutCall funcApp
  extInOutCall m = pyInOutCall (extFuncApp m)

  multi = lift1List multiStateDocD endStatement

instance ControlStatementSym PythonCode where
  ifCond bs b = mkStNoEnd <$> lift4Pair ifCondDocD ifBodyStart elseIf blockEnd
    b bs
  ifNoElse bs = ifCond bs $ body []
  switch = switchAsIf
  switchAsIf v cs = ifCond cases
    where cases = map (first (v ?==)) cs

  ifExists v ifBody = ifCond [(notNull v, ifBody)]

  for _ _ _ _ = error $ "Classic for loops not available in Python, please " ++
    "use forRange, forEach, or while instead"
  forRange i initv finalv stepv b = mkStNoEnd <$> liftA6 pyForRange i
    iterInLabel initv finalv stepv b
  forEach e v b = mkStNoEnd <$> liftA5 pyForEach e iterForEachLabel 
    iterInLabel v b
  while v b = mkStNoEnd <$> liftA2 pyWhile v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 pyTryCatch tb cb

  checkState l = switch (valueOf $ var l string)
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
  private = return empty
  public = return empty

instance MethodTypeSym PythonCode where
  type MethodType PythonCode = TypeData
  mType t = t
  construct n = return $ td (Object n) n (constructDocD n)

instance ParameterSym PythonCode where
  type Parameter PythonCode = ParamData
  param = fmap (mkParam varDoc)
  pointerParam = param

  parameterName = variableName . fmap paramVar
  parameterType = variableType . fmap paramVar

instance MethodSym PythonCode where
  type Method PythonCode = MethodData
  method n c s p t = intMethod n c s p (mType t)
  getMethod c v = method (getterName $ variableName v) c public dynamic_ 
    (variableType v) [] getBody
    where getBody = oneLiner $ returnState (valueOf $ self c $-> v)
  setMethod c v = method (setterName $ variableName v) c public dynamic_ void 
    [param v] setBody
    where setBody = oneLiner $ (self c $-> v) &= valueOf v
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = intMethod initName n public dynamic_ (construct n)
  destructor _ _ = error "Destructors not allowed in Python"

  docMain = mainFunction

  function n s p t = intFunc n s p (mType t)
  mainFunction = fmap (mthd True [])

  docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

  inOutFunc n s p ins [] [] b = function n s p void (map param ins) b
  inOutFunc n s p ins outs both b = function n s p void (map 
    param $ both ++ ins) (if null rets then b else liftA3 surroundBody 
    (multi $ map varDec outs) b (multiReturn $ map valueOf rets))
    where rets = filterOutObjs both ++ outs

  docInOutFunc n s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is) 
    (map fst $ bRets ++ os) (inOutFunc n s p (map snd is) (map snd os) (map snd
    bs) b)
    where bRets = filter (not . isObject . getType . variableType . snd) bs

  parameters m = map return $ (mthdParams . unPC) m

instance InternalMethod PythonCode where
  intMethod n l _ _ _ ps b = liftA2 (mthd False) (checkParams n <$> sequence ps)
    (liftA3 (pyMethod n) (self l) (liftList paramListDocD ps) b)
  intFunc n _ _ _ ps b = liftA2 (mthd False) (checkParams n <$> sequence ps) 
    (liftA2 (pyFunction n) (liftList paramListDocD ps) b)
  commentedFunc cmt fn = liftA3 mthd (fmap isMainMthd fn) (fmap mthdParams fn)
    (liftA2 commentedItem cmt (fmap mthdDoc fn))

instance StateVarSym PythonCode where
  type StateVar PythonCode = Doc
  stateVar _ _ _ _ = return empty
  stateVarDef _ _ _ _ vr vl = fst <$> state (varDecDef vr vl)
  constVar _ _ _ vr vl = fst <$> state (constDecDef vr vl)
  privMVar del = stateVar del private dynamic_
  pubMVar del = stateVar del public dynamic_
  pubGVar del = stateVar del public static_

instance ClassSym PythonCode where
  type Class PythonCode = (Doc, Bool)
  buildClass n p _ vs fs = liftPairFst (liftA3 (pyClass n) pname (liftList stateVarListDocD vs) (liftList 
    methodListDocD (map (fmap mthdDoc) fs)), any (isMainMthd . unPC) fs)
    where pname = case p of Nothing -> return empty
                            Just pn -> return $ parens (text pn)
  enum n es _ = liftPairFst (liftA3 (pyClass n) (return empty) (return $ 
    enumElementsDocD' es) (return empty), False)
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public

  docClass d = commentedClass (docComment $ classDoc d)

  commentedClass cmt cs = liftPair (liftA2 commentedItem cmt (fmap fst cs), 
    fmap snd cs)

instance ModuleSym PythonCode where
  type Module PythonCode = ModData
  buildModule n ls fs cs = fmap (md n (any (isMainMthd . unPC) fs || 
    any (snd . unPC) cs)) (if all (isEmpty . fst . unPC) cs && all 
    (isEmpty . mthdDoc . unPC) fs then return empty else
    liftA3 pyModule (liftList pyModuleImportList (map include ls)) 
    (liftList methodListDocD (map (fmap mthdDoc) fs)) (liftList 
    pyModuleClassList cs))

  moduleName m = name (unPC m)

instance BlockCommentSym PythonCode where
  type BlockComment PythonCode = Doc
  blockComment lns = fmap (pyBlockComment lns) commentStart
  docComment lns = liftA2 (pyDocComment lns) docCommentStart commentStart

-- convenience
imp, incl, initName :: Label
imp = "import"
incl = "from"
initName = "__init__"

pyName :: String
pyName = "Python"

pytop :: Doc 
pytop = vcat [   -- There are also imports from the libraries supplied by module. These will be handled by module.
  text incl <+> text "__future__" <+> text imp <+> text "print_function",
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

pyExtStateObj :: Label -> TypeData -> Doc -> Doc
pyExtStateObj l t vs = text l <> dot <> typeDoc t <> parens vs

pyInlineIf :: ValData -> ValData -> ValData -> ValData
pyInlineIf c v1 v2 = vd (valPrec c) (valType v1) (valDoc v1 <+> text "if" <+> 
  valDoc c <+> text "else" <+> valDoc v2)

pyListSize :: ValData -> FuncData -> Doc
pyListSize v f = funcDoc f <> parens (valDoc v)

pyStringType :: TypeData
pyStringType = td String "str" (text "str")

pyListDec :: VarData -> Doc
pyListDec v = varDoc v <+> equals <+> typeDoc (varType v)

pyListDecDef :: VarData -> Doc -> Doc
pyListDecDef v vs = varDoc v <+> equals <+> brackets vs

pyPrint :: Bool ->  ValData -> ValData -> ValData -> Doc
pyPrint newLn prf v f = valDoc prf <> parens (valDoc v <> nl <> fl)
  where nl = if newLn then empty else text ", end=''"
        fl = emptyIfEmpty (valDoc f) $ text ", file=" <> valDoc f

pyOut :: (RenderSym repr) => Bool -> repr (Value repr) -> repr (Value repr) 
  -> Maybe (repr (Value repr)) -> repr (Statement repr)
pyOut newLn printFn v f = pyOut' (getType $ valueType v)
  where pyOut' (List _) = printSt newLn printFn v f
        pyOut' _ = outDoc newLn printFn v f

pyInput :: PythonCode (Value PythonCode) -> PythonCode (Variable PythonCode) -> 
  PythonCode (Statement PythonCode)
pyInput inSrc v = v &= pyInput' (getType $ variableType v)
  where pyInput' Integer = funcApp "int" int [inSrc]
        pyInput' Float = funcApp "float" float [inSrc]
        pyInput' Boolean = inSrc ?!= litString "0"
        pyInput' String = objMethodCall string inSrc "rstrip" []
        pyInput' Char = inSrc
        pyInput' _ = error "Attempt to read a value of unreadable type"

pyThrow ::  ValData -> Doc
pyThrow errMsg = text "raise" <+> text "Exception" <> parens (valDoc errMsg)

pyForRange :: VarData -> Doc ->  ValData ->  ValData ->
  ValData -> Doc -> Doc
pyForRange i inLbl initv finalv stepv b = vcat [
  forLabel <+> varDoc i <+> inLbl <+> text "range" <> parens (valDoc initv <> 
    text ", " <> valDoc finalv <> text ", " <> valDoc stepv) <> colon,
  indent b]

pyForEach :: VarData -> Doc -> Doc ->  ValData -> Doc -> Doc
pyForEach i forEachLabel inLbl lstVar b = vcat [
  forEachLabel <+> varDoc i <+> inLbl <+> valDoc lstVar <> colon,
  indent b]

pyWhile ::  ValData -> Doc -> Doc
pyWhile v b = vcat [
  text "while" <+> valDoc v <> colon,
  indent b]

pyTryCatch :: Doc -> Doc -> Doc
pyTryCatch tryB catchB = vcat [
  text "try" <+> colon,
  indent tryB,
  text "except" <+> text "Exception" <+> colon,
  indent catchB]

pyListSlice :: VarData -> ValData -> 
  ValData -> ValData -> ValData -> Doc
pyListSlice vnew vold b e s = varDoc vnew <+> equals <+> valDoc vold <> 
  brackets (valDoc b <> colon <> valDoc e <> colon <> valDoc s)

pyMethod :: Label -> VarData -> Doc -> Doc -> Doc
pyMethod n slf ps b = vcat [
  text "def" <+> text n <> parens (varDoc slf <> oneParam <> ps) <> colon,
  indent bodyD]
      where oneParam = emptyIfEmpty ps $ text ", "
            bodyD | isEmpty b = text "None"
                  | otherwise = b

pyFunction :: Label -> Doc -> Doc -> Doc
pyFunction n ps b = vcat [
  text "def" <+> text n <> parens ps <> colon,
  indent bodyD]
  where bodyD | isEmpty b = text "None"
              | otherwise = b

pyClass :: Label -> Doc -> Doc -> Doc -> Doc
pyClass n pn vs fs = vcat [
  classDec <+> text n <> pn <> colon,
  indent funcSec]
  where funcSec | isEmpty (vs <> fs) = text "None"
                | isEmpty vs = fs
                | isEmpty fs = vs
                | otherwise = vcat [vs, blank, fs]    

pyModuleImportList :: [Doc] -> Doc
pyModuleImportList = vcat

pyModuleClassList :: [(Doc, Bool)] -> Doc
pyModuleClassList cs = vibcat $ map fst cs 

pyModule :: Doc -> Doc -> Doc -> Doc
pyModule ls fs cs =
  libs $+$
  funcs $+$
  cs
  where libs = emptyIfEmpty ls $ ls $+$ blank
        funcs = emptyIfEmpty fs $ fs $+$ blank

pyInOutCall :: (Label -> PythonCode (Type PythonCode) -> 
  [PythonCode (Value PythonCode)] -> PythonCode (Value PythonCode)) -> Label -> 
  [PythonCode (Value PythonCode)] -> [PythonCode (Variable PythonCode)] -> 
  [PythonCode (Variable PythonCode)] -> PythonCode (Statement PythonCode)
pyInOutCall f n ins [] [] = valState $ f n void ins
pyInOutCall f n ins outs both = if null rets then valState (f n void (map 
  valueOf both ++ ins)) else multiAssign (filterOutObjs both ++ outs) 
  [f n void (map valueOf both ++ ins)]
  where rets = filterOutObjs both ++ outs

pyBlockComment :: [String] -> Doc -> Doc
pyBlockComment lns cmt = vcat $ map ((<+>) cmt . text) lns

pyDocComment :: [String] -> Doc -> Doc -> Doc
pyDocComment [] _ _ = empty
pyDocComment (l:lns) start mid = vcat $ start <+> text l : map ((<+>) mid . text) lns
