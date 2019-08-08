{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.PythonRenderer (
  -- * Python Code Configuration -- defines syntax of all Python code
  PythonCode(..)
) where

import Utils.Drasil (indent)

import Language.Drasil.Code.Code (CodeType(..))
import Language.Drasil.Code.Imperative.Symantics (Label, PackageSym(..), 
  ProgramSym(..), RenderSym(..), InternalFile(..), AuxiliarySym(..), 
  KeywordSym(..), PermanenceSym(..), BodySym(..), BlockSym(..), 
  ControlBlockSym(..), StateTypeSym(..), UnaryOpSym(..), BinaryOpSym(..), 
  VariableSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), InternalValue(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
  ClassSym(..), ModuleSym(..), BlockCommentSym(..))
import Language.Drasil.Code.Imperative.LanguageRenderer (addExt, fileDoc', 
  enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, outDoc, intTypeDocD, 
  floatTypeDocD, typeDocD, enumTypeDocD, constructDocD, paramListDocD, mkParam,
  methodListDocD, ifCondDocD, stratDocD, assignDocD, multiAssignDoc, 
  plusEqualsDocD', plusPlusDocD', statementDocD, returnDocD, commentDocD, 
  mkStNoEnd, stringListVals', stringListLists', unOpPrec, notOpDocD', 
  negateOpDocD, sqrtOpDocD', absOpDocD', expOpDocD', sinOpDocD', cosOpDocD', 
  tanOpDocD', asinOpDocD', acosOpDocD', atanOpDocD', unExpr, unExpr',
  typeUnExpr, powerPrec, multPrec, andPrec, orPrec, equalOpDocD, notEqualOpDocD,
  greaterOpDocD, greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, 
  minusOpDocD, multOpDocD, divideOpDocD, moduloOpDocD, binExpr, typeBinExpr, 
  mkVal, litCharD, litFloatD, litIntD, litStringD, varDocD, extVarDocD, argDocD,
  enumElemDocD, objVarDocD, funcAppDocD, extFuncAppDocD, funcDocD, 
  listSetFuncDocD, listAccessFuncDocD, objAccessDocD, castObjDocD, breakDocD, 
  continueDocD, staticDocD, dynamicDocD, classDec, dot, forLabel, 
  observerListName, doxConfigName, makefileName, commentedItem, addCommentsDocD,
  classDoc, moduleDoc, docFuncRepr, valList, appendToBody, getterName, 
  setterName)
import Language.Drasil.Code.Imperative.Data (Terminator(..), AuxData(..), ad, 
  FileData(..), file, updateFileMod, FuncData(..), fd, ModData(..), md, 
  updateModDoc, MethodData(..), mthd, OpData(..), PackData(..), packD, 
  ParamData(..), ProgData(..), progD, TypeData(..), td, ValData(..), vd,
  VarData(..), vard)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, interpMM)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.Helpers (blank, vibcat, emptyIfEmpty, 
  liftA4, liftA5, liftList, lift1List, lift2Lists, lift4Pair, liftPair, 
  liftPairFst, getInnerType, convType, checkParams)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe)
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

instance PackageSym PythonCode where
  type Package PythonCode = PackData
  package = lift1List packD

instance ProgramSym PythonCode where
  type Program PythonCode = ProgData
  prog n = liftList (progD n)

instance RenderSym PythonCode where
  type RenderFile PythonCode = FileData
  fileDoc code = liftA2 file (fmap (addExt pyExt . name) code) (liftA2 
    updateModDoc (liftA2 emptyIfEmpty (fmap modDoc code) $ liftA3 fileDoc' 
    (top code) (fmap modDoc code) bottom) code)

  docMod d m = commentedMod (docComment $ moduleDoc d (moduleName 
    (fmap fileMod m)) pyExt) m

  commentedMod cmt m = liftA2 updateFileMod (liftA2 updateModDoc
    (liftA2 commentedItem cmt (fmap (modDoc . fileMod) m)) (fmap fileMod m)) m

instance InternalFile PythonCode where
  top _ = return pytop
  bottom = return empty

instance AuxiliarySym PythonCode where
  type Auxiliary PythonCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)

  optimizeDox = return $ text "YES"

  makefile cms = fmap (ad makefileName . makeBuild cms Nothing pyRunnable)

instance KeywordSym PythonCode where
  type Keyword PythonCode = Doc
  endStatement = return empty
  endStatementLoop = return empty

  include n = return $ pyInclude n
  inherit = return empty

  list _ = return empty
  listObj = return empty

  blockStart = return colon
  blockEnd = return empty

  ifBodyStart = blockStart
  elseIf = return $ text "elif"
  
  iterForEachLabel = return forLabel
  iterInLabel = return $ text "in"

  commentStart = return $ text "#"
  blockCommentStart = return empty
  blockCommentEnd = return empty
  docCommentStart = return $ text "##"
  docCommentEnd = return empty

instance PermanenceSym PythonCode where
  type Permanence PythonCode = Doc
  static_ = return staticDocD
  dynamic_ = return dynamicDocD

instance BodySym PythonCode where
  type Body PythonCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner s = bodyStatements [s]

  addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym PythonCode where
  type Block PythonCode = Doc
  block sts = lift1List blockDocD endStatement (map (fmap fst . state) sts)

instance StateTypeSym PythonCode where
  type StateType PythonCode = TypeData
  bool = return $ td Boolean empty
  int = return intTypeDocD
  float = return floatTypeDocD
  char = return $ td Char empty
  string = return pyStringType
  infile = return $ td File empty
  outfile = return $ td File empty
  listType _ t = liftA2 td (fmap (List . cType) t) (return $ brackets empty)
  listInnerType t = fmap (getInnerType . cType) t >>= convType
  obj t = return $ typeDocD t
  enumType t = return $ enumTypeDocD t
  iterator _ = error "Iterator-type variables do not exist in Python"
  void = return $ td Void (text "NoneType")

  getType = cType . unPC

instance ControlBlockSym PythonCode where
  runStrategy l strats rv av = maybe
    (strError l "RunStrategy called on non-existent strategy") 
    (liftA2 (flip stratDocD) (state resultState)) 
    (Map.lookup l (Map.fromList strats))
    where resultState = maybe (return (mkStNoEnd empty)) asgState av
          asgState v = maybe (strError l 
            "Attempt to assign null return to a Value") (assign v) rv
          strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

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
  var n t = liftA2 (vard n) t (return $ varDocD n) 
  const = var
  extVar l n t = liftA2 (vard $ l ++ "." ++ n) t (return $ extVarDocD l n)
  self l = liftA2 (vard "self") (obj l) (return $ text "self")
  enumVar e en = var e (enumType en)
  objVar o v = liftA2 (vard $ variableName o ++ "." ++ variableName v)
    (variableType v) (liftA2 objVarDocD o v)
  objVarSelf l n t = liftA2 (vard $ "self." ++ n) t (liftA2 objVarDocD
    (self l) (var n t))
  listVar n p t = var n (listType p t)
  n `listOf` t = listVar n static_ t
  iterVar n t = var n (iterator t)

  ($->) = objVar

  variableName = varName . unPC
  variableType = fmap varType
  variableDoc = varDoc . unPC

instance ValueSym PythonCode where
  type Value PythonCode = ValData
  litTrue = liftA2 mkVal bool (return $ text "True")
  litFalse = liftA2 mkVal bool (return $ text "False")
  litChar c = liftA2 mkVal char (return $ litCharD c)
  litFloat v = liftA2 mkVal float (return $ litFloatD v)
  litInt v = liftA2 mkVal int (return $ litIntD v)
  litString s = liftA2 mkVal string (return $ litStringD s)

  ($:) = enumElement

  valueOf v = liftA2 mkVal (variableType v) (return $ variableDoc v) 
  arg n = liftA2 mkVal string (liftA2 argDocD (litInt (n + 1)) argsList)
  enumElement en e = liftA2 mkVal (enumType en) (return $ enumElemDocD en e)
  argsList = liftA2 mkVal (listType static_ string) (return $ text "sys.argv")

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
  funcApp n t vs = liftA2 mkVal t (liftList (funcAppDocD n) vs)
  selfFuncApp = funcApp
  extFuncApp l n t vs = liftA2 mkVal t (liftList (extFuncAppDocD l n) vs)
  stateObj t vs = liftA2 mkVal t (liftA2 pyStateObj t (liftList valList vs))
  extStateObj l t vs = liftA2 mkVal t (liftA2 (pyExtStateObj l) t (liftList 
    valList vs))
  listStateObj t _ = liftA2 mkVal t (fmap typeDoc t)

  exists v = v ?!= valueOf (var "None" void)
  notNull = exists

instance InternalValue PythonCode where
  inputFunc = liftA2 mkVal string (return $ text "input()")  -- raw_input() for < Python 3.0
  printFunc = liftA2 mkVal void (return $ text "print")
  printLnFunc = liftA2 mkVal void (return empty)
  printFileFunc _ = liftA2 mkVal void (return empty)
  printFileLnFunc _ = liftA2 mkVal void (return empty)
  
  cast t v = liftA2 mkVal t $ liftA2 castObjDocD (fmap typeDoc t) v

instance Selector PythonCode where
  objAccess v f = liftA2 mkVal (fmap funcType f) (liftA2 objAccessDocD v f)
  ($.) = objAccess 

  objMethodCall t o f ps = objAccess o (func f t ps)
  objMethodCallNoParams t o f = objMethodCall t o f []

  selfAccess l = objAccess (valueOf $ self l)

  listIndexExists lst index = listSize lst ?> index
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf l v = objAccess l (func "index" int [v])

instance FunctionSym PythonCode where
  type Function PythonCode = FuncData
  func l t vs = liftA2 fd t (fmap funcDocD (funcApp l t vs))

  get v vToGet = v $. getFunc vToGet
  set v vToSet toVal = v $. setFunc (valueType v) vToSet toVal

  listSize v = liftA2 mkVal (fmap funcType listSizeFunc) 
    (liftA2 pyListSize v listSizeFunc)
  listAdd v i vToAdd = v $. listAddFunc v i vToAdd
  listAppend v vToApp = v $. listAppendFunc vToApp

  iterBegin v = v $. iterBeginFunc (valueType v)
  iterEnd v = v $. iterEndFunc (valueType v)

instance SelectorFunction PythonCode where
  listAccess v i = v $. listAccessFunc (listInnerType $ valueType v) i
  listSet v i toVal = v $. listSetFunc v i toVal
  at v l = listAccess v (valueOf $ var l int)

instance InternalFunction PythonCode where
  getFunc v = func (getterName $ variableName v) (variableType v) []
  setFunc t v toVal = func (setterName $ variableName v) t [toVal]

  listSizeFunc = liftA2 fd int (return $ text "len")
  listAddFunc _ i v = func "insert" (listType static_ $ fmap valType v) [i, v]
  listAppendFunc v = func "append" (listType static_ $ fmap valType v) [v]

  iterBeginFunc _ = error "Attempt to use iterBeginFunc in Python, but Python has no iterators"
  iterEndFunc _ = error "Attempt to use iterEndFunc in Python, but Python has no iterators"

  listAccessFunc t v = liftA2 fd t (fmap listAccessFuncDocD v)
  listSetFunc v i toVal = liftA2 fd (valueType v) 
    (liftA2 listSetFuncDocD i toVal)

  atFunc t l = listAccessFunc t (valueOf $ var l int)

instance InternalStatement PythonCode where
  printSt nl p v f = mkStNoEnd <$> liftA3 (pyPrint nl) p v 
    (fromMaybe (liftA2 mkVal void (return empty)) f)

  state = fmap statementDocD
  loopState = fmap statementDocD 

instance StatementSym PythonCode where
  -- Terminator determines how statements end
  type Statement PythonCode = (Doc, Terminator)
  assign vr vl = mkStNoEnd <$> liftA2 assignDocD vr vl
  assignToListIndex lst index v = valState $ listSet (valueOf lst) index v
  multiAssign vrs vls = mkStNoEnd <$> lift2Lists multiAssignDoc vrs vls
  (&=) = assign
  (&-=) vr vl = vr &= (valueOf vr #- vl)
  (&+=) vr vl = mkStNoEnd <$> liftA3 plusEqualsDocD' vr plusOp vl
  (&++) v = mkStNoEnd <$> liftA2 plusPlusDocD' v plusOp
  (&~-) v = v &= (valueOf v #- litInt 1)

  varDec _ = return (mkStNoEnd empty)
  varDecDef = assign
  listDec _ v = mkStNoEnd <$> fmap pyListDec v
  listDecDef v vs = mkStNoEnd <$> liftA2 pyListDecDef v (liftList valList vs)
  objDecDef = varDecDef
  objDecNew v vs = varDecDef v (stateObj (variableType v) vs)
  extObjDecNew lib v vs = varDecDef v (extStateObj lib (variableType v) vs)
  objDecNewVoid v = varDecDef v (stateObj (variableType v) [])
  extObjDecNewVoid lib v = varDecDef v (extStateObj lib (variableType v) [])
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
  closeFile f = valState $ objMethodCall void f "close" []

  getFileInputLine = getFileInput
  discardFileLine f = valState $ objMethodCall string f "readline" []
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
    where cases = map (\(l, b) -> (v ?== l, b)) cs

  ifExists v ifBody = ifCond [(notNull v, ifBody)]

  for _ _ _ _ = error $ "Classic for loops not available in Python, please " ++
    "use forRange, forEach, or while instead"
  forRange i initv finalv stepv b = mkStNoEnd <$> liftA5 (pyForRange i) 
    iterInLabel initv finalv stepv b
  forEach l v b = mkStNoEnd <$> liftA4 (pyForEach l) iterForEachLabel 
    iterInLabel v b
  while v b = mkStNoEnd <$> liftA2 pyWhile v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 pyTryCatch tb cb

  checkState l = switch (valueOf $ var l string)
  notifyObservers f t = forRange index initv (listSize obsList) 
    (litInt 1) notify
    where obsList = valueOf $ observerListName `listOf` t
          index = "observerIndex"
          initv = litInt 0
          notify = oneLiner $ valState $ at obsList index $. f

  getFileInputAll f v = v &= objMethodCall (listType static_ string) f
    "readlines" []

instance ScopeSym PythonCode where
  type Scope PythonCode = Doc
  private = return empty
  public = return empty

instance InternalScope PythonCode where
  includeScope s = s

instance MethodTypeSym PythonCode where
  type MethodType PythonCode = TypeData
  mState t = t
  construct n = return $ td (Object n) (constructDocD n)

instance ParameterSym PythonCode where
  type Parameter PythonCode = ParamData
  stateParam = fmap (mkParam varDoc)
  pointerParam = stateParam

  parameterName = paramName . unPC
  parameterType = fmap paramType

instance MethodSym PythonCode where
  type Method PythonCode = MethodData
  method n l _ _ _ ps b = liftA2 (mthd False) (checkParams n <$> sequence ps) 
    (liftA3 (pyMethod n) (self l) (liftList paramListDocD ps) b)
  getMethod c v = method (getterName $ variableName v) c public dynamic_ 
    (mState $ variableType v) [] getBody
    where getBody = oneLiner $ returnState (valueOf $ self c $-> v)
  setMethod c v = method (setterName $ variableName v) c public dynamic_
    (mState void) [stateParam v] setBody
    where setBody = oneLiner $ (self c $-> v) &= valueOf v
  mainMethod _ = fmap (mthd True [])
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method initName n public dynamic_ (construct n)
  destructor _ _ = error "Destructors not allowed in Python"

  docMain = mainMethod

  function n _ _ _ ps b = liftA2 (mthd False) (checkParams n <$> sequence ps) 
    (liftA2 (pyFunction n) (liftList paramListDocD ps) b)

  docFunc = docFuncRepr

  inOutFunc n s p ins [] [] b = function n s p (mState void) (map stateParam 
    ins) b
  inOutFunc n s p ins outs both b = function n s p (mState void) (map 
    stateParam $ both ++ ins) (liftA2 appendToBody b (multiReturn $
    map valueOf $ both ++ outs))

  docInOutFunc desc iComms _ bComms = docFuncRepr desc (bComms ++ iComms)

  commentedFunc cmt fn = liftA3 mthd (fmap isMainMthd fn) (fmap mthdParams fn)
    (liftA2 commentedItem cmt (fmap mthdDoc fn))

  parameters m = map return $ (mthdParams . unPC) m

instance StateVarSym PythonCode where
  type StateVar PythonCode = Doc
  stateVar _ _ _ _ = return empty
  privMVar del = stateVar del private dynamic_
  pubMVar del = stateVar del public dynamic_
  pubGVar del = stateVar del public static_

instance ClassSym PythonCode where
  type Class PythonCode = (Doc, Bool)
  buildClass n p _ _ fs = liftPairFst (liftA2 (pyClass n) pname (liftList 
    methodListDocD (map (fmap mthdDoc) fs)), any (isMainMthd . unPC) fs)
    where pname = case p of Nothing -> return empty
                            Just pn -> return $ parens (text pn)
  enum n es _ = liftPairFst (liftA2 (pyClass n) (return empty) (return $ 
    enumElementsDocD' es), False)
  mainClass _ _ fs = liftPairFst (liftList methodListDocD (map (fmap mthdDoc) 
    fs), True)
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

pyStateObj :: TypeData -> Doc -> Doc
pyStateObj t vs = typeDoc t <> parens vs

pyExtStateObj :: Label -> TypeData -> Doc -> Doc
pyExtStateObj l t vs = text l <> dot <> typeDoc t <> parens vs

pyInlineIf :: ValData -> ValData -> ValData -> ValData
pyInlineIf c v1 v2 = vd (valPrec c) (valType v1) (valDoc v1 <+> text "if" <+> 
  valDoc c <+> text "else" <+> valDoc v2)

pyListSize :: ValData -> FuncData -> Doc
pyListSize v f = funcDoc f <> parens (valDoc v)

pyStringType :: TypeData
pyStringType = td String (text "str")

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

pyForRange :: Label -> Doc ->  ValData ->  ValData ->
  ValData -> Doc -> Doc
pyForRange i inLabel initv finalv stepv b = vcat [
  forLabel <+> text i <+> inLabel <+> text "range" <> parens (valDoc initv <> 
    text ", " <> valDoc finalv <> text ", " <> valDoc stepv) <> colon,
  indent b]

pyForEach :: Label -> Doc -> Doc ->  ValData -> Doc -> Doc
pyForEach i forEachLabel inLabel lstVar b = vcat [
  forEachLabel <+> text i <+> inLabel <+> valDoc lstVar <> colon,
  indent b]

pyWhile ::  ValData -> Doc -> Doc
pyWhile v b = vcat [
  text "while" <+> valDoc v <> colon,
  indent b]

pyTryCatch :: Doc -> Doc -> Doc
pyTryCatch tryB catchB = vcat [
  text "try" <+> colon,
  indent tryB,
  text "except" <+> text "Exception" <+> text "as" <+> text "exc" <+> colon,
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

pyClass :: Label -> Doc -> Doc -> Doc
pyClass n pn fs = vcat [
  classDec <+> text n <> pn <> colon,
  indent funcSec]
  where funcSec | isEmpty fs = text "None"
                | otherwise = fs    

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

pyInOutCall :: (Label -> PythonCode (StateType PythonCode) -> 
  [PythonCode (Value PythonCode)] -> PythonCode (Value PythonCode)) -> Label -> 
  [PythonCode (Value PythonCode)] -> [PythonCode (Variable PythonCode)] -> 
  [PythonCode (Variable PythonCode)] -> PythonCode (Statement PythonCode)
pyInOutCall f n ins [] [] = valState $ f n void ins
pyInOutCall f n ins outs both = multiAssign (both ++ outs) 
  [f n void (map valueOf both ++ ins)]

pyBlockComment :: [String] -> Doc -> Doc
pyBlockComment lns cmt = vcat $ map ((<+>) cmt . text) lns

pyDocComment :: [String] -> Doc -> Doc -> Doc
pyDocComment [] _ _ = empty
pyDocComment (l:lns) start mid = vcat $ start <+> text l : map ((<+>) mid . text) lns

pyRunnable :: Runnable
pyRunnable = interpMM "python"
