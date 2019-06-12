{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.NewPythonRenderer (
  -- * Python Code Configuration -- defines syntax of all Python code
  PythonCode(..)
) where

import Language.Drasil.Code.Imperative.New (Label,
  PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
  UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), 
  StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.NewLanguageRenderer (
  fileDoc', enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, 
  intTypeDocD, floatTypeDocD, typeDocD, voidDocD, constructDocD, 
  paramListDocD, methodListDocD, ifCondDocD, stratDocD, assignDocD, 
  plusEqualsDocD', plusPlusDocD', statementDocD, returnDocD, commentDocD,
  mkStNoEnd, notOpDocD', negateOpDocD, sqrtOpDocD', absOpDocD', expOpDocD',
  sinOpDocD', cosOpDocD', tanOpDocD', asinOpDocD', acosOpDocD', atanOpDocD', 
  unExpr, equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, 
  lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, 
  divideOpDocD, moduloOpDocD, binExpr, mkVal, litCharD, litFloatD, litIntD, 
  litStringD, defaultCharD, defaultFloatD, defaultIntD, defaultStringD, varDocD,
  extVarDocD, argDocD, enumElemDocD, objVarDocD, funcAppDocD, extFuncAppDocD,
  funcDocD, listSetDocD, objAccessDocD, castObjDocD, breakDocD, continueDocD,
  staticDocD, dynamicDocD, classDec, dot, forLabel, observerListName,
  addCommentsDocD, callFuncParamList, getterName, setterName)
import Language.Drasil.Code.Imperative.Helpers (Terminator(..), ModData(..), md,
  blank, oneTab, vibcat, liftA4, liftA5, liftList, lift1List, lift4Pair, 
  liftPairFst)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($+$), parens, empty,
  equals, vcat, colon, brackets, isEmpty, render)

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
  type Package PythonCode = ([ModData], Label)
  packMods n ms = liftPairFst (sequence ms, n)

instance RenderSym PythonCode where
  type RenderFile PythonCode = ModData
  fileDoc code = liftA3 md (fmap name code) (fmap isMainMod code) 
    (liftA3 fileDoc' (top code) (fmap modDoc code) bottom)
  top _ = return pytop
  bottom = return empty

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
  
  printFunc = return $ text "print"
  printLnFunc = return empty
  printFileFunc _ = return empty
  printFileLnFunc _ = return empty

instance PermanenceSym PythonCode where
  type Permanence PythonCode = Doc
  static_ = return staticDocD
  dynamic = return dynamicDocD

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
  type StateType PythonCode = Doc
  bool = return empty
  int = return intTypeDocD
  float = return floatTypeDocD
  char = return empty
  string = return pyStringType
  infile = return empty
  outfile = return empty
  listType _ _ = return $ brackets empty
  intListType _ = return $ brackets empty
  floatListType _ = return $ brackets empty
  boolListType = return $ brackets empty
  obj t = return $ typeDocD t
  enumType t = return $ typeDocD t
  iterator _ = error "Iterator-type variables do not exist in Python"

instance ControlBlockSym PythonCode where
  runStrategy l strats rv av = maybe
    (strError l "RunStrategy called on non-existent strategy") 
    (liftA2 (flip stratDocD) (state resultState)) 
    (Map.lookup l (Map.fromList strats))
    where resultState = maybe (return (mkStNoEnd empty)) asgState av
          asgState v = maybe (strError l 
            "Attempt to assign null return to a Value") (assign v) rv
          strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

  listSlice _ vnew vold b e s = liftA5 pyListSlice vnew vold (getVal b) 
    (getVal e) (getVal s)
    where getVal = fromMaybe (return (mkVal empty))

instance UnaryOpSym PythonCode where
  type UnaryOp PythonCode = Doc
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
  floorOp = return $ text "math.floor"
  ceilOp = return $ text "math.ceil"

instance BinaryOpSym PythonCode where
  type BinaryOp PythonCode = Doc
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
  powerOp = return $ text "**"
  moduloOp = return moduloOpDocD
  andOp = return $ text "and"
  orOp = return $ text "or"

instance ValueSym PythonCode where
  type Value PythonCode = (Doc, Maybe String)
  litTrue = return (text "True", Just "True")
  litFalse = return (text "False", Just "False")
  litChar c = return (litCharD c, Just $ "\'" ++ [c] ++ "\'")
  litFloat v = return (litFloatD v, Just $ show v)
  litInt v = return (litIntD v, Just $ show v)
  litString s = return (litStringD s, Just $ "\"" ++ s ++ "\"")

  defaultChar = return (defaultCharD, Just "space character")
  defaultFloat = return (defaultFloatD, Just "0.0")
  defaultInt = return (defaultIntD, Just "0")
  defaultString = return (defaultStringD, Just "empty string")
  defaultBool = litFalse

  ($->) = objVar
  ($:) = enumElement

  const = var
  var n = return (varDocD n, Just n)
  extVar l n = return (extVarDocD l n, Just $ l ++ "." ++ n)
  self = return (text "self", Just "self")
  arg n = mkVal <$> liftA2 argDocD (litInt (n + 1)) argsList
  enumElement en e = return (enumElemDocD en e, Just $ en ++ "." ++ e)
  enumVar = var
  objVar o v = liftPairFst (liftA2 objVarDocD o v, Just $ valName o ++ "." ++ 
    valName v)
  objVarSelf n = liftPairFst (liftA2 objVarDocD self (var n), Just $ "self." ++
    n)
  listVar n _ = var n
  n `listOf` t = listVar n t
  iterVar = var

  inputFunc = return (mkVal $ text "input()")  -- raw_input() for < Python 3.0
  argsList = return (mkVal $ text "sys.argv")

  valName (PC (v, s)) = fromMaybe 
    (error $ "Attempt to print unprintable Value (" ++ render v ++ ")") s

instance NumericExpression PythonCode where
  (#~) = liftA2 unExpr negateOp
  (#/^) = liftA2 unExpr sqrtOp
  (#|) = liftA2 unExpr absOp
  (#+) = liftA3 binExpr plusOp
  (#-) = liftA3 binExpr minusOp
  (#*) = liftA3 binExpr multOp
  (#/) = liftA3 binExpr divideOp
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
  (?!) = liftA2 unExpr notOp
  (?&&) = liftA3 binExpr andOp
  (?||) = liftA3 binExpr orOp

  (?<) = liftA3 binExpr lessOp
  (?<=) = liftA3 binExpr lessEqualOp
  (?>) = liftA3 binExpr greaterOp
  (?>=) = liftA3 binExpr greaterEqualOp
  (?==) = liftA3 binExpr equalOp
  (?!=) = liftA3 binExpr notEqualOp

instance ValueExpression PythonCode where
  inlineIf b v1 v2 = mkVal <$> liftA3 pyInlineIf b v1 v2
  funcApp n vs = mkVal <$> liftList (funcAppDocD n) vs
  selfFuncApp = funcApp
  extFuncApp l n vs = mkVal <$> liftList (extFuncAppDocD l n) vs
  stateObj t vs = mkVal <$> liftA2 pyStateObj t (liftList callFuncParamList 
    vs)
  extStateObj l t vs = mkVal <$> liftA2 (pyExtStateObj l) t (liftList 
    callFuncParamList vs)
  listStateObj t _ = mkVal <$> t

  exists v = v ?!= var "None"
  notNull = exists

instance Selector PythonCode where
  objAccess v f = mkVal <$> liftA2 objAccessDocD v f
  ($.) = objAccess 

  objMethodCall o f ps = objAccess o (func f ps)
  objMethodCallVoid o f = objMethodCall o f []

  selfAccess = objAccess self

  listPopulateAccess v f = mkVal <$> liftA2 pyListPopAccess v f
  listSizeAccess v = mkVal <$> liftA2 pyListSizeAccess v listSize

  listIndexExists lst index = listSizeAccess lst ?> index
  argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))
  
  indexOf l v = objAccess l (fmap funcDocD (funcApp "index" [v]))

  stringEqual v1 v2 = v1 ?== v2

  castObj f v = mkVal <$> liftA2 castObjDocD f v
  castStrToFloat = castObj (cast float string)

instance FunctionSym PythonCode where
  type Function PythonCode = Doc
  func l vs = fmap funcDocD (funcApp l vs)
  cast targT _ = targT
  castListToInt = cast int (listType static_ int)
  get n = fmap funcDocD (var n)
  set n v = fmap funcDocD (mkVal . fst <$> assign (var n) v)

  listSize = return $ text "len"
  listAdd i v = fmap funcDocD (funcApp "insert" [i, v])
  listPopulateInt = liftA2 pyListPop defaultInt
  listPopulateFloat = liftA2 pyListPop defaultFloat
  listPopulateChar = liftA2 pyListPop defaultChar
  listPopulateBool = liftA2 pyListPop defaultBool
  listPopulateString = liftA2 pyListPop defaultString
  listAppend v = fmap funcDocD (funcApp "append" [v])
  listExtendInt = fmap pyListExtend defaultInt 
  listExtendFloat = fmap pyListExtend defaultFloat 
  listExtendChar = fmap pyListExtend defaultChar 
  listExtendBool = fmap pyListExtend defaultBool
  listExtendString = fmap pyListExtend defaultString
  listExtendList n _ = return $ pyListExtendList n

  iterBegin = fmap funcDocD (funcApp "begin" [])
  iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction PythonCode where
  listAccess = fmap pyListAccess
  listSet = liftA2 listSetDocD

  listAccessEnum _ = listAccess
  listSetEnum t i = listSet (castObj (cast int t) i)

  at l = listAccess (var l)

instance StatementSym PythonCode where
  -- Terminator determines how statements end to end in a separator
  type Statement PythonCode = (Doc, Terminator)
  assign v1 v2 = mkStNoEnd <$> liftA2 assignDocD v1 v2
  assignToListIndex lst index v = valState $ lst $. listSet index v
  (&=) = assign
  (&.=) l = assign (var l)
  (&=.) v l = assign v (var l)
  (&-=) v1 v2 = v1 &= (v1 #- v2)
  (&.-=) l v = l &.= (var l #- v)
  (&+=) v1 v2 = mkStNoEnd <$> liftA3 plusEqualsDocD' v1 plusOp v2
  (&.+=) l v = var l &+= v
  (&++) v = mkStNoEnd <$> liftA2 plusPlusDocD' v plusOp
  (&.++) l = (&++) (var l)
  (&~-) v = v &= (v #- litInt 1)
  (&.~-) l = (&~-) (var l)

  varDec _ _ = return (mkStNoEnd empty)
  varDecDef l _ v = mkStNoEnd <$> fmap (pyVarDecDef l) v
  listDec l _ t = mkStNoEnd <$> fmap (pyListDec l) (listType static_ t)
  listDecDef l _ vs = mkStNoEnd <$> fmap (pyListDecDef l) (liftList 
    callFuncParamList vs)
  objDecDef = varDecDef
  objDecNew l t vs = varDecDef l t (stateObj t vs)
  extObjDecNew l lib t vs = varDecDef l t (extStateObj lib t vs)
  objDecNewVoid l t = varDecDef l t (stateObj t [])
  extObjDecNewVoid l lib t = varDecDef l t (extStateObj lib t [])
  constDecDef = varDecDef

  print _ v = mkStNoEnd <$> liftA4 pyOut printFunc v (return $ text ", end=''") 
    (return (mkVal empty))
  printLn _ v = mkStNoEnd <$> liftA4 pyOut printFunc v (return empty)
    (return (mkVal empty))
  printStr s = print string (litString s)
  printStrLn s = printLn string (litString s)

  printFile f _ v = mkStNoEnd <$> liftA4 pyOut printFunc v (return $ 
    text ", end='', file=") f
  printFileLn f _ v = mkStNoEnd <$> liftA4 pyOut printFunc v (return $ 
    text ", file=") f
  printFileStr f s = printFile f string (litString s)
  printFileStrLn f s = printFileLn f string (litString s)

  printList = print
  printLnList = printLn
  printFileList = printFile
  printFileLnList = printFileLn

  getIntInput v = v &= funcApp "int" [inputFunc]
  getFloatInput v = v &= funcApp "float" [inputFunc]
  getBoolInput v = v &= inputFunc ?!= litString "0"
  getStringInput v = v &= objMethodCall inputFunc "rstrip" []
  getCharInput v = v &= inputFunc
  discardInput = valState inputFunc
  getIntFileInput f v = v &= funcApp "int" [objMethodCall f "readline" []]
  getFloatFileInput f v = v &= funcApp "float" [objMethodCall f "readline" []]
  getBoolFileInput f v =  v &= (objMethodCall f "readline" [] ?!= litString "0")
  getStringFileInput f v = v &= objMethodCall (objMethodCall f "readline" []) 
    "rstrip" []
  getCharFileInput f v = v &= objMethodCall f "readline" []
  discardFileInput f = valState (objMethodCall f "readline" [])

  openFileR f n = f &= funcApp "open" [n, litString "r"]
  openFileW f n = f &= funcApp "open" [n, litString "w"]
  openFileA f n = f &= funcApp "open" [n, litString "a"]
  closeFile f = valState $ objMethodCall f "close" []

  getFileInputLine f v = v &= objMethodCall f "readline" []
  discardFileLine f = valState $ objMethodCall f "readline" []
  stringSplit d vnew s = assign vnew (objAccess s 
    (func "split" [litString [d]]))  

  break = return (mkStNoEnd breakDocD)
  continue = return (mkStNoEnd continueDocD)

  returnState v = mkStNoEnd <$> fmap returnDocD v
  returnVar l = mkStNoEnd <$> fmap returnDocD (var l)

  valState v = mkStNoEnd <$> fmap fst v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free v = v &= var "None"

  throw errMsg = mkStNoEnd <$> fmap pyThrow (litString errMsg)

  initState fsmName initialState = varDecDef fsmName string 
    (litString initialState)
  changeState fsmName toState = fsmName &.= litString toState

  initObserverList = listDecDef observerListName
  addObserver t o = valState $ obsList $. listAdd lastelem o
    where obsList = observerListName `listOf` t
          lastelem = listSizeAccess obsList

  state = fmap statementDocD
  loopState = fmap statementDocD 
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
  forEach l _ v b = mkStNoEnd <$> liftA4 (pyForEach l) iterForEachLabel 
    iterInLabel v b
  while v b = mkStNoEnd <$> liftA2 pyWhile v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 pyTryCatch tb cb

  checkState l = switch (var l)
  notifyObservers fn t ps = forRange index initv (listSizeAccess obsList) 
    (litInt 1) notify
    where obsList = observerListName `listOf` t
          index = "observerIndex"
          initv = litInt 0
          notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

  getFileInputAll f v = v &= objMethodCall f "readlines" []

instance ScopeSym PythonCode where
  type Scope PythonCode = Doc
  private = return empty
  public = return empty

  includeScope s = s

instance MethodTypeSym PythonCode where
  type MethodType PythonCode = Doc
  mState t = t
  void = return voidDocD
  construct n = return $ constructDocD n

instance ParameterSym PythonCode where
  type Parameter PythonCode = Doc
  stateParam n _ = return $ text n
  pointerParam = stateParam

instance MethodSym PythonCode where
  type Method PythonCode = (Doc, Bool)
  method n _ _ _ _ ps b = liftPairFst (liftA3 (pyMethod n) self (liftList 
    paramListDocD ps) b, False)
  getMethod n c t = method (getterName n) c public dynamic t [] getBody
    where getBody = oneLiner $ returnState (self $-> var n)
  setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic
    void [stateParam paramLbl t] setBody
    where setBody = oneLiner $ (self $-> var setLbl) &=. paramLbl
  mainMethod _ b = liftPairFst (b, True)
  privMethod n c = method n c private dynamic
  pubMethod n c = method n c public dynamic
  constructor n = method initName n public dynamic (construct n)
  destructor _ _ = error "Destructors not allowed in Python"


  function n _ _ _ ps b = liftPairFst (liftA2 (pyFunction n) (liftList 
    paramListDocD ps) b, False)

instance StateVarSym PythonCode where
  type StateVar PythonCode = Doc
  stateVar _ _ _ _ _ = return empty
  privMVar del l = stateVar del l private dynamic
  pubMVar del l = stateVar del l public dynamic
  pubGVar del l = stateVar del l public static_
  listStateVar = stateVar

instance ClassSym PythonCode where
  type Class PythonCode = (Doc, Bool)
  buildClass n p _ _ fs = liftPairFst (liftA2 (pyClass n) pname (liftList 
    methodListDocD fs), any (snd . unPC) fs)
    where pname = case p of Nothing -> return empty
                            Just pn -> return $ parens (text pn)
  enum n es _ = liftPairFst (liftA2 (pyClass n) (return empty) (return $ 
    enumElementsDocD' es), False)
  mainClass _ _ fs = liftPairFst (liftList methodListDocD fs, True)
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public

instance ModuleSym PythonCode where
  type Module PythonCode = ModData
  buildModule n ls vs fs cs = fmap (md n (any (snd . unPC) fs || 
    any (snd . unPC) cs)) (liftA4 pyModule (liftList pyModuleImportList (map 
    include ls)) (liftList pyModuleVarList (map state vs)) (liftList 
    methodListDocD fs) (liftList pyModuleClassList cs))

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

pyLogOp :: Doc
pyLogOp = text "math.log10"

pyLnOp :: Doc
pyLnOp = text "math.log"

pyStateObj :: Doc -> Doc -> Doc
pyStateObj t vs = t <> parens vs

pyExtStateObj :: Label -> Doc -> Doc -> Doc
pyExtStateObj l t vs = text l <> dot <> t <> parens vs

pyInlineIf :: (Doc, Maybe String) -> (Doc, Maybe String) -> 
  (Doc, Maybe String) -> Doc
pyInlineIf (c, _) (v1, _) (v2, _) = parens $ v1 <+> text "if" <+> c <+> 
  text "else" <+> v2

pyListPopAccess :: (Doc, Maybe String) -> Doc -> Doc
pyListPopAccess (v, _) f = v <+> equals <+> f

pyListSizeAccess :: (Doc, Maybe String) -> Doc -> Doc
pyListSizeAccess (v, _) f = f <> parens v

pyStringType :: Doc
pyStringType = text "str"

pyListPop :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
pyListPop (dftVal, _) (size, _) = brackets dftVal <+> text "*" <+> size

pyListExtend :: (Doc, Maybe String) -> Doc
pyListExtend (dftVal, _) = dot <> text "append" <> parens dftVal

pyListExtendList :: Integer -> Doc
pyListExtendList ns = dot <> text "append" <> parens (nestedList ns)
  where nestedList 0 = empty
        nestedList n = brackets $ nestedList (n-1)

pyVarDecDef :: Label ->  (Doc, Maybe String) -> Doc
pyVarDecDef l (v, _) = text l <+> equals <+> v

pyListDec :: Label -> Doc -> Doc
pyListDec l t = text l <+> equals <+> t

pyListDecDef :: Label -> Doc -> Doc
pyListDecDef l vs = text l <+> equals <+> brackets vs

pyOut :: Doc ->  (Doc, Maybe String) -> Doc ->  (Doc, Maybe String) -> Doc
pyOut prf (v, _) txt (f, _) = prf <> parens (v <> txt <> f)

pyThrow ::  (Doc, Maybe String) -> Doc
pyThrow (errMsg, _) = text "raise" <+> text "Exception" <> parens errMsg

pyListAccess :: (Doc, Maybe String) -> Doc
pyListAccess (v, _) = brackets v

pyForRange :: Label -> Doc ->  (Doc, Maybe String) ->  (Doc, Maybe String) ->
  (Doc, Maybe String) -> Doc -> Doc
pyForRange i inLabel (initv, _) (finalv, _) (stepv, _) b = vcat [
  forLabel <+> text i <+> inLabel <+> text "range" <> parens (initv <> 
    text ", " <> finalv <> text ", " <> stepv) <> colon,
  oneTab b]

pyForEach :: Label -> Doc -> Doc ->  (Doc, Maybe String) -> Doc -> Doc
pyForEach i forEachLabel inLabel (lstVar, _) b = vcat [
  forEachLabel <+> text i <+> inLabel <+> lstVar <> colon,
  oneTab b]

pyWhile ::  (Doc, Maybe String) -> Doc -> Doc
pyWhile (v, _) b = vcat [
  text "while" <+> v <> colon,
  oneTab b]

pyTryCatch :: Doc -> Doc -> Doc
pyTryCatch tryB catchB = vcat [
  text "try" <+> colon,
  oneTab tryB,
  text "except" <+> text "Exception" <+> text "as" <+> text "exc" <+> colon,
  oneTab catchB]

pyListSlice :: (Doc, Maybe String) -> (Doc, Maybe String) -> 
  (Doc, Maybe String) -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
pyListSlice (vnew, _) (vold, _) (b, _) (e, _) (s, _) = vnew <+> equals <+> 
  vold <> brackets (b <> colon <> e <> colon <> s)

pyMethod :: Label ->  (Doc, Maybe String) -> Doc -> Doc -> Doc
pyMethod n (slf, _) ps b = vcat [
  text "def" <+> text n <> parens (slf <> oneParam <> ps) <> colon,
  oneTab bodyD]
      where oneParam | isEmpty ps = empty
                     | otherwise  = text ", "
            bodyD | isEmpty b = text "None"
                  | otherwise = b

pyFunction :: Label -> Doc -> Doc -> Doc
pyFunction n ps b = vcat [
  text "def" <+> text n <> parens ps <> colon,
  oneTab bodyD]
  where bodyD | isEmpty b = text "None"
              | otherwise = b

pyClass :: Label -> Doc -> Doc -> Doc
pyClass n pn fs = vcat [
  classDec <+> text n <> pn <> colon,
  oneTab fs]

pyModuleImportList :: [Doc] -> Doc
pyModuleImportList = vcat

pyModuleVarList :: [(Doc, Terminator)] -> Doc
pyModuleVarList vs = vcat (map fst vs)

pyModuleClassList :: [(Doc, Bool)] -> Doc
pyModuleClassList cs = vibcat $ map fst cs 

pyModule :: Doc -> Doc -> Doc -> Doc -> Doc
pyModule ls vs fs cs =
  libs $+$
  vars $+$
  funcs $+$
  cs
  where libs | isEmpty ls = empty
             | otherwise  = ls $+$ blank
        vars | isEmpty vs = empty
             | otherwise  = vs $+$ blank
        funcs | isEmpty fs = empty
              | otherwise  = fs $+$ blank