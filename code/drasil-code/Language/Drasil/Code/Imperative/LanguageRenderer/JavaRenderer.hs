{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer (
  -- * Java Code Configuration -- defines syntax of all Java code
  JavaCode(..), jNameOpts
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
import Language.Drasil.Code.Imperative.LanguageRenderer (addExt,
  packageDocD, fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
  multiStateDocD, blockDocD, bodyDocD, outDoc, printDoc, printFileDocD, 
  boolTypeDocD, intTypeDocD, charTypeDocD, typeDocD, enumTypeDocD, listTypeDocD,
  voidDocD, constructDocD, stateParamDocD, paramListDocD, mkParam, 
  methodListDocD, stateVarDocD, stateVarListDocD, ifCondDocD, switchDocD, 
  forDocD, forEachDocD, whileDocD, stratDocD, assignDocD, plusEqualsDocD, 
  plusPlusDocD, varDecDocD, varDecDefDocD, listDecDocD, objDecDefDocD, 
  statementDocD, returnDocD, commentDocD, mkSt, mkStNoEnd, stringListVals', 
  stringListLists', unOpPrec, notOpDocD, negateOpDocD, unExpr, unExpr',
  typeUnExpr, powerPrec, equalOpDocD, notEqualOpDocD, greaterOpDocD, 
  greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, 
  multOpDocD, divideOpDocD, moduloOpDocD, andOpDocD, orOpDocD, binExpr, 
  binExpr', typeBinExpr, mkVal, litTrueD, litFalseD, litCharD, litFloatD, 
  litIntD, litStringD, varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, 
  objVarDocD, inlineIfD, funcAppDocD, extFuncAppDocD, stateObjDocD, 
  listStateObjDocD, notNullDocD, funcDocD, castDocD, objAccessDocD, castObjDocD,
  breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD, publicDocD, 
  dot, new, forLabel, blockCmtStart, blockCmtEnd, docCmtStart, observerListName,
  doxConfigName, makefileName, doubleSlash, blockCmtDoc, docCmtDoc, 
  commentedItem, addCommentsDocD, functionDoc, classDoc, moduleDoc, docFuncRepr,
  valList, appendToBody, surroundBody, getterName, setterName, setMain, 
  setMainMethod, setEmpty, intValue)
import Language.Drasil.Code.Imperative.Data (Terminator(..), AuxData(..), ad, 
  FileData(..), file, updateFileMod, FuncData(..), fd, ModData(..), md, 
  updateModDoc, MethodData(..), mthd, OpData(..), ParamData(..), pd, 
  PackData(..), packD, ProgData(..), progD, TypeData(..), td, ValData(..), 
  VarData(..), vard)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  NameOpts(NameOpts), asFragment, buildSingle, includeExt, inCodePackage, 
  interp, mainModule, mainModuleFile, packSep, withExt)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.Helpers (angles, emptyIfEmpty, 
  liftA4, liftA5, liftA6, liftA7, liftList, lift1List, lift3Pair, 
  lift4Pair, liftPair, liftPairFst, getInnerType, convType, checkParams)
import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
  semi, vcat, lbrace, rbrace, render, colon, comma, render)

jExt :: String
jExt = "java"

jNameOpts :: NameOpts
jNameOpts = NameOpts {
  packSep = ".",
  includeExt = False
}

newtype JavaCode a = JC {unJC :: a}

instance Functor JavaCode where
  fmap f (JC x) = JC (f x)

instance Applicative JavaCode where
  pure = JC
  (JC f) <*> (JC x) = JC (f x)

instance Monad JavaCode where
  return = JC
  JC x >>= f = f x

instance PackageSym JavaCode where
  type Package JavaCode = PackData
  package = lift1List packD

instance ProgramSym JavaCode where
  type Program JavaCode = ProgData
  prog n ms = liftList (progD n) (map (liftA2 (packageDocD n) endStatement) ms)

instance RenderSym JavaCode where
  type RenderFile JavaCode = FileData
  fileDoc code = liftA2 file (fmap (addExt jExt . name) code) (liftA2 
    updateModDoc (liftA2 emptyIfEmpty (fmap modDoc code) $ liftA3 fileDoc' 
    (top code) (fmap modDoc code) bottom) code)

  docMod d m = commentedMod (docComment $ moduleDoc d (moduleName 
    (fmap fileMod m)) jExt) m

  commentedMod cmt m = liftA2 updateFileMod (liftA2 updateModDoc
    (liftA2 commentedItem cmt (fmap (modDoc . fileMod) m)) (fmap fileMod m)) m

instance InternalFile JavaCode where
  top _ = liftA3 jtop endStatement (include "") (list static_)
  bottom = return empty

instance AuxiliarySym JavaCode where
  type Auxiliary JavaCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)

  optimizeDox = return $ text "YES"

  makefile cms = fmap (ad makefileName . makeBuild cms jBuildConfig jRunnable)

instance KeywordSym JavaCode where
  type Keyword JavaCode = Doc
  endStatement = return semi
  endStatementLoop = return empty

  include _ = return $ text "import"
  inherit = return $ text "extends"

  list _ = return $ text "ArrayList"
  listObj = return new

  blockStart = return lbrace
  blockEnd = return rbrace

  ifBodyStart = blockStart
  elseIf = return $ text "else if"
  
  iterForEachLabel = return forLabel
  iterInLabel = return colon

  commentStart = return doubleSlash
  blockCommentStart = return blockCmtStart
  blockCommentEnd = return blockCmtEnd
  docCommentStart = return docCmtStart
  docCommentEnd = blockCommentEnd

instance PermanenceSym JavaCode where
  type Permanence JavaCode = Doc
  static_ = return staticDocD
  dynamic_ = return dynamicDocD

instance BodySym JavaCode where
  type Body JavaCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner s = bodyStatements [s]

  addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym JavaCode where
  type Block JavaCode = Doc
  block sts = lift1List blockDocD endStatement (map (fmap fst .state) sts)

instance StateTypeSym JavaCode where
  type StateType JavaCode = TypeData
  bool = return boolTypeDocD
  int = return intTypeDocD
  float = return jFloatTypeDocD
  char = return charTypeDocD
  string = return jStringTypeDoc
  infile = return jInfileTypeDoc
  outfile = return jOutfileTypeDoc
  listType p st = liftA2 jListType st (list p)
  listInnerType t = fmap (getInnerType . cType) t >>= convType
  obj t = return $ typeDocD t
  enumType t = return $ enumTypeDocD t
  iterator _ = error "Iterator-type variables do not exist in Java"
  void = return voidDocD

  getType = cType . unJC

instance ControlBlockSym JavaCode where
  runStrategy l strats rv av = maybe
    (strError l "RunStrategy called on non-existent strategy") 
    (liftA2 (flip stratDocD) (state resultState)) 
    (Map.lookup l (Map.fromList strats))
    where resultState = maybe (return (mkStNoEnd empty)) asgState av
          asgState v = maybe (strError l 
            "Attempt to assign null return to a Value") (assign v) rv
          strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

  listSlice vnew vold b e s = 
    let l_temp = "temp"
        var_temp = var l_temp (variableType vnew)
        v_temp = valueOf var_temp
        l_i = "i_temp"
        var_i = var l_i int
        v_i = valueOf var_i
    in
      block [
        listDec 0 var_temp,
        for (varDecDef var_i (fromMaybe (litInt 0) b)) 
          (v_i ?< fromMaybe (listSize vold) e) (maybe (var_i &++) (var_i &+=) s)
          (oneLiner $ valState $ listAppend v_temp (listAccess vold v_i)),
        vnew &= v_temp]

instance UnaryOpSym JavaCode where
  type UnaryOp JavaCode = OpData
  notOp = return notOpDocD
  negateOp = return negateOpDocD
  sqrtOp = return $ unOpPrec "Math.sqrt"
  absOp = return $ unOpPrec "Math.abs"
  logOp = return $ unOpPrec "Math.log10"
  lnOp = return $ unOpPrec "Math.log"
  expOp = return $ unOpPrec "Math.exp"
  sinOp = return $ unOpPrec "Math.sin"
  cosOp = return $ unOpPrec "Math.cos"
  tanOp = return $ unOpPrec "Math.tan"
  asinOp = return $ unOpPrec "Math.asin"
  acosOp = return $ unOpPrec "Math.acos"
  atanOp = return $ unOpPrec "Math.atan"
  floorOp = return $ unOpPrec "Math.floor"
  ceilOp = return $ unOpPrec "Math.ceil"

instance BinaryOpSym JavaCode where
  type BinaryOp JavaCode = OpData
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
  powerOp = return $ powerPrec "Math.pow"
  moduloOp = return moduloOpDocD
  andOp = return andOpDocD
  orOp = return orOpDocD

instance VariableSym JavaCode where
  type Variable JavaCode = VarData
  var n t = liftA2 (vard n) t (return $ varDocD n) 
  const = var
  extVar l n t = liftA2 (vard $ l ++ "." ++ n) t (return $ extVarDocD l n)
  self l = liftA2 (vard "this") (obj l) (return selfDocD)
  enumVar e en = var e (enumType en)
  objVar o v = liftA2 (vard $ variableName o ++ "." ++ variableName v)
    (variableType v) (liftA2 objVarDocD o v)
  objVarSelf _ = var
  listVar n p t = var n (listType p t)
  n `listOf` t = listVar n static_ t
  iterVar n t = var n (iterator t)

  ($->) = objVar

  variableName = varName . unJC
  variableType = fmap varType
  variableDoc = varDoc . unJC

instance ValueSym JavaCode where
  type Value JavaCode = ValData
  litTrue = liftA2 mkVal bool (return litTrueD)
  litFalse = liftA2 mkVal bool (return litFalseD)
  litChar c = liftA2 mkVal char (return $ litCharD c)
  litFloat v = liftA2 mkVal float (return $ litFloatD v)
  litInt v = liftA2 mkVal int (return $ litIntD v)
  litString s = liftA2 mkVal string (return $ litStringD s)

  ($:) = enumElement

  valueOf v = liftA2 mkVal (variableType v) (return $ variableDoc v) 
  arg n = liftA2 mkVal string (liftA2 argDocD (litInt n) argsList)
  enumElement en e = liftA2 mkVal (enumType en) (return $ enumElemDocD en e)
  
  argsList = liftA2 mkVal (listType static_ string) (return $ text "args")

  valueType = fmap valType
  valueDoc = valDoc . unJC

instance NumericExpression JavaCode where
  (#~) = liftA2 unExpr' negateOp
  (#/^) = liftA2 unExpr sqrtOp
  (#|) = liftA2 unExpr absOp
  (#+) = liftA3 binExpr plusOp
  (#-) = liftA3 binExpr minusOp
  (#*) = liftA3 binExpr multOp
  (#/) = liftA3 binExpr divideOp
  (#%) = liftA3 binExpr moduloOp
  (#^) = liftA3 binExpr' powerOp

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

instance BooleanExpression JavaCode where
  (?!) = liftA3 typeUnExpr notOp bool
  (?&&) = liftA4 typeBinExpr andOp bool
  (?||) = liftA4 typeBinExpr orOp bool

  (?<) = liftA4 typeBinExpr lessOp bool
  (?<=) = liftA4 typeBinExpr lessEqualOp bool
  (?>) = liftA4 typeBinExpr greaterOp bool
  (?>=) = liftA4 typeBinExpr greaterEqualOp bool
  (?==) = jEquality
  (?!=) = liftA4 typeBinExpr notEqualOp bool
  
instance ValueExpression JavaCode where
  inlineIf = liftA3 inlineIfD
  funcApp n t vs = liftA2 mkVal t (liftList (funcAppDocD n) vs)
  selfFuncApp = funcApp
  extFuncApp l n t vs = liftA2 mkVal t (liftList (extFuncAppDocD l n) vs)
  stateObj t vs = liftA2 mkVal t (liftA2 stateObjDocD t (liftList valList vs))
  extStateObj _ = stateObj
  listStateObj t vs = liftA2 mkVal t (liftA3 listStateObjDocD listObj t 
    (liftList valList vs))

  exists = notNull
  notNull v = liftA2 mkVal bool (liftA3 notNullDocD notEqualOp v (valueOf (var 
    "null" (fmap valType v))))

instance InternalValue JavaCode where
  inputFunc = liftA2 mkVal (obj "Scanner") (return $ parens (
    text "new Scanner(System.in)"))
  printFunc = liftA2 mkVal void (return $ text "System.out.print")
  printLnFunc = liftA2 mkVal void (return $ text "System.out.println")
  printFileFunc f = liftA2 mkVal void (fmap (printFileDocD "print") f)
  printFileLnFunc f = liftA2 mkVal void (fmap (printFileDocD "println") f)
  
  cast = jCast

instance Selector JavaCode where
  objAccess v f = liftA2 mkVal (fmap funcType f) (liftA2 objAccessDocD v f)
  ($.) = objAccess

  objMethodCall t o f ps = objAccess o (func f t ps)
  objMethodCallNoParams t o f = objMethodCall t o f []

  selfAccess l = objAccess (valueOf $ self l)

  listIndexExists l i = listSize l ?> i 

  argExists i = listAccess argsList (litInt $ fromIntegral i)

  indexOf l v = objAccess l (func "indexOf" int [v])

instance FunctionSym JavaCode where
  type Function JavaCode = FuncData
  func l t vs = liftA2 fd t (fmap funcDocD (funcApp l t vs))

  get v vToGet = v $. getFunc vToGet
  set v vToSet toVal = v $. setFunc (valueType v) vToSet toVal

  listSize v = v $. listSizeFunc
  listAdd v i vToAdd = v $. listAddFunc v i vToAdd
  listAppend v vToApp = v $. listAppendFunc vToApp
  
  iterBegin v = v $. iterBeginFunc (valueType v)
  iterEnd v = v $. iterEndFunc (valueType v)

instance SelectorFunction JavaCode where
  listAccess v i = v $. listAccessFunc (listInnerType $ valueType v) i
  listSet v i toVal = v $. listSetFunc v i toVal
  at v l = listAccess v (valueOf $ var l int)

instance InternalFunction JavaCode where
  getFunc v = func (getterName $ variableName v) (variableType v) []
  setFunc t v toVal = func (setterName $ variableName v) t [toVal]

  listSizeFunc = func "size" int []
  listAddFunc _ i v = func "add" (listType static_ $ fmap valType v) [i, v]
  listAppendFunc v = func "add" (listType static_ $ fmap valType v) [v]

  iterBeginFunc _ = error "Attempt to use iterBeginFunc in Java, but Java has no iterators"
  iterEndFunc _ = error "Attempt to use iterEndFunc in Java, but Java has no iterators"
  
  listAccessFunc t i = func "get" t [intValue i]
  listSetFunc v i toVal = func "set" (valueType v) [intValue i, toVal]

  atFunc t l = listAccessFunc t (valueOf $ var l int)

instance InternalStatement JavaCode where
  printSt _ p v _ = mkSt <$> liftA2 printDoc p v

  state = fmap statementDocD
  loopState = fmap (statementDocD . setEmpty)

instance StatementSym JavaCode where
  -- Terminator determines how statements end
  type Statement JavaCode = (Doc, Terminator)
  assign vr vl = mkSt <$> liftA2 assignDocD vr vl
  assignToListIndex lst index v = valState $ listSet (valueOf lst) index v
  multiAssign _ _ = error "No multiple assignment statements in Java"
  (&=) = assign
  (&-=) vr vl = vr &= (valueOf vr #- vl)
  (&+=) vr vl = mkSt <$> liftA2 plusEqualsDocD vr vl
  (&++) v = mkSt <$> fmap plusPlusDocD v
  (&~-) v = v &= (valueOf v #- litInt 1)

  varDec v = mkSt <$> fmap varDecDocD v
  varDecDef v def = mkSt <$> liftA2 varDecDefDocD v def
  listDec n v = mkSt <$> liftA2 listDecDocD v (litInt n)
  listDecDef v vs = mkSt <$> liftA2 jListDecDef v (liftList valList vs)
  objDecDef v def = mkSt <$> liftA2 objDecDefDocD v def
  objDecNew v vs = mkSt <$> liftA2 objDecDefDocD v (stateObj (variableType v) 
    vs)
  extObjDecNew _ = objDecNew
  objDecNewVoid v = mkSt <$> liftA2 objDecDefDocD v (stateObj (variableType v) 
    [])
  extObjDecNewVoid _ = objDecNewVoid
  constDecDef v def = mkSt <$> liftA2 jConstDecDef v def

  print v = outDoc False printFunc v Nothing
  printLn v = outDoc True printLnFunc v Nothing
  printStr s = outDoc False printFunc (litString s) Nothing
  printStrLn s = outDoc True printLnFunc (litString s) Nothing

  printFile f v = outDoc False (printFileFunc f) v (Just f)
  printFileLn f v = outDoc True (printFileLnFunc f) v (Just f)
  printFileStr f s = outDoc False (printFileFunc f) (litString s) (Just f)
  printFileStrLn f s = outDoc True (printFileLnFunc f) (litString s) (Just f)

  getInput v = v &= liftA2 jInput (variableType v) inputFunc
  discardInput = mkSt <$> fmap jDiscardInput inputFunc
  getFileInput f v = v &= liftA2 jInput (variableType v) f
  discardFileInput f = mkSt <$> fmap jDiscardInput f

  openFileR f n = f &= liftA2 jOpenFileR n infile
  openFileW f n = f &= liftA3 jOpenFileWorA n outfile litFalse
  openFileA f n = f &= liftA3 jOpenFileWorA n outfile litTrue 
  closeFile f = valState $ objMethodCall void f "close" []

  getFileInputLine f v = v &= f $. func "nextLine" string []
  discardFileLine f = valState $ f $. func "nextLine" string []
  stringSplit d vnew s = mkSt <$> liftA2 jStringSplit vnew 
    (funcApp "Arrays.asList" (listType static_ string) 
    [s $. func "split" (listType static_ string) [litString [d]]])

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = return (mkSt breakDocD)  -- I could have a JumpSym class with functions for "return $ text "break" and then reference those functions here?
  continue = return (mkSt continueDocD)

  returnState v = mkSt <$> liftList returnDocD [v]
  multiReturn _ = error "Cannot return multiple values in Java"

  valState v = mkSt <$> fmap valDoc v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free _ = error "Cannot free variables in Java" -- could set variable to null? Might be misleading.

  throw errMsg = mkSt <$> fmap jThrowDoc (litString errMsg)

  initState fsmName initialState = varDecDef (var fsmName string) 
    (litString initialState)
  changeState fsmName toState = var fsmName string &= litString toState

  initObserverList t = listDecDef (var observerListName t)
  addObserver o = valState $ listAdd obsList lastelem o
    where obsList = valueOf $ observerListName `listOf` valueType o
          lastelem = listSize obsList

  inOutCall = jInOutCall funcApp
  extInOutCall m = jInOutCall (extFuncApp m)

  multi = lift1List multiStateDocD endStatement

instance ControlStatementSym JavaCode where
  ifCond bs b = mkStNoEnd <$> lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b
    bs
  ifNoElse bs = ifCond bs $ body []
  switch v cs c = mkStNoEnd <$> lift3Pair switchDocD (state break) v c cs
  switchAsIf v cs = ifCond cases
    where cases = map (\(l, b) -> (v ?== l, b)) cs

  ifExists v ifBody = ifCond [(notNull v, ifBody)]

  for sInit vGuard sUpdate b = mkStNoEnd <$> liftA6 forDocD blockStart blockEnd 
    (loopState sInit) vGuard (loopState sUpdate) b
  forRange i initv finalv stepv = for (varDecDef (var i int) initv) 
    (valueOf (var i int) ?< finalv) (var i int &+= stepv)
  forEach l v b = mkStNoEnd <$> liftA7 (forEachDocD l) blockStart blockEnd
    iterForEachLabel iterInLabel (listInnerType $ valueType v) v b
  while v b = mkStNoEnd <$> liftA4 whileDocD blockStart blockEnd v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 jTryCatch tb cb
  
  checkState l = switch (valueOf $ var l string)
  notifyObservers f t = for initv (v_index ?< listSize obsList) 
    (var_index &++) notify
    where obsList = valueOf $ observerListName `listOf` t
          index = "observerIndex"
          var_index = var index int
          v_index = valueOf var_index
          initv = varDecDef var_index $ litInt 0
          notify = oneLiner $ valState $ at obsList index $. f

  getFileInputAll f v = while (f $. func "hasNextLine" bool [])
    (oneLiner $ valState $ listAppend (valueOf v) (f $. func "nextLine" string []))

instance ScopeSym JavaCode where
  type Scope JavaCode = Doc
  private = return privateDocD
  public = return publicDocD

instance InternalScope JavaCode where
  includeScope s = s

instance MethodTypeSym JavaCode where
  type MethodType JavaCode = TypeData
  mState t = t
  construct n = return $ td (Object n) (constructDocD n)

instance ParameterSym JavaCode where
  type Parameter JavaCode = ParamData
  stateParam = fmap (mkParam stateParamDocD)
  pointerParam = stateParam

  parameterName = paramName . unJC
  parameterType = fmap paramType

instance MethodSym JavaCode where
  type Method JavaCode = MethodData
  method n _ s p t ps b = liftA2 (mthd False) (checkParams n <$> sequence ps) 
    (liftA5 (jMethod n) s p t (liftList paramListDocD ps) b)
  getMethod c v = method (getterName $ variableName v) c public dynamic_ 
    (mState $ variableType v) [] getBody
    where getBody = oneLiner $ returnState (valueOf $ self c $-> v)
  setMethod c v = method (setterName $ variableName v) c public dynamic_ 
    (mState void) [stateParam v] setBody
    where setBody = oneLiner $ (self c $-> v) &= valueOf v
  mainMethod c b = setMainMethod <$> method "main" c public static_ 
    (mState void) [liftA2 (pd "args") (listType static_ string) 
    (return $ text "String[] args")] b
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor _ _ = error "Destructors not allowed in Java"

  docMain c b = commentedFunc (docComment $ functionDoc 
    "Controls the flow of the program" 
    [("args", "List of command-line arguments")]) (mainMethod c b)

  function n = method n ""

  docFunc = docFuncRepr

  inOutFunc n s p ins [] [] b = function n s p (mState void) (map stateParam 
    ins) b
  inOutFunc n s p ins [v] [] b = function n s p (mState $ variableType v) 
    (map stateParam ins) (liftA3 surroundBody (varDec v) b (returnState $ 
    valueOf v))
  inOutFunc n s p ins [] [v] b = function n s p (mState $ variableType v) (map 
    stateParam $ v : ins) (liftA2 appendToBody b (returnState $ valueOf v))
  inOutFunc n s p ins outs both b = function n s p jArrayType
    (map stateParam $ both ++ ins) (liftA3 surroundBody decls b (multi 
      (varDecDef outputs (valueOf (var 
        ("new Object[" ++ show (length $ both ++ outs) ++ "]") jArrayType))
      : assignArray 0 (map valueOf $ both ++ outs)
      ++ [returnState (valueOf outputs)])))
      where assignArray :: Int -> [JavaCode (Value JavaCode)] -> 
              [JavaCode (Statement JavaCode)]
            assignArray _ [] = []
            assignArray c (v:vs) = (var ("outputs[" ++ show c ++ "]") 
              (fmap valType v) &= v) : assignArray (c+1) vs
            decls = multi $ map varDec outs
            outputs = var "outputs" jArrayType
    
  docInOutFunc desc iComms _ bComms = docFuncRepr desc (bComms ++ iComms)
            
  commentedFunc cmt fn = liftA3 mthd (fmap isMainMthd fn) (fmap mthdParams fn) 
    (liftA2 commentedItem cmt (fmap mthdDoc fn))
    
  parameters m = map return $ (mthdParams . unJC) m

instance StateVarSym JavaCode where
  type StateVar JavaCode = Doc
  stateVar _ s p v = liftA4 stateVarDocD (includeScope s) p v endStatement
  privMVar del = stateVar del private dynamic_
  pubMVar del = stateVar del public dynamic_
  pubGVar del = stateVar del public static_

instance ClassSym JavaCode where
  -- Bool is True if the method is a main method, False otherwise
  type Class JavaCode = (Doc, Bool)
  buildClass n p s vs fs = liftPairFst (liftA4 (classDocD n p) inherit s 
    (liftList stateVarListDocD vs) (liftList methodListDocD (map (fmap mthdDoc) 
    fs)), any (isMainMthd . unJC) fs)
  enum n es s = liftPairFst (liftA2 (enumDocD n) (return $ 
    enumElementsDocD es enumsEqualInts) s, False)
  mainClass n vs fs = setMain <$> buildClass n Nothing public vs fs
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public
  
  docClass d = commentedClass (docComment $ classDoc d)

  commentedClass cmt cs = liftPair (liftA2 commentedItem cmt (fmap fst cs), 
    fmap snd cs)

instance ModuleSym JavaCode where
  type Module JavaCode = ModData
  buildModule n _ ms cs = fmap (md n (any (isMainMthd . unJC) ms || 
    any (snd . unJC) cs)) (liftList moduleDocD (if null ms then cs 
    else pubClass n Nothing [] ms : cs))

  moduleName m = name (unJC m)

instance BlockCommentSym JavaCode where
  type BlockComment JavaCode = Doc
  blockComment lns = liftA2 (blockCmtDoc lns) blockCommentStart blockCommentEnd
  docComment lns = liftA2 (docCmtDoc lns) docCommentStart docCommentEnd 

enumsEqualInts :: Bool
enumsEqualInts = False

jtop :: Doc -> Doc -> Doc -> Doc
jtop end inc lst = vcat [
  inc <+> text "java.util.Arrays" <> end,
  inc <+> text "java.util.BitSet" <> end, --TODO: only include these if they are used in the code?
  inc <+> text "java.util.Scanner" <> end,
  inc <+> text "java.io.PrintWriter" <> end,
  inc <+> text "java.io.FileWriter" <> end,
  inc <+> text "java.io.File" <> end,
  inc <+> text ("java.util." ++ render lst) <> end]

jFloatTypeDocD :: TypeData
jFloatTypeDocD = td Float (text "double")

jStringTypeDoc :: TypeData
jStringTypeDoc = td String (text "String")

jInfileTypeDoc :: TypeData
jInfileTypeDoc = td File (text "Scanner")

jOutfileTypeDoc :: TypeData
jOutfileTypeDoc = td File (text "PrintWriter")

jListType :: TypeData -> Doc -> TypeData
jListType (TD Integer _) lst = td (List Integer) (lst <> angles (text "Integer"))
jListType (TD Float _) lst = td (List Float) (lst <> angles (text "Double"))
jListType t lst = listTypeDocD t lst

jArrayType :: JavaCode (StateType JavaCode)
jArrayType = return $ td (List $ Object "Object") (text "Object[]")

jEquality :: JavaCode (Value JavaCode) -> JavaCode (Value JavaCode) -> 
  JavaCode (Value JavaCode)
jEquality v1 v2 = jEquality' (getType $ valueType v2)
  where jEquality' String = objAccess v1 (func "equals" bool [v2])
        jEquality' _ = liftA4 typeBinExpr equalOp bool v1 v2

jCast :: JavaCode (StateType JavaCode) -> JavaCode (Value JavaCode) -> 
  JavaCode (Value JavaCode)
jCast t v = jCast' (getType t) (getType $ valueType v)
  where jCast' Float String = funcApp "Double.parseDouble" float [v]
        jCast' Integer (Enum _) = v $. func "ordinal" int []
        jCast' _ _ = liftA2 mkVal t $ liftA2 castObjDocD (fmap castDocD t) v

jListDecDef :: VarData -> Doc -> Doc
jListDecDef v vs = typeDoc (varType v) <+> varDoc v <+> equals <+> new <+> 
  typeDoc (varType v) <+> parens listElements
  where listElements = emptyIfEmpty vs $ text "Arrays.asList" <> parens vs

jConstDecDef :: VarData -> ValData -> Doc
jConstDecDef v def = text "final" <+> typeDoc (varType v) <+> varDoc v <+> 
  equals <+> valDoc def

jThrowDoc :: ValData -> Doc
jThrowDoc errMsg = text "throw new" <+> text "Exception" <> parens (valDoc 
  errMsg)

jTryCatch :: Doc -> Doc -> Doc
jTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent tb,
  rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> 
    lbrace,
  indent cb,
  rbrace]

jDiscardInput :: ValData -> Doc
jDiscardInput inFn = valDoc inFn <> dot <> text "next()"

jInput :: TypeData -> ValData -> ValData
jInput t inFn = mkVal t $ jInput' (cType t) 
  where jInput' Integer = text "Integer.parseInt" <> parens (valDoc inFn <> 
          dot <> text "nextLine()")
        jInput' Float = text "Double.parseDouble" <> parens (valDoc inFn <> 
          dot <> text "nextLine()")
        jInput' Boolean = valDoc inFn <> dot <> text "nextBoolean()"
        jInput' String = valDoc inFn <> dot <> text "nextLine()"
        jInput' Char = valDoc inFn <> dot <> text "next().charAt(0)"
        jInput' _ = error "Attempt to read value of unreadable type"

jOpenFileR :: ValData -> TypeData -> ValData
jOpenFileR n t = mkVal t $ new <+> text "Scanner" <> parens 
  (new <+> text "File" <> parens (valDoc n))

jOpenFileWorA :: ValData -> TypeData -> ValData -> ValData
jOpenFileWorA n t wa = mkVal t $ new <+> text "PrintWriter" <> 
  parens (new <+> text "FileWriter" <> parens (new <+> text "File" <> 
  parens (valDoc n) <> comma <+> valDoc wa))

jStringSplit :: VarData -> ValData -> Doc
jStringSplit vnew s = varDoc vnew <+> equals <+> new <+> typeDoc (varType vnew)
  <> parens (valDoc s)

jMethod :: Label -> Doc -> Doc -> TypeData -> Doc -> Doc -> Doc
jMethod n s p t ps b = vcat [
  s <+> p <+> typeDoc t <+> text n <> parens ps <+> text "throws Exception" <+> 
    lbrace,
  indent b,
  rbrace]

jAssignFromArray :: Int -> [JavaCode (Variable JavaCode)] -> 
  [JavaCode (Statement JavaCode)]
jAssignFromArray _ [] = []
jAssignFromArray c (v:vs) = (v &= cast (variableType v)
  (valueOf (var ("outputs[" ++ show c ++ "]") (variableType v))))
  : jAssignFromArray (c+1) vs

jInOutCall :: (Label -> JavaCode (StateType JavaCode) -> 
  [JavaCode (Value JavaCode)] -> JavaCode (Value JavaCode)) -> Label -> 
  [JavaCode (Value JavaCode)] -> [JavaCode (Variable JavaCode)] -> 
  [JavaCode (Variable JavaCode)] -> JavaCode (Statement JavaCode)
jInOutCall f n ins [] [] = valState $ f n void ins
jInOutCall f n ins [out] [] = assign out $ f n (variableType out) ins
jInOutCall f n ins [] [out] = assign out $ f n (variableType out) (valueOf out 
  : ins)
jInOutCall f n ins outs both = multi $ varDecDef (var "outputs" jArrayType) 
  (f n jArrayType (map valueOf both ++ ins)) : jAssignFromArray 0 (both ++ outs)

jBuildConfig :: Maybe BuildConfig
jBuildConfig = buildSingle (\i _ -> asFragment "javac" : i) $
  inCodePackage mainModuleFile

jRunnable :: Runnable
jRunnable = interp (flip withExt ".class" $ inCodePackage mainModule) 
  jNameOpts "java"