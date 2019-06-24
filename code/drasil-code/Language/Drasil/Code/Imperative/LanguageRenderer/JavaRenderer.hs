{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer (
  -- * Java Code Configuration -- defines syntax of all Java code
  JavaCode(..), jNameOpts
) where

import Utils.Drasil (indent)

import Language.Drasil.Code.Code (CodeType(..))
import Language.Drasil.Code.Imperative.Symantics (Label,
  PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
  UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  Selector(..), FunctionSym(..), SelectorFunction(..), StatementSym(..), 
  ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..), ParameterSym(..),
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.Build.AST (includeExt, 
  NameOpts(NameOpts), packSep)
import Language.Drasil.Code.Imperative.LanguageRenderer ( 
  packageDocD, fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
  multiStateDocD, blockDocD, bodyDocD, outDocD, printFileDocD, boolTypeDocD,
  intTypeDocD, charTypeDocD, typeDocD, listTypeDocD, voidDocD, constructDocD, 
  stateParamDocD, paramListDocD, methodListDocD, stateVarDocD, 
  stateVarListDocD, ifCondDocD, switchDocD, forDocD, forEachDocD, whileDocD, 
  stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD,
  varDecDefDocD, listDecDocD, objDecDefDocD, statementDocD, returnDocD,
  commentDocD, mkSt, mkStNoEnd, notOpDocD, negateOpDocD, unExpr, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', mkVal, litTrueD, 
  litFalseD, litCharD, litFloatD, litIntD, litStringD, varDocD, extVarDocD, 
  selfDocD, argDocD, enumElemDocD, objVarDocD, inlineIfDocD, funcAppDocD, 
  extFuncAppDocD, stateObjDocD, listStateObjDocD, notNullDocD, funcDocD, 
  castDocD, objAccessDocD, castObjDocD, breakDocD, continueDocD, staticDocD, 
  dynamicDocD, privateDocD, publicDocD, dot, new, forLabel, observerListName,
  doubleSlash, addCommentsDocD, callFuncParamList, getterName, setterName,
  setMain, setEmpty, statementsToStateVars)
import Language.Drasil.Code.Imperative.Helpers (Terminator(..), ModData(..), md,
  TypeData(..), td, angles, liftA4, liftA5, liftA6, liftA7, liftList, lift1List,
  lift3Pair, lift4Pair, liftPairFst)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
  semi, vcat, lbrace, rbrace, render, colon, comma, isEmpty, render)

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
  type Package JavaCode = ([ModData], Label)
  packMods n ms = liftPairFst (mapM (liftA2 (packageDocD n) endStatement) mods, n)
    where mods = filter (not . isEmpty . modDoc . unJC) ms

instance RenderSym JavaCode where
  type RenderFile JavaCode = ModData
  fileDoc code = liftA3 md (fmap name code) (fmap isMainMod code) 
    (if isEmpty (modDoc (unJC code)) then return empty else
    liftA3 fileDoc' (top code) (fmap modDoc code) bottom)
  top _ = liftA3 jtop endStatement (include "") (list static_)
  bottom = return empty

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
  
  printFunc = return $ text "System.out.print"
  printLnFunc = return $ text "System.out.println"
  printFileFunc = fmap (printFileDocD "print")
  printFileLnFunc = fmap (printFileDocD "println")

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
  obj t = return $ typeDocD t
  enumType t = return $ typeDocD t
  iterator _ = error "Iterator-type variables do not exist in Java"

instance ControlBlockSym JavaCode where
  runStrategy l strats rv av = maybe
    (strError l "RunStrategy called on non-existent strategy") 
    (liftA2 (flip stratDocD) (state resultState)) 
    (Map.lookup l (Map.fromList strats))
    where resultState = maybe (return (mkStNoEnd empty)) asgState av
          asgState v = maybe (strError l 
            "Attempt to assign null return to a Value") (assign v) rv
          strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

  listSlice t vnew vold b e s = 
    let l_temp = "temp"
        v_temp = var l_temp
        l_i = "i_temp"
        v_i = var l_i
    in
      block [
        listDec l_temp 0 t,
        for (varDecDef l_i int (fromMaybe (litInt 0) b)) 
          (v_i ?< fromMaybe (vold $. listSize) e) (maybe (v_i &++) (v_i &+=) s)
          (oneLiner $ valState $ v_temp $. listAppend (vold $. listAccess v_i)),
        vnew &= v_temp]

instance UnaryOpSym JavaCode where
  type UnaryOp JavaCode = Doc
  notOp = return notOpDocD
  negateOp = return negateOpDocD
  sqrtOp = return $ text "Math.sqrt"
  absOp = return $ text "Math.abs"
  logOp = return $ text "Math.log10"
  lnOp = return $ text "Math.log"
  expOp = return $ text "Math.exp"
  sinOp = return $ text "Math.sin"
  cosOp = return $ text "Math.cos"
  tanOp = return $ text "Math.tan"
  asinOp = return $ text "Math.asin"
  acosOp = return $ text "Math.acos"
  atanOp = return $ text "Math.atan"
  floorOp = return $ text "Math.floor"
  ceilOp = return $ text "Math.ceil"

instance BinaryOpSym JavaCode where
  type BinaryOp JavaCode = Doc
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
  powerOp = return $ text "Math.pow"
  moduloOp = return moduloOpDocD
  andOp = return andOpDocD
  orOp = return orOpDocD

instance ValueSym JavaCode where
  -- Maybe String is the String representation of the value
  type Value JavaCode = (Doc, Maybe String)
  litTrue = return (litTrueD, Just "true")
  litFalse = return (litFalseD, Just "false")
  litChar c = return (litCharD c, Just $ "\'" ++ [c] ++ "\'")
  litFloat v = return (litFloatD v, Just $ show v)
  litInt v = return (litIntD v, Just $ show v)
  litString s = return (litStringD s, Just $ "\"" ++ s ++ "\"")

  ($->) = objVar
  ($:) = enumElement

  const = var
  var n = return (varDocD n, Just n)
  extVar l n = return (extVarDocD l n, Just $ l ++ "." ++ n)
  self = return (selfDocD, Just "this")
  arg n = mkVal <$> liftA2 argDocD (litInt n) argsList
  enumElement en e = return (enumElemDocD en e, Just $ en ++ "." ++ e)
  enumVar = var
  objVar o v = liftPairFst (liftA2 objVarDocD o v, Just $ valName o ++ "." ++ 
    valName v)
  objVarSelf = var
  listVar n _ = var n
  n `listOf` t = listVar n t
  iterVar = var
  
  inputFunc = return (mkVal $ parens (text "new Scanner(System.in)"))
  argsList = return (mkVal $ text "args")

  valName (JC (v, s)) = fromMaybe 
    (error $ "Attempt to print unprintable Value (" ++ render v ++ ")") s

instance NumericExpression JavaCode where
  (#~) = liftA2 unExpr negateOp
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
  (?!) = liftA2 unExpr notOp
  (?&&) = liftA3 binExpr andOp
  (?||) = liftA3 binExpr orOp

  (?<) = liftA3 binExpr lessOp
  (?<=) = liftA3 binExpr lessEqualOp
  (?>) = liftA3 binExpr greaterOp
  (?>=) = liftA3 binExpr greaterEqualOp
  (?==) = liftA3 binExpr equalOp
  (?!=) = liftA3 binExpr notEqualOp
  
instance ValueExpression JavaCode where
  inlineIf b v1 v2 = mkVal <$> liftA3 inlineIfDocD b v1 v2
  funcApp n vs = mkVal <$> liftList (funcAppDocD n) vs
  selfFuncApp = funcApp
  extFuncApp l n vs = mkVal <$> liftList (extFuncAppDocD l n) vs
  stateObj t vs = mkVal <$> liftA2 stateObjDocD t 
    (liftList callFuncParamList vs)
  extStateObj _ = stateObj
  listStateObj t vs = mkVal <$> liftA3 listStateObjDocD listObj t 
    (liftList callFuncParamList vs)

  exists = notNull
  notNull v = mkVal <$> liftA3 notNullDocD notEqualOp v (var "null")

instance Selector JavaCode where
  objAccess v f = mkVal <$> liftA2 objAccessDocD v f
  ($.) = objAccess

  objMethodCall o f ps = objAccess o (func f ps)
  objMethodCallVoid o f = objMethodCall o f []

  selfAccess = objAccess self

  listSizeAccess v = objAccess v listSize

  listIndexExists l i = mkVal <$> liftA3 jListIndexExists greaterOp l i

  argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))

  indexOf l v = objAccess l (fmap funcDocD (funcApp "indexOf" [v]))

  stringEqual v1 str = objAccess v1 (func "equals" [str])

  castObj f v = mkVal <$> liftA2 castObjDocD f v
  castStrToFloat v = funcApp "Double.parseDouble" [v]

instance FunctionSym JavaCode where
  type Function JavaCode = Doc
  func l vs = fmap funcDocD (funcApp l vs)
  cast targT _ = fmap castDocD targT
  castListToInt = fmap funcDocD (funcApp "ordinal" [])
  get n = fmap funcDocD (funcApp (getterName n) [])
  set n v = fmap funcDocD (funcApp (setterName n) [v])

  listSize = fmap funcDocD (funcApp "size" [])
  listAdd i v = fmap funcDocD (funcApp "add" [i, v])
  listAppend v = fmap funcDocD (funcApp "add" [v])

  iterBegin = fmap funcDocD (funcApp "begin" [])
  iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction JavaCode where
  listAccess i = fmap funcDocD (funcApp "get" [i])
  listSet i v = fmap funcDocD (funcApp "set" [i, v])

  listAccessEnum t v = listAccess (castObj (cast int t) v)
  listSetEnum t i = listSet (castObj (cast int t) i)

  at l = listAccess (var l)

instance StatementSym JavaCode where
  -- Terminator determines how statements end
  type Statement JavaCode = (Doc, Terminator)
  assign v1 v2 = mkSt <$> liftA2 assignDocD v1 v2
  assignToListIndex lst index v = valState $ lst $. listSet index v
  (&=) = assign
  (&.=) l = assign (var l)
  (&=.) v l = assign v (var l)
  (&-=) v1 v2 = v1 &= (v1 #- v2)
  (&.-=) l v = l &.= (var l #- v)
  (&+=) v1 v2 = mkSt <$> liftA2 plusEqualsDocD v1 v2
  (&.+=) l v = var l &+= v
  (&++) v = mkSt <$> fmap plusPlusDocD v
  (&.++) l = (&++) (var l)
  (&~-) v = v &= (v #- litInt 1)
  (&.~-) l = (&~-) (var l)

  varDec l t = mkSt <$> fmap (varDecDocD l) t
  varDecDef l t v = mkSt <$> liftA2 (varDecDefDocD l) t v
  listDec l n t = mkSt <$> liftA2 (listDecDocD l) (litInt n) t -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
  listDecDef l t vs = mkSt <$> liftA2 (jListDecDef l) t (liftList 
    callFuncParamList vs)
  objDecDef l t v = mkSt <$> liftA2 (objDecDefDocD l) t v
  objDecNew l t vs = mkSt <$> liftA2 (objDecDefDocD l) t (stateObj t vs)
  extObjDecNew l _ = objDecNew l
  objDecNewVoid l t = mkSt <$> liftA2 (objDecDefDocD l) t (stateObj t [])
  extObjDecNewVoid l _ = objDecNewVoid l
  constDecDef l t v = mkSt <$> liftA2 (jConstDecDef l) t v

  print _ v = mkSt <$> liftA2 outDocD printFunc v
  printLn _ v = mkSt <$> liftA2 outDocD printLnFunc v
  printStr s = mkSt <$> liftA2 outDocD printFunc (litString s)
  printStrLn s = mkSt <$> liftA2 outDocD printLnFunc (litString s)

  printFile f _ v = mkSt <$> liftA2 outDocD (printFileFunc f) v
  printFileLn f _ v = mkSt <$> liftA2 outDocD (printFileLnFunc f) v
  printFileStr f s = mkSt <$> liftA2 outDocD (printFileFunc f) 
    (litString s)
  printFileStrLn f s = mkSt <$> liftA2 outDocD (printFileLnFunc f) 
    (litString s)

  printList t v = multi [state (printStr "["), 
    for (varDecDef "i" int (litInt 0)) (var "i" ?< 
      ((v $. listSize) #- litInt 1)) ("i" &.++) 
      (bodyStatements [print t (v $. listAccess (var "i")), printStr ","]), 
    state (print t (v $. listAccess ((v $. listSize) #- litInt 1))), 
    printStr "]"]
  printLnList t v = multi [state (printStr "["), 
    for (varDecDef "i" int (litInt 0)) (var "i" ?< 
      ((v $. listSize) #- litInt 1)) ("i" &.++) 
      (bodyStatements [print t (v $. listAccess (var "i")), printStr ","]), 
    state (print t (v $. listAccess ((v $. listSize) #- litInt 1))), 
    printStrLn "]"]
  printFileList f t v = multi [state (printFileStr f "["), 
    for (varDecDef "i" int (litInt 0)) (var "i" ?< 
      ((v $. listSize) #- litInt 1)) ("i" &.++) (bodyStatements 
      [printFile f t (v $. listAccess (var "i")), printFileStr f ","]), 
    state (printFile f t (v $. listAccess ((v $. listSize) #- litInt 1))), 
    printFileStr f "]"]
  printFileLnList f t v = multi [state (printFileStr f "["), 
    for (varDecDef "i" int (litInt 0)) (var "i" ?< 
      ((v $. listSize) #- litInt 1)) ("i" &.++) (bodyStatements 
      [printFile f t (v $. listAccess (var "i")), printFileStr f ","]), 
    state (printFile f t (v $. listAccess ((v $. listSize) #- litInt 1))), 
    printFileStrLn f "]"]

  getIntInput v = mkSt <$> liftA3 jInput' (return $ text "Integer.parseInt")
    v inputFunc
  getFloatInput v = mkSt <$> liftA3 jInput' (return $ 
    text "Double.parseDouble") v inputFunc
  getBoolInput v = mkSt <$> liftA3 jInput (return $ text "nextBoolean()") v 
    inputFunc
  getStringInput v = mkSt <$> liftA3 jInput (return $ text "nextLine()") v 
    inputFunc
  getCharInput _ = return (mkStNoEnd empty)
  discardInput = mkSt <$> fmap jDiscardInput inputFunc
  getIntFileInput f v = mkSt <$> liftA3 jInput' (return $ 
    text "Integer.parseInt") v f
  getFloatFileInput f v = mkSt <$> liftA3 jInput' (return $ 
    text "Double.parseDouble") v f
  getBoolFileInput f v = mkSt <$> liftA3 jInput (return $
    text "nextBoolean()") v f
  getStringFileInput f v = mkSt <$> liftA3 jInput (return $ 
    text "nextLine()") v f
  getCharFileInput _ _ = return (mkStNoEnd empty)
  discardFileInput f = mkSt <$> fmap jDiscardInput f

  openFileR f n = mkSt <$> liftA2 jOpenFileR f n
  openFileW f n = mkSt <$> liftA3 jOpenFileWorA f n litFalse
  openFileA f n = mkSt <$> liftA3 jOpenFileWorA f n litTrue
  closeFile f = valState $ objMethodCall f "close" []

  getFileInputLine f v = v &= f $. func "nextLine" []
  discardFileLine f = valState $ f $. func "nextLine" []
  stringSplit d vnew s = mkSt <$> liftA3 jStringSplit vnew 
    (listType dynamic_ string) 
    (funcApp "Arrays.asList" [s $. func "split" [litString [d]]])

  break = return (mkSt breakDocD)  -- I could have a JumpSym class with functions for "return $ text "break" and then reference those functions here?
  continue = return (mkSt continueDocD)

  returnState v = mkSt <$> fmap returnDocD v
  returnVar l = mkSt <$> fmap returnDocD (var l)

  valState v = mkSt <$> fmap fst v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free _ = error "Cannot free variables in Java" -- could set variable to null? Might be misleading.

  throw errMsg = mkSt <$> fmap jThrowDoc (litString errMsg)

  initState fsmName initialState = varDecDef fsmName string 
    (litString initialState)
  changeState fsmName toState = fsmName &.= litString toState

  initObserverList = listDecDef observerListName
  addObserver t o = valState $ obsList $. listAdd lastelem o
    where obsList = observerListName `listOf` t
          lastelem = obsList $. listSize

  state = fmap statementDocD
  loopState = fmap (statementDocD . setEmpty)
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
  forRange i initv finalv stepv = for (varDecDef i int initv) (var i ?< finalv)
    (i &.+= stepv)
  forEach l t v b = mkStNoEnd <$> liftA7 (forEachDocD l) blockStart blockEnd
    iterForEachLabel iterInLabel t v b
  while v b = mkStNoEnd <$> liftA4 whileDocD blockStart blockEnd v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 jTryCatch tb cb
  
  checkState l = switch (var l)
  notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) 
    (index &.++) notify
    where obsList = observerListName `listOf` t
          index = "observerIndex"
          initv = varDecDef index int $ litInt 0
          notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

  getFileInputAll f v = while (f $. func "hasNextLine" [])
    (oneLiner $ valState $ v $. listAppend (f $. func "nextLine" []))

instance ScopeSym JavaCode where
  type Scope JavaCode = Doc
  private = return privateDocD
  public = return publicDocD

  includeScope s = s

instance MethodTypeSym JavaCode where
  type MethodType JavaCode = Doc
  mState = fmap typeDoc
  void = return voidDocD
  construct n = return $ constructDocD n

instance ParameterSym JavaCode where
  type Parameter JavaCode = Doc
  stateParam n = fmap (stateParamDocD n)
  pointerParam = stateParam

instance MethodSym JavaCode where
  -- Bool is True if the method is a main method, False otherwise
  type Method JavaCode = (Doc, Bool)
  method n _ s p t ps b = liftPairFst (liftA5 (jMethod n) s p t (liftList 
    paramListDocD ps) b, False)
  getMethod n c t = method (getterName n) c public dynamic_ t [] getBody
    where getBody = oneLiner $ returnState (self $-> var n)
  setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic_ 
    void [stateParam paramLbl t] setBody
    where setBody = oneLiner $ (self $-> var setLbl) &=. paramLbl
  mainMethod c b = setMain <$> method "main" c public static_ void 
    [return $ text "String[] args"] b
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor _ _ = error "Destructors not allowed in Java"

  function n = method n ""

instance StateVarSym JavaCode where
  type StateVar JavaCode = Doc
  stateVar _ l s p t = liftA4 (stateVarDocD l) (includeScope s) p t endStatement
  privMVar del l = stateVar del l private dynamic_
  pubMVar del l = stateVar del l public dynamic_
  pubGVar del l = stateVar del l public static_
  listStateVar = stateVar

instance ClassSym JavaCode where
  -- Bool is True if the method is a main method, False otherwise
  type Class JavaCode = (Doc, Bool)
  buildClass n p s vs fs = liftPairFst (liftA4 (classDocD n p) inherit s 
    (liftList stateVarListDocD vs) (liftList methodListDocD fs), 
    any (snd . unJC) fs)
  enum n es s = liftPairFst (liftA2 (enumDocD n) (return $ 
    enumElementsDocD es enumsEqualInts) s, False)
  mainClass n vs fs = setMain <$> buildClass n Nothing public vs fs
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public

instance ModuleSym JavaCode where
  type Module JavaCode = ModData
  buildModule n _ vs ms cs = fmap (md n (any (snd . unJC) ms || 
    any (snd . unJC) cs)) (liftList moduleDocD (if null vs && null ms then cs 
    else pubClass n Nothing (map (liftA4 statementsToStateVars public static_
    endStatement) vs) ms : cs))

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

jListDecDef :: Label -> TypeData -> Doc -> Doc
jListDecDef l st vs = typeDoc st <+> text l <+> equals <+> new <+> 
  typeDoc st <+> parens listElements
  where listElements = if isEmpty vs then empty else text "Arrays.asList" <> 
                         parens vs

jConstDecDef :: Label -> TypeData -> (Doc, Maybe String) -> Doc
jConstDecDef l st (v, _) = text "final" <+> typeDoc st <+> text l <+> equals <+>
  v

jThrowDoc :: (Doc, Maybe String) -> Doc
jThrowDoc (errMsg, _) = text "throw new" <+> text "Exception" <> parens errMsg

jTryCatch :: Doc -> Doc -> Doc
jTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent tb,
  rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> 
    lbrace,
  indent cb,
  rbrace]

jDiscardInput :: (Doc, Maybe String) -> Doc
jDiscardInput (inFn, _) = inFn <> dot <> text "next()"

jInput :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jInput it (v, _) (inFn, _) = v <+> equals <+> parens (inFn <> dot <> it) -- Changed from original GOOL, original GOOL was wrong.

jInput' :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jInput' it (v, _) (inFn, _) = v <+> equals <+> it <> parens (inFn <> dot <> 
  text "nextLine()")

jOpenFileR :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jOpenFileR (f, _) (n, _) = f <+> equals <+> new <+> text "Scanner" <> parens 
  (new <+> text "File" <> parens n)

jOpenFileWorA :: (Doc, Maybe String) -> (Doc, Maybe String) -> 
  (Doc, Maybe String) -> Doc
jOpenFileWorA (f, _) (n, _) (wa, _) = f <+> equals <+> new <+> 
  text "PrintWriter" <> parens (new <+> text "FileWriter" <> parens (new <+> 
  text "File" <> parens n <> comma <+> wa))

jStringSplit :: (Doc, Maybe String) -> TypeData -> (Doc, Maybe String) -> Doc
jStringSplit (vnew, _) t (s, _) = vnew <+> equals <+> new <+> typeDoc t
  <> parens s

jMethod :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
jMethod n s p t ps b = vcat [
  s <+> p <+> t <+> text n <> parens ps <+> text "throws Exception" <+> lbrace,
  indent b,
  rbrace]

jListIndexExists :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jListIndexExists greater (lst, _) (index, _) = parens (lst <> text ".length" <+> greater <+> index)