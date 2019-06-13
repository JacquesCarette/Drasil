{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.NewCSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..)
) where

import Language.Drasil.Code.Imperative.New (Label,
  PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
  UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), StatementSym(..), ControlStatementSym(..), ScopeSym(..),
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
  ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.NewLanguageRenderer (
  fileDoc', moduleDocD, classDocD, enumDocD,
  enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, outDocD,
  printFileDocD, boolTypeDocD, 
  intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, listTypeDocD, voidDocD,
  constructDocD, stateParamDocD, paramListDocD, methodDocD, methodListDocD, 
  stateVarDocD, stateVarListDocD, ifCondDocD, switchDocD, forDocD, 
  forEachDocD, whileDocD, stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD,
  varDecDocD, varDecDefDocD, listDecDocD, listDecDefDocD, objDecDefDocD, 
  constDecDefDocD, statementDocD, returnDocD, mkSt, mkStNoEnd,
  commentDocD, notOpDocD, negateOpDocD, unExpr, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binExpr, binExpr',
  mkVal, litTrueD, litFalseD, litCharD, litFloatD, litIntD, litStringD, 
  defaultCharD, defaultFloatD, defaultIntD, defaultStringD, varDocD, extVarDocD,
  selfDocD, argDocD, enumElemDocD, objVarDocD, inlineIfDocD, funcAppDocD, 
  extFuncAppDocD, stateObjDocD, listStateObjDocD, notNullDocD, 
  listIndexExistsDocD, funcDocD, castDocD, listSetDocD, listAccessDocD,
  objAccessDocD, castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD, 
  privateDocD, publicDocD, dot, new, observerListName, doubleSlash, 
  addCommentsDocD, callFuncParamList, getterName, setterName, setMain, setEmpty,
  statementsToStateVars)
import Language.Drasil.Code.Imperative.Helpers (Terminator(..), ModData(..), md,
  oneTab,  liftA4, liftA5, liftA6, liftA7, liftList, lift1List, lift3Pair, 
  lift4Pair, liftPairFst)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, comma, empty,
  equals, semi, vcat, lbrace, rbrace, colon, render, isEmpty)

newtype CSharpCode a = CSC {unCSC :: a}

instance Functor CSharpCode where
  fmap f (CSC x) = CSC (f x)

instance Applicative CSharpCode where
  pure = CSC
  (CSC f) <*> (CSC x) = CSC (f x)

instance Monad CSharpCode where
  return = CSC
  CSC x >>= f = f x

instance PackageSym CSharpCode where
  type Package CSharpCode = ([ModData], Label)
  packMods n ms = liftPairFst (sequence mods, n)
    where mods = filter (not . isEmpty . modDoc . unCSC) ms

instance RenderSym CSharpCode where
  type RenderFile CSharpCode = ModData
  fileDoc code = liftA3 md (fmap name code) (fmap isMainMod code) 
    (if isEmpty (modDoc (unCSC code)) then return empty else
    liftA3 fileDoc' (top code) (fmap modDoc code) bottom)
  top _ = liftA2 cstop endStatement (include "")
  bottom = return empty

instance KeywordSym CSharpCode where
  type Keyword CSharpCode = Doc
  endStatement = return semi
  endStatementLoop = return empty

  include _ = return $ text "using"
  inherit = return colon

  list _ = return $ text "List"
  listObj = return new

  blockStart = return lbrace
  blockEnd = return rbrace

  ifBodyStart = blockStart
  elseIf = return $ text "else if"
  
  iterForEachLabel = return $ text "foreach"
  iterInLabel = return $ text "in"

  commentStart = return doubleSlash
  
  printFunc = return $ text "Console.Write"
  printLnFunc = return $ text "Console.WriteLine"
  printFileFunc = fmap (printFileDocD "Write")
  printFileLnFunc = fmap (printFileDocD "WriteLine")

instance PermanenceSym CSharpCode where
  type Permanence CSharpCode = Doc
  static_ = return staticDocD
  dynamic_ = return dynamicDocD

instance BodySym CSharpCode where
  type Body CSharpCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner s = bodyStatements [s]

  addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym CSharpCode where
  type Block CSharpCode = Doc
  block sts = lift1List blockDocD endStatement (map (fmap fst . state) sts)

instance StateTypeSym CSharpCode where
  type StateType CSharpCode = Doc
  bool = return boolTypeDocD
  int = return intTypeDocD
  float = return csFloatTypeDoc
  char = return charTypeDocD
  string = return stringTypeDocD
  infile = return csInfileTypeDoc
  outfile = return csOutfileTypeDoc
  listType p st = liftA2 listTypeDocD st (list p)
  intListType p = listType p int
  floatListType p = listType p float
  boolListType = listType dynamic_ bool
  obj t = return $ typeDocD t
  enumType t = return $ typeDocD t
  iterator _ = error "Iterator-type variables do not exist in C#"

instance ControlBlockSym CSharpCode where
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

instance UnaryOpSym CSharpCode where
  type UnaryOp CSharpCode = Doc
  notOp = return notOpDocD
  negateOp = return negateOpDocD
  sqrtOp = return $ text "Math.Sqrt"
  absOp = return $ text "Math.Abs"
  logOp = return $ text "Math.Log10"
  lnOp = return $ text "Math.Log"
  expOp = return $ text "Math.Exp"
  sinOp = return $ text "Math.Sin"
  cosOp = return $ text "Math.Cos"
  tanOp = return $ text "Math.Tan"
  asinOp = return $ text "Math.Asin"
  acosOp = return $ text "Math.Acos"
  atanOp = return $ text "Math.Atan"
  floorOp = return $ text "Math.Floor"
  ceilOp = return $ text "Math.Ceiling"

instance BinaryOpSym CSharpCode where
  type BinaryOp CSharpCode = Doc
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
  powerOp = return $ text "Math.Pow"
  moduloOp = return moduloOpDocD
  andOp = return andOpDocD
  orOp = return orOpDocD

instance ValueSym CSharpCode where
  type Value CSharpCode = (Doc, Maybe String)
  litTrue = return (litTrueD, Just "true")
  litFalse = return (litFalseD, Just "false")
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
  self = return (selfDocD, Just "this")
  arg n = mkVal <$> liftA2 argDocD (litInt n) argsList
  enumElement en e = return (enumElemDocD en e, Just $ en ++ "." ++ e)
  enumVar = var
  objVar o v = liftPairFst (liftA2 objVarDocD o v, 
    Just $ valName o ++ "." ++ valName v)
  objVarSelf n = liftPairFst (liftA2 objVarDocD self (var n), 
    Just $ "self." ++ n)
  listVar n _ = var n
  n `listOf` t = listVar n t
  iterVar = var
  
  inputFunc = return (mkVal $ text "Console.ReadLine()")
  argsList = return (mkVal $ text "args")

  valName (CSC (v, s)) = fromMaybe (error $ 
    "Attempt to print unprintable Value (" ++ render v ++ ")") s

instance NumericExpression CSharpCode where
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

instance BooleanExpression CSharpCode where
  (?!) = liftA2 unExpr notOp
  (?&&) = liftA3 binExpr andOp
  (?||) = liftA3 binExpr orOp

  (?<) = liftA3 binExpr lessOp
  (?<=) = liftA3 binExpr lessEqualOp
  (?>) = liftA3 binExpr greaterOp
  (?>=) = liftA3 binExpr greaterEqualOp
  (?==) = liftA3 binExpr equalOp
  (?!=) = liftA3 binExpr notEqualOp
  
instance ValueExpression CSharpCode where
  inlineIf b v1 v2 = mkVal <$> liftA3 inlineIfDocD b v1 v2
  funcApp n vs = mkVal <$> liftList (funcAppDocD n) vs
  selfFuncApp = funcApp
  extFuncApp l n vs = mkVal <$> liftList (extFuncAppDocD l n) vs
  stateObj t vs = mkVal <$> liftA2 stateObjDocD t (liftList 
    callFuncParamList vs)
  extStateObj _ = stateObj
  listStateObj t vs = mkVal <$> liftA3 listStateObjDocD listObj t (liftList 
    callFuncParamList vs)

  exists = notNull
  notNull v = mkVal <$> liftA3 notNullDocD notEqualOp v (var "null")

instance Selector CSharpCode where
  objAccess v f = mkVal <$> liftA2 objAccessDocD v f
  ($.) = objAccess

  objMethodCall o f ps = objAccess o (func f ps)
  objMethodCallVoid o f = objMethodCall o f []

  selfAccess = objAccess self

  listPopulateAccess _ _ = return (mkVal empty)
  listSizeAccess v = objAccess v listSize

  listIndexExists l i = mkVal <$> liftA3 listIndexExistsDocD greaterOp l i
  
  argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))

  indexOf l v = objAccess l (fmap funcDocD (funcApp "IndexOf" [v]))

  stringEqual v1 v2 = v1 ?== v2

  castObj f v = mkVal <$> liftA2 castObjDocD f v
  castStrToFloat v = funcApp "Double.Parse" [v]

instance FunctionSym CSharpCode where
  type Function CSharpCode = Doc
  func l vs = fmap funcDocD (funcApp l vs)
  cast targT _ = fmap castDocD targT
  castListToInt = cast (listType static_ int) int
  get n = fmap funcDocD (funcApp (getterName n) [])
  set n v = fmap funcDocD (funcApp (setterName n) [v])

  listSize = fmap funcDocD (var "Count")
  listAdd i v = fmap funcDocD (funcApp "Insert" [i, v])
  listPopulateInt _ = return empty
  listPopulateFloat _ = return empty
  listPopulateChar _ = return empty
  listPopulateBool _ = return empty
  listPopulateString _ = return empty
  listAppend v = fmap funcDocD (funcApp "Add" [v])
  listExtendInt = fmap csListExtend defaultInt 
  listExtendFloat = fmap csListExtend defaultFloat 
  listExtendChar = fmap csListExtend defaultChar 
  listExtendBool = fmap csListExtend defaultBool
  listExtendString = fmap csListExtend defaultString
  listExtendList _ = fmap csListExtendList

  iterBegin = fmap funcDocD (funcApp "begin" [])
  iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction CSharpCode where
  listAccess = fmap listAccessDocD
  listSet = liftA2 listSetDocD

  listAccessEnum t v = listAccess (castObj (cast int t) v)
  listSetEnum t i = listSet (castObj (cast int t) i)

  at l = listAccess (var l)

instance StatementSym CSharpCode where
  type Statement CSharpCode = (Doc, Terminator)
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
  listDecDef l t vs = mkSt <$> lift1List (listDecDefDocD l) t vs
  objDecDef l t v = mkSt <$> liftA2 (objDecDefDocD l) t v
  objDecNew l t vs = mkSt <$> liftA2 (objDecDefDocD l) t (stateObj t vs)
  extObjDecNew l _ = objDecNew l
  objDecNewVoid l t = mkSt <$> liftA2 (objDecDefDocD l) t (stateObj t [])
  extObjDecNewVoid l _ = objDecNewVoid l
  constDecDef l t v = mkSt <$> liftA2 (constDecDefDocD l) t v

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

  getIntInput v = mkSt <$> liftA2 (csInput "Int32.Parse") v inputFunc
  getFloatInput v = mkSt <$> liftA2 (csInput "Double.Parse") v inputFunc
  getBoolInput _ = error "Boolean input not yet implemented for C#"
  getStringInput v = mkSt <$> liftA2 (csInput "") v inputFunc
  getCharInput _ = error "Char input not yet implemented for C#"
  discardInput = mkSt <$> fmap csDiscardInput inputFunc

  getIntFileInput f v = mkSt <$> liftA2 (csInput "Int32.Parse") v 
    (fmap csFileInput f)
  getFloatFileInput f v = mkSt <$> liftA2 (csInput "Double.Parse") v 
    (fmap csFileInput f)
  getBoolFileInput _ _ = error "Boolean input not yet implemented for C#"
  getStringFileInput f v = mkSt <$> liftA2 (csInput "") v
    (fmap csFileInput f)
  getCharFileInput _ _ = error "Char input not yet implemented for C#"
  discardFileInput f = valState $ fmap csFileInput f

  openFileR f n = mkSt <$> liftA3 csOpenFileR f n infile
  openFileW f n = mkSt <$> liftA4 csOpenFileWorA f n outfile litFalse
  openFileA f n = mkSt <$> liftA4 csOpenFileWorA f n outfile litTrue
  closeFile f = valState $ objMethodCall f "Close" []

  getFileInputLine = getStringFileInput
  discardFileLine f = valState $ fmap csFileInput f
  stringSplit d vnew s = assign vnew $ listStateObj (listType dynamic_ string) 
    [s $. func "Split" [litChar d]]

  break = return (mkSt breakDocD)
  continue = return (mkSt continueDocD)

  returnState v = mkSt <$> fmap returnDocD v
  returnVar l = mkSt <$> fmap returnDocD (var l)

  valState v = mkSt <$> fmap fst v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free _ = error "Cannot free variables in C#" -- could set variable to null? Might be misleading.

  throw errMsg = mkSt <$> fmap csThrowDoc (litString errMsg)

  initState fsmName initialState = varDecDef fsmName string (litString initialState)
  changeState fsmName toState = fsmName &.= litString toState

  initObserverList = listDecDef observerListName
  addObserver t o = valState $ obsList $. listAdd lastelem o
    where obsList = observerListName `listOf` t
          lastelem = obsList $. listSize

  state = fmap statementDocD
  loopState = fmap (statementDocD . setEmpty)
  multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CSharpCode where
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

  tryCatch tb cb = mkStNoEnd <$> liftA2 csTryCatch tb cb

  checkState l = switch (var l)

  notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) 
    (index &.++) notify
    where obsList = observerListName `listOf` t
          index = "observerIndex"
          initv = varDecDef index int $ litInt 0
          notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

  getFileInputAll f v = while (objVar f (var "EndOfStream") ?!)
    (oneLiner $ valState $ v $. listAppend (fmap csFileInput f))

instance ScopeSym CSharpCode where
  type Scope CSharpCode = Doc
  private = return privateDocD
  public = return publicDocD

  includeScope s = s

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = Doc
  mState t = t
  void = return voidDocD
  construct n = return $ constructDocD n

instance ParameterSym CSharpCode where
  type Parameter CSharpCode = Doc
  stateParam n = fmap (stateParamDocD n)
  pointerParam = stateParam

instance MethodSym CSharpCode where
  -- Bool is True if the method is a main method, False otherwise
  type Method CSharpCode = (Doc, Bool)
  method n _ s p t ps b = liftPairFst (liftA5 (methodDocD n) s p t 
    (liftList paramListDocD ps) b, False)
  getMethod n c t = method (getterName n) c public dynamic_ t [] getBody
    where getBody = oneLiner $ returnState (self $-> var n)
  setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic_ 
    void [stateParam paramLbl t] setBody
    where setBody = oneLiner $ (self $-> var setLbl) &=. paramLbl
  mainMethod c b = setMain <$> method "Main" c public static_ void 
    [return $ text "string[] args"] b
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor _ _ = error "Destructors not allowed in C#"

  function n = method n ""

instance StateVarSym CSharpCode where
  type StateVar CSharpCode = Doc
  stateVar _ l s p t = liftA4 (stateVarDocD l) (includeScope s) p t endStatement
  privMVar del l = stateVar del l private dynamic_
  pubMVar del l = stateVar del l public dynamic_
  pubGVar del l = stateVar del l public static_
  listStateVar = stateVar

instance ClassSym CSharpCode where
  -- Bool is True if the method is a main method, False otherwise
  type Class CSharpCode = (Doc, Bool)
  buildClass n p s vs fs = liftPairFst (liftA4 (classDocD n p) inherit s 
    (liftList stateVarListDocD vs) (liftList methodListDocD fs), 
    any (snd . unCSC) fs)
  enum n es s = liftPairFst (liftA2 (enumDocD n) (return $ 
    enumElementsDocD es False) s, False)
  mainClass n vs fs = setMain <$> buildClass n Nothing public vs fs
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public

instance ModuleSym CSharpCode where
  type Module CSharpCode = ModData
  buildModule n _ vs ms cs = fmap (md n (any (snd . unCSC) ms || 
    any (snd . unCSC) cs)) (liftList moduleDocD (if null vs && null ms then cs 
    else pubClass n Nothing (map (liftA4 statementsToStateVars public static_ 
    endStatement) vs) ms : cs))

cstop :: Doc -> Doc -> Doc
cstop end inc = vcat [
  inc <+> text "System" <> end,
  inc <+> text "System.IO" <> end,
  inc <+> text "System.Collections" <> end,
  inc <+> text "System.Collections.Generic" <> end]

csFloatTypeDoc :: Doc
csFloatTypeDoc = text "double" -- Same as Java, maybe make a common function

csInfileTypeDoc :: Doc
csInfileTypeDoc = text "StreamReader"

csOutfileTypeDoc :: Doc
csOutfileTypeDoc = text "StreamWriter"

csThrowDoc :: (Doc, Maybe String) -> Doc
csThrowDoc (errMsg, _) = text "throw new" <+> text "Exception" <> parens errMsg

csTryCatch :: Doc -> Doc -> Doc
csTryCatch tb cb= vcat [
  text "try" <+> lbrace,
  oneTab tb,
  rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> 
    lbrace,
  oneTab cb,
  rbrace]

csDiscardInput :: (Doc, Maybe String) -> Doc
csDiscardInput (inFn, _) = inFn

csInput :: Label -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
csInput it (v, _) (inFn, _) = v <+> equals <+> text it <> parens inFn

csFileInput :: (Doc, Maybe String) -> (Doc, Maybe String)
csFileInput (f, s) = (f <> dot <> text "ReadLine()", s)

csOpenFileR :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc -> Doc
csOpenFileR (f, _) (n, _) r = f <+> equals <+> new <+> r <> parens n

csOpenFileWorA :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc -> 
  (Doc, Maybe String) -> Doc
csOpenFileWorA (f, _) (n, _) w (a, _) = f <+> equals <+> new <+> w <> parens 
  (n <> comma <+> a)

csListExtend :: (Doc, Maybe String) -> Doc
csListExtend (v, _) = dot <> text "Add" <> parens v

csListExtendList :: Doc -> Doc
csListExtendList t = dot <> text "Add" <> parens (new <+> t <> parens empty)
