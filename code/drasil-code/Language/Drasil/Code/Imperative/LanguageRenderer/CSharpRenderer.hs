{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..)
) where

import Utils.Drasil (indent)

import Language.Drasil.Code.Code (CodeType(..))
import Language.Drasil.Code.Imperative.Symantics (Label,
  PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
  UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), StatementSym(..), ControlStatementSym(..), ScopeSym(..),
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
  ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.LanguageRenderer (
  fileDoc', moduleDocD, classDocD, enumDocD,
  enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, outDocD,
  printFileDocD, boolTypeDocD, 
  intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, listTypeDocD, voidDocD,
  constructDocD, stateParamDocD, paramListDocD, methodDocD, methodListDocD, 
  stateVarDocD, stateVarListDocD, ifCondDocD, switchDocD, forDocD, 
  forEachDocD, whileDocD, stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD,
  varDecDocD, varDecDefDocD, listDecDocD, listDecDefDocD, objDecDefDocD, 
  constDecDefDocD, statementDocD, returnDocD, mkSt, mkStNoEnd,
  commentDocD, notOpDocD, negateOpDocD, unExpr, typeUnExpr, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', typeBinExpr,
  mkVal, litTrueD, litFalseD, litCharD, litFloatD, litIntD, litStringD, 
  varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, objVarDocD, 
  inlineIfDocD, funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, 
  notNullDocD, listIndexExistsDocD, funcDocD, castDocD, listSetDocD, 
  listAccessDocD, objAccessDocD, castObjDocD, breakDocD, continueDocD, 
  staticDocD, dynamicDocD, privateDocD, publicDocD, dot, new, observerListName,
  doubleSlash, addCommentsDocD, valList, appendToBody, getterName, setterName, 
  setMain, setEmpty, statementsToStateVars)
import Language.Drasil.Code.Imperative.Helpers (Terminator(..), FuncData(..),  
  fd, ModData(..), md, TypeData(..), td, ValData(..), vd, mapPairFst, liftA4, 
  liftA5, liftA6, liftA7, liftList, lift1List, lift3Pair, lift4Pair, 
  liftPairFst, getInnerType, convType)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Data.List (nub)
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, comma, empty,
  equals, semi, vcat, lbrace, rbrace, colon, render, isEmpty)

newtype CSharpCode a = CSC {unCSC :: a} deriving Eq

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
  type StateType CSharpCode = TypeData
  bool = return boolTypeDocD
  int = return intTypeDocD
  float = return csFloatTypeDoc
  char = return charTypeDocD
  string = return stringTypeDocD
  infile = return csInfileTypeDoc
  outfile = return csOutfileTypeDoc
  listType p st = liftA2 listTypeDocD st (list p)
  listInnerType t = fmap (getInnerType . cType) t >>= convType
  obj t = return $ typeDocD t
  enumType t = return $ typeDocD t
  iterator _ = error "Iterator-type variables do not exist in C#"
  void = return voidDocD

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
        v_temp = var l_temp (fmap valType vnew)
        l_i = "i_temp"
        v_i = var l_i int
    in
      block [
        listDec l_temp 0 (fmap valType vnew),
        for (varDecDef l_i int (fromMaybe (litInt 0) b)) 
          (v_i ?< fromMaybe (vold $. listSize) e) (maybe (v_i &++) (v_i &+=) s)
          (oneLiner $ valState $ v_temp $. listAppend (vold $. listAccess t v_i)),
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
  type Value CSharpCode = ValData
  litTrue = liftA2 (vd (Just "true")) bool (return litTrueD)
  litFalse = liftA2 (vd (Just "false")) bool (return litFalseD)
  litChar c = liftA2 (vd (Just $ "\'" ++ [c] ++ "\'")) char 
    (return $ litCharD c)
  litFloat v = liftA2 (vd (Just $ show v)) float (return $ litFloatD v)
  litInt v = liftA2 (vd (Just $ show v)) int (return $ litIntD v)
  litString s = liftA2 (vd (Just $ "\"" ++ s ++ "\"")) string 
    (return $ litStringD s)

  ($->) = objVar
  ($:) = enumElement

  const = var
  var n t = liftA2 (vd (Just n)) t (return $ varDocD n) 
  extVar l n t = liftA2 (vd (Just $ l ++ "." ++ n)) t (return $ extVarDocD l n)
  self l = liftA2 (vd (Just "this")) (obj l) (return selfDocD)
  arg n = liftA2 mkVal string (liftA2 argDocD (litInt n) argsList)
  enumElement en e = liftA2 (vd (Just $ en ++ "." ++ e)) (obj en) 
    (return $ enumElemDocD en e)
  enumVar e en = var e (obj en)
  objVar o v = liftA2 (vd (Just $ valueName o ++ "." ++ valueName v))
    (fmap valType v) (liftA2 objVarDocD o v)
  objVarSelf l n t = liftA2 (vd (Just $ "this." ++ n)) t (liftA2 objVarDocD 
    (self l) (var n t))
  listVar n p t = var n (listType p t)
  n `listOf` t = listVar n static_ t
  iterVar n t = var n (iterator t)
  
  inputFunc = liftA2 mkVal string (return $ text "Console.ReadLine()")
  argsList = liftA2 mkVal (listType static_ string) (return $ text "args")

  valueName v = fromMaybe 
    (error $ "Attempt to print unprintable Value (" ++ render (valDoc $ unCSC v)
    ++ ")") (valName $ unCSC v)
  valueType = fmap valType

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
  (?!) = liftA3 typeUnExpr notOp bool
  (?&&) = liftA4 typeBinExpr andOp bool
  (?||) = liftA4 typeBinExpr orOp bool

  (?<) = liftA4 typeBinExpr lessOp bool
  (?<=) = liftA4 typeBinExpr lessEqualOp bool
  (?>) = liftA4 typeBinExpr greaterOp bool
  (?>=) = liftA4 typeBinExpr greaterEqualOp bool
  (?==) = liftA4 typeBinExpr equalOp bool
  (?!=) = liftA4 typeBinExpr notEqualOp bool
  
instance ValueExpression CSharpCode where
  inlineIf b v1 v2 = liftA2 mkVal (fmap valType v1) (liftA3 inlineIfDocD b v1 
    v2)
  funcApp n t vs = liftA2 mkVal t (liftList (funcAppDocD n) vs)
  selfFuncApp = funcApp
  extFuncApp l n t vs = liftA2 mkVal t (liftList (extFuncAppDocD l n) vs)
  stateObj t vs = liftA2 mkVal t (liftA2 stateObjDocD t (liftList valList vs))
  extStateObj _ = stateObj
  listStateObj t vs = liftA2 mkVal t (liftA3 listStateObjDocD listObj t 
    (liftList valList vs))

  exists = notNull
  notNull v = liftA2 mkVal bool (liftA3 notNullDocD notEqualOp v (var "null" 
    (fmap valType v)))

instance Selector CSharpCode where
  objAccess v f = liftA2 mkVal (fmap funcType f) (liftA2 objAccessDocD v f)
  ($.) = objAccess

  objMethodCall t o f ps = objAccess o (func f t ps)
  objMethodCallNoParams t o f = objMethodCall t o f []

  selfAccess l = objAccess (self l)

  listSizeAccess v = objAccess v listSize

  listIndexExists l i = liftA2 mkVal bool (liftA3 listIndexExistsDocD greaterOp
    l i)
  argExists i = objAccess argsList (listAccess string (litInt $ fromIntegral i))

  indexOf l v = objAccess l (func "IndexOf" int [v])

  stringEqual v1 v2 = v1 ?== v2

  castObj f v = liftA2 mkVal (fmap funcType f) (liftA2 castObjDocD f v)
  castStrToFloat v = funcApp "Double.Parse" float [v]

instance FunctionSym CSharpCode where
  type Function CSharpCode = FuncData
  func l t vs = liftA2 fd t (fmap funcDocD (funcApp l t vs))
  cast targT _ = liftA2 fd targT (fmap castDocD targT)
  castListToInt = cast int (listType static_ int)
  get n t = func (getterName n) t []
  set n v = func (setterName n) (fmap valType v) [v]

  listSize = liftA2 fd int (fmap funcDocD (var "Count" int))
  listAdd _ i v = func "Insert" (fmap valType v) [i, v]
  listAppend v = func "Add" (fmap valType v) [v]

  iterBegin _ = error "Attempt to use iterBegin in C#, but C# has no iterators"
  iterEnd _ = error "Attempt to use iterEnd in C#, but C# has no iterators"

instance SelectorFunction CSharpCode where
  listAccess t v = liftA2 fd t (fmap listAccessDocD v)
  listSet i v = liftA2 fd (listType static_ $ fmap valType v) 
    (liftA2 listSetDocD i v)

  listAccessEnum et t v = listAccess t (castObj (cast int et) v)
  listSetEnum t i = listSet (castObj (cast int t) i)

  at t l = listAccess t (var l int)

instance StatementSym CSharpCode where
  type Statement CSharpCode = (Doc, Terminator)
  assign v1 v2 = mkSt <$> liftA2 assignDocD v1 v2
  assignToListIndex lst index v = valState $ lst $. listSet index v
  multiAssign _ _ = error "No multiple assignment statements in C#"
  (&=) = assign
  (&-=) v1 v2 = v1 &= (v1 #- v2)
  (&+=) v1 v2 = mkSt <$> liftA2 plusEqualsDocD v1 v2
  (&++) v = mkSt <$> fmap plusPlusDocD v
  (&~-) v = v &= (v #- litInt 1)

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
    for (varDecDef "i" int (litInt 0)) (var "i" int ?< ((v $. listSize) #-
      litInt 1)) (var "i" int &++)  (bodyStatements [
        print t (v $. listAccess t (var "i" int)), printStr ","]), 
    state (print t (v $. listAccess t ((v $. listSize) #- litInt 1))), 
    printStr "]"]
  printLnList t v = multi [state (printStr "["), 
    for (varDecDef "i" int (litInt 0)) (var "i" int ?<  ((v $. listSize) #- 
      litInt 1)) (var "i" int &++) (bodyStatements [
        print t (v $. listAccess t (var "i" int)), printStr ","]), 
    state (print t (v $. listAccess t ((v $. listSize) #- litInt 1))), 
    printStrLn "]"]
  printFileList f t v = multi [state (printFileStr f "["),
    for (varDecDef "i" int (litInt 0)) (var "i" int ?< ((v $. listSize) #- 
      litInt 1)) (var "i" int &++) (bodyStatements [
        printFile f t (v $. listAccess t (var "i" int)), printFileStr f ","]), 
    state (printFile f t (v $. listAccess t ((v $. listSize) #- litInt 1))), 
    printFileStr f "]"]
  printFileLnList f t v = multi [state (printFileStr f "["), 
    for (varDecDef "i" int (litInt 0)) (var "i" int ?< ((v $. listSize) #- 
      litInt 1)) (var "i" int &++) (bodyStatements [
        printFile f t (v $. listAccess t (var "i" int)), printFileStr f ","]), 
    state (printFile f t (v $. listAccess t ((v $. listSize) #- litInt 1))), 
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
  closeFile f = valState $ objMethodCall void f "Close" []

  getFileInputLine = getStringFileInput
  discardFileLine f = valState $ fmap csFileInput f
  stringSplit d vnew s = assign vnew $ listStateObj (listType dynamic_ string) 
    [s $. func "Split" (listType static_ string) [litChar d]]

  break = return (mkSt breakDocD)
  continue = return (mkSt continueDocD)

  returnState v = mkSt <$> liftList returnDocD [v]
  returnVar l t = mkSt <$> liftList returnDocD [var l t]
  multiReturn _ = error "Cannot return multiple values in C#"

  valState v = mkSt <$> fmap valDoc v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free _ = error "Cannot free variables in C#" -- could set variable to null? Might be misleading.

  throw errMsg = mkSt <$> fmap csThrowDoc (litString errMsg)

  initState fsmName initialState = varDecDef fsmName string (litString initialState)
  changeState fsmName toState = var fsmName string &= litString toState

  initObserverList = listDecDef observerListName
  addObserver t o = valState $ obsList $. listAdd obsList lastelem o
    where obsList = observerListName `listOf` t
          lastelem = obsList $. listSize

  inOutCall n ins [out] = assign out $ funcApp n ins
  inOutCall n ins outs = valState $ funcApp n (nub $ map (\v -> 
    if v `elem` outs then fmap (mapPairFst csRef) v else v) ins ++
    map (fmap (mapPairFst csRef)) outs)

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
  forRange i initv finalv stepv = for (varDecDef i int initv) (var i int ?< 
    finalv) (var i int &+= stepv)
  forEach l t v b = mkStNoEnd <$> liftA7 (forEachDocD l) blockStart blockEnd 
    iterForEachLabel iterInLabel t v b
  while v b = mkStNoEnd <$> liftA4 whileDocD blockStart blockEnd v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 csTryCatch tb cb

  checkState l = switch (var l string)
  notifyObservers ft fn t ps = for initv (var index int ?< 
    (obsList $. listSize)) (var index int &++) notify
    where obsList = observerListName `listOf` t
          index = "observerIndex"
          initv = varDecDef index int $ litInt 0
          notify = oneLiner $ valState $ (obsList $. at int index) $. func fn 
            ft ps

  getFileInputAll f v = while (objVar f (var "EndOfStream" bool) ?!)
    (oneLiner $ valState $ v $. listAppend (fmap csFileInput f))

instance ScopeSym CSharpCode where
  type Scope CSharpCode = Doc
  private = return privateDocD
  public = return publicDocD

  includeScope s = s

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypeData
  mState t = t
  construct n = return $ td (Object n) (constructDocD n)

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
    where getBody = oneLiner $ returnState (self c $-> var n t)
  setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic_ 
    void [stateParam paramLbl t] setBody
    where setBody = oneLiner $ (self c $-> var setLbl t) &= var paramLbl t
  mainMethod c b = setMain <$> method "Main" c public static_ void 
    [return $ text "string[] args"] b
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor _ _ = error "Destructors not allowed in C#"

  function n = method n ""

  inOutFunc n s p ins [(l,t)] b = function n s p (mState t) (map (uncurry 
    stateParam) ins) (liftA2 appendToBody b $ returnVar l t)
  inOutFunc n s p ins outs b = function n s p (mState void) (map (fmap csRef . 
    uncurry stateParam) outs ++ map (uncurry stateParam) (filter (\(l,_) -> l 
    `notElem` map fst outs) ins)) b

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

csFloatTypeDoc :: TypeData
csFloatTypeDoc = td Float (text "double") -- Same as Java, maybe make a common function

csInfileTypeDoc :: TypeData
csInfileTypeDoc = td File (text "StreamReader")

csOutfileTypeDoc :: TypeData
csOutfileTypeDoc = td File (text "StreamWriter")

csThrowDoc :: ValData -> Doc
csThrowDoc errMsg = text "throw new" <+> text "Exception" <> 
  parens (valDoc errMsg)

csTryCatch :: Doc -> Doc -> Doc
csTryCatch tb cb= vcat [
  text "try" <+> lbrace,
  indent tb,
  rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> 
    lbrace,
  indent cb,
  rbrace]

csDiscardInput :: ValData -> Doc
csDiscardInput = valDoc

csInput :: Label -> ValData -> ValData -> Doc
csInput it v inFn = valDoc v <+> equals <+> text it <> parens (valDoc inFn)

csFileInput :: ValData -> ValData
csFileInput f = vd (valName f) (valType f) (valDoc f <> dot <> text "ReadLine()")

csOpenFileR :: ValData -> ValData -> TypeData -> Doc
csOpenFileR f n r = valDoc f <+> equals <+> new <+> typeDoc r <> 
  parens (valDoc n)

csOpenFileWorA :: ValData -> ValData -> TypeData 
  -> ValData -> Doc
csOpenFileWorA f n w a = valDoc f <+> equals <+> new <+> typeDoc w <> 
  parens (valDoc n <> comma <+> valDoc a)

csRef :: Doc -> Doc
csRef p = text "ref" <+> p
