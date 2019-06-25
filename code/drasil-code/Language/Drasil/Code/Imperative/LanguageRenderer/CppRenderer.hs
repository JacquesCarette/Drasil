{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer (
  -- * C++ Code Configuration -- defines syntax of all C++ code
  CppSrcCode(..), CppHdrCode(..), CppCode(..), unSrc, unHdr
) where

import Utils.Drasil (indent, indentList)

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
  fileDoc', enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, 
  intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, listTypeDocD, voidDocD,
  constructDocD, stateParamDocD, paramListDocD, methodListDocD, stateVarDocD, 
  stateVarListDocD, alwaysDel, ifCondDocD, switchDocD, forDocD, whileDocD, 
  stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD, 
  varDecDefDocD, objDecDefDocD, constDecDefDocD, statementDocD,
  returnDocD, commentDocD, freeDocD, mkSt, mkStNoEnd, notOpDocD, negateOpDocD, 
  sqrtOpDocD, absOpDocD, expOpDocD, sinOpDocD, cosOpDocD, tanOpDocD, asinOpDocD,
  acosOpDocD, atanOpDocD, unExpr, typeUnExpr, equalOpDocD, notEqualOpDocD, 
  greaterOpDocD, greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, 
  minusOpDocD, multOpDocD, divideOpDocD, moduloOpDocD, powerOpDocD, andOpDocD,
  orOpDocD, binExpr, binExpr', typeBinExpr, mkVal, litTrueD, litFalseD, 
  litCharD, litFloatD, litIntD, litStringD, varDocD, selfDocD, argDocD, 
  objVarDocD, inlineIfDocD, funcAppDocD, funcDocD, castDocD, objAccessDocD,
  castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD,
  publicDocD, classDec, dot, observerListName, doubleSlash, addCommentsDocD, 
  valList, appendToBody, getterName, setterName, setEmpty)
import Language.Drasil.Code.Imperative.Helpers (Pair(..), Terminator(..),  
  ScopeTag (..), FuncData(..), fd, ModData(..), md, MethodData(..), mthd, 
  StateVarData(..), svd, TypeData(..), td, ValData(..), vd, angles, blank, 
  doubleQuotedText, mapPairFst, mapPairSnd, vibcat, liftA4, liftA5, liftA6, 
  liftA8, liftList, lift2Lists, lift1List, lift3Pair, lift4Pair, liftPairFst,
  liftPairSnd, getInnerType, convType)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import Data.List (nub)
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), braces, parens, comma,
  empty, equals, semi, vcat, lbrace, rbrace, quotes, render, colon, isEmpty)

data CppCode x y a = CPPC {src :: x a, hdr :: y a}

instance Pair CppCode where
  pfst (CPPC xa _) = xa
  psnd (CPPC _ yb) = yb
  pair = CPPC

unSrc :: CppCode CppSrcCode CppHdrCode a -> a
unSrc (CPPC (CPPSC a) _) = a

unHdr :: CppCode CppSrcCode CppHdrCode a -> a
unHdr (CPPC _ (CPPHC a)) = a

instance (Pair p) => PackageSym (p CppSrcCode CppHdrCode) where
  type Package (p CppSrcCode CppHdrCode) = ([ModData], Label)
  packMods n ms = pair (packMods n (map pfst ms)) (packMods n (map psnd ms))

instance (Pair p) => RenderSym (p CppSrcCode CppHdrCode) where
  type RenderFile (p CppSrcCode CppHdrCode) = ModData
  fileDoc code = pair (fileDoc $ pfst code) (fileDoc $ psnd code)
  top m = pair (top $ pfst m) (top $ psnd m)
  bottom = pair bottom bottom

instance (Pair p) => KeywordSym (p CppSrcCode CppHdrCode) where
  type Keyword (p CppSrcCode CppHdrCode) = Doc
  endStatement = pair endStatement endStatement
  endStatementLoop = pair endStatementLoop endStatementLoop

  include n = pair (include n) (include n)
  inherit = pair inherit inherit

  list p = pair (list $ pfst p) (list $ psnd p)
  listObj = pair listObj listObj

  blockStart = pair blockStart blockStart
  blockEnd = pair blockEnd blockEnd

  ifBodyStart = pair ifBodyStart ifBodyStart
  elseIf = pair elseIf elseIf
  
  iterForEachLabel = pair iterForEachLabel iterForEachLabel
  iterInLabel = pair iterInLabel iterInLabel

  commentStart = pair commentStart commentStart
  
  printFunc = pair printFunc printFunc
  printLnFunc = pair printLnFunc printLnFunc
  printFileFunc v = pair (printFileFunc $ pfst v) (printFileFunc $ psnd v)
  printFileLnFunc v = pair (printFileLnFunc $ pfst v) (printFileLnFunc $ psnd v)

instance (Pair p) => PermanenceSym (p CppSrcCode CppHdrCode) where
  type Permanence (p CppSrcCode CppHdrCode) = Doc
  static_ = pair static_ static_
  dynamic_ = pair dynamic_ dynamic_

instance (Pair p) => BodySym (p CppSrcCode CppHdrCode) where
  type Body (p CppSrcCode CppHdrCode) = Doc
  body bs = pair (body $ map pfst bs) (body $ map psnd bs)
  bodyStatements sts = pair (bodyStatements $ map pfst sts) (bodyStatements $ 
    map psnd sts)
  oneLiner s = pair (oneLiner $ pfst s) (oneLiner $ psnd s)

  addComments s b = pair (addComments s $ pfst b) (addComments s $ psnd b)

instance (Pair p) => BlockSym (p CppSrcCode CppHdrCode) where
  type Block (p CppSrcCode CppHdrCode) = Doc
  block sts = pair (block $ map pfst sts) (block $ map psnd sts)

instance (Pair p) => StateTypeSym (p CppSrcCode CppHdrCode) where
  type StateType (p CppSrcCode CppHdrCode) = TypeData
  bool = pair bool bool
  int = pair int int
  float = pair float float
  char = pair char char
  string = pair string string
  infile = pair infile infile
  outfile = pair outfile outfile
  listType p st = pair (listType (pfst p) (pfst st)) (listType (psnd p) 
    (psnd st))
  listInnerType st = pair (listInnerType $ pfst st) (listInnerType $ psnd st)
  obj t = pair (obj t) (obj t)
  enumType t = pair (enumType t) (enumType t)
  iterator t = pair (iterator $ pfst t) (iterator $ psnd t)
  void = pair void void

instance (Pair p) => ControlBlockSym (p CppSrcCode CppHdrCode) where
  runStrategy l strats rv av = pair (runStrategy l (map (mapPairSnd pfst) 
    strats) (fmap pfst rv) (fmap pfst av)) (runStrategy l (map 
    (mapPairSnd psnd) strats) (fmap psnd rv) (fmap psnd av))

  listSlice t vnew vold b e s = pair (listSlice (pfst t) (pfst vnew) (pfst vold)
    (fmap pfst b) (fmap pfst e) (fmap pfst s)) (listSlice (psnd t) (psnd vnew) 
    (psnd vold) (fmap psnd b) (fmap psnd e) (fmap psnd s))

instance (Pair p) => UnaryOpSym (p CppSrcCode CppHdrCode) where
  type UnaryOp (p CppSrcCode CppHdrCode) = Doc
  notOp = pair notOp notOp
  negateOp = pair negateOp negateOp
  sqrtOp = pair sqrtOp sqrtOp
  absOp = pair absOp absOp
  logOp = pair logOp logOp
  lnOp = pair lnOp lnOp
  expOp = pair expOp expOp
  sinOp = pair sinOp sinOp
  cosOp = pair cosOp cosOp
  tanOp = pair tanOp tanOp
  asinOp = pair asinOp asinOp
  acosOp = pair acosOp acosOp
  atanOp = pair atanOp atanOp
  floorOp = pair floorOp floorOp
  ceilOp = pair ceilOp ceilOp

instance (Pair p) => BinaryOpSym (p CppSrcCode CppHdrCode) where
  type BinaryOp (p CppSrcCode CppHdrCode) = Doc
  equalOp = pair equalOp equalOp
  notEqualOp = pair notEqualOp notEqualOp
  greaterOp = pair greaterOp greaterOp
  greaterEqualOp = pair greaterEqualOp greaterEqualOp
  lessOp = pair lessOp lessOp
  lessEqualOp = pair lessEqualOp lessEqualOp
  plusOp = pair plusOp plusOp
  minusOp = pair minusOp minusOp
  multOp = pair multOp multOp
  divideOp = pair divideOp divideOp
  powerOp = pair powerOp powerOp
  moduloOp = pair moduloOp moduloOp
  andOp = pair andOp andOp
  orOp = pair orOp orOp

instance (Pair p) => ValueSym (p CppSrcCode CppHdrCode) where
  type Value (p CppSrcCode CppHdrCode) = ValData
  litTrue = pair litTrue litTrue
  litFalse = pair litFalse litFalse
  litChar c = pair (litChar c) (litChar c)
  litFloat v = pair (litFloat v) (litFloat v)
  litInt v = pair (litInt v) (litInt v)
  litString s = pair (litString s) (litString s)

  ($->) v1 v2 = pair (($->) (pfst v1) (pfst v2)) (($->) (psnd v1) (psnd v2))
  ($:) l1 l2 = pair (($:) l1 l2) (($:) l1 l2)

  const n t = pair (const n $ pfst t) (const n $ psnd t)
  var n t = pair (var n $ pfst t) (var n $ psnd t)
  extVar l n t = pair (extVar l n $ pfst t) (extVar l n $ psnd t)
  self l = pair (self l) (self l)
  arg n = pair (arg n) (arg n)
  enumElement en e = pair (enumElement en e) (enumElement en e)
  enumVar e en = pair (enumVar e en) (enumVar e en)
  objVar o v = pair (objVar (pfst o) (pfst v)) (objVar (psnd o) (psnd v))
  objVarSelf l n t = pair (objVarSelf l n $ pfst t) (objVarSelf l n $ psnd t)
  listVar n p t = pair (listVar n (pfst p) (pfst t)) (listVar n (psnd p) (psnd t))
  n `listOf` t = pair (n `listOf` pfst t) (n `listOf` psnd t)
  iterVar l t = pair (iterVar l $ pfst t) (iterVar l $ psnd t)
  
  inputFunc = pair inputFunc inputFunc
  argsList = pair argsList argsList

  valueName v = valueName $ pfst v
  valueType v = pair (valueType $ pfst v) (valueType $ psnd v)

instance (Pair p) => NumericExpression (p CppSrcCode CppHdrCode) where
  (#~) v = pair ((#~) $ pfst v) ((#~) $ psnd v)
  (#/^) v = pair ((#/^) $ pfst v) ((#/^) $ psnd v)
  (#|) v = pair ((#|) $ pfst v) ((#|) $ psnd v)
  (#+) v1 v2 = pair ((#+) (pfst v1) (pfst v2)) ((#+) (psnd v1) (psnd v2))
  (#-) v1 v2 = pair ((#-) (pfst v1) (pfst v2)) ((#-) (psnd v1) (psnd v2))
  (#*) v1 v2 = pair ((#*) (pfst v1) (pfst v2)) ((#*) (psnd v1) (psnd v2))
  (#/) v1 v2 = pair ((#/) (pfst v1) (pfst v2)) ((#/) (psnd v1) (psnd v2))
  (#%) v1 v2 = pair ((#%) (pfst v1) (pfst v2)) ((#%) (psnd v1) (psnd v2))
  (#^) v1 v2 = pair ((#^) (pfst v1) (pfst v2)) ((#^) (psnd v1) (psnd v2))

  log v = pair (log $ pfst v) (log $ psnd v)
  ln v = pair (ln $ pfst v) (ln $ psnd v)
  exp v = pair (exp $ pfst v) (exp $ psnd v)
  sin v = pair (sin $ pfst v) (sin $ psnd v)
  cos v = pair (cos $ pfst v) (cos $ psnd v)
  tan v = pair (tan $ pfst v) (tan $ psnd v)
  csc v = pair (csc $ pfst v) (csc $ psnd v)
  sec v = pair (sec $ pfst v) (sec $ psnd v)
  cot v = pair (cot $ pfst v) (cot $ psnd v)
  arcsin v = pair (arcsin $ pfst v) (arcsin $ psnd v)
  arccos v = pair (arccos $ pfst v) (arccos $ psnd v)
  arctan v = pair (arctan $ pfst v) (arctan $ psnd v)
  floor v = pair (floor $ pfst v) (floor $ psnd v)
  ceil v = pair (ceil $ pfst v) (ceil $ psnd v)

instance (Pair p) => BooleanExpression (p CppSrcCode CppHdrCode) where
  (?!) v = pair ((?!) $ pfst v) ((?!) $ psnd v)
  (?&&) v1 v2 = pair ((?&&) (pfst v1) (pfst v2)) ((?&&) (psnd v1) (psnd v2))
  (?||) v1 v2 = pair ((?||) (pfst v1) (pfst v2)) ((?||) (psnd v1) (psnd v2))

  (?<) v1 v2 = pair ((?<) (pfst v1) (pfst v2)) ((?<) (psnd v1) (psnd v2))
  (?<=) v1 v2 = pair ((?<=) (pfst v1) (pfst v2)) ((?<=) (psnd v1) (psnd v2))
  (?>) v1 v2 = pair ((?>) (pfst v1) (pfst v2)) ((?>) (psnd v1) (psnd v2))
  (?>=) v1 v2 = pair ((?>=) (pfst v1) (pfst v2)) ((?>=) (psnd v1) (psnd v2))
  (?==) v1 v2 = pair ((?==) (pfst v1) (pfst v2)) ((?==) (psnd v1) (psnd v2))
  (?!=) v1 v2 = pair ((?!=) (pfst v1) (pfst v2)) ((?!=) (psnd v1) (psnd v2))
  
instance (Pair p) => ValueExpression (p CppSrcCode CppHdrCode) where
  inlineIf b v1 v2 = pair (inlineIf (pfst b) (pfst v1) (pfst v2)) (inlineIf 
    (psnd b) (psnd v1) (psnd v2))
  funcApp n t vs = pair (funcApp n (pfst t) (map pfst vs)) (funcApp n (psnd t) 
    (map psnd vs))
  selfFuncApp n t vs = pair (selfFuncApp n (pfst t) (map pfst vs)) 
    (selfFuncApp n (psnd t) (map psnd vs))
  extFuncApp l n t vs = pair (extFuncApp l n (pfst t) (map pfst vs)) 
    (extFuncApp l n (psnd t) (map psnd vs))
  stateObj t vs = pair (stateObj (pfst t) (map pfst vs)) (stateObj (psnd t) 
    (map psnd vs))
  extStateObj l t vs = pair (extStateObj l (pfst t) (map pfst vs)) 
    (extStateObj l (psnd t) (map psnd vs))
  listStateObj t vs = pair (listStateObj (pfst t) (map pfst vs)) 
    (listStateObj (psnd t) (map psnd vs))

  exists v = pair (exists $ pfst v) (exists $ psnd v)
  notNull v = pair (notNull $ pfst v) (notNull $ psnd v)

instance (Pair p) => Selector (p CppSrcCode CppHdrCode) where
  objAccess v f = pair (objAccess (pfst v) (pfst f)) (objAccess (psnd v) 
    (psnd f))
  ($.) v f = pair (($.) (pfst v) (pfst f)) (($.) (psnd v) (psnd f))

  objMethodCall t o f ps = pair (objMethodCall (pfst t) (pfst o) f 
    (map pfst ps)) (objMethodCall (psnd t) (psnd o) f (map psnd ps))
  objMethodCallNoParams t o f = pair (objMethodCallNoParams (pfst t) (pfst o) f)
    (objMethodCallNoParams (psnd t) (psnd o) f)

  selfAccess l f = pair (selfAccess l $ pfst f) (selfAccess l $ psnd f)

  listSizeAccess v = pair (listSizeAccess $ pfst v) (listSizeAccess $ psnd v)

  listIndexExists v i = pair (listIndexExists (pfst v) (pfst i)) 
    (listIndexExists (psnd v) (psnd i))
  argExists i = pair (argExists i) (argExists i)
  
  indexOf l v = pair (indexOf (pfst l) (pfst v)) (indexOf (psnd l) (psnd v))

  stringEqual v1 v2 = pair (stringEqual (pfst v1) (pfst v2)) (stringEqual 
    (psnd v1) (psnd v2))

  castObj f v = pair (castObj (pfst f) (pfst v)) (castObj (psnd f) (psnd v))
  castStrToFloat v = pair (castStrToFloat $ pfst v) (castStrToFloat $ psnd v)

instance (Pair p) => FunctionSym (p CppSrcCode CppHdrCode) where
  type Function (p CppSrcCode CppHdrCode) = FuncData
  func l t vs = pair (func l (pfst t) (map pfst vs)) (func l (psnd t) (map psnd vs))
  cast targT srcT = pair (cast (pfst targT) (pfst srcT)) 
    (cast (psnd targT) (psnd srcT))
  castListToInt = pair castListToInt castListToInt
  get n t = pair (get n $ pfst t) (get n $ psnd t)
  set n v = pair (set n $ pfst v) (set n $ psnd v)

  listSize = pair listSize listSize
  listAdd l i v = pair (listAdd (pfst l) (pfst i) (pfst v)) (listAdd (psnd l)
    (psnd i) (psnd v))
  listAppend v = pair (listAppend $ pfst v) (listAppend $ psnd v)

  iterBegin t = pair (iterBegin $ pfst t) (iterBegin $ psnd t)
  iterEnd t = pair (iterEnd $ pfst t) (iterEnd $ psnd t)

instance (Pair p) => SelectorFunction (p CppSrcCode CppHdrCode) where
  listAccess t v = pair (listAccess (pfst t) (pfst v)) (listAccess (psnd t) 
    (psnd v))
  listSet i v = pair (listSet (pfst i) (pfst v)) (listSet (psnd i) (psnd v))

  listAccessEnum et t v = pair (listAccessEnum (pfst et) (pfst t) (pfst v)) 
    (listAccessEnum (psnd et) (psnd t) (psnd v))
  listSetEnum t i v = pair (listSetEnum (pfst t) (pfst i) (pfst v)) 
    (listSetEnum (psnd t) (psnd i) (psnd v))

  at t l = pair (at (pfst t) l) (at (psnd t) l)

instance (Pair p) => StatementSym (p CppSrcCode CppHdrCode) where
  type Statement (p CppSrcCode CppHdrCode) = (Doc, Terminator)
  assign v1 v2 = pair (assign (pfst v1) (pfst v2)) (assign (psnd v1) (psnd v2))
  assignToListIndex lst index v = pair (assignToListIndex (pfst lst) (pfst 
    index) (pfst v)) (assignToListIndex (psnd lst) (psnd index) (psnd v))
  multiAssign vs1 vs2 = pair (multiAssign (map pfst vs1) (map pfst vs2)) 
    (multiAssign (map psnd vs1) (map psnd vs2))
  (&=) v1 v2 = pair ((&=) (pfst v1) (pfst v2)) ((&=) (psnd v1) (psnd v2))
  (&-=) v1 v2 = pair ((&-=) (pfst v1) (pfst v2)) ((&-=) (psnd v1) (psnd v2))
  (&+=) v1 v2 = pair ((&+=) (pfst v1) (pfst v2)) ((&+=) (psnd v1) (psnd v2))
  (&++) v = pair ((&++) $ pfst v) ((&++) $ psnd v)
  (&~-) v = pair ((&~-) $ pfst v) ((&~-) $ psnd v)

  varDec l t = pair (varDec l $ pfst t) (varDec l $ psnd t)
  varDecDef l t v = pair (varDecDef l (pfst t) (pfst v)) (varDecDef l (psnd t) 
    (psnd v))
  listDec l n t = pair (listDec l n $ pfst t) (listDec l n $ psnd t)
  listDecDef l t vs = pair (listDecDef l (pfst t) (map pfst vs)) (listDecDef l 
    (psnd t) (map psnd vs))
  objDecDef l t v = pair (objDecDef l (pfst t) (pfst v)) (objDecDef l (psnd t)
    (psnd v))
  objDecNew l t vs = pair (objDecNew l (pfst t) (map pfst vs)) (objDecNew l 
    (psnd t) (map psnd vs))
  extObjDecNew l lib t vs = pair (extObjDecNew l lib (pfst t) (map pfst vs)) 
    (extObjDecNew l lib (psnd t) (map psnd vs))
  objDecNewVoid l t = pair (objDecNewVoid l $ pfst t) (objDecNewVoid l $ psnd t)
  extObjDecNewVoid l lib t = pair (extObjDecNewVoid l lib $ pfst t) 
    (extObjDecNewVoid l lib $ psnd t)
  constDecDef l t v = pair (constDecDef l (pfst t) (pfst v)) (constDecDef l 
    (psnd t) (psnd v))

  print t v = pair (print (pfst t) (pfst v)) (print (psnd t) (psnd v))
  printLn t v = pair (printLn (pfst t) (pfst v)) (printLn (psnd t) (psnd v))
  printStr s = pair (printStr s) (printStr s)
  printStrLn s = pair (printStrLn s) (printStrLn s)

  printFile f t v = pair (printFile (pfst f) (pfst t) (pfst v)) (printFile 
    (psnd f) (psnd t) (psnd v))
  printFileLn f t v = pair (printFileLn (pfst f) (pfst t) (pfst v)) 
    (printFileLn (psnd f) (psnd t) (psnd v))
  printFileStr f s = pair (printFileStr (pfst f) s) (printFileStr (psnd f) s)
  printFileStrLn f s = pair (printFileStrLn (pfst f) s) (printFileStrLn (psnd f)
    s)

  printList t v = pair (printList (pfst t) (pfst v)) (printList (psnd t) 
    (psnd v))
  printLnList t v = pair (printLnList (pfst t) (pfst v)) (printLnList (psnd t) 
    (psnd v))
  printFileList f t v = pair (printFileList (pfst f) (pfst t) (pfst v)) 
    (printFileList (psnd f) (psnd t) (psnd v))
  printFileLnList f t v = pair (printFileLnList (pfst f) (pfst t) (pfst v)) 
    (printFileLnList (psnd f) (psnd t) (psnd v))

  getIntInput v = pair (getIntInput $ pfst v) (getIntInput $ psnd v)
  getFloatInput v = pair (getFloatInput $ pfst v) (getFloatInput $ psnd v)
  getBoolInput v = pair (getBoolInput $ pfst v) (getBoolInput $ psnd v)
  getStringInput v = pair (getStringInput $ pfst v) (getStringInput $ psnd v)
  getCharInput v = pair (getCharInput $ pfst v) (getCharInput $ psnd v)
  discardInput = pair discardInput discardInput

  getIntFileInput f v = pair (getIntFileInput (pfst f) (pfst v)) 
    (getIntFileInput (psnd f) (psnd v))
  getFloatFileInput f v = pair (getFloatFileInput (pfst f) (pfst v)) 
    (getFloatFileInput (psnd f) (psnd v))
  getBoolFileInput f v = pair (getBoolFileInput (pfst f) (pfst v)) 
    (getBoolFileInput (psnd f) (psnd v))
  getStringFileInput f v = pair (getStringFileInput (pfst f) (pfst v)) 
    (getStringFileInput (psnd f) (psnd v))
  getCharFileInput f v = pair (getCharFileInput (pfst f) (pfst v)) 
    (getCharFileInput (psnd f) (psnd v))
  discardFileInput f = pair (discardFileInput $ pfst f) (discardFileInput $
    psnd f)

  openFileR f n = pair (openFileR (pfst f) (pfst n)) 
    (openFileR (psnd f) (psnd n))
  openFileW f n = pair (openFileW (pfst f) (pfst n)) 
    (openFileW (psnd f) (psnd n))
  openFileA f n = pair (openFileA (pfst f) (pfst n)) 
    (openFileA (psnd f) (psnd n))
  closeFile f = pair (closeFile $ pfst f) (closeFile $ psnd f)

  getFileInputLine f v = pair (getFileInputLine (pfst f) (pfst v)) 
    (getFileInputLine (psnd f) (psnd v))
  discardFileLine f = pair (discardFileLine $ pfst f) (discardFileLine $ psnd f)
  stringSplit d vnew s = pair (stringSplit d (pfst vnew) (pfst s)) 
    (stringSplit d (psnd vnew) (psnd s))

  break = pair break break
  continue = pair continue continue

  returnState v = pair (returnState $ pfst v) (returnState $ psnd v)
  returnVar l t = pair (returnVar l $ pfst t) (returnVar l $ psnd t)
  multiReturn vs = pair (multiReturn $ map pfst vs) (multiReturn $ map psnd vs)

  valState v = pair (valState $ pfst v) (valState $ psnd v)

  comment cmt = pair (comment cmt) (comment cmt)

  free v = pair (free $ pfst v) (free $ psnd v)

  throw errMsg = pair (throw errMsg) (throw errMsg)

  initState fsmName initialState = pair (initState fsmName initialState) 
    (initState fsmName initialState)
  changeState fsmName toState = pair (changeState fsmName toState) 
    (changeState fsmName toState)

  initObserverList t vs = pair (initObserverList (pfst t) (map pfst vs)) 
    (initObserverList (psnd t) (map psnd vs))
  addObserver t o = pair (addObserver (pfst t) (pfst o)) (addObserver (psnd t) 
    (psnd o))

  inOutCall n ins outs = pair (inOutCall n (map pfst ins) (map pfst outs)) 
    (inOutCall n (map psnd ins) (map psnd outs))

  state s = pair (state $ pfst s) (state $ psnd s)
  loopState s = pair (loopState $ pfst s) (loopState $ psnd s)
  multi ss = pair (multi $ map pfst ss) (multi $ map psnd ss)

instance (Pair p) => ControlStatementSym (p CppSrcCode CppHdrCode) where
  ifCond bs b = pair (ifCond (map (mapPairFst pfst . mapPairSnd pfst) bs) 
    (pfst b)) (ifCond (map (mapPairFst psnd . mapPairSnd psnd) bs) (psnd b))
  ifNoElse bs = pair (ifNoElse $ map (mapPairFst pfst . mapPairSnd pfst) bs) 
    (ifNoElse $ map (mapPairFst psnd . mapPairSnd psnd) bs)
  switch v cs c = pair (switch (pfst v) (map (mapPairFst pfst . mapPairSnd pfst)
    cs) (pfst c)) (switch (psnd v) (map (mapPairFst psnd . mapPairSnd psnd) cs)
    (psnd c))
  switchAsIf v cs b = pair (switchAsIf (pfst v) (map 
    (mapPairFst pfst . mapPairSnd pfst) cs) (pfst b)) 
    (switchAsIf (psnd v) (map (mapPairFst psnd . mapPairSnd psnd) cs) (psnd b))

  ifExists cond ifBody elseBody = pair (ifExists (pfst cond) (pfst ifBody)
    (pfst elseBody)) (ifExists (psnd cond) (psnd ifBody) (psnd elseBody))

  for sInit vGuard sUpdate b = pair (for (pfst sInit) (pfst vGuard) (pfst 
    sUpdate) (pfst b)) (for (psnd sInit) (psnd vGuard) (psnd sUpdate) (psnd b))
  forRange i initv finalv stepv b = pair (forRange i (pfst initv) (pfst finalv) 
    (pfst stepv) (pfst b)) (forRange i (psnd initv) (psnd finalv) (psnd stepv) 
    (psnd b))
  forEach l t v b = pair (forEach l (pfst t) (pfst v) (pfst b)) (forEach l 
    (psnd t) (psnd v) (psnd b))
  while v b = pair (while (pfst v) (pfst b)) (while (psnd v) (psnd b))

  tryCatch tb cb = pair (tryCatch (pfst tb) (pfst cb)) (tryCatch (psnd tb) 
    (psnd cb))

  checkState l vs b = pair (checkState l (map 
    (mapPairFst pfst . mapPairSnd pfst) vs) (pfst b)) 
    (checkState l (map (mapPairFst psnd . mapPairSnd psnd) vs) (psnd b))

  notifyObservers ft fn t ps = pair (notifyObservers (pfst ft) fn (pfst t) 
    (map pfst ps)) (notifyObservers (psnd ft) fn (psnd t) (map psnd ps))

  getFileInputAll f v = pair (getFileInputAll (pfst f) (pfst v)) 
    (getFileInputAll (psnd f) (psnd v))

instance (Pair p) => ScopeSym (p CppSrcCode CppHdrCode) where
  type Scope (p CppSrcCode CppHdrCode) = (Doc, ScopeTag)
  private = pair private private
  public = pair public public

  includeScope s = pair (includeScope $ pfst s) (includeScope $ psnd s)

instance (Pair p) => MethodTypeSym (p CppSrcCode CppHdrCode) where
  type MethodType (p CppSrcCode CppHdrCode) = TypeData
  mState t = pair (mState $ pfst t) (mState $ psnd t)
  construct n = pair (construct n) (construct n)

instance (Pair p) => ParameterSym (p CppSrcCode CppHdrCode) where
  type Parameter (p CppSrcCode CppHdrCode) = Doc
  stateParam n t = pair (stateParam n $ pfst t) (stateParam n $ psnd t)
  pointerParam n t = pair (pointerParam n $ pfst t) (pointerParam n $ psnd t)

instance (Pair p) => MethodSym (p CppSrcCode CppHdrCode) where
  type Method (p CppSrcCode CppHdrCode) = MethodData
  method n c s p t ps b = pair (method n c (pfst s) (pfst p) (pfst t) (map pfst
    ps) (pfst b)) (method n c (psnd s) (psnd p) (psnd t) (map psnd ps) (psnd b))
  getMethod n c t = pair (getMethod n c $ pfst t) (getMethod n c $ psnd t) 
  setMethod setLbl c paramLbl t = pair (setMethod setLbl c paramLbl $ pfst t) 
    (setMethod setLbl c paramLbl $ psnd t)
  mainMethod l b = pair (mainMethod l $ pfst b) (mainMethod l $ psnd b)
  privMethod n c t ps b = pair (privMethod n c (pfst t) (map pfst ps) (pfst b))
    (privMethod n c (psnd t) (map psnd ps) (psnd b))
  pubMethod n c t ps b = pair (pubMethod n c (pfst t) (map pfst ps) (pfst b)) 
    (pubMethod n c (psnd t) (map psnd ps) (psnd b))
  constructor n ps b = pair (constructor n (map pfst ps) (pfst b))
    (constructor n (map psnd ps) (psnd b))
  destructor n vs = pair (destructor n $ map pfst vs) 
    (destructor n $ map psnd vs)

  function n s p t ps b = pair (function n (pfst s) (pfst p) (pfst t) (map pfst
    ps) (pfst b)) (function n (psnd s) (psnd p) (psnd t) (map psnd ps) (psnd b))
  inOutFunc n s p ins outs b = pair (inOutFunc n (pfst s) (pfst p) (map (mapPairSnd pfst) ins) (map (mapPairSnd pfst) outs) (pfst b)) (inOutFunc n (psnd s) (psnd p) (map (mapPairSnd psnd) ins) (map (mapPairSnd psnd) outs) (psnd b))

instance (Pair p) => StateVarSym (p CppSrcCode CppHdrCode) where
  type StateVar (p CppSrcCode CppHdrCode) = StateVarData
  stateVar del l s p t = pair (stateVar del l (pfst s) (pfst p) (pfst t))
    (stateVar del l (psnd s) (psnd p) (psnd t))
  privMVar del l t = pair (privMVar del l $ pfst t) (privMVar del l $ psnd t)
  pubMVar del l t = pair (pubMVar del l $ pfst t) (pubMVar del l $ psnd t)
  pubGVar del l t = pair (pubGVar del l $ pfst t) (pubGVar del l $ psnd t)
  listStateVar del l s p t = pair (listStateVar del l (pfst s) (pfst p) 
    (pfst t)) (listStateVar del l (psnd s) (psnd p) (psnd t))

instance (Pair p) => ClassSym (p CppSrcCode CppHdrCode) where
  -- Bool is True if the class is a main class, False otherwise
  type Class (p CppSrcCode CppHdrCode) = (Doc, Bool)
  buildClass n p s vs fs = pair (buildClass n p (pfst s) (map pfst vs) 
    (map pfst fs)) (buildClass n p (psnd s) (map psnd vs) (map psnd fs))
  enum l ls s = pair (enum l ls $ pfst s) (enum l ls $ psnd s)
  mainClass l vs fs = pair (mainClass l (map pfst vs) (map pfst fs)) 
    (mainClass l (map psnd vs) (map psnd fs))
  privClass n p vs fs = pair (privClass n p (map pfst vs) (map pfst fs))
    (privClass n p (map psnd vs) (map psnd fs))
  pubClass n p vs fs = pair (pubClass n p (map pfst vs) (map pfst fs)) 
    (pubClass n p (map psnd vs) (map psnd fs))

instance (Pair p) => ModuleSym (p CppSrcCode CppHdrCode) where
  type Module (p CppSrcCode CppHdrCode) = ModData
  buildModule n l vs ms cs = pair (buildModule n l (map pfst vs) (map pfst ms) 
    (map pfst cs)) (buildModule n l (map psnd vs) (map psnd ms) (map psnd cs))

-----------------
-- Source File --
-----------------

newtype CppSrcCode a = CPPSC {unCPPSC :: a} deriving Eq

instance Functor CppSrcCode where
  fmap f (CPPSC x) = CPPSC (f x)

instance Applicative CppSrcCode where
  pure = CPPSC
  (CPPSC f) <*> (CPPSC x) = CPPSC (f x)

instance Monad CppSrcCode where
  return = CPPSC
  CPPSC x >>= f = f x

instance PackageSym CppSrcCode where
  type Package CppSrcCode = ([ModData], Label)
  packMods n ms = liftPairFst (sequence mods, n)
    where mods = filter (not . isEmpty . modDoc . unCPPSC) ms
  
instance RenderSym CppSrcCode where
  type RenderFile CppSrcCode = ModData
  fileDoc code = liftA3 md (fmap name code) (fmap isMainMod code)
    (if isEmpty (modDoc (unCPPSC code)) then return empty else
    liftA3 fileDoc' (top code) (fmap modDoc code) bottom)
  top m = liftA3 cppstop m (list dynamic_) endStatement
  bottom = return empty

instance KeywordSym CppSrcCode where
  type Keyword CppSrcCode = Doc
  endStatement = return semi
  endStatementLoop = return empty

  include n = return $ text "#include" <+> doubleQuotedText (n ++ cppHeaderExt)
  inherit = return colon

  list _ = return $ text "vector"
  listObj = return empty

  blockStart = return lbrace
  blockEnd = return rbrace

  ifBodyStart = blockStart
  elseIf = return $ text "else if"
  
  iterForEachLabel = return empty
  iterInLabel = return empty

  commentStart = return doubleSlash
  
  printFunc = return $ text "std::cout"
  printLnFunc = return $ text "std::cout"
  printFileFunc = fmap valDoc -- is this right?
  printFileLnFunc = fmap valDoc

instance PermanenceSym CppSrcCode where
  type Permanence CppSrcCode = Doc
  static_ = return staticDocD
  dynamic_ = return dynamicDocD

instance BodySym CppSrcCode where
  type Body CppSrcCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner s = bodyStatements [s]

  addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym CppSrcCode where
  type Block CppSrcCode = Doc
  block sts = lift1List blockDocD endStatement (map (fmap fst . state) sts)

instance StateTypeSym CppSrcCode where
  type StateType CppSrcCode = TypeData
  bool = return cppBoolTypeDoc
  int = return intTypeDocD
  float = return cppFloatTypeDoc
  char = return charTypeDocD
  string = return stringTypeDocD
  infile = return cppInfileTypeDoc
  outfile = return cppOutfileTypeDoc
  listType p st = liftA2 listTypeDocD st (list p)
  listInnerType t = fmap (getInnerType . cType) t >>= convType
  obj t = return $ typeDocD t
  enumType t = return $ typeDocD t
  iterator t = fmap cppIterTypeDoc (listType dynamic_ t)
  void = return voidDocD

instance ControlBlockSym CppSrcCode where
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

instance UnaryOpSym CppSrcCode where
  type UnaryOp CppSrcCode = Doc
  notOp = return notOpDocD
  negateOp = return negateOpDocD
  sqrtOp = return sqrtOpDocD
  absOp = return absOpDocD
  logOp = return $ text "log10"
  lnOp = return $ text "log"
  expOp = return expOpDocD
  sinOp = return sinOpDocD
  cosOp = return cosOpDocD
  tanOp = return tanOpDocD
  asinOp = return asinOpDocD
  acosOp = return acosOpDocD
  atanOp = return atanOpDocD
  floorOp = return $ text "floor"
  ceilOp = return $ text "ceil"

instance BinaryOpSym CppSrcCode where
  type BinaryOp CppSrcCode = Doc
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
  powerOp = return powerOpDocD
  moduloOp = return moduloOpDocD
  andOp = return andOpDocD
  orOp = return orOpDocD

instance ValueSym CppSrcCode where
  type Value CppSrcCode = ValData
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
  extVar _ = var
  self l = liftA2 (vd (Just "this")) (obj l) (return selfDocD)
  arg n = liftA2 mkVal string (liftA2 argDocD (litInt (n+1)) argsList)
  enumElement en e = liftA2 (vd (Just e)) (obj en) (return $ text e)
  enumVar e en = var e (obj en)
  objVar o v = liftA2 (vd (Just $ valueName o ++ "." ++ valueName v))
    (fmap valType v) (liftA2 objVarDocD o v)
  objVarSelf _ = var
  listVar n p t = var n (listType p t)
  n `listOf` t = listVar n static_ t
  iterVar l t = liftA2 mkVal (iterator t) (return $ text $ "(*" ++ l ++ ")")
  
  inputFunc = liftA2 mkVal string (return $ text "std::cin")
  argsList = liftA2 mkVal (listType static_ string) (return $ text "argv")

  valueName v = fromMaybe 
    (error $ "Attempt to print unprintable Value (" ++ render (valDoc $ unCPPSC 
    v) ++ ")") (valName $ unCPPSC v)
  valueType = fmap valType

instance NumericExpression CppSrcCode where
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

instance BooleanExpression CppSrcCode where
  (?!) = liftA3 typeUnExpr notOp bool
  (?&&) = liftA4 typeBinExpr andOp bool
  (?||) = liftA4 typeBinExpr orOp bool

  (?<) = liftA4 typeBinExpr lessOp bool
  (?<=) = liftA4 typeBinExpr lessEqualOp bool
  (?>) = liftA4 typeBinExpr greaterOp bool
  (?>=) = liftA4 typeBinExpr greaterEqualOp bool
  (?==) = liftA4 typeBinExpr equalOp bool
  (?!=) = liftA4 typeBinExpr notEqualOp bool
   
instance ValueExpression CppSrcCode where
  inlineIf b v1 v2 = liftA2 mkVal (fmap valType v1) (liftA3 inlineIfDocD b v1 
    v2)
  funcApp n t vs = liftA2 mkVal t (liftList (funcAppDocD n) vs)
  selfFuncApp = funcApp
  extFuncApp _ = funcApp
  stateObj t vs = liftA2 mkVal t (liftA2 cppStateObjDoc t (liftList valList vs))
  extStateObj _ = stateObj
  listStateObj = stateObj

  exists = notNull
  notNull v = v

instance Selector CppSrcCode where
  objAccess v f = liftA2 mkVal (fmap funcType f) (liftA2 objAccessDocD v f)
  ($.) = objAccess

  objMethodCall t o f ps = objAccess o (func f t ps)
  objMethodCallNoParams t o f = objMethodCall t o f []

  selfAccess l = objAccess (self l)

  listSizeAccess v = objAccess v listSize

  listIndexExists v i = listSizeAccess v ?> i
  argExists i = objAccess argsList (listAccess string (litInt $ fromIntegral i))
  
  indexOf l v = funcApp "find" int [l $. iterBegin (fmap valType v), 
    l $. iterEnd (fmap valType v), v] #- l $. iterBegin (fmap valType v)

  stringEqual v1 v2 = v1 ?== v2

  castObj f v = liftA2 mkVal (fmap funcType f) (liftA2 castObjDocD f v)
  castStrToFloat v = funcApp "std::stod" float [v]

instance FunctionSym CppSrcCode where
  type Function CppSrcCode = FuncData
  func l t vs = liftA2 fd t (fmap funcDocD (funcApp l t vs))
  cast targT _ = liftA2 fd targT (fmap castDocD targT)
  castListToInt = cast int (listType static_ int)
  get n t = func (getterName n) t []
  set n v = func (setterName n) (fmap valType v) [v]

  listSize = func "size" int []
  listAdd l i v = func "insert" (listType static_ $ fmap valType v) [(l $.
    iterBegin (fmap valType v)) #+ i, v]
  listAppend v = func "push_back" (listType static_ $ fmap valType v) [v]

  iterBegin t = func "begin" (iterator t) []
  iterEnd t = func "end" (iterator t) []

instance SelectorFunction CppSrcCode where
  listAccess t v = func "at" t [v]
  listSet i v = liftA2 fd (listType static_ $ fmap valType v) 
    (liftA2 cppListSetDoc i v)

  listAccessEnum et t v = listAccess t (castObj (cast int et) v)
  listSetEnum t i = listSet (castObj (cast int t) i)

  at t l = listAccess t (var l int) 

instance StatementSym CppSrcCode where
  type Statement CppSrcCode = (Doc, Terminator)
  assign v1 v2 = mkSt <$> liftA2 assignDocD v1 v2
  assignToListIndex lst index v = valState $ lst $. listSet index v
  multiAssign _ _ = error "No multiple assignment statements in C++"
  (&=) = assign
  (&-=) v1 v2 = v1 &= (v1 #- v2)
  (&+=) v1 v2 = mkSt <$> liftA2 plusEqualsDocD v1 v2
  (&++) v = mkSt <$> fmap plusPlusDocD v
  (&~-) v = v &= (v #- litInt 1)

  varDec l t = mkSt <$> fmap (varDecDocD l) t
  varDecDef l t v = mkSt <$> liftA2 (varDecDefDocD l) t v
  listDec l n t = mkSt <$> liftA2 (cppListDecDoc l) (litInt n) t -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
  listDecDef l t vs = mkSt <$> liftA2 (cppListDecDefDoc l) t (liftList 
    valList vs)
  objDecDef l t v = mkSt <$> liftA2 (objDecDefDocD l) t v
  objDecNew l t vs = mkSt <$> liftA2 (objDecDefDocD l) t (stateObj t vs)
  extObjDecNew l _ = objDecNew l
  objDecNewVoid l t = mkSt <$> liftA2 (objDecDefDocD l) t (stateObj t [])
  extObjDecNewVoid l _ = objDecNewVoid l
  constDecDef l t v = mkSt <$> liftA2 (constDecDefDocD l) t v

  print _ v = mkSt <$> liftA2 (cppPrintDocD False) printFunc v
  printLn _ v = mkSt <$> liftA2 (cppPrintDocD True) printLnFunc v
  printStr s = mkSt <$> liftA2 (cppPrintDocD False) printFunc (litString s)
  printStrLn s = mkSt <$> liftA2 (cppPrintDocD True) printLnFunc 
    (litString s)

  printFile f _ v = mkSt <$> liftA2 (cppPrintDocD False) 
    (printFileFunc f) v
  printFileLn f _ v = mkSt <$> liftA2 (cppPrintDocD True) 
    (printFileLnFunc f) v
  printFileStr f s = mkSt <$> liftA2 (cppPrintDocD False) 
    (printFileFunc f) (litString s)
  printFileStrLn f s = mkSt <$> liftA2 (cppPrintDocD True) 
    (printFileLnFunc f) (litString s)

  printList t v = multi [state (printStr "["), 
    for (varDecDef "i" int (litInt 0)) (var "i" int ?< ((v $. listSize) #- 
      litInt 1)) (var "i" int &++) (bodyStatements [
        print t (v $. listAccess t (var "i" int)), printStr ","]),
    state (print t (v $. listAccess t ((v $. listSize) #- litInt 1))),
    printStr "]"]
  printLnList t v = multi [state (printStr "["), 
    for (varDecDef "i" int (litInt 0)) (var "i" int ?< ((v $. listSize) #- 
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

  getIntInput v = mkSt <$> liftA3 cppInput v inputFunc endStatement
  getFloatInput v = mkSt <$> liftA3 cppInput v inputFunc endStatement
  getBoolInput v = mkSt <$> liftA3 cppInput v inputFunc endStatement
  getStringInput v = mkSt <$> liftA3 cppInput v inputFunc endStatement
  getCharInput v = mkSt <$> liftA3 cppInput v inputFunc endStatement
  discardInput = mkSt <$> fmap (cppDiscardInput "\\n") inputFunc

  getIntFileInput f v = mkSt <$> liftA3 cppInput v f endStatement
  getFloatFileInput f v = mkSt <$> liftA3 cppInput v f endStatement
  getBoolFileInput f v = mkSt <$> liftA3 cppInput v f endStatement
  getStringFileInput f v = mkSt <$> liftA3 cppInput v f endStatement
  getCharFileInput f v = mkSt <$> liftA3 cppInput v f endStatement
  discardFileInput f = mkSt <$> fmap (cppDiscardInput " ") f

  openFileR f n = mkSt <$> liftA2 (cppOpenFile "std::fstream::in") f n
  openFileW f n = mkSt <$> liftA2 (cppOpenFile "std::fstream::out") f n
  openFileA f n = mkSt <$> liftA2 (cppOpenFile "std::fstream::app") f n
  closeFile f = valState $ objMethodCall void f "close" []

  getFileInputLine f v = valState $ funcApp "std::getline" string [f, v]
  discardFileLine f = mkSt <$> fmap (cppDiscardInput "\\n") f
  stringSplit d vnew s = let l_ss = "ss"
                             v_ss = var l_ss (obj "std::stringstream")
                             l_word = "word"
                             v_word = var l_word string
                         in
    multi [
      valState $ vnew $. func "clear" void [],
      varDec l_ss (obj "std::stringstream"),
      valState $ objMethodCall string v_ss "str" [s],
      varDec l_word string,
      while (funcApp "std::getline" string [v_ss, v_word, litChar d]) 
        (oneLiner $ valState $ vnew $. listAppend v_word)
    ]

  break = return (mkSt breakDocD)
  continue = return (mkSt continueDocD)

  returnState v = mkSt <$> liftList returnDocD [v]
  returnVar l t = mkSt <$> liftList returnDocD [var l t]
  multiReturn _ = error "Cannot return multiple values in C++"

  valState v = mkSt <$> fmap valDoc v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free v = mkSt <$> fmap freeDocD v

  throw errMsg = mkSt <$> fmap cppThrowDoc (litString errMsg)

  initState fsmName initialState = varDecDef fsmName string 
    (litString initialState)
  changeState fsmName toState = var fsmName string &= litString toState

  initObserverList = listDecDef observerListName
  addObserver t o = valState $ obsList $. listAdd obsList lastelem o
    where obsList = observerListName `listOf` t
          lastelem = obsList $. listSize

  inOutCall n ins [out] = assign out $ funcApp n (fmap valType out) ins
  inOutCall n ins outs = valState $ funcApp n void (nub $ ins ++ outs)

  state = fmap statementDocD
  loopState = fmap (statementDocD . setEmpty)
  multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CppSrcCode where
  ifCond bs b = mkStNoEnd <$> lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b 
    bs
  ifNoElse bs = ifCond bs $ body []
  switch v cs c = mkStNoEnd <$> lift3Pair switchDocD (state break) v c cs
  switchAsIf v cs = ifCond cases
    where cases = map (\(l, b) -> (v ?== l, b)) cs

  ifExists _ ifBody _ = mkStNoEnd <$> ifBody -- All variables are initialized in C++

  for sInit vGuard sUpdate b = mkStNoEnd <$> liftA6 forDocD blockStart blockEnd 
    (loopState sInit) vGuard (loopState sUpdate) b
  forRange i initv finalv stepv = for (varDecDef i int initv) 
    (var i int ?< finalv) (var i int &+= stepv)
  forEach l t v = for (varDecDef l (iterator t) (v $. iterBegin t)) 
    (var l (iterator t) ?!= v $. iterEnd t) (var l (iterator t) &++)
  while v b = mkStNoEnd <$> liftA4 whileDocD blockStart blockEnd v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 cppTryCatch tb cb

  checkState l = switchAsIf (var l string) 

  notifyObservers ft fn t ps = for initv (var index int ?< 
    (obsList $. listSize)) (var index int &++) notify
    where obsList = observerListName `listOf` t
          index = "observerIndex"
          initv = varDecDef index int $ litInt 0
          notify = oneLiner $ valState $ (obsList $. at int index) $. 
            func fn ft ps

  getFileInputAll f v = let l_line = "nextLine"
                            v_line = var l_line string
                        in
    multi [varDec l_line string,
      while (funcApp "std::getline" string [f, v_line])
      (oneLiner $ valState $ v $. listAppend v_line)]

instance ScopeSym CppSrcCode where
  type Scope CppSrcCode = (Doc, ScopeTag)
  private = return (privateDocD, Priv)
  public = return (publicDocD, Pub)

  includeScope _ = return (empty, Priv)

instance MethodTypeSym CppSrcCode where
  type MethodType CppSrcCode = TypeData
  mState t = t
  construct n = return $ td (Object n) (constructDocD n)

instance ParameterSym CppSrcCode where
  type Parameter CppSrcCode = Doc
  stateParam n = fmap (stateParamDocD n)
  pointerParam n = fmap (cppPointerParamDoc n)

instance MethodSym CppSrcCode where
  type Method CppSrcCode = MethodData
  method n c s _ t ps b = liftA2 (mthd False) (fmap snd s) (liftA5 
    (cppsMethod n c) t (liftList paramListDocD ps) b blockStart blockEnd)
  getMethod n c t = method (getterName n) c public dynamic_ t [] getBody
    where getBody = oneLiner $ returnState (self c $-> var n t)
  setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic_ 
    void [stateParam paramLbl t] setBody
    where setBody = oneLiner $ (self c $-> var setLbl t) &= var paramLbl t
  mainMethod _ b = fmap (mthd True Pub) (liftA4 cppMainMethod int b blockStart 
    blockEnd)
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor n vs = 
    let i = "i"
        deleteStatements = map (fmap destructSts) vs
        loopIndexDec = varDec i int
        dbody = if all (isEmpty . fst . unCPPSC) deleteStatements then 
          return empty else bodyStatements $ loopIndexDec : deleteStatements
    in pubMethod ('~':n) n void [] dbody

  function n s _ t ps b = liftA2 (mthd False) (fmap snd s) (liftA5 
    (cppsFunction n) t (liftList paramListDocD ps) b blockStart blockEnd)

  inOutFunc n s p ins [(l,t)] b = function n s p (mState t) (map (fmap (uncurry 
    getParam) . liftPairSnd) ins) (liftA2 appendToBody b $ returnVar l t)
  inOutFunc n s p ins outs b = function n s p (mState void) (map (uncurry 
    pointerParam) outs ++ map (fmap (uncurry getParam) . liftPairSnd) (filter 
    (\(l,_) -> l `notElem` map fst outs) ins)) b

instance StateVarSym CppSrcCode where
  type StateVar CppSrcCode = StateVarData
  stateVar del l s p t = liftA3 svd (fmap snd s) (liftA4 (stateVarDocD l) 
    (fst <$> includeScope s) p t endStatement) (if del < alwaysDel then
    return (mkStNoEnd empty) else free $ var l t)
  privMVar del l = stateVar del l private dynamic_
  pubMVar del l = stateVar del l public dynamic_
  pubGVar del l = stateVar del l public static_
  listStateVar del l s p t = 
    let i = "i"
        guard = var i int ?< (var l t $. listSize)
        loopBody = oneLiner $ free (var l t $. at int i)
        initv = (var i int &= litInt 0)
        deleteLoop = for initv guard (var i int &++) loopBody
    in liftA3 svd (fmap snd s) (stVarDoc <$> stateVar del l s p t) 
      (if del < alwaysDel then return (mkStNoEnd empty) else deleteLoop)

instance ClassSym CppSrcCode where
  -- Bool is True if the class is a main class, False otherwise
  type Class CppSrcCode = (Doc, Bool)
  buildClass n _ _ vs fs = liftPairFst (liftList methodListDocD 
    (map (fmap (\(MthD b _ d) -> (d,b))) (fs ++ [destructor n vs])), 
    any (isMainMthd . unCPPSC) fs)
  enum _ _ _ = return (empty, False)
  mainClass _ vs fs = liftPairFst (liftA2 (cppMainClass (null vs)) (liftList 
    stateVarListDocD (map (fmap stVarDoc) vs)) (liftList methodListDocD (map 
    (fmap (\(MthD b _ d) -> (d,b))) fs)), True)
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public

instance ModuleSym CppSrcCode where
  type Module CppSrcCode = ModData
  buildModule n l _ ms cs = fmap (md n (any (snd . unCPPSC) cs || 
    any (isMainMthd . unCPPSC) ms)) (if all (isEmpty . fst . unCPPSC) cs && all 
    (isEmpty . mthdDoc . unCPPSC) ms then return empty else
    liftA5 cppModuleDoc (liftList vcat (map include l)) 
    (if not (null l) && any (not . isEmpty . fst . unCPPSC) cs then
    return blank else return empty) (liftList methodListDocD (map (fmap 
    (\(MthD b _ d) -> (d,b))) ms)) (if (any (not . isEmpty . fst . unCPPSC) cs 
    || (all (isEmpty . fst . unCPPSC) cs && not (null l))) && 
    any (not . isEmpty . mthdDoc . unCPPSC) ms then return blank else 
    return empty) (liftList vibcat (map (fmap fst) cs)))

-----------------
-- Header File --
-----------------

newtype CppHdrCode a = CPPHC {unCPPHC :: a}

instance Functor CppHdrCode where
  fmap f (CPPHC x) = CPPHC (f x)

instance Applicative CppHdrCode where
  pure = CPPHC
  (CPPHC f) <*> (CPPHC x) = CPPHC (f x)

instance Monad CppHdrCode where
  return = CPPHC
  CPPHC x >>= f = f x

instance PackageSym CppHdrCode where
  type Package CppHdrCode = ([ModData], Label)
  packMods n ms = liftPairFst (sequence mods, n)
    where mods = filter (not . isEmpty . modDoc . unCPPHC) ms

instance RenderSym CppHdrCode where
  type RenderFile CppHdrCode = ModData
  fileDoc code = liftA3 md (fmap name code) (fmap isMainMod code) 
    (if isEmpty (modDoc (unCPPHC code)) then return empty else 
    liftA3 fileDoc' (top code) (fmap modDoc code) bottom)
  top m = liftA3 cpphtop m (list dynamic_) endStatement
  bottom = return $ text "#endif"

instance KeywordSym CppHdrCode where
  type Keyword CppHdrCode = Doc
  endStatement = return semi
  endStatementLoop = return empty

  include n = return $ text "#include" <+> doubleQuotedText (n ++ cppHeaderExt)
  inherit = return colon

  list _ = return $ text "vector"
  listObj = return empty

  blockStart = return lbrace
  blockEnd = return rbrace

  ifBodyStart = return empty
  elseIf = return empty
  
  iterForEachLabel = return empty
  iterInLabel = return empty

  commentStart = return empty
  
  printFunc = return empty
  printLnFunc = return empty
  printFileFunc _ = return empty
  printFileLnFunc _ = return empty

instance PermanenceSym CppHdrCode where
  type Permanence CppHdrCode = Doc
  static_ = return staticDocD
  dynamic_ = return dynamicDocD

instance BodySym CppHdrCode where
  type Body CppHdrCode = Doc
  body _ = return empty
  bodyStatements _ = return empty
  oneLiner _ = return empty

  addComments _ _ = return empty

instance BlockSym CppHdrCode where
  type Block CppHdrCode = Doc
  block _ = return empty

instance StateTypeSym CppHdrCode where
  type StateType CppHdrCode = TypeData
  bool = return cppBoolTypeDoc
  int = return intTypeDocD
  float = return cppFloatTypeDoc
  char = return charTypeDocD
  string = return stringTypeDocD
  infile = return cppInfileTypeDoc
  outfile = return cppOutfileTypeDoc
  listType p st = liftA2 listTypeDocD st (list p)
  listInnerType t = fmap (getInnerType . cType) t >>= convType
  obj t = return $ typeDocD t
  enumType t = return $ typeDocD t
  iterator t = fmap cppIterTypeDoc (listType dynamic_ t)
  void = return voidDocD

instance ControlBlockSym CppHdrCode where
  runStrategy _ _ _ _ = return empty

  listSlice _ _ _ _ _ _ = return empty

instance UnaryOpSym CppHdrCode where
  type UnaryOp CppHdrCode = Doc
  notOp = return empty
  negateOp = return empty
  sqrtOp = return empty
  absOp = return empty
  logOp = return empty
  lnOp = return empty
  expOp = return empty
  sinOp = return empty
  cosOp = return empty
  tanOp = return empty
  asinOp = return empty
  acosOp = return empty
  atanOp = return empty
  floorOp = return empty
  ceilOp = return empty

instance BinaryOpSym CppHdrCode where
  type BinaryOp CppHdrCode = Doc
  equalOp = return empty
  notEqualOp = return empty
  greaterOp = return empty
  greaterEqualOp = return empty
  lessOp = return empty
  lessEqualOp = return empty
  plusOp = return empty
  minusOp = return empty
  multOp = return empty
  divideOp = return empty
  powerOp = return empty
  moduloOp = return empty
  andOp = return empty
  orOp = return empty

instance ValueSym CppHdrCode where
  type Value CppHdrCode = ValData
  litTrue = liftA2 mkVal void (return empty)
  litFalse = liftA2 mkVal void (return empty)
  litChar _ = liftA2 mkVal void (return empty)
  litFloat _ = liftA2 mkVal void (return empty)
  litInt _ = liftA2 mkVal void (return empty)
  litString _ = liftA2 mkVal void (return empty)

  ($->) _ _ = liftA2 mkVal void (return empty)
  ($:) _ _ = liftA2 mkVal void (return empty)

  const _ _ = liftA2 mkVal void (return empty)
  var _ _ = liftA2 mkVal void (return empty)
  extVar _ _ _ = liftA2 mkVal void (return empty)
  self _ = liftA2 mkVal void (return empty)
  arg _ = liftA2 mkVal void (return empty)
  enumElement _ _ = liftA2 mkVal void (return empty)
  enumVar _ _ = liftA2 mkVal void (return empty)
  objVar _ _ = liftA2 mkVal void (return empty)
  objVarSelf _ _ _ = liftA2 mkVal void (return empty)
  listVar _ _ _ = liftA2 mkVal void (return empty)
  listOf _ _ = liftA2 mkVal void (return empty)
  iterVar _ _ = liftA2 mkVal void (return empty)
  
  inputFunc = liftA2 mkVal void (return empty)
  argsList = liftA2 mkVal void (return empty)

  valueName _ = error "Attempted to extract string from Value for C++ header file"
  valueType = error "Attempted to extract type from Value for C++ header file"

instance NumericExpression CppHdrCode where
  (#~) _ = liftA2 mkVal void (return empty)
  (#/^) _ = liftA2 mkVal void (return empty)
  (#|) _ = liftA2 mkVal void (return empty)
  (#+) _ _ = liftA2 mkVal void (return empty)
  (#-) _ _ = liftA2 mkVal void (return empty)
  (#*) _ _ = liftA2 mkVal void (return empty)
  (#/) _ _ = liftA2 mkVal void (return empty)
  (#%) _ _ = liftA2 mkVal void (return empty)
  (#^) _ _ = liftA2 mkVal void (return empty)

  log _ = liftA2 mkVal void (return empty)
  ln _ = liftA2 mkVal void (return empty)
  exp _ = liftA2 mkVal void (return empty)
  sin _ = liftA2 mkVal void (return empty)
  cos _ = liftA2 mkVal void (return empty)
  tan _ = liftA2 mkVal void (return empty)
  csc _ = liftA2 mkVal void (return empty)
  sec _ = liftA2 mkVal void (return empty)
  cot _ = liftA2 mkVal void (return empty)
  arcsin _ = liftA2 mkVal void (return empty)
  arccos _ = liftA2 mkVal void (return empty)
  arctan _ = liftA2 mkVal void (return empty)
  floor _ = liftA2 mkVal void (return empty)
  ceil _ = liftA2 mkVal void (return empty)

instance BooleanExpression CppHdrCode where
  (?!) _ = liftA2 mkVal void (return empty)
  (?&&) _ _ = liftA2 mkVal void (return empty)
  (?||) _ _ = liftA2 mkVal void (return empty)

  (?<) _ _ = liftA2 mkVal void (return empty)
  (?<=) _ _ = liftA2 mkVal void (return empty)
  (?>) _ _ = liftA2 mkVal void (return empty)
  (?>=) _ _ = liftA2 mkVal void (return empty)
  (?==) _ _ = liftA2 mkVal void (return empty)
  (?!=) _ _ = liftA2 mkVal void (return empty)
   
instance ValueExpression CppHdrCode where
  inlineIf _ _ _ = liftA2 mkVal void (return empty)
  funcApp _ _ _ = liftA2 mkVal void (return empty)
  selfFuncApp _ _ _ = liftA2 mkVal void (return empty)
  extFuncApp _ _ _ _ = liftA2 mkVal void (return empty)
  stateObj _ _ = liftA2 mkVal void (return empty)
  extStateObj _ _ _ = liftA2 mkVal void (return empty)
  listStateObj _ _ = liftA2 mkVal void (return empty)

  exists _ = liftA2 mkVal void (return empty)
  notNull _ = liftA2 mkVal void (return empty)

instance Selector CppHdrCode where
  objAccess _ _ = liftA2 mkVal void (return empty)
  ($.) _ _ = liftA2 mkVal void (return empty)

  objMethodCall _ _ _ _ = liftA2 mkVal void (return empty)
  objMethodCallNoParams _ _ _ = liftA2 mkVal void (return empty)

  selfAccess _ _ = liftA2 mkVal void (return empty)

  listSizeAccess _ = liftA2 mkVal void (return empty)

  listIndexExists _ _ = liftA2 mkVal void (return empty)
  argExists _ = liftA2 mkVal void (return empty)
  
  indexOf _ _ = liftA2 mkVal void (return empty)

  stringEqual _ _ = liftA2 mkVal void (return empty)

  castObj _ _ = liftA2 mkVal void (return empty)
  castStrToFloat _ = liftA2 mkVal void (return empty)

instance FunctionSym CppHdrCode where
  type Function CppHdrCode = FuncData
  func _ _ _ = liftA2 fd void (return empty)
  cast _ _ = liftA2 fd void (return empty)
  castListToInt = liftA2 fd void (return empty)
  get _ _ = liftA2 fd void (return empty)
  set _ _ = liftA2 fd void (return empty)

  listSize = liftA2 fd void (return empty)
  listAdd _ _ _ = liftA2 fd void (return empty)
  listAppend _ = liftA2 fd void (return empty)

  iterBegin _ = liftA2 fd void (return empty)
  iterEnd _ = liftA2 fd void (return empty)

instance SelectorFunction CppHdrCode where
  listAccess _ _ = liftA2 fd void (return empty)
  listSet _ _ = liftA2 fd void (return empty)

  listAccessEnum _ _ _ = liftA2 fd void (return empty)
  listSetEnum _ _ _ = liftA2 fd void (return empty)

  at _ _ = liftA2 fd void (return empty)

instance StatementSym CppHdrCode where
  type Statement CppHdrCode = (Doc, Terminator)
  assign _ _ = return (mkStNoEnd empty)
  assignToListIndex _ _ _ = return (mkStNoEnd empty)
  multiAssign _ _ = return (mkStNoEnd empty)
  (&=) _ _ = return (mkStNoEnd empty)
  (&-=) _ _ = return (mkStNoEnd empty)
  (&+=) _ _ = return (mkStNoEnd empty)
  (&++) _ = return (mkStNoEnd empty)
  (&~-) _ = return (mkStNoEnd empty)

  varDec _ _ = return (mkStNoEnd empty)
  varDecDef _ _ _ = return (mkStNoEnd empty)
  listDec _ _ _ = return (mkStNoEnd empty)
  listDecDef _ _ _ = return (mkStNoEnd empty)
  objDecDef _ _ _ = return (mkStNoEnd empty)
  objDecNew _ _ _ = return (mkStNoEnd empty)
  extObjDecNew _ _ _ _ = return (mkStNoEnd empty)
  objDecNewVoid _ _ = return (mkStNoEnd empty)
  extObjDecNewVoid _ _ _ = return (mkStNoEnd empty)
  constDecDef _ _ _ = return (mkStNoEnd empty)

  print _ _ = return (mkStNoEnd empty)
  printLn _ _ = return (mkStNoEnd empty)
  printStr _ = return (mkStNoEnd empty)
  printStrLn _ = return (mkStNoEnd empty)

  printFile _ _ _ = return (mkStNoEnd empty)
  printFileLn _ _ _ = return (mkStNoEnd empty)
  printFileStr _ _ = return (mkStNoEnd empty)
  printFileStrLn _ _ = return (mkStNoEnd empty)

  printList _ _ = return (mkStNoEnd empty)
  printLnList _ _ = return (mkStNoEnd empty)
  printFileList _ _ _ = return (mkStNoEnd empty)
  printFileLnList _ _ _ = return (mkStNoEnd empty)

  getIntInput _ = return (mkStNoEnd empty)
  getFloatInput _ = return (mkStNoEnd empty)
  getBoolInput _ = return (mkStNoEnd empty)
  getStringInput _ = return (mkStNoEnd empty)
  getCharInput _ = return (mkStNoEnd empty)
  discardInput = return (mkStNoEnd empty)

  getIntFileInput _ _ = return (mkStNoEnd empty)
  getFloatFileInput _ _ = return (mkStNoEnd empty)
  getBoolFileInput _ _ = return (mkStNoEnd empty)
  getStringFileInput _ _ = return (mkStNoEnd empty)
  getCharFileInput _ _ = return (mkStNoEnd empty)
  discardFileInput _ = return (mkStNoEnd empty)

  openFileR _ _ = return (mkStNoEnd empty)
  openFileW _ _ = return (mkStNoEnd empty)
  openFileA _ _ = return (mkStNoEnd empty)
  closeFile _ = return (mkStNoEnd empty)

  getFileInputLine _ _ = return (mkStNoEnd empty)
  discardFileLine _ = return (mkStNoEnd empty)
  stringSplit _ _ _ = return (mkStNoEnd empty)

  break = return (mkStNoEnd empty)
  continue = return (mkStNoEnd empty)

  returnState _ = return (mkStNoEnd empty)
  returnVar _ _ = return (mkStNoEnd empty)
  multiReturn _ = return (mkStNoEnd empty)

  valState _ = return (mkStNoEnd empty)

  comment _ = return (mkStNoEnd empty)

  free _ = return (mkStNoEnd empty)

  throw _ = return (mkStNoEnd empty)

  initState _ _ = return (mkStNoEnd empty)
  changeState _ _ = return (mkStNoEnd empty)

  initObserverList _ _ = return (mkStNoEnd empty)
  addObserver _ _ = return (mkStNoEnd empty)

  inOutCall _ _ _ = return (mkStNoEnd empty)

  state _ = return (mkStNoEnd empty)
  loopState _ = return (mkStNoEnd empty)
  multi _ = return (mkStNoEnd empty)

instance ControlStatementSym CppHdrCode where
  ifCond _ _ = return (mkStNoEnd empty)
  ifNoElse _ = return (mkStNoEnd empty)
  switch _ _ _ = return (mkStNoEnd empty)
  switchAsIf _ _ _ = return (mkStNoEnd empty)

  ifExists _ _ _ = return (mkStNoEnd empty)

  for _ _ _ _ = return (mkStNoEnd empty)
  forRange _ _ _ _ _ = return (mkStNoEnd empty)
  forEach _ _ _ _ = return (mkStNoEnd empty)
  while _ _ = return (mkStNoEnd empty)

  tryCatch _ _ = return (mkStNoEnd empty)

  checkState _ _ _ = return (mkStNoEnd empty)

  notifyObservers _ _ _ _ = return (mkStNoEnd empty)

  getFileInputAll _ _ = return (mkStNoEnd empty)

instance ScopeSym CppHdrCode where
  type Scope CppHdrCode = (Doc, ScopeTag)
  private = return (privateDocD, Priv)
  public = return (publicDocD, Pub)

  includeScope _ = return (empty, Priv)

instance MethodTypeSym CppHdrCode where
  type MethodType CppHdrCode = TypeData
  mState t = t
  construct n = return $ td (Object n) (constructDocD n)

instance ParameterSym CppHdrCode where
  type Parameter CppHdrCode = Doc
  stateParam n = fmap (stateParamDocD n)
  pointerParam n = fmap (cppPointerParamDoc n)

instance MethodSym CppHdrCode where
  type Method CppHdrCode = MethodData
  method n _ s _ t ps _ = liftA2 (mthd False) (fmap snd s) 
    (liftA3 (cpphMethod n) t (liftList paramListDocD ps) endStatement)
  getMethod n c t = method (getterName n) c public dynamic_ t [] (return empty)
  setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic_ 
    void [stateParam paramLbl t] (return empty)
  mainMethod _ _ = return (mthd True Pub empty)
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor n _ = pubMethod ('~':n) n void [] (return empty)

  function n = method n ""

  inOutFunc n s p ins [(l,t)] b = function n s p (mState t) (map (fmap (uncurry 
    getParam) . liftPairSnd) ins) (liftA2 appendToBody b $ returnVar l t)
  inOutFunc n s p ins outs b = function n s p (mState void) (map (uncurry 
    pointerParam) outs ++ map (fmap (uncurry getParam) . liftPairSnd) (filter 
    (\(l,_) -> l `notElem` map fst outs) ins)) b

instance StateVarSym CppHdrCode where
  type StateVar CppHdrCode = StateVarData
  stateVar _ l s p t = liftA3 svd (fmap snd s) (liftA4 (stateVarDocD l) 
    (fmap fst (includeScope s)) p t endStatement) (return (mkStNoEnd empty))
  privMVar del l = stateVar del l private dynamic_
  pubMVar del l = stateVar del l public dynamic_
  pubGVar del l = stateVar del l public static_
  listStateVar = stateVar

instance ClassSym CppHdrCode where
  -- Bool is True if the class is a main class, False otherwise
  type Class CppHdrCode = (Doc, Bool)
  -- do this with a do? avoids liftA8...
  buildClass n p _ vs fs = liftPairFst (liftA8 (cpphClass n p) (lift2Lists 
    (cpphVarsFuncsList Pub) vs (fs ++ [destructor n vs])) (lift2Lists 
    (cpphVarsFuncsList Priv) vs (fs ++ [destructor n vs])) (fmap fst public)
    (fmap fst private) inherit blockStart blockEnd endStatement, 
    any (isMainMthd . unCPPHC) fs)
  enum n es _ = liftPairFst (liftA4 (cpphEnum n) (return $ enumElementsDocD es 
    enumsEqualInts) blockStart blockEnd endStatement, False)
  mainClass _ _ _ = return (empty, True)
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public

instance ModuleSym CppHdrCode where
  type Module CppHdrCode = ModData
  buildModule n l _ ms cs = fmap (md n (any (snd . unCPPHC) cs || 
    any (snd . unCPPHC) methods)) (if all (isEmpty . fst . unCPPHC) cs && all 
    (isEmpty . mthdDoc . unCPPHC) ms then return empty else liftA5 cppModuleDoc
    (liftList vcat (map include l)) (if not (null l) && any 
    (not . isEmpty . fst . unCPPHC) cs then return blank else return empty) 
    (liftList methodListDocD methods) (if (any (not . isEmpty . fst . unCPPHC) 
    cs || (all (isEmpty . fst . unCPPHC) cs && not (null l))) && any 
    (not . isEmpty . mthdDoc . unCPPHC) ms then return blank else return empty)
    (liftList vibcat (map (fmap fst) cs)))
    where methods = map (fmap (\(MthD m _ d) -> (d, m))) ms

-- helpers
isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

getParam :: Label -> TypeData -> Doc
getParam l st@(TD (List _) _) = cppPointerParamDoc l st
getParam l st@(TD (Object _) _) = cppPointerParamDoc l st
getParam l st = stateParamDocD l st
 
-- convenience
enumsEqualInts :: Bool
enumsEqualInts = False

cppHeaderExt :: Label
cppHeaderExt = ".hpp"

cppstop :: ModData -> Doc -> Doc -> Doc
cppstop (MD n b _) lst end = vcat [
  if b then empty else inc <+> doubleQuotedText (n ++ cppHeaderExt),
  if b then empty else blank,
  inc <+> angles (text "algorithm"),
  inc <+> angles (text "iostream"),
  inc <+> angles (text "fstream"),
  inc <+> angles (text "iterator"),
  inc <+> angles (text "string"),
  inc <+> angles (text "math.h"),
  inc <+> angles (text "sstream"),
  inc <+> angles (text "limits"),
  inc <+> angles lst,
  blank,
  usingNameSpace "std" (Just "string") end,
  usingNameSpace "std" (Just $ render lst) end,
  usingNameSpace "std" (Just "ifstream") end,
  usingNameSpace "std" (Just "ofstream") end]
  where inc = text "#include"

cpphtop :: ModData -> Doc -> Doc -> Doc
cpphtop (MD n _ _) lst end = vcat [
  text "#ifndef" <+> text n <> text "_h",
  text "#define" <+> text n <> text "_h",
  blank,
  inc <+> angles (text "string"),
  inc <+> angles lst,
  blank,
  usingNameSpace "std" (Just "string") end,
  usingNameSpace "std" (Just $ render lst) end,
  usingNameSpace "std" (Just "ifstream") end,
  usingNameSpace "std" (Just "ofstream") end]
  where inc = text "#include"

usingNameSpace :: Label -> Maybe Label -> Doc -> Doc
usingNameSpace n (Just m) end = text "using" <+> text n <> colon <> colon <>
  text m <> end
usingNameSpace n Nothing end = text "using namespace" <+> text n <> end

cppBoolTypeDoc :: TypeData
cppBoolTypeDoc = td Boolean (text "bool")

cppFloatTypeDoc :: TypeData
cppFloatTypeDoc = td Float (text "double")

cppInfileTypeDoc :: TypeData
cppInfileTypeDoc = td File (text "ifstream")

cppOutfileTypeDoc :: TypeData
cppOutfileTypeDoc = td File (text "ofstream")

cppIterTypeDoc :: TypeData -> TypeData
cppIterTypeDoc t = td (Iterator (cType t)) (text "std::" <> typeDoc t <>
  text "::iterator")

cppStateObjDoc :: TypeData -> Doc -> Doc
cppStateObjDoc t ps = typeDoc t <> parens ps

cppListSetDoc :: ValData -> ValData -> Doc
cppListSetDoc i v = dot <> text "at" <> parens (valDoc i) <+> equals <+> valDoc v

cppListDecDoc :: Label -> ValData -> TypeData -> Doc
cppListDecDoc l n t = typeDoc t <+> text l <> parens (valDoc n)

cppListDecDefDoc :: Label -> TypeData -> Doc -> Doc
cppListDecDefDoc l t vs = typeDoc t <+> text l <> braces vs

cppPrintDocD :: Bool -> Doc -> ValData -> Doc
cppPrintDocD newLn printFn v = printFn <+> text "<<" <+> valDoc v <+> end
  where end = if newLn then text "<<" <+> text "std::endl" else empty

cppThrowDoc :: ValData -> Doc
cppThrowDoc errMsg = text "throw" <> parens (valDoc errMsg)

cppTryCatch :: Doc -> Doc -> Doc
cppTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent tb,
  rbrace <+> text "catch" <+> parens (text "...") <+> lbrace,
  indent cb,
  rbrace]

cppDiscardInput :: Label -> ValData -> Doc
cppDiscardInput sep inFn = valDoc inFn <> dot <> text "ignore" <> parens 
  (text "std::numeric_limits<std::streamsize>::max()" <> comma <+>
  quotes (text sep))

cppInput :: ValData -> ValData -> Doc -> Doc
cppInput v inFn end = vcat [
  valDoc inFn <+> text ">>" <+> valDoc v <> end,
  valDoc inFn <> dot <> 
    text "ignore(std::numeric_limits<std::streamsize>::max(), '\\n')"]

cppOpenFile :: Label -> ValData -> ValData -> Doc
cppOpenFile mode f n = valDoc f <> dot <> text "open" <> 
  parens (valDoc n <> comma <+> text mode)

cppPointerParamDoc :: Label -> TypeData  -> Doc
cppPointerParamDoc n t = typeDoc t <+> text "&" <> text n

cppsMethod :: Label -> Label -> TypeData -> Doc -> Doc -> Doc -> Doc -> Doc
cppsMethod n c t ps b bStart bEnd = vcat [ttype <+> text c <> text "::" <> 
  text n <> parens ps <+> bStart,
  indent b,
  bEnd]
  where ttype | isDtor n = empty
              | otherwise = typeDoc t

cppsFunction :: Label -> TypeData -> Doc -> Doc -> Doc -> Doc -> Doc
cppsFunction n t ps b bStart bEnd = vcat [
  typeDoc t <+> text n <> parens ps <+> bStart,
  indent b,
  bEnd]

cpphMethod :: Label -> TypeData -> Doc -> Doc -> Doc
cpphMethod n t ps end | isDtor n = text n <> parens ps <> end
                      | otherwise = typeDoc t <+> text n <> parens ps <> end

cppMainMethod :: TypeData  -> Doc -> Doc -> Doc -> Doc
cppMainMethod t b bStart bEnd = vcat [
  typeDoc t <+> text "main" <> parens (text "int argc, const char *argv[]") <+> bStart,
  indent b,
  blank,
  indent $ text "return 0;",
  bEnd]

cpphVarsFuncsList :: ScopeTag -> [StateVarData] -> [MethodData] -> Doc
cpphVarsFuncsList st vs fs = 
  let scopedVs = [stVarDoc v | v <- vs, getStVarScp v == st]
      scopedFs = [mthdDoc f | f <- fs, getMthdScp f == st]
  in vcat $ scopedVs ++ (if null scopedVs then empty else blank) : scopedFs

cpphClass :: Label -> Maybe Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> 
  Doc -> Doc -> Doc
cpphClass n p pubs privs pub priv inhrt bStart bEnd end =
  let baseClass = case p of Nothing -> empty
                            Just pn -> inhrt <+> pub <+> text pn
  in vcat [
      classDec <+> text n <+> baseClass <+> bStart,
      indentList [
        pub <> colon,
        indent pubs,
        blank,
        priv <> colon,
        indent privs],
      bEnd <> end]

cppMainClass :: Bool -> Doc -> Doc -> Doc
cppMainClass b vs fs = vcat [
  vs,
  if b then empty else blank,
  fs]

cpphEnum :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
cpphEnum n es bStart bEnd end = vcat [
  text "enum" <+> text n <+> bStart,
  indent es,
  bEnd <> end]

cppModuleDoc :: Doc -> Doc -> Doc -> Doc -> Doc -> Doc
cppModuleDoc ls blnk1 fs blnk2 cs = vcat [
  ls,
  blnk1,
  cs,
  blnk2,
  fs]