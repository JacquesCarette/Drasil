{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer (
  -- * C++ Code Configuration -- defines syntax of all C++ code
  CppSrcCode(..), CppHdrCode(..), CppCode(..), unCPPC
) where

import Utils.Drasil (indent, indentList)

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
  fileDoc', enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, outDoc,
  intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, enumTypeDocD, 
  listTypeDocD, voidDocD, constructDocD, stateParamDocD, paramListDocD, mkParam,
  methodListDocD, stateVarDocD, stateVarListDocD, alwaysDel, ifCondDocD, 
  switchDocD, forDocD, whileDocD, stratDocD, assignDocD, plusEqualsDocD, 
  plusPlusDocD, varDecDocD, varDecDefDocD, objDecDefDocD, constDecDefDocD, 
  statementDocD, returnDocD, commentDocD, freeDocD, mkSt, mkStNoEnd, 
  stringListVals', stringListLists', unOpPrec, notOpDocD, 
  negateOpDocD, sqrtOpDocD, absOpDocD, expOpDocD, sinOpDocD, cosOpDocD, 
  tanOpDocD, asinOpDocD, acosOpDocD, atanOpDocD, unExpr, unExpr', typeUnExpr, 
  equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', 
  typeBinExpr, mkVal, litTrueD, litFalseD, litCharD, litFloatD, litIntD, 
  litStringD, varDocD, selfDocD, argDocD, objVarDocD, inlineIfD, funcAppDocD,
  funcDocD, castDocD, objAccessDocD, castObjDocD, breakDocD, continueDocD, 
  staticDocD, dynamicDocD, privateDocD, publicDocD, classDec, dot, 
  blockCmtStart, blockCmtEnd, docCmtStart, observerListName, doxConfigName, 
  makefileName, doubleSlash, blockCmtDoc, docCmtDoc, commentedItem, 
  addCommentsDocD, functionDoc, classDoc, moduleDoc, docFuncRepr, valList, 
  appendToBody, surroundBody, getterName, setterName, setEmpty, intValue)
import Language.Drasil.Code.Imperative.Data (Pair(..), pairList, Terminator(..),
  ScopeTag(..), AuxData(..), ad, emptyAux, FileData(..), srcFile, hdrFile, 
  updateFileMod, FuncData(..), fd, ModData(..), md, updateModDoc, OpData(..), 
  od, PackData(..), packD, emptyPack, ParamData(..), pd, ProgData(..), progD, 
  emptyProg, StateVarData(..), svd, TypeData(..), td, ValData(..), VarData(..), 
  vard)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, cppCompiler, nativeBinary)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.Helpers (angles, blank, doubleQuotedText,
  emptyIfEmpty, mapPairFst, mapPairSnd, vibcat, liftA4, liftA5, liftA6, liftA8,
  liftList, lift2Lists, lift1List, lift3Pair, lift4Pair, liftPair, liftPairFst, 
  getInnerType, convType, checkParams)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), braces, parens, comma,
  empty, equals, semi, vcat, lbrace, rbrace, quotes, render, colon, isEmpty)

cppHdrExt, cppSrcExt :: String
cppHdrExt = "hpp"
cppSrcExt = "cpp"

data CppCode x y a = CPPC {src :: x a, hdr :: y a}

instance Pair CppCode where
  pfst (CPPC xa _) = xa
  psnd (CPPC _ yb) = yb
  pair = CPPC

unCPPC :: CppCode CppSrcCode CppHdrCode a -> a
unCPPC (CPPC (CPPSC a) _) = a

hdrToSrc :: CppHdrCode a -> CppSrcCode a
hdrToSrc (CPPHC a) = CPPSC a

instance (Pair p) => PackageSym (p CppSrcCode CppHdrCode) where
  type Package (p CppSrcCode CppHdrCode) = PackData
  package p aux = pair (package (pfst p) (map pfst aux)) (return emptyPack)

instance (Pair p) => ProgramSym (p CppSrcCode CppHdrCode) where
  type Program (p CppSrcCode CppHdrCode) = ProgData
  prog n ms = pair (prog n $ map (hdrToSrc . psnd) ms ++ map pfst ms) 
    (return emptyProg)

instance (Pair p) => RenderSym (p CppSrcCode CppHdrCode) where
  type RenderFile (p CppSrcCode CppHdrCode) = FileData
  fileDoc code = pair (fileDoc $ pfst code) (fileDoc $ psnd code)

  docMod d a dt m = pair (docMod d a dt $ pfst m) (docMod d a dt $ psnd m)

  commentedMod cmt m = pair (commentedMod (pfst cmt) (pfst m)) 
    (commentedMod (psnd cmt) (psnd m))

instance (Pair p) => InternalFile (p CppSrcCode CppHdrCode) where
  top m = pair (top $ pfst m) (top $ psnd m)
  bottom = pair bottom bottom

instance (Pair p) => AuxiliarySym (p CppSrcCode CppHdrCode) where
  type Auxiliary (p CppSrcCode CppHdrCode) = AuxData
  doxConfig pName p = pair (doxConfig pName $ pfst p) (return emptyAux)

  optimizeDox = pair optimizeDox (return empty)

  makefile cms p = pair (makefile cms $ pfst p) (return emptyAux)

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
  blockCommentStart = pair blockCommentStart blockCommentStart
  blockCommentEnd = pair blockCommentEnd blockCommentEnd
  docCommentStart = pair docCommentStart docCommentStart
  docCommentEnd = pair docCommentEnd docCommentEnd

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

  getType s = getType $ pfst s

instance (Pair p) => ControlBlockSym (p CppSrcCode CppHdrCode) where
  runStrategy l strats rv av = pair (runStrategy l (map (mapPairSnd pfst) 
    strats) (fmap pfst rv) (fmap pfst av)) (runStrategy l (map 
    (mapPairSnd psnd) strats) (fmap psnd rv) (fmap psnd av))

  listSlice vnew vold b e s = pair (listSlice (pfst vnew) (pfst vold)
    (fmap pfst b) (fmap pfst e) (fmap pfst s)) (listSlice (psnd vnew) 
    (psnd vold) (fmap psnd b) (fmap psnd e) (fmap psnd s))

instance (Pair p) => UnaryOpSym (p CppSrcCode CppHdrCode) where
  type UnaryOp (p CppSrcCode CppHdrCode) = OpData
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
  type BinaryOp (p CppSrcCode CppHdrCode) = OpData
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

instance (Pair p) => VariableSym (p CppSrcCode CppHdrCode) where
  type Variable (p CppSrcCode CppHdrCode) = VarData
  var n t = pair (var n $ pfst t) (var n $ psnd t)
  const n t = pair (const n $ pfst t) (const n $ psnd t)
  extVar l n t = pair (extVar l n $ pfst t) (extVar l n $ psnd t)
  self l = pair (self l) (self l)
  enumVar e en = pair (enumVar e en) (enumVar e en)
  objVar o v = pair (objVar (pfst o) (pfst v)) (objVar (psnd o) (psnd v))
  objVarSelf l n t = pair (objVarSelf l n $ pfst t) (objVarSelf l n $ psnd t)
  listVar n p t = pair (listVar n (pfst p) (pfst t)) (listVar n (psnd p) (psnd t))
  n `listOf` t = pair (n `listOf` pfst t) (n `listOf` psnd t)
  iterVar l t = pair (iterVar l $ pfst t) (iterVar l $ psnd t)
  
  ($->) v1 v2 = pair (($->) (pfst v1) (pfst v2)) (($->) (psnd v1) (psnd v2))

  variableName v = variableName $ pfst v
  variableType v = pair (variableType $ pfst v) (variableType $ psnd v)
  variableDoc v = variableDoc $ pfst v

instance (Pair p) => ValueSym (p CppSrcCode CppHdrCode) where
  type Value (p CppSrcCode CppHdrCode) = ValData
  litTrue = pair litTrue litTrue
  litFalse = pair litFalse litFalse
  litChar c = pair (litChar c) (litChar c)
  litFloat v = pair (litFloat v) (litFloat v)
  litInt v = pair (litInt v) (litInt v)
  litString s = pair (litString s) (litString s)

  ($:) l1 l2 = pair (($:) l1 l2) (($:) l1 l2)

  valueOf v = pair (valueOf $ pfst v) (valueOf $ psnd v)
  arg n = pair (arg n) (arg n)
  enumElement en e = pair (enumElement en e) (enumElement en e)
  
  argsList = pair argsList argsList

  valueType v = pair (valueType $ pfst v) (valueType $ psnd v)
  valueDoc v = valueDoc $ pfst v

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
  
instance (Pair p) => InternalValue (p CppSrcCode CppHdrCode) where
  inputFunc = pair inputFunc inputFunc
  printFunc = pair printFunc printFunc
  printLnFunc = pair printLnFunc printLnFunc
  printFileFunc v = pair (printFileFunc $ pfst v) (printFileFunc $ psnd v)
  printFileLnFunc v = pair (printFileLnFunc $ pfst v) (printFileLnFunc $ psnd v)

  cast t v = pair (cast (pfst t) (pfst v)) (cast (psnd t) (psnd v))

instance (Pair p) => Selector (p CppSrcCode CppHdrCode) where
  objAccess v f = pair (objAccess (pfst v) (pfst f)) (objAccess (psnd v) 
    (psnd f))
  ($.) v f = pair (($.) (pfst v) (pfst f)) (($.) (psnd v) (psnd f))

  objMethodCall t o f ps = pair (objMethodCall (pfst t) (pfst o) f 
    (map pfst ps)) (objMethodCall (psnd t) (psnd o) f (map psnd ps))
  objMethodCallNoParams t o f = pair (objMethodCallNoParams (pfst t) (pfst o) f)
    (objMethodCallNoParams (psnd t) (psnd o) f)

  selfAccess l f = pair (selfAccess l $ pfst f) (selfAccess l $ psnd f)

  listIndexExists v i = pair (listIndexExists (pfst v) (pfst i)) 
    (listIndexExists (psnd v) (psnd i))
  argExists i = pair (argExists i) (argExists i)
  
  indexOf l v = pair (indexOf (pfst l) (pfst v)) (indexOf (psnd l) (psnd v))

instance (Pair p) => FunctionSym (p CppSrcCode CppHdrCode) where
  type Function (p CppSrcCode CppHdrCode) = FuncData
  func l t vs = pair (func l (pfst t) (map pfst vs)) (func l (psnd t) (map psnd vs))

  get v vToGet = pair (get (pfst v) (pfst vToGet)) (get (psnd v) (psnd vToGet))
  set v vToSet toVal = pair (set (pfst v) (pfst vToSet) (pfst toVal))
    (set (psnd v) (psnd vToSet) (psnd toVal))

  listSize v = pair (listSize $ pfst v) (listSize $ psnd v)
  listAdd v i vToAdd = pair (listAdd (pfst v) (pfst i) (pfst vToAdd)) 
    (listAdd (psnd v) (psnd i) (psnd vToAdd))
  listAppend v vToApp = pair (listAppend (pfst v) (pfst vToApp)) 
    (listAppend (psnd v) (psnd vToApp))

  iterBegin v = pair (iterBegin $ pfst v) (iterBegin $ psnd v)
  iterEnd v = pair (iterEnd $ pfst v) (iterEnd $ psnd v)

instance (Pair p) => SelectorFunction (p CppSrcCode CppHdrCode) where
  listAccess v i = pair (listAccess (pfst v) (pfst i)) 
    (listAccess (psnd v) (psnd i))
  listSet v i toVal = pair (listSet (pfst v) (pfst i) (pfst toVal)) 
    (listSet (psnd v) (psnd i) (psnd toVal))
  at v l = pair (at (pfst v) l) (at (psnd v) l)

instance (Pair p) => InternalFunction (p CppSrcCode CppHdrCode) where  
  getFunc v = pair (getFunc $ pfst v) (getFunc $ psnd v)
  setFunc t v toVal = pair (setFunc (pfst t) (pfst v) (pfst toVal)) 
    (setFunc (psnd t) (psnd v) (psnd toVal))

  listSizeFunc = pair listSizeFunc listSizeFunc
  listAddFunc l i v = pair (listAddFunc (pfst l) (pfst i) (pfst v)) 
    (listAddFunc (psnd l) (psnd i) (psnd v))
  listAppendFunc v = pair (listAppendFunc $ pfst v) (listAppendFunc $ psnd v)

  iterBeginFunc t = pair (iterBeginFunc $ pfst t) (iterBeginFunc $ psnd t)
  iterEndFunc t = pair (iterEndFunc $ pfst t) (iterEndFunc $ psnd t)

  listAccessFunc t v = pair (listAccessFunc (pfst t) (pfst v)) (listAccessFunc 
    (psnd t) (psnd v))
  listSetFunc v i toVal = pair (listSetFunc (pfst v) (pfst i) (pfst toVal)) 
    (listSetFunc (psnd v) (psnd i) (psnd toVal))

  atFunc t l = pair (atFunc (pfst t) l) (atFunc (psnd t) l)

instance (Pair p) => InternalStatement (p CppSrcCode CppHdrCode) where
  printSt nl p v f = pair (printSt nl (pfst p) (pfst v) (fmap pfst f)) 
    (printSt nl (psnd p) (psnd v) (fmap psnd f))
    
  state s = pair (state $ pfst s) (state $ psnd s)
  loopState s = pair (loopState $ pfst s) (loopState $ psnd s)

instance (Pair p) => StatementSym (p CppSrcCode CppHdrCode) where
  type Statement (p CppSrcCode CppHdrCode) = (Doc, Terminator)
  assign vr vl = pair (assign (pfst vr) (pfst vl)) (assign (psnd vr) (psnd vl))
  assignToListIndex lst index v = pair (assignToListIndex (pfst lst) (pfst 
    index) (pfst v)) (assignToListIndex (psnd lst) (psnd index) (psnd v))
  multiAssign vrs vls = pair (multiAssign (map pfst vrs) (map pfst vls)) 
    (multiAssign (map psnd vrs) (map psnd vls))
  (&=) vr vl = pair ((&=) (pfst vr) (pfst vl)) ((&=) (psnd vr) (psnd vl))
  (&-=) vr vl = pair ((&-=) (pfst vr) (pfst vl)) ((&-=) (psnd vr) (psnd vl))
  (&+=) vr vl = pair ((&+=) (pfst vr) (pfst vl)) ((&+=) (psnd vr) (psnd vl))
  (&++) v = pair ((&++) $ pfst v) ((&++) $ psnd v)
  (&~-) v = pair ((&~-) $ pfst v) ((&~-) $ psnd v)

  varDec v = pair (varDec $ pfst v) (varDec $ psnd v)
  varDecDef v def = pair (varDecDef (pfst v) (pfst def)) (varDecDef (psnd v) 
    (psnd def))
  listDec n v = pair (listDec n $ pfst v) (listDec n $ psnd v)
  listDecDef v vs = pair (listDecDef (pfst v) (map pfst vs)) (listDecDef 
    (psnd v) (map psnd vs))
  objDecDef v def = pair (objDecDef (pfst v) (pfst def)) (objDecDef (psnd v)
    (psnd def))
  objDecNew v vs = pair (objDecNew (pfst v) (map pfst vs)) (objDecNew 
    (psnd v) (map psnd vs))
  extObjDecNew lib v vs = pair (extObjDecNew lib (pfst v) (map pfst vs)) 
    (extObjDecNew lib (psnd v) (map psnd vs))
  objDecNewVoid v = pair (objDecNewVoid $ pfst v) (objDecNewVoid $ psnd v)
  extObjDecNewVoid lib v = pair (extObjDecNewVoid lib $ pfst v) 
    (extObjDecNewVoid lib $ psnd v)
  constDecDef v def = pair (constDecDef (pfst v) (pfst def)) (constDecDef 
    (psnd v) (psnd def))

  print v = pair (print $ pfst v) (print $ psnd v)
  printLn v = pair (printLn $ pfst v) (printLn $ psnd v)
  printStr s = pair (printStr s) (printStr s)
  printStrLn s = pair (printStrLn s) (printStrLn s)

  printFile f v = pair (printFile (pfst f) (pfst v)) (printFile (psnd f) 
    (psnd v))
  printFileLn f v = pair (printFileLn (pfst f) (pfst v)) (printFileLn (psnd f) 
    (psnd v))
  printFileStr f s = pair (printFileStr (pfst f) s) (printFileStr (psnd f) s)
  printFileStrLn f s = pair (printFileStrLn (pfst f) s) (printFileStrLn (psnd f)
    s)

  getInput v = pair (getInput $ pfst v) (getInput $ psnd v)
  discardInput = pair discardInput discardInput
  getFileInput f v = pair (getFileInput (pfst f) (pfst v)) 
    (getFileInput (psnd f) (psnd v))
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

  stringListVals vals sl = pair (stringListVals (map pfst vals) (pfst sl))
    (stringListVals (map psnd vals) (psnd sl))
  stringListLists lsts sl = pair (stringListLists (map pfst lsts) (pfst sl))
    (stringListLists (map psnd lsts) (psnd sl))

  break = pair break break
  continue = pair continue continue

  returnState v = pair (returnState $ pfst v) (returnState $ psnd v)
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
  addObserver o = pair (addObserver $ pfst o) (addObserver $ psnd o)

  inOutCall n ins outs both = pair (inOutCall n (map pfst ins) (map pfst outs) 
    (map pfst both)) (inOutCall n (map psnd ins) (map psnd outs) (map psnd both))
  extInOutCall m n ins outs both = pair (extInOutCall m n (map pfst ins) (map 
    pfst outs) (map pfst both)) (extInOutCall m n (map psnd ins) (map psnd outs)
    (map psnd both)) 

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
  forEach l v b = pair (forEach l (pfst v) (pfst b)) (forEach l (psnd v) 
    (psnd b))
  while v b = pair (while (pfst v) (pfst b)) (while (psnd v) (psnd b))

  tryCatch tb cb = pair (tryCatch (pfst tb) (pfst cb)) (tryCatch (psnd tb) 
    (psnd cb))

  checkState l vs b = pair (checkState l (map 
    (mapPairFst pfst . mapPairSnd pfst) vs) (pfst b)) 
    (checkState l (map (mapPairFst psnd . mapPairSnd psnd) vs) (psnd b))

  notifyObservers f t = pair (notifyObservers (pfst f) (pfst t)) 
    (notifyObservers (psnd f) (psnd t))

  getFileInputAll f v = pair (getFileInputAll (pfst f) (pfst v)) 
    (getFileInputAll (psnd f) (psnd v))

instance (Pair p) => ScopeSym (p CppSrcCode CppHdrCode) where
  type Scope (p CppSrcCode CppHdrCode) = (Doc, ScopeTag)
  private = pair private private
  public = pair public public

instance (Pair p) => InternalScope (p CppSrcCode CppHdrCode) where
  includeScope s = pair (includeScope $ pfst s) (includeScope $ psnd s)

instance (Pair p) => MethodTypeSym (p CppSrcCode CppHdrCode) where
  type MethodType (p CppSrcCode CppHdrCode) = TypeData
  mState t = pair (mState $ pfst t) (mState $ psnd t)
  construct n = pair (construct n) (construct n)

instance (Pair p) => ParameterSym (p CppSrcCode CppHdrCode) where
  type Parameter (p CppSrcCode CppHdrCode) = ParamData
  stateParam v = pair (stateParam $ pfst v) (stateParam $ psnd v)
  pointerParam v = pair (pointerParam $ pfst v) (pointerParam $ psnd v)

  parameterName p = parameterName $ pfst p
  parameterType p = pair (parameterType $ pfst p) (parameterType $ psnd p)

instance (Pair p) => MethodSym (p CppSrcCode CppHdrCode) where
  type Method (p CppSrcCode CppHdrCode) = MethodData
  method n c s p t ps b = pair (method n c (pfst s) (pfst p) (pfst t) (map pfst
    ps) (pfst b)) (method n c (psnd s) (psnd p) (psnd t) (map psnd ps) (psnd b))
  getMethod c v = pair (getMethod c $ pfst v) (getMethod c $ psnd v) 
  setMethod c v = pair (setMethod c $ pfst v) (setMethod c $ psnd v)
  mainMethod l b = pair (mainMethod l $ pfst b) (mainMethod l $ psnd b)
  privMethod n c t ps b = pair (privMethod n c (pfst t) (map pfst ps) (pfst b))
    (privMethod n c (psnd t) (map psnd ps) (psnd b))
  pubMethod n c t ps b = pair (pubMethod n c (pfst t) (map pfst ps) (pfst b)) 
    (pubMethod n c (psnd t) (map psnd ps) (psnd b))
  constructor n ps b = pair (constructor n (map pfst ps) (pfst b))
    (constructor n (map psnd ps) (psnd b))
  destructor n vs = pair (destructor n $ map pfst vs) 
    (destructor n $ map psnd vs)

  docMain c b = pair (docMain c $ pfst b) (docMain c $ psnd b)

  function n s p t ps b = pair (function n (pfst s) (pfst p) (pfst t) (map pfst
    ps) (pfst b)) (function n (psnd s) (psnd p) (psnd t) (map psnd ps) (psnd b))

  docFunc desc pComms rComm f = pair (docFunc desc pComms rComm $ pfst f) 
    (docFunc desc pComms rComm $ psnd f)

  inOutFunc n s p ins outs both b = pair (inOutFunc n (pfst s) (pfst p) (map
    pfst ins) (map pfst outs) (map pfst both) (pfst b)) (inOutFunc n (psnd s) 
    (psnd p) (map psnd ins) (map psnd outs) (map psnd both) (psnd b))

  docInOutFunc desc iComms oComms bComms f = pair (docInOutFunc desc iComms 
    oComms bComms $ pfst f) (docInOutFunc desc iComms oComms bComms $ psnd f)

  commentedFunc cmt fn = pair (commentedFunc (pfst cmt) (pfst fn)) 
    (commentedFunc (psnd cmt) (psnd fn)) 

  parameters m = pairList (parameters $ pfst m) (parameters $ psnd m)

instance (Pair p) => StateVarSym (p CppSrcCode CppHdrCode) where
  type StateVar (p CppSrcCode CppHdrCode) = StateVarData
  stateVar del s p v = pair (stateVar del (pfst s) (pfst p) (pfst v))
    (stateVar del (psnd s) (psnd p) (psnd v))
  privMVar del v = pair (privMVar del $ pfst v) (privMVar del $ psnd v)
  pubMVar del v = pair (pubMVar del $ pfst v) (pubMVar del $ psnd v)
  pubGVar del v = pair (pubGVar del $ pfst v) (pubGVar del $ psnd v)

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

  docClass d c = pair (docClass d $ pfst c) (docClass d $ psnd c)

  commentedClass cmt cs = pair (commentedClass (pfst cmt) (pfst cs)) 
    (commentedClass (psnd cmt) (psnd cs))

instance (Pair p) => ModuleSym (p CppSrcCode CppHdrCode) where
  type Module (p CppSrcCode CppHdrCode) = ModData
  buildModule n l ms cs = pair (buildModule n l (map pfst ms) (map pfst cs)) 
    (buildModule n l (map psnd ms) (map psnd cs))

  moduleName m = moduleName $ pfst m

instance (Pair p) => BlockCommentSym (p CppSrcCode CppHdrCode) where
  type BlockComment (p CppSrcCode CppHdrCode) = Doc
  blockComment lns = pair (blockComment lns) (blockComment lns)
  docComment lns = pair (docComment lns) (docComment lns)

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
  type Package CppSrcCode = PackData
  package = lift1List packD

instance ProgramSym CppSrcCode where
  type Program CppSrcCode = ProgData
  prog n = liftList (progD n)
  
instance RenderSym CppSrcCode where
  type RenderFile CppSrcCode = FileData
  fileDoc code = liftA2 srcFile (fmap (addExt cppSrcExt . name) code) (liftA2 
    updateModDoc (liftA2 emptyIfEmpty (fmap modDoc code) $ liftA3 fileDoc' 
    (top code) (fmap modDoc code) bottom) code)

  docMod d a dt m = commentedMod (docComment $ moduleDoc d a dt $ filePath 
    (unCPPSC m)) m

  commentedMod cmt m = if (isMainMod . fileMod . unCPPSC) m then liftA2 
    updateFileMod (liftA2 updateModDoc (liftA2 commentedItem cmt (fmap (modDoc 
    . fileMod) m)) (fmap fileMod m)) m else m 

instance InternalFile CppSrcCode where
  top m = liftA3 cppstop m (list dynamic_) endStatement
  bottom = return empty

instance AuxiliarySym CppSrcCode where
  type Auxiliary CppSrcCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)

  optimizeDox = return $ text "NO"
  
  makefile cms = fmap (ad makefileName . makeBuild cms cppBuildConfig 
    cppRunnable)

instance KeywordSym CppSrcCode where
  type Keyword CppSrcCode = Doc
  endStatement = return semi
  endStatementLoop = return empty

  include n = return $ text "#include" <+> doubleQuotedText (addExt cppHdrExt n)
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
  blockCommentStart = return blockCmtStart
  blockCommentEnd = return blockCmtEnd
  docCommentStart = return docCmtStart
  docCommentEnd = blockCommentEnd

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
  enumType t = return $ enumTypeDocD t
  iterator t = fmap cppIterTypeDoc (listType dynamic_ t)
  void = return voidDocD

  getType = cType . unCPPSC

instance ControlBlockSym CppSrcCode where
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

instance UnaryOpSym CppSrcCode where
  type UnaryOp CppSrcCode = OpData
  notOp = return notOpDocD
  negateOp = return negateOpDocD
  sqrtOp = return sqrtOpDocD
  absOp = return absOpDocD
  logOp = return $ unOpPrec "log10"
  lnOp = return $ unOpPrec "log"
  expOp = return expOpDocD
  sinOp = return sinOpDocD
  cosOp = return cosOpDocD
  tanOp = return tanOpDocD
  asinOp = return asinOpDocD
  acosOp = return acosOpDocD
  atanOp = return atanOpDocD
  floorOp = return $ unOpPrec "floor"
  ceilOp = return $ unOpPrec "ceil"

instance BinaryOpSym CppSrcCode where
  type BinaryOp CppSrcCode = OpData
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

instance VariableSym CppSrcCode where
  type Variable CppSrcCode = VarData
  var n t = liftA2 (vard n) t (return $ varDocD n) 
  const = var
  extVar _ = var
  self l = liftA2 (vard "this") (obj l) (return selfDocD)
  enumVar e en = var e (enumType en)
  objVar o v = liftA2 (vard $ variableName o ++ "." ++ variableName v)
    (variableType v) (liftA2 objVarDocD o v)
  objVarSelf _ = var
  listVar n p t = var n (listType p t)
  n `listOf` t = listVar n static_ t
  iterVar l t = liftA2 (vard l) (iterator t) (return $ text $ "(*" ++ l ++ ")")

  ($->) = objVar

  variableName = varName . unCPPSC
  variableType = fmap varType
  variableDoc = varDoc . unCPPSC

instance ValueSym CppSrcCode where
  type Value CppSrcCode = ValData
  litTrue = liftA2 mkVal bool (return litTrueD)
  litFalse = liftA2 mkVal bool (return litFalseD)
  litChar c = liftA2 mkVal char (return $ litCharD c)
  litFloat v = liftA2 mkVal float (return $ litFloatD v)
  litInt v = liftA2 mkVal int (return $ litIntD v)
  litString s = liftA2 mkVal string (return $ litStringD s)

  ($:) = enumElement

  valueOf v = liftA2 mkVal (variableType v) (return $ variableDoc v) 
  arg n = liftA2 mkVal string (liftA2 argDocD (litInt (n+1)) argsList)
  enumElement en e = liftA2 mkVal (enumType en) (return $ text e)
  
  argsList = liftA2 mkVal (listType static_ string) (return $ text "argv")

  valueType = fmap valType
  valueDoc = valDoc . unCPPSC

instance NumericExpression CppSrcCode where
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
  inlineIf = liftA3 inlineIfD
  funcApp n t vs = liftA2 mkVal t (liftList (funcAppDocD n) vs)
  selfFuncApp = funcApp
  extFuncApp _ = funcApp
  stateObj t vs = liftA2 mkVal t (liftA2 cppStateObjDoc t (liftList valList vs))
  extStateObj _ = stateObj
  listStateObj = stateObj

  exists = notNull
  notNull v = v

instance InternalValue CppSrcCode where
  inputFunc = liftA2 mkVal string (return $ text "std::cin")
  printFunc = liftA2 mkVal void (return $ text "std::cout")
  printLnFunc = liftA2 mkVal void (return $ text "std::cout")
  printFileFunc f = liftA2 mkVal void (fmap valDoc f)
  printFileLnFunc f = liftA2 mkVal void (fmap valDoc f)

  cast = cppCast

instance Selector CppSrcCode where
  objAccess v f = liftA2 mkVal (fmap funcType f) (liftA2 objAccessDocD v f)
  ($.) = objAccess

  objMethodCall t o f ps = objAccess o (func f t ps)
  objMethodCallNoParams t o f = objMethodCall t o f []

  selfAccess l = objAccess (valueOf $ self l)

  listIndexExists v i = listSize v ?> i
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf l v = funcApp "find" int [iterBegin l, iterEnd l, v] #- iterBegin l

instance FunctionSym CppSrcCode where
  type Function CppSrcCode = FuncData
  func l t vs = liftA2 fd t (fmap funcDocD (funcApp l t vs))

  get v vToGet = v $. getFunc vToGet
  set v vToSet toVal = v $. setFunc (valueType v) vToSet toVal

  listSize v = cast int (v $. listSizeFunc)
  listAdd v i vToAdd = v $. listAddFunc v i vToAdd
  listAppend v vToApp = v $. listAppendFunc vToApp

  iterBegin v = v $. iterBeginFunc (listInnerType $ valueType v)
  iterEnd v = v $. iterEndFunc (listInnerType $ valueType v)

instance SelectorFunction CppSrcCode where
  listAccess v i = v $. listAccessFunc (listInnerType $ valueType v) i
  listSet v i toVal = v $. listSetFunc v i toVal
  at v l = listAccess v (valueOf $ var l int)

instance InternalFunction CppSrcCode where
  getFunc v = func (getterName $ variableName v) (variableType v) []
  setFunc t v toVal = func (setterName $ variableName v) t [toVal]

  listSizeFunc = func "size" int []
  listAddFunc l i v = func "insert" (listType static_ $ fmap valType v) 
    [iterBegin l #+ i, v]
  listAppendFunc v = func "push_back" (listType static_ $ fmap valType v) [v]

  iterBeginFunc t = func "begin" (iterator t) []
  iterEndFunc t = func "end" (iterator t) []

  listAccessFunc t v = func "at" t [intValue v]
  listSetFunc v i toVal = liftA2 fd (valueType v) 
    (liftA2 cppListSetDoc (intValue i) toVal)

  atFunc t l = listAccessFunc t (valueOf $ var l int) 

instance InternalStatement CppSrcCode where
  printSt nl p v _ = mkSt <$> liftA2 (cppPrint nl) p v

  state = fmap statementDocD
  loopState = fmap (statementDocD . setEmpty)

instance StatementSym CppSrcCode where
  type Statement CppSrcCode = (Doc, Terminator)
  assign vr vl = mkSt <$> liftA2 assignDocD vr vl
  assignToListIndex lst index v = valState $ listSet (valueOf lst) index v
  multiAssign _ _ = error "No multiple assignment statements in C++"
  (&=) = assign
  (&-=) vr vl = vr &= (valueOf vr #- vl)
  (&+=) vr vl = mkSt <$> liftA2 plusEqualsDocD vr vl
  (&++) v = mkSt <$> fmap plusPlusDocD v
  (&~-) v = v &= (valueOf v #- litInt 1)

  varDec v = mkSt <$> fmap varDecDocD v
  varDecDef v def = mkSt <$> liftA2 varDecDefDocD v def
  listDec n v = mkSt <$> liftA2 cppListDecDoc v (litInt n)
  listDecDef v vs = mkSt <$> liftA2 cppListDecDefDoc v (liftList valList vs)
  objDecDef v def = mkSt <$> liftA2 objDecDefDocD v def
  objDecNew v vs = mkSt <$> liftA2 objDecDefDocD v (stateObj (variableType v) 
    vs)
  extObjDecNew _ = objDecNew
  objDecNewVoid v = mkSt <$> liftA2 objDecDefDocD v (stateObj (variableType v) 
    [])
  extObjDecNewVoid _ = objDecNewVoid
  constDecDef v def = mkSt <$> liftA2 constDecDefDocD v def

  print v = outDoc False printFunc v Nothing
  printLn v = outDoc True printLnFunc v Nothing
  printStr s = outDoc False printFunc (litString s) Nothing
  printStrLn s = outDoc True printLnFunc (litString s) Nothing

  printFile f v = outDoc False (printFileFunc f) v (Just f)
  printFileLn f v = outDoc True (printFileLnFunc f) v (Just f)
  printFileStr f s = outDoc False (printFileFunc f) (litString s) (Just f)
  printFileStrLn f s = outDoc True (printFileLnFunc f) (litString s) (Just f)

  getInput v = mkSt <$> liftA3 cppInput v inputFunc endStatement
  discardInput = mkSt <$> fmap (cppDiscardInput "\\n") inputFunc
  getFileInput f v = mkSt <$> liftA3 cppInput v f endStatement
  discardFileInput f = mkSt <$> fmap (cppDiscardInput " ") f

  openFileR f n = mkSt <$> liftA2 (cppOpenFile "std::fstream::in") f n
  openFileW f n = mkSt <$> liftA2 (cppOpenFile "std::fstream::out") f n
  openFileA f n = mkSt <$> liftA2 (cppOpenFile "std::fstream::app") f n
  closeFile f = valState $ objMethodCall void f "close" []

  getFileInputLine f v = valState $ funcApp "std::getline" string [f, valueOf v]
  discardFileLine f = mkSt <$> fmap (cppDiscardInput "\\n") f
  stringSplit d vnew s = let l_ss = "ss"
                             var_ss = var l_ss (obj "std::stringstream")
                             v_ss = valueOf var_ss
                             l_word = "word"
                             var_word = var l_word string
                             v_word = valueOf var_word
                         in
    multi [
      valState $ valueOf vnew $. func "clear" void [],
      varDec var_ss,
      valState $ objMethodCall string v_ss "str" [s],
      varDec var_word,
      while (funcApp "std::getline" string [v_ss, v_word, litChar d]) 
        (oneLiner $ valState $ listAppend (valueOf vnew) v_word)
    ]

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = return (mkSt breakDocD)
  continue = return (mkSt continueDocD)

  returnState v = mkSt <$> liftList returnDocD [v]
  multiReturn _ = error "Cannot return multiple values in C++"

  valState v = mkSt <$> fmap valDoc v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free v = mkSt <$> fmap freeDocD v

  throw errMsg = mkSt <$> fmap cppThrowDoc (litString errMsg)

  initState fsmName initialState = varDecDef (var fsmName string)
    (litString initialState)
  changeState fsmName toState = var fsmName string &= litString toState

  initObserverList t = listDecDef (var observerListName t)
  addObserver o = valState $ listAdd obsList lastelem o
    where obsList = valueOf $ observerListName `listOf` valueType o
          lastelem = listSize obsList

  inOutCall = cppInOutCall funcApp
  extInOutCall m = cppInOutCall (extFuncApp m)

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
  forRange i initv finalv stepv = for (varDecDef (var i int) initv) 
    (valueOf (var i int) ?< finalv) (var i int &+= stepv)
  forEach l v = for (varDecDef (var l (iterator t)) (iterBegin v)) 
    (valueOf (var l (iterator t)) ?!= iterEnd v) (var l (iterator t) &++)
    where t = listInnerType $ valueType v
  while v b = mkStNoEnd <$> liftA4 whileDocD blockStart blockEnd v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 cppTryCatch tb cb

  checkState l = switchAsIf (valueOf $ var l string) 

  notifyObservers f t = for initv (v_index ?< listSize obsList) 
    (var_index &++) notify
    where obsList = valueOf $ observerListName `listOf` t
          index = "observerIndex"
          var_index = var index int
          v_index = valueOf var_index
          initv = varDecDef var_index $ litInt 0
          notify = oneLiner $ valState $ at obsList index $. f

  getFileInputAll f v = let l_line = "nextLine"
                            var_line = var l_line string
                            v_line = valueOf var_line
                        in
    multi [varDec var_line,
      while (funcApp "std::getline" string [f, v_line])
      (oneLiner $ valState $ listAppend (valueOf v) v_line)]

instance ScopeSym CppSrcCode where
  type Scope CppSrcCode = (Doc, ScopeTag)
  private = return (privateDocD, Priv)
  public = return (publicDocD, Pub)

instance InternalScope CppSrcCode where
  includeScope _ = return (empty, Priv)

instance MethodTypeSym CppSrcCode where
  type MethodType CppSrcCode = TypeData
  mState t = t
  construct n = return $ td (Object n) (constructDocD n)

instance ParameterSym CppSrcCode where
  type Parameter CppSrcCode = ParamData
  stateParam = fmap (mkParam stateParamDocD)
  pointerParam = fmap (mkParam cppPointerParamDoc)

  parameterName = paramName . unCPPSC
  parameterType = fmap paramType

instance MethodSym CppSrcCode where
  type Method CppSrcCode = MethodData
  method n c s _ t ps b = liftA3 (mthd False) (fmap snd s) (checkParams n <$> 
    sequence ps) (liftA5 (cppsMethod n c) t (liftList paramListDocD ps) b 
    blockStart blockEnd)
  getMethod c v = method (getterName $ variableName v) c public dynamic_ 
    (mState $ variableType v) [] getBody
    where getBody = oneLiner $ returnState (valueOf $ self c $-> v)
  setMethod c v = method (setterName $ variableName v) c public dynamic_ 
    (mState void) [stateParam v] setBody
    where setBody = oneLiner $ (self c $-> v) &= valueOf v
  mainMethod _ b = setMainMethod <$> function "main" public static_ int 
    [stateParam $ var "argc" int, 
    liftA2 (pd "argv") (listType static_ string) 
    (return $ text "const char *argv[]")] 
    (liftA2 appendToBody b (returnState $ litInt 0))
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor n vs = 
    let i = var "i" int
        deleteStatements = map (fmap destructSts) vs
        loopIndexDec = varDec i
        dbody = liftA2 emptyIfEmpty 
          (fmap vcat (mapM (fmap fst) deleteStatements)) $
          bodyStatements $ loopIndexDec : deleteStatements
    in pubMethod ('~':n) n void [] dbody

  docMain c b = commentedFunc (docComment $ functionDoc 
    "Controls the flow of the program" 
    [("argc", "Number of command-line arguments"),
    ("argv", "List of command-line arguments")] ["exit code"]) (mainMethod c b)

  function n s _ t ps b = liftA3 (mthd False) (fmap snd s) (checkParams n <$> 
    sequence ps) (liftA5 (cppsFunction n) t (liftList paramListDocD ps) b 
    blockStart blockEnd)

  docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

  inOutFunc n s p ins [v] [] b = function n s p (mState $ variableType v)
    (map (fmap getParam) ins) (liftA3 surroundBody (varDec v) b (returnState $ 
    valueOf v))
  inOutFunc n s p ins [] [v] b = function n s p (mState $ variableType v)
    (map (fmap getParam) $ v : ins) (liftA2 appendToBody b (returnState $ 
    valueOf v))
  inOutFunc n s p ins outs both b = function n s p (mState void) (map
    pointerParam both ++ map (fmap getParam) ins ++ map pointerParam outs) b

  docInOutFunc desc iComms [oComm] [] = docFuncRepr desc iComms [oComm]
  docInOutFunc desc iComms [] [bComm] = docFuncRepr desc (bComm : iComms)
    [bComm]
  docInOutFunc desc iComms oComms bComms = docFuncRepr desc 
    (bComms ++ iComms ++ oComms) []

  commentedFunc cmt fn = if isMainMthd (unCPPSC fn) then 
    liftA4 mthd (fmap isMainMthd fn) (fmap getMthdScp fn) (fmap mthdParams fn)
    (liftA2 commentedItem cmt (fmap mthdDoc fn)) else fn

  parameters m = map return $ (mthdParams . unCPPSC) m

instance StateVarSym CppSrcCode where
  type StateVar CppSrcCode = StateVarData
  stateVar del s p v = liftA3 svd (fmap snd s) (liftA4 stateVarDocD 
    (fst <$> includeScope s) p v endStatement) (if del < alwaysDel then
    return (mkStNoEnd empty) else cppDestruct v)
  privMVar del = stateVar del private dynamic_
  pubMVar del = stateVar del public dynamic_
  pubGVar del = stateVar del public static_

instance ClassSym CppSrcCode where
  -- Bool is True if the class is a main class, False otherwise
  type Class CppSrcCode = (Doc, Bool)
  buildClass n _ _ vs fs = liftPairFst (liftList methodListDocD 
    (map (fmap mthdDoc) (fs ++ [destructor n vs])), 
    any (isMainMthd . unCPPSC) fs)
  enum _ _ _ = return (empty, False)
  mainClass _ vs fs = liftPairFst (liftA2 (cppMainClass (null vs)) (liftList 
    stateVarListDocD (map (fmap stVarDoc) vs)) (liftList methodListDocD (map 
    (fmap mthdDoc) fs)), True)
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public
  
  docClass d = commentedClass (docComment $ classDoc d)

  commentedClass _ cs = cs

instance ModuleSym CppSrcCode where
  type Module CppSrcCode = ModData
  buildModule n l ms cs = fmap (md n (any (snd . unCPPSC) cs || 
    any (isMainMthd . unCPPSC) ms)) (if all (isEmpty . fst . unCPPSC) cs && all 
    (isEmpty . mthdDoc . unCPPSC) ms then return empty else
    liftA5 cppModuleDoc (liftList vcat (map include l)) 
    (if not (null l) && any (not . isEmpty . fst . unCPPSC) cs then
    return blank else return empty) (liftList methodListDocD (map (fmap 
    mthdDoc) ms)) (if (any (not . isEmpty . fst . unCPPSC) cs
    || (all (isEmpty . fst . unCPPSC) cs && not (null l))) && 
    any (not . isEmpty . mthdDoc . unCPPSC) ms then return blank else 
    return empty) (liftList vibcat (map (fmap fst) cs)))
    
  moduleName m = name (unCPPSC m)

instance BlockCommentSym CppSrcCode where
  type BlockComment CppSrcCode = Doc
  blockComment lns = liftA2 (blockCmtDoc lns) blockCommentStart blockCommentEnd
  docComment lns = liftA2 (docCmtDoc lns) docCommentStart docCommentEnd

-----------------
-- Header File --
-----------------

newtype CppHdrCode a = CPPHC {unCPPHC :: a} deriving Eq

instance Functor CppHdrCode where
  fmap f (CPPHC x) = CPPHC (f x)

instance Applicative CppHdrCode where
  pure = CPPHC
  (CPPHC f) <*> (CPPHC x) = CPPHC (f x)

instance Monad CppHdrCode where
  return = CPPHC
  CPPHC x >>= f = f x

instance RenderSym CppHdrCode where
  type RenderFile CppHdrCode = FileData
  fileDoc code = liftA2 hdrFile (fmap (addExt cppHdrExt . name) code) (liftA2 
    updateModDoc (liftA2 emptyIfEmpty (fmap modDoc code) $ liftA3 fileDoc' 
    (top code) (fmap modDoc code) bottom) code)
  
  docMod d a dt m = commentedMod (docComment $ moduleDoc d a dt $ filePath 
    (unCPPHC m)) m

  commentedMod cmt m = if (isMainMod . fileMod . unCPPHC) m then m else liftA2 
    updateFileMod (liftA2 updateModDoc (liftA2 commentedItem cmt (fmap (modDoc 
    . fileMod) m)) (fmap fileMod m)) m

instance InternalFile CppHdrCode where
  top m = liftA3 cpphtop m (list dynamic_) endStatement
  bottom = return $ text "#endif"

instance KeywordSym CppHdrCode where
  type Keyword CppHdrCode = Doc
  endStatement = return semi
  endStatementLoop = return empty

  include n = return $ text "#include" <+> doubleQuotedText (addExt cppHdrExt n)
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
  blockCommentStart = return blockCmtStart
  blockCommentEnd = return blockCmtEnd
  docCommentStart = return docCmtStart
  docCommentEnd = blockCommentEnd

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
  enumType t = return $ enumTypeDocD t
  iterator t = fmap cppIterTypeDoc (listType dynamic_ t)
  void = return voidDocD

  getType = cType . unCPPHC

instance ControlBlockSym CppHdrCode where
  runStrategy _ _ _ _ = return empty

  listSlice _ _ _ _ _ = return empty

instance UnaryOpSym CppHdrCode where
  type UnaryOp CppHdrCode = OpData
  notOp = return $ od 0 empty
  negateOp = return $ od 0 empty
  sqrtOp = return $ od 0 empty
  absOp = return $ od 0 empty
  logOp = return $ od 0 empty
  lnOp = return $ od 0 empty
  expOp = return $ od 0 empty
  sinOp = return $ od 0 empty
  cosOp = return $ od 0 empty
  tanOp = return $ od 0 empty
  asinOp = return $ od 0 empty
  acosOp = return $ od 0 empty
  atanOp = return $ od 0 empty
  floorOp = return $ od 0 empty
  ceilOp = return $ od 0 empty

instance BinaryOpSym CppHdrCode where
  type BinaryOp CppHdrCode = OpData
  equalOp = return $ od 0 empty
  notEqualOp = return $ od 0 empty
  greaterOp = return $ od 0 empty
  greaterEqualOp = return $ od 0 empty
  lessOp = return $ od 0 empty
  lessEqualOp = return $ od 0 empty
  plusOp = return $ od 0 empty
  minusOp = return $ od 0 empty
  multOp = return $ od 0 empty
  divideOp = return $ od 0 empty
  powerOp = return $ od 0 empty
  moduloOp = return $ od 0 empty
  andOp = return $ od 0 empty
  orOp = return $ od 0 empty

instance VariableSym CppHdrCode where
  type Variable CppHdrCode = VarData
  var n t = liftA2 (vard n) t (return $ varDocD n) 
  const _ _ = liftA2 (vard "") void (return empty)
  extVar _ _ _ = liftA2 (vard "") void (return empty)
  self _ = liftA2 (vard "") void (return empty)
  enumVar _ _ = liftA2 (vard "") void (return empty)
  objVar _ _ = liftA2 (vard "") void (return empty)
  objVarSelf _ _ _ = liftA2 (vard "") void (return empty)
  listVar _ _ _ = liftA2 (vard "") void (return empty)
  listOf _ _ = liftA2 (vard "") void (return empty)
  iterVar _ _ = liftA2 (vard "") void (return empty)

  ($->) _ _ = liftA2 (vard "") void (return empty)

  variableName = varName . unCPPHC
  variableType = fmap varType
  variableDoc = varDoc . unCPPHC

instance ValueSym CppHdrCode where
  type Value CppHdrCode = ValData
  litTrue = liftA2 mkVal void (return empty)
  litFalse = liftA2 mkVal void (return empty)
  litChar _ = liftA2 mkVal void (return empty)
  litFloat _ = liftA2 mkVal void (return empty)
  litInt _ = liftA2 mkVal void (return empty)
  litString _ = liftA2 mkVal void (return empty)

  ($:) _ _ = liftA2 mkVal void (return empty)

  valueOf _ = liftA2 mkVal void (return empty)
  arg _ = liftA2 mkVal void (return empty)
  enumElement _ _ = liftA2 mkVal void (return empty)
  
  argsList = liftA2 mkVal void (return empty)

  valueType _ = error "Attempted to extract type from Value for C++ header file"
  valueDoc _ = error "Attempted to extract doc from Value for C++ header file"

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

instance InternalValue CppHdrCode where
  inputFunc = liftA2 mkVal void (return empty)
  printFunc = liftA2 mkVal void (return empty)
  printLnFunc = liftA2 mkVal void (return empty)
  printFileFunc _ = liftA2 mkVal void (return empty)
  printFileLnFunc _ = liftA2 mkVal void (return empty)
  
  cast _ _ = liftA2 mkVal void (return empty)

instance Selector CppHdrCode where
  objAccess _ _ = liftA2 mkVal void (return empty)
  ($.) _ _ = liftA2 mkVal void (return empty)

  objMethodCall _ _ _ _ = liftA2 mkVal void (return empty)
  objMethodCallNoParams _ _ _ = liftA2 mkVal void (return empty)

  selfAccess _ _ = liftA2 mkVal void (return empty)

  listIndexExists _ _ = liftA2 mkVal void (return empty)
  argExists _ = liftA2 mkVal void (return empty)
  
  indexOf _ _ = liftA2 mkVal void (return empty)

instance FunctionSym CppHdrCode where
  type Function CppHdrCode = FuncData
  func _ _ _ = liftA2 fd void (return empty)
  
  get _ _ = liftA2 mkVal void (return empty)
  set _ _ _ = liftA2 mkVal void (return empty)

  listSize _ = liftA2 mkVal void (return empty)
  listAdd _ _ _ = liftA2 mkVal void (return empty)
  listAppend _ _ = liftA2 mkVal void (return empty)

  iterBegin _ = liftA2 mkVal void (return empty)
  iterEnd _ = liftA2 mkVal void (return empty)

instance SelectorFunction CppHdrCode where
  listAccess _ _ = liftA2 mkVal void (return empty)
  listSet _ _ _ = liftA2 mkVal void (return empty)
  at _ _ = liftA2 mkVal void (return empty)

instance InternalFunction CppHdrCode where
  getFunc _ = liftA2 fd void (return empty)
  setFunc _ _ _ = liftA2 fd void (return empty)

  listSizeFunc = liftA2 fd void (return empty)
  listAddFunc _ _ _ = liftA2 fd void (return empty)
  listAppendFunc _ = liftA2 fd void (return empty)

  iterBeginFunc _ = liftA2 fd void (return empty)
  iterEndFunc _ = liftA2 fd void (return empty)

  listAccessFunc _ _ = liftA2 fd void (return empty)
  listSetFunc _ _ _ = liftA2 fd void (return empty)

  atFunc _ _ = liftA2 fd void (return empty)

instance InternalStatement CppHdrCode where
  printSt _ _ _ _ = return (mkStNoEnd empty)

  state _ = return (mkStNoEnd empty)
  loopState _ = return (mkStNoEnd empty)

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

  varDec _ = return (mkStNoEnd empty)
  varDecDef _ _ = return (mkStNoEnd empty)
  listDec _ _ = return (mkStNoEnd empty)
  listDecDef _ _ = return (mkStNoEnd empty)
  objDecDef _ _ = return (mkStNoEnd empty)
  objDecNew _ _ = return (mkStNoEnd empty)
  extObjDecNew _ _ _ = return (mkStNoEnd empty)
  objDecNewVoid _ = return (mkStNoEnd empty)
  extObjDecNewVoid _ _ = return (mkStNoEnd empty)
  constDecDef _ _ = return (mkStNoEnd empty)

  print _ = return (mkStNoEnd empty)
  printLn _ = return (mkStNoEnd empty)
  printStr _ = return (mkStNoEnd empty)
  printStrLn _ = return (mkStNoEnd empty)

  printFile _ _ = return (mkStNoEnd empty)
  printFileLn _ _ = return (mkStNoEnd empty)
  printFileStr _ _ = return (mkStNoEnd empty)
  printFileStrLn _ _ = return (mkStNoEnd empty)

  getInput _ = return (mkStNoEnd empty)
  discardInput = return (mkStNoEnd empty)
  getFileInput _ _ = return (mkStNoEnd empty)
  discardFileInput _ = return (mkStNoEnd empty)

  openFileR _ _ = return (mkStNoEnd empty)
  openFileW _ _ = return (mkStNoEnd empty)
  openFileA _ _ = return (mkStNoEnd empty)
  closeFile _ = return (mkStNoEnd empty)

  getFileInputLine _ _ = return (mkStNoEnd empty)
  discardFileLine _ = return (mkStNoEnd empty)
  stringSplit _ _ _ = return (mkStNoEnd empty)

  stringListVals _ _ = return (mkStNoEnd empty)
  stringListLists _ _ = return (mkStNoEnd empty)

  break = return (mkStNoEnd empty)
  continue = return (mkStNoEnd empty)

  returnState _ = return (mkStNoEnd empty)
  multiReturn _ = return (mkStNoEnd empty)

  valState _ = return (mkStNoEnd empty)

  comment _ = return (mkStNoEnd empty)

  free _ = return (mkStNoEnd empty)

  throw _ = return (mkStNoEnd empty)

  initState _ _ = return (mkStNoEnd empty)
  changeState _ _ = return (mkStNoEnd empty)

  initObserverList _ _ = return (mkStNoEnd empty)
  addObserver _ = return (mkStNoEnd empty)

  inOutCall _ _ _ _ = return (mkStNoEnd empty)
  extInOutCall _ _ _ _ _ = return (mkStNoEnd empty)

  multi _ = return (mkStNoEnd empty)

instance ControlStatementSym CppHdrCode where
  ifCond _ _ = return (mkStNoEnd empty)
  ifNoElse _ = return (mkStNoEnd empty)
  switch _ _ _ = return (mkStNoEnd empty)
  switchAsIf _ _ _ = return (mkStNoEnd empty)

  ifExists _ _ _ = return (mkStNoEnd empty)

  for _ _ _ _ = return (mkStNoEnd empty)
  forRange _ _ _ _ _ = return (mkStNoEnd empty)
  forEach _ _ _ = return (mkStNoEnd empty)
  while _ _ = return (mkStNoEnd empty)

  tryCatch _ _ = return (mkStNoEnd empty)

  checkState _ _ _ = return (mkStNoEnd empty)

  notifyObservers _ _ = return (mkStNoEnd empty)

  getFileInputAll _ _ = return (mkStNoEnd empty)

instance ScopeSym CppHdrCode where
  type Scope CppHdrCode = (Doc, ScopeTag)
  private = return (privateDocD, Priv)
  public = return (publicDocD, Pub)

instance InternalScope CppHdrCode where
  includeScope _ = return (empty, Priv)

instance MethodTypeSym CppHdrCode where
  type MethodType CppHdrCode = TypeData
  mState t = t
  construct n = return $ td (Object n) (constructDocD n)

instance ParameterSym CppHdrCode where
  type Parameter CppHdrCode = ParamData
  stateParam = fmap (mkParam stateParamDocD)
  pointerParam = fmap (mkParam cppPointerParamDoc)

  parameterName = paramName . unCPPHC
  parameterType = fmap paramType

instance MethodSym CppHdrCode where
  type Method CppHdrCode = MethodData
  method n _ s _ t ps _ = liftA3 (mthd False) (fmap snd s) (checkParams n <$>
    sequence ps) (liftA3 (cpphMethod n) t (liftList paramListDocD ps) 
    endStatement)
  getMethod c v = method (getterName $ variableName v) c public dynamic_ 
    (mState $ variableType v) [] (return empty)
  setMethod c v = method (setterName $ variableName v) c public dynamic_ 
    (mState void) [stateParam v] (return empty)
  mainMethod _ _ = return (mthd True Pub [] empty)
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor n _ = pubMethod ('~':n) n void [] (return empty)

  docMain = mainMethod

  function n = method n ""

  docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

  inOutFunc n s p ins [v] [] b = function n s p (mState $ variableType v) 
    (map (fmap getParam) ins) b
  inOutFunc n s p ins [] [v] b = function n s p (mState $ variableType v) 
    (map (fmap getParam) $ v : ins) b
  inOutFunc n s p ins outs both b = function n s p (mState void) (map 
    pointerParam both ++ map (fmap getParam) ins ++ map pointerParam outs) b

  docInOutFunc desc iComms [oComm] [] = docFuncRepr desc iComms [oComm]
  docInOutFunc desc iComms [] [bComm] = docFuncRepr desc (bComm : iComms)
    [bComm]
  docInOutFunc desc iComms oComms bComms = docFuncRepr desc 
    (bComms ++ iComms ++ oComms) []

  commentedFunc cmt fn = if isMainMthd (unCPPHC fn) then fn else 
    liftA4 mthd (fmap isMainMthd fn) (fmap getMthdScp fn) (fmap mthdParams fn)
    (liftA2 commentedItem cmt (fmap mthdDoc fn))
    
  parameters m = map return $ (mthdParams . unCPPHC) m

instance StateVarSym CppHdrCode where
  type StateVar CppHdrCode = StateVarData
  stateVar _ s p v = liftA3 svd (fmap snd s) (liftA4 stateVarDocD 
    (fmap fst (includeScope s)) p v endStatement) (return (mkStNoEnd empty))
  privMVar del = stateVar del private dynamic_
  pubMVar del = stateVar del public dynamic_
  pubGVar del = stateVar del public static_

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
  
  docClass d = commentedClass (docComment $ classDoc d)

  commentedClass cmt cs = if snd (unCPPHC cs) then cs else 
    liftPair (liftA2 commentedItem cmt (fmap fst cs), fmap snd cs)

instance ModuleSym CppHdrCode where
  type Module CppHdrCode = ModData
  buildModule n l ms cs = fmap (md n (any (snd . unCPPHC) cs || 
    any (isMainMthd . unCPPHC) ms)) (if all (isEmpty . fst . unCPPHC) cs && all 
    (isEmpty . mthdDoc . unCPPHC) ms then return empty else liftA5 cppModuleDoc
    (liftList vcat (map include l)) (if not (null l) && any 
    (not . isEmpty . fst . unCPPHC) cs then return blank else return empty) 
    (liftList methodListDocD methods) (if (any (not . isEmpty . fst . unCPPHC) 
    cs || (all (isEmpty . fst . unCPPHC) cs && not (null l))) && any 
    (not . isEmpty . mthdDoc . unCPPHC) ms then return blank else return empty)
    (liftList vibcat (map (fmap fst) cs)))
    where methods = map (fmap mthdDoc) ms
      
  moduleName m = name (unCPPHC m)

instance BlockCommentSym CppHdrCode where
  type BlockComment CppHdrCode = Doc
  blockComment lns = liftA2 (blockCmtDoc lns) blockCommentStart blockCommentEnd
  docComment lns = liftA2 (docCmtDoc lns) docCommentStart docCommentEnd

-- helpers
isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

getParam :: VarData -> ParamData
getParam v = mkParam (getParamFunc ((cType . varType) v)) v
  where getParamFunc (List _) = cppPointerParamDoc
        getParamFunc (Object _) = cppPointerParamDoc
        getParamFunc _ = stateParamDocD
 
data MethodData = MthD {isMainMthd :: Bool, getMthdScp :: ScopeTag, 
  mthdParams :: [ParamData], mthdDoc :: Doc}

mthd :: Bool -> ScopeTag -> [ParamData] -> Doc -> MethodData
mthd = MthD 

setMainMethod :: MethodData -> MethodData
setMainMethod (MthD _ s ps d) = MthD True s ps d

-- convenience
enumsEqualInts :: Bool
enumsEqualInts = False

cppstop :: ModData -> Doc -> Doc -> Doc
cppstop (MD n b _) lst end = vcat [
  if b then empty else inc <+> doubleQuotedText (addExt cppHdrExt n),
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

cppCast :: CppSrcCode (StateType CppSrcCode) -> 
  CppSrcCode (Value CppSrcCode) -> CppSrcCode (Value CppSrcCode)
cppCast t v = cppCast' (getType t) (getType $ valueType v)
  where cppCast' Float String = funcApp "std::stod" float [v]
        cppCast' _ _ = liftA2 mkVal t $ liftA2 castObjDocD (fmap castDocD t) v

cppListSetDoc :: ValData -> ValData -> Doc
cppListSetDoc i v = dot <> text "at" <> parens (valDoc i) <+> equals <+> valDoc v

cppListDecDoc :: VarData -> ValData -> Doc
cppListDecDoc v n = typeDoc (varType v) <+> varDoc v <> parens (valDoc n)

cppListDecDefDoc :: VarData -> Doc -> Doc
cppListDecDefDoc v vs = typeDoc (varType v) <+> varDoc v <> braces vs

cppPrint :: Bool -> ValData -> ValData -> Doc
cppPrint newLn printFn v = valDoc printFn <+> text "<<" <+> val (valDoc v) <+> 
  end
  where val = if maybe False (< 9) (valPrec v) then parens else id
        end = if newLn then text "<<" <+> text "std::endl" else empty

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

cppInput :: VarData -> ValData -> Doc -> Doc
cppInput v inFn end = vcat [
  valDoc inFn <+> text ">>" <+> varDoc v <> end,
  valDoc inFn <> dot <> 
    text "ignore(std::numeric_limits<std::streamsize>::max(), '\\n')"]

cppOpenFile :: Label -> VarData -> ValData -> Doc
cppOpenFile mode f n = varDoc f <> dot <> text "open" <> 
  parens (valDoc n <> comma <+> text mode)

cppPointerParamDoc :: VarData -> Doc
cppPointerParamDoc v = typeDoc (varType v) <+> text "&" <> varDoc v

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

cppDestruct :: CppSrcCode (Variable CppSrcCode) -> 
  CppSrcCode (Statement CppSrcCode)
cppDestruct v = cppDestruct' (getType $ variableType v)
  where cppDestruct' (List _) = deleteLoop
        cppDestruct' _ = free v
        i = "i"
        var_i = var i int
        v_i = valueOf var_i
        guard = v_i ?< listSize (valueOf v)
        listelem = at (valueOf v) i
        loopBody = oneLiner $ free (liftA2 (vard "") (valueType listelem) 
          (return $ valueDoc listelem))
        initv = var_i &= litInt 0
        deleteLoop = for initv guard (var_i &++) loopBody

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

cppInOutCall :: (Label -> CppSrcCode (StateType CppSrcCode) -> 
  [CppSrcCode (Value CppSrcCode)] -> CppSrcCode (Value CppSrcCode)) -> Label -> 
  [CppSrcCode (Value CppSrcCode)] -> [CppSrcCode (Variable CppSrcCode)] -> 
  [CppSrcCode (Variable CppSrcCode)] -> CppSrcCode (Statement CppSrcCode)
cppInOutCall f n ins [out] [] = assign out $ f n (variableType out) ins
cppInOutCall f n ins [] [out] = assign out $ f n (variableType out) (valueOf 
  out : ins)
cppInOutCall f n ins outs both = valState $ f n void (map valueOf both ++ ins 
  ++ map valueOf outs)

cppBuildConfig :: Maybe BuildConfig
cppBuildConfig = buildAll $ \i o -> cppCompiler : i ++ map asFragment
  ["--std=c++11", "-o"] ++ [o]

cppRunnable :: Runnable
cppRunnable = nativeBinary