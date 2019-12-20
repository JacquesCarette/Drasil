{-# LANGUAGE TypeFamilies, Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ code is contained in this module
module GOOL.Drasil.LanguageRenderer.CppRenderer (
  -- * C++ Code Configuration -- defines syntax of all C++ code
  CppSrcCode(..), CppHdrCode(..), CppCode(..), unCPPC
) where

import Utils.Drasil (blank, indent, indentList)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym, FileSym(..),
  InternalFile(..), KeywordSym(..), PermanenceSym(..), InternalPerm(..), 
  BodySym(..), BlockSym(..), InternalBlock(..), ControlBlockSym(..), 
  TypeSym(..), InternalType(..), UnaryOpSym(..), BinaryOpSym(..), 
  InternalOp(..), VariableSym(..), InternalVariable(..), ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  InternalValue(..), Selector(..), InternalSelector(..), objMethodCall, 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), MethodTypeSym(..), ParameterSym(..), 
  InternalParam(..), MethodSym(..), InternalMethod(..), StateVarSym(..), 
  InternalStateVar(..), ClassSym(..), InternalClass(..), ModuleSym(..), 
  InternalMod(..), BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (addExt, enumElementsDocD, multiStateDocD, 
  bodyDocD, outDoc, paramDocD, stateVarDocD, constVarDocD, freeDocD, mkSt, 
  mkStNoEnd, breakDocD, continueDocD, unOpPrec, notOpDocD, negateOpDocD, 
  sqrtOpDocD, absOpDocD, expOpDocD, sinOpDocD, cosOpDocD, tanOpDocD, asinOpDocD,
  acosOpDocD, atanOpDocD, unExpr, unExpr', typeUnExpr, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', 
  typeBinExpr, mkStateVal, mkVal, mkStateVar, mkVar, classVarCheckStatic,
  newObjDocD', castDocD, castObjDocD,  staticDocD, dynamicDocD, privateDocD, 
  publicDocD, classDec, dot, blockCmtStart, blockCmtEnd, docCmtStart, 
  doubleSlash, elseIfLabel, blockCmtDoc, docCmtDoc, commentedItem, 
  addCommentsDocD, functionDox, commentedModD, valueList, parameterList, 
  appendToBody, surroundBody, getterName, setterName, filterOutObjs)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  oneLiner, block, int, double, char, string, listType, listInnerType, obj, 
  enumType, void, runStrategy, listSlice, var, staticVar, self, enumVar, objVar,
  listVar, listOf, litTrue, litFalse, litChar, litFloat, litInt, litString, 
  valueOf, arg, argsList, inlineIf, objAccess, objMethodCall, 
  objMethodCallNoParams, selfAccess, listIndexExists, funcApp, newObj, func, 
  get, set, listSize, listAdd, listAppend, iterBegin, iterEnd, listAccess, 
  listSet, getFunc, setFunc, listSizeFunc, listAppendFunc, listAccessFunc', 
  listSetFunc, state, loopState, emptyState, assign, assignToListIndex, 
  multiAssignError, decrement, increment, decrement1, increment1, varDec, 
  varDecDef, listDec, listDecDef, objDecNew, objDecNewNoParams, constDecDef, 
  discardInput, discardFileInput, closeFile, stringListVals, stringListLists, 
  returnState, multiReturnError, valState, comment, throw, initState, 
  changeState, initObserverList, addObserver, ifCond, ifNoElse, switch, 
  switchAsIf, for, forRange, while, tryCatch, notifyObservers, construct, param,
  method, getMethod, setMethod, privMethod, pubMethod, constructor, function, 
  docFunc, docInOutFunc, intFunc, privMVar, pubMVar, pubGVar, privClass, 
  pubClass, docClass, commentedClass, buildModule, modFromData, fileDoc, docMod,
  fileFromData)
import GOOL.Drasil.Data (Pair(..), Terminator(..), ScopeTag(..), 
  Binding(..), BindData(..), bd, FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateModDoc, OpData(..), od, 
  ParamData(..), pd, ProgData(..), progD, emptyProg, StateVarData(..), svd, 
  TypeData(..), td, ValData(..), vd, VarData(..), vard)
import GOOL.Drasil.Helpers (angles, doubleQuotedText, emptyIfEmpty, 
  toCode, toState, onCodeValue, onStateValue, on2CodeValues, 
  on2StateValues, on3CodeValues, on3StateValues, on4CodeValues, onCodeList, 
  onStateList, on2StateLists, on1CodeValue1List, on1StateValue1List)
import GOOL.Drasil.State (GS, MS, FS, lensGStoFS, lensFStoGS, lensFStoMS, 
  lensMStoGS, getPutReturn, addLangImport, addModuleImport, setCurrMain,
  getCurrMain, setScope, getScope, setCurrMainFunc, getCurrMainFunc)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,pi,const,log,exp,mod)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (State, modify)
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

instance (Pair p) => ProgramSym (p CppSrcCode CppHdrCode) where
  type Program (p CppSrcCode CppHdrCode) = ProgData
  prog n mods = do
    m <-  mapM (zoom lensGStoFS) mods
    let fm = map pfst m
        sm = map (hdrToSrc . psnd) m
    p1 <- prog n $ map toState sm ++ map toState fm
    toState $ pair p1 (toCode emptyProg)

instance (Pair p) => RenderSym (p CppSrcCode CppHdrCode)

instance (Pair p) => FileSym (p CppSrcCode CppHdrCode) where
  type RenderFile (p CppSrcCode CppHdrCode) = FileData
  fileDoc = pair1 fileDoc fileDoc

  docMod d a dt = pair1 (docMod d a dt) (docMod d a dt)

  commentedMod = pair2 commentedMod commentedMod

instance (Pair p) => InternalFile (p CppSrcCode CppHdrCode) where
  top m = pair (top $ pfst m) (top $ psnd m)
  bottom = pair bottom bottom
  
  fileFromData ft fp = pair1 (fileFromData ft fp) (fileFromData ft fp)

instance (Pair p) => KeywordSym (p CppSrcCode CppHdrCode) where
  type Keyword (p CppSrcCode CppHdrCode) = Doc
  endStatement = pair endStatement endStatement
  endStatementLoop = pair endStatementLoop endStatementLoop

  include n = pair (include n) (include n)
  inherit n = pair (inherit n) (inherit n)

  list p = pair (list $ pfst p) (list $ psnd p)

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

  keyDoc k = keyDoc $ pfst k

instance (Pair p) => PermanenceSym (p CppSrcCode CppHdrCode) where
  type Permanence (p CppSrcCode CppHdrCode) = BindData
  static_ = pair static_ static_
  dynamic_ = pair dynamic_ dynamic_

instance (Pair p) => InternalPerm (p CppSrcCode CppHdrCode) where
  permDoc p = permDoc $ pfst p
  binding p = binding $ pfst p

instance (Pair p) => BodySym (p CppSrcCode CppHdrCode) where
  type Body (p CppSrcCode CppHdrCode) = Doc
  body = pair1List body body
  bodyStatements = pair1List bodyStatements bodyStatements
  oneLiner = pair1 oneLiner oneLiner

  addComments s = pair1 (addComments s) (addComments s)

  bodyDoc b = bodyDoc $ pfst b

instance (Pair p) => BlockSym (p CppSrcCode CppHdrCode) where
  type Block (p CppSrcCode CppHdrCode) = Doc
  block = pair1List block block

instance (Pair p) => InternalBlock (p CppSrcCode CppHdrCode) where
  blockDoc b = blockDoc $ pfst b
  docBlock d = on2StateValues pair (docBlock d) (docBlock d)

instance (Pair p) => TypeSym (p CppSrcCode CppHdrCode) where
  type Type (p CppSrcCode CppHdrCode) = TypeData
  bool = on2StateValues pair bool bool
  int = on2StateValues pair int int
  float = on2StateValues pair float float
  char = on2StateValues pair char char
  string = on2StateValues pair string string
  infile = on2StateValues pair infile infile
  outfile = on2StateValues pair outfile outfile
  listType p = pair1 (listType (pfst p)) (listType (psnd p))
  listInnerType = pair1 listInnerType listInnerType
  obj t = on2StateValues pair (obj t) (obj t)
  enumType t = on2StateValues pair (enumType t) (enumType t)
  iterator = pair1 iterator iterator
  void = on2StateValues pair void void

  getType s = getType $ pfst s
  getTypeString s = getTypeString $ pfst s
  getTypeDoc s = getTypeDoc $ pfst s
  
instance (Pair p) => InternalType (p CppSrcCode CppHdrCode) where
  typeFromData t s d = pair (typeFromData t s d) (typeFromData t s d)

instance (Pair p) => ControlBlockSym (p CppSrcCode CppHdrCode) where
  -- How I handle values with both State and Maybe might cause problems later on, 
  -- because it will make the state transitions run twice for the value in the 
  -- Maybe. For now, given what we store in the State for Values/Variables, this 
  -- doesn't matter. If problems occur in the future, an alternative way to do 
  -- this (which wouldn't duplicate state transitions) would be to unwrap the 
  -- maybes, pass them to a function like pair2, and then have the anonymous 
  -- functions rewrap the values in Maybes. This would be messy so I don't want to 
  -- do it unless there's a need.
  runStrategy l strats rv av = pair1List
    (\s -> runStrategy l (zip (map fst strats) s) (fmap (onStateValue pfst) rv)
      (fmap (onStateValue pfst) av)) 
    (\s -> runStrategy l (zip (map fst strats) s) (fmap (onStateValue psnd) rv) 
      (fmap (onStateValue psnd) av)) (map snd strats)

  listSlice' b e s = pair2 
    (listSlice' (fmap (onStateValue pfst) b) (fmap (onStateValue pfst) e) 
      (fmap (onStateValue pfst) s)) 
    (listSlice' (fmap (onStateValue psnd) b) (fmap (onStateValue psnd) e) 
      (fmap (onStateValue psnd) s))

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

instance (Pair p) => InternalOp (p CppSrcCode CppHdrCode) where
  uOpDoc o = uOpDoc $ pfst o
  bOpDoc o = bOpDoc $ pfst o
  uOpPrec o = uOpPrec $ pfst o
  bOpPrec o = bOpPrec $ pfst o

instance (Pair p) => VariableSym (p CppSrcCode CppHdrCode) where
  type Variable (p CppSrcCode CppHdrCode) = VarData
  var n = pair1 (var n) (var n)
  staticVar n = pair1 (staticVar n) (staticVar n)
  const n = pair1 (const n) (const n)
  extVar l n = pair1 (extVar l n) (extVar l n)
  self l = on2StateValues pair (self l) (self l)
  enumVar e en = on2StateValues pair (enumVar e en) (enumVar e en)
  classVar = pair2 classVar classVar
  extClassVar = pair2 extClassVar extClassVar
  objVar = pair2 objVar objVar
  objVarSelf l = pair1 (objVarSelf l) (objVarSelf l)
  listVar n p = pair1 (listVar n (pfst p)) (listVar n (psnd p))
  listOf n = pair1 (n `listOf`) (n `listOf`)
  iterVar l = pair1 (iterVar l) (iterVar l)
  
  ($->) = pair2 ($->) ($->)

  variableBind v = variableBind $ pfst v
  variableName v = variableName $ pfst v
  variableType v = pair (variableType $ pfst v) (variableType $ psnd v)
  variableDoc v = variableDoc $ pfst v

instance (Pair p) => InternalVariable (p CppSrcCode CppHdrCode) where
  varFromData b n t d = pair (varFromData b n (pfst t) d) 
    (varFromData b n (psnd t) d)

instance (Pair p) => ValueSym (p CppSrcCode CppHdrCode) where
  type Value (p CppSrcCode CppHdrCode) = ValData
  litTrue = on2StateValues pair litTrue litTrue
  litFalse = on2StateValues pair litFalse litFalse
  litChar c = on2StateValues pair (litChar c) (litChar c)
  litFloat v = on2StateValues pair (litFloat v) (litFloat v)
  litInt v =on2StateValues  pair (litInt v) (litInt v)
  litString s = on2StateValues pair (litString s) (litString s)

  pi = on2StateValues pair pi pi

  ($:) l1 l2 = on2StateValues pair (($:) l1 l2) (($:) l1 l2)

  valueOf = pair1 valueOf valueOf
  arg n = on2StateValues pair (arg n) (arg n)
  enumElement en e = on2StateValues pair (enumElement en e) (enumElement en e)
  
  argsList = on2StateValues pair argsList argsList

  valueType v = pair (valueType $ pfst v) (valueType $ psnd v)
  valueDoc v = valueDoc $ pfst v

instance (Pair p) => NumericExpression (p CppSrcCode CppHdrCode) where
  (#~) = pair1 (#~) (#~)
  (#/^) = pair1 (#/^) (#/^)
  (#|) = pair1 (#|) (#|)
  (#+) = pair2 (#+) (#+)
  (#-) = pair2 (#-) (#-)
  (#*) = pair2 (#*) (#*)
  (#/) = pair2 (#/) (#/)
  (#%) = pair2 (#%) (#%)
  (#^) = pair2 (#^) (#^)

  log = pair1 log log
  ln = pair1 ln ln
  exp = pair1 exp exp
  sin = pair1 sin sin
  cos = pair1 cos cos
  tan = pair1 tan tan
  csc = pair1 csc csc
  sec = pair1 sec sec
  cot = pair1 cot cot
  arcsin = pair1 arcsin arcsin
  arccos = pair1 arccos arccos
  arctan = pair1 arctan arctan
  floor = pair1 floor floor
  ceil = pair1 ceil ceil

instance (Pair p) => BooleanExpression (p CppSrcCode CppHdrCode) where
  (?!) = pair1 (?!) (?!)
  (?&&) = pair2 (?&&) (?&&)
  (?||) = pair2 (?||) (?||)

  (?<) = pair2 (?<) (?<)
  (?<=) = pair2 (?<=) (?<=)
  (?>) = pair2 (?>) (?>)
  (?>=) = pair2 (?>=) (?>=)
  (?==) = pair2 (?==) (?==)
  (?!=) = pair2 (?!=) (?!=)
  
instance (Pair p) => ValueExpression (p CppSrcCode CppHdrCode) where
  inlineIf = pair3 inlineIf inlineIf
  funcApp n = pair1Val1List (funcApp n) (funcApp n)
  selfFuncApp c n = pair1Val1List (selfFuncApp c n) (selfFuncApp c n)
  extFuncApp l n = pair1Val1List (extFuncApp l n) (extFuncApp l n)
  newObj = pair1Val1List newObj newObj
  extNewObj l = pair1Val1List (extNewObj l) (extNewObj l)

  exists = pair1 exists exists
  notNull = pair1 notNull notNull
  
instance (Pair p) => InternalValue (p CppSrcCode CppHdrCode) where
  inputFunc = on2StateValues pair inputFunc inputFunc
  printFunc = on2StateValues pair printFunc printFunc
  printLnFunc = on2StateValues pair printLnFunc printLnFunc
  printFileFunc = pair1 printFileFunc printFileFunc
  printFileLnFunc = pair1 printFileLnFunc printFileLnFunc

  cast = pair2 cast cast

  valuePrec v = valuePrec $ pfst v
  valFromData p t d = pair (valFromData p (pfst t) d) (valFromData p (psnd t) d)

instance (Pair p) => Selector (p CppSrcCode CppHdrCode) where
  objAccess = pair2 objAccess objAccess
  ($.) = pair2 ($.) ($.)

  selfAccess l = pair1 (selfAccess l) (selfAccess l)

  listIndexExists = pair2 listIndexExists listIndexExists
  argExists i = on2StateValues pair (argExists i) (argExists i)
  
  indexOf = pair2 indexOf indexOf

instance (Pair p) => InternalSelector (p CppSrcCode CppHdrCode) where
  objMethodCall' f = pair2Vals1List (objMethodCall' f) (objMethodCall' f)
  objMethodCallNoParams' f = pair2 
    (objMethodCallNoParams' f) 
    (objMethodCallNoParams' f)

instance (Pair p) => FunctionSym (p CppSrcCode CppHdrCode) where
  type Function (p CppSrcCode CppHdrCode) = FuncData
  func l = pair1Val1List (func l) (func l)

  get = pair2 get get
  set = pair3 set set

  listSize = pair1 listSize listSize
  listAdd = pair3 listAdd listAdd
  listAppend = pair2 listAppend listAppend

  iterBegin = pair1 iterBegin iterBegin
  iterEnd = pair1 iterEnd iterEnd

instance (Pair p) => SelectorFunction (p CppSrcCode CppHdrCode) where
  listAccess = pair2 listAccess listAccess
  listSet = pair3 listSet listSet
  at = pair2 at at

instance (Pair p) => InternalFunction (p CppSrcCode CppHdrCode) where  
  getFunc = pair1 getFunc getFunc
  setFunc = pair3 setFunc setFunc

  listSizeFunc = on2StateValues pair listSizeFunc listSizeFunc
  listAddFunc = pair3 listAddFunc listAddFunc
  listAppendFunc = pair1 listAppendFunc listAppendFunc

  iterBeginFunc = pair1 iterBeginFunc iterBeginFunc
  iterEndFunc = pair1 iterEndFunc iterEndFunc

  listAccessFunc = pair2 listAccessFunc listAccessFunc
  listSetFunc = pair3 listSetFunc listSetFunc

  functionType f = pair (functionType $ pfst f) (functionType $ psnd f)
  functionDoc f = functionDoc $ pfst f
  
  funcFromData d = pair1 (funcFromData d) (funcFromData d)

instance (Pair p) => InternalStatement (p CppSrcCode CppHdrCode) where
  -- Another Maybe/State combination
  printSt nl f = pair2
    (printSt nl (fmap (onStateValue pfst) f)) 
    (printSt nl (fmap (onStateValue psnd) f))
    
  state = pair1 state state
  loopState = pair1 loopState loopState

  emptyState = on2StateValues pair emptyState emptyState
  statementDoc s = statementDoc $ pfst s
  statementTerm s = statementTerm $ pfst s
  
  stateFromData d t = pair (stateFromData d t) (stateFromData d t)

instance (Pair p) => StatementSym (p CppSrcCode CppHdrCode) where
  type Statement (p CppSrcCode CppHdrCode) = (Doc, Terminator)
  assign = pair2 assign assign
  assignToListIndex = pair3 assignToListIndex assignToListIndex
  multiAssign = pair2Lists multiAssign multiAssign
  (&=) = pair2 (&=) (&=)
  (&-=) = pair2 (&-=) (&-=)
  (&+=) = pair2 (&+=) (&+=)
  (&++) = pair1 (&++) (&++)
  (&~-) = pair1 (&~-) (&~-)

  varDec = pair1 varDec varDec
  varDecDef = pair2 varDecDef varDecDef
  listDec n = pair1 (listDec n) (listDec n)
  listDecDef = pair1Val1List listDecDef listDecDef
  objDecDef = pair2 objDecDef objDecDef
  objDecNew = pair1Val1List objDecNew objDecNew
  extObjDecNew lib = pair1Val1List (extObjDecNew lib) (extObjDecNew lib)
  objDecNewNoParams = pair1 objDecNewNoParams objDecNewNoParams
  extObjDecNewNoParams lib = pair1 
    (extObjDecNewNoParams lib) 
    (extObjDecNewNoParams lib)
  constDecDef = pair2 constDecDef constDecDef

  print = pair1 print print
  printLn = pair1 printLn printLn
  printStr s = on2StateValues pair (printStr s) (printStr s)
  printStrLn s = on2StateValues pair (printStrLn s) (printStrLn s)

  printFile = pair2 printFile printFile 
  printFileLn = pair2 printFileLn printFileLn
  printFileStr f s = pair1 (`printFileStr` s) (`printFileStr` s) f
  printFileStrLn f s = pair1 (`printFileStrLn` s) (`printFileStrLn` s) f

  getInput = pair1 getInput getInput
  discardInput = on2StateValues pair discardInput discardInput
  getFileInput = pair2 getFileInput getFileInput
  discardFileInput = pair1 discardFileInput discardFileInput

  openFileR = pair2 openFileR openFileR
  openFileW = pair2 openFileW openFileW
  openFileA = pair2 openFileA openFileA
  closeFile = pair1 closeFile closeFile

  getFileInputLine = pair2 getFileInputLine getFileInputLine
  discardFileLine = pair1 discardFileLine discardFileLine
  stringSplit d = pair2 (stringSplit d) (stringSplit d)

  stringListVals = pair1List1Val stringListVals stringListVals
  stringListLists = pair1List1Val stringListLists stringListLists

  break = on2StateValues pair break break
  continue = on2StateValues pair continue continue

  returnState = pair1 returnState returnState
  multiReturn = pair1List multiReturn multiReturn

  valState = pair1 valState valState

  comment cmt = on2StateValues pair (comment cmt) (comment cmt)

  free = pair1 free free

  throw errMsg = on2StateValues pair (throw errMsg) (throw errMsg)

  initState fsmName iState = on2StateValues pair (initState fsmName iState) 
    (initState fsmName iState)
  changeState fsmName postState = on2StateValues pair (changeState fsmName 
    postState) (changeState fsmName postState)

  initObserverList = pair1Val1List initObserverList initObserverList
  addObserver = pair1 addObserver addObserver

  inOutCall n = pair3Lists (inOutCall n) (inOutCall n)
  selfInOutCall c n = pair3Lists (selfInOutCall c n) (selfInOutCall c n)
  extInOutCall m n = pair3Lists (extInOutCall m n) (extInOutCall m n) 

  multi = pair1List multi multi

instance (Pair p) => ControlStatementSym (p CppSrcCode CppHdrCode) where
  ifCond bs = pair2Lists1Val
    (\cs bods -> ifCond (zip cs bods)) 
    (\cs bods -> ifCond (zip cs bods)) (map fst bs) (map snd bs)
  ifNoElse bs = pair2Lists
    (\cs bods -> ifNoElse (zip cs bods))
    (\cs bods -> ifNoElse (zip cs bods)) (map fst bs) (map snd bs) 
  switch v cs = pairVal2ListsVal 
    (\s cv cb -> switch s (zip cv cb))
    (\s cv cb -> switch s (zip cv cb))
    v (map fst cs) (map snd cs)
  switchAsIf v cs = pairVal2ListsVal
    (\s cv cb -> switchAsIf s (zip cv cb))
    (\s cv cb -> switchAsIf s (zip cv cb))
    v (map fst cs) (map snd cs)

  ifExists = pair3 ifExists ifExists

  for = pair4 for for
  forRange = pair5 forRange forRange
  forEach = pair3 forEach forEach
  while = pair2 while while

  tryCatch = pair2 tryCatch tryCatch

  checkState l vs = pair2Lists1Val
    (\sts bods -> checkState l (zip sts bods))
    (\sts bods -> checkState l (zip sts bods)) (map fst vs) (map snd vs)

  notifyObservers = pair2 notifyObservers notifyObservers

  getFileInputAll = pair2 getFileInputAll getFileInputAll

instance (Pair p) => ScopeSym (p CppSrcCode CppHdrCode) where
  type Scope (p CppSrcCode CppHdrCode) = (Doc, ScopeTag)
  private = pair private private
  public = pair public public

instance (Pair p) => InternalScope (p CppSrcCode CppHdrCode) where
  scopeDoc s = scopeDoc $ pfst s

instance (Pair p) => MethodTypeSym (p CppSrcCode CppHdrCode) where
  type MethodType (p CppSrcCode CppHdrCode) = TypeData
  mType = pair1 mType mType
  construct n = on2StateValues pair (construct n) (construct n)

instance (Pair p) => ParameterSym (p CppSrcCode CppHdrCode) where
  type Parameter (p CppSrcCode CppHdrCode) = ParamData
  param = pair1 param param . zoom lensMStoGS
  pointerParam = pair1 pointerParam pointerParam . zoom lensMStoGS 

instance (Pair p) => InternalParam (p CppSrcCode CppHdrCode) where
  parameterName p = parameterName $ pfst p
  parameterType p = pair (parameterType $ pfst p) (parameterType $ psnd p)
  parameterDoc p = parameterDoc $ pfst p
  paramFromData v d = pair (paramFromData (pfst v) d) (paramFromData (psnd v) d)

instance (Pair p) => MethodSym (p CppSrcCode CppHdrCode) where
  type Method (p CppSrcCode CppHdrCode) = MethodData
  method n c s p t ps = pairValListVal
    (method n c (pfst s) (pfst p)) (method n c (psnd s) (psnd p))
    (zoom lensMStoGS t) ps . zoom lensMStoGS
  getMethod c = pair1 (getMethod c) (getMethod c) . zoom lensMStoGS
  setMethod c = pair1 (setMethod c) (setMethod c) . zoom lensMStoGS
  privMethod n c t ps = pairValListVal (privMethod n c) (privMethod n c)
    (zoom lensMStoGS t) ps . zoom lensMStoGS
  pubMethod n c t ps = pairValListVal (pubMethod n c) (pubMethod n c)
    (zoom lensMStoGS t) ps . zoom lensMStoGS
  constructor n ps = pair1List1Val (constructor n) (constructor n) ps . 
    zoom lensMStoGS
  destructor n = pair1List (destructor n) (destructor n) . map (zoom lensMStoGS)

  docMain = pair1 docMain docMain . zoom lensMStoGS

  function n s p t ps = pairValListVal 
    (function n (pfst s) (pfst p)) (function n (psnd s) (psnd p))
    (zoom lensMStoGS t) ps . zoom lensMStoGS 
  mainFunction = pair1 mainFunction mainFunction . zoom lensMStoGS

  docFunc desc pComms rComm = pair1 (docFunc desc pComms rComm) 
    (docFunc desc pComms rComm)

  inOutMethod n c s p ins outs both = pair3Lists1Val 
    (inOutMethod n c (pfst s) (pfst p)) (inOutMethod n c (psnd s) (psnd p)) 
    (map (zoom lensMStoGS) ins) (map (zoom lensMStoGS) outs) 
    (map (zoom lensMStoGS) both) . zoom lensMStoGS

  docInOutMethod n c s p desc is os bs = pair3Lists1Val
    (\ins outs both -> docInOutMethod n c (pfst s) (pfst p) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (\ins outs both -> docInOutMethod n c (psnd s) (psnd p) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (map (zoom lensMStoGS . snd) is) (map (zoom lensMStoGS . snd) os) 
    (map (zoom lensMStoGS . snd) bs) . zoom lensMStoGS

  inOutFunc n s p ins outs both = pair3Lists1Val
    (inOutFunc n (pfst s) (pfst p)) (inOutFunc n (psnd s) (psnd p))
    (map (zoom lensMStoGS) ins) (map (zoom lensMStoGS) outs) 
    (map (zoom lensMStoGS) both) . zoom lensMStoGS

  docInOutFunc n s p desc is os bs = pair3Lists1Val 
    (\ins outs both -> docInOutFunc n (pfst s) (pfst p) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (\ins outs both -> docInOutFunc n (psnd s) (psnd p) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (map (zoom lensMStoGS . snd) is) (map (zoom lensMStoGS . snd) os) 
    (map (zoom lensMStoGS . snd) bs) . zoom lensMStoGS
  
instance (Pair p) => InternalMethod (p CppSrcCode CppHdrCode) where
  intMethod m n c s p t ps = pairValListVal
    (intMethod m n c (pfst s) (pfst p)) (intMethod m n c (psnd s) (psnd p))
    (zoom lensMStoGS t) ps . zoom lensMStoGS
  intFunc m n s p t ps = pairValListVal
    (intFunc m n (pfst s) (pfst p)) (intFunc m n (psnd s) (psnd p))
    (zoom lensMStoGS t) ps . zoom lensMStoGS
  commentedFunc = pair2 commentedFunc commentedFunc
    
  methodDoc m = methodDoc $ pfst m
  methodFromData s d = pair (methodFromData s d) (methodFromData s d)

instance (Pair p) => StateVarSym (p CppSrcCode CppHdrCode) where
  type StateVar (p CppSrcCode CppHdrCode) = StateVarData
  stateVar s p = pair1 (stateVar (pfst s) (pfst p)) (stateVar (psnd s) (psnd p))
  stateVarDef n s p = pair2
    (stateVarDef n (pfst s) (pfst p)) 
    (stateVarDef n (psnd s) (psnd p))
  constVar n s = pair2 (constVar n (pfst s)) (constVar n (psnd s))
  privMVar = pair1 privMVar privMVar
  pubMVar = pair1 pubMVar pubMVar
  pubGVar = pair1 pubGVar pubGVar

instance (Pair p) => InternalStateVar (p CppSrcCode CppHdrCode) where
  stateVarDoc v = stateVarDoc $ pfst v
  stateVarFromData d = on2StateValues pair (stateVarFromData d) 
    (stateVarFromData d)

instance (Pair p) => ClassSym (p CppSrcCode CppHdrCode) where
  type Class (p CppSrcCode CppHdrCode) = Doc
  buildClass n p s vs fs = pair2Lists 
    (buildClass n p (pfst s)) 
    (buildClass n p (psnd s)) 
    (map (zoom lensFStoGS) vs) (map (zoom lensFStoMS) fs)
  enum l ls s = on2StateValues pair (enum l ls $ pfst s) (enum l ls $ psnd s)
  privClass n p vs fs = pair2Lists (privClass n p) (privClass n p)
    (map (zoom lensFStoGS) vs) (map (zoom lensFStoMS) fs)
  pubClass n p vs fs = pair2Lists (pubClass n p) (pubClass n p)
    (map (zoom lensFStoGS) vs) (map (zoom lensFStoMS) fs)

  docClass d = pair1 (docClass d) (docClass d)

  commentedClass = pair2 commentedClass commentedClass

instance (Pair p) => InternalClass (p CppSrcCode CppHdrCode) where
  classDoc c = classDoc $ pfst c
  classFromData d = on2StateValues pair (classFromData d) (classFromData d)

instance (Pair p) => ModuleSym (p CppSrcCode CppHdrCode) where
  type Module (p CppSrcCode CppHdrCode) = ModData
  buildModule n l ms = pair2Lists (buildModule n l) (buildModule n l) 
    (map (zoom lensFStoMS) ms)
  
instance (Pair p) => InternalMod (p CppSrcCode CppHdrCode) where
  moduleDoc m = moduleDoc $ pfst m
  modFromData n m d = on2StateValues pair (modFromData n m d) (modFromData n m 
    d)
  updateModuleDoc f m = pair (updateModuleDoc f $ pfst m) (updateModuleDoc f $ 
    psnd m)

instance (Pair p) => BlockCommentSym (p CppSrcCode CppHdrCode) where
  type BlockComment (p CppSrcCode CppHdrCode) = Doc
  blockComment lns = pair (blockComment lns) (blockComment lns)
  docComment lns = on2StateValues pair (docComment lns) (docComment lns)

  blockCommentDoc c = blockCommentDoc $ pfst c

-- Helpers for pair instance

pair1 :: (Pair p) => (State r (CppSrcCode a) -> State s (CppSrcCode b)) -> 
  (State r (CppHdrCode a) -> State s (CppHdrCode b)) -> 
  State s (p CppSrcCode CppHdrCode a) -> State s (p CppSrcCode CppHdrCode b)
pair1 srcf hdrf stv = do
  v <- stv
  let fp = toState $ pfst v
      sp = toState $ psnd v
  p1 <- srcf fp
  p2 <- hdrf sp
  toState $ pair p1 p2

pair2 :: (Pair p) => (State r (CppSrcCode a) -> State s (CppSrcCode b) -> 
  State t (CppSrcCode c)) -> (State r (CppHdrCode a) -> State s (CppHdrCode b) 
  -> State t (CppHdrCode c)) -> State t (p CppSrcCode CppHdrCode a) -> 
  State t (p CppSrcCode CppHdrCode b) -> State t (p CppSrcCode CppHdrCode c)
pair2 srcf hdrf stv1 stv2 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair1 (srcf fv1) (hdrf sv1) stv2

pair3 :: (Pair p) => (State r (CppSrcCode a) -> State s (CppSrcCode b) -> 
  State t (CppSrcCode c) -> State u (CppSrcCode d)) -> (State r (CppHdrCode a) 
  -> State s (CppHdrCode b) -> State t (CppHdrCode c) -> State u (CppHdrCode d))
  -> State u (p CppSrcCode CppHdrCode a) -> State u (p CppSrcCode CppHdrCode b) 
  -> State u (p CppSrcCode CppHdrCode c) -> State u (p CppSrcCode CppHdrCode d)
pair3 srcf hdrf stv1 stv2 stv3 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair2 (srcf fv1) (hdrf sv1) stv2 stv3

pair4 :: (Pair p) => (State r (CppSrcCode a) -> State s (CppSrcCode b) -> 
  State t (CppSrcCode c) -> State u (CppSrcCode d) -> State v (CppSrcCode e)) 
  -> (State r (CppHdrCode a) -> State s (CppHdrCode b) -> State t (CppHdrCode c)
  -> State u (CppHdrCode d) -> State v (CppHdrCode e)) -> 
  State v (p CppSrcCode CppHdrCode a) -> State v (p CppSrcCode CppHdrCode b) -> 
  State v (p CppSrcCode CppHdrCode c) -> State v (p CppSrcCode CppHdrCode d) -> 
  State v (p CppSrcCode CppHdrCode e)
pair4 srcf hdrf stv1 stv2 stv3 stv4 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair3 (srcf fv1) (hdrf sv1) stv2 stv3 stv4

pair5 :: (Pair p) => (State r (CppSrcCode a) -> State s (CppSrcCode b) -> 
  State t (CppSrcCode c) -> State u (CppSrcCode d) -> State v (CppSrcCode e) -> 
  State w (CppSrcCode f)) -> (State r (CppHdrCode a) -> State s (CppHdrCode b) 
  -> State t (CppHdrCode c) -> State u (CppHdrCode d) -> State v (CppHdrCode e) 
  -> State w (CppHdrCode f)) -> State w (p CppSrcCode CppHdrCode a) -> 
  State w (p CppSrcCode CppHdrCode b) -> State w (p CppSrcCode CppHdrCode c) -> 
  State w (p CppSrcCode CppHdrCode d) -> State w (p CppSrcCode CppHdrCode e) ->
  State w (p CppSrcCode CppHdrCode f)
pair5 srcf hdrf stv1 stv2 stv3 stv4 stv5 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair4 (srcf fv1) (hdrf sv1) stv2 stv3 stv4 stv5
  

pair1List :: (Pair p) => ([State r (CppSrcCode a)] -> State s (CppSrcCode b)) 
  -> ([State r (CppHdrCode a)] -> State s (CppHdrCode b)) -> 
  [State s (p CppSrcCode CppHdrCode a)] -> State s (p CppSrcCode CppHdrCode b)
pair1List srcf hdrf stv = do
  v <- sequence stv
  let fl = map (toState . pfst) v
      sl = map (toState . psnd) v
  p1 <- srcf fl
  p2 <- hdrf sl
  toState $ pair p1 p2

pair2Lists :: (Pair p) => ([State r (CppSrcCode a)] -> [State s (CppSrcCode b)] 
  -> State t (CppSrcCode c)) -> ([State r (CppHdrCode a)] -> 
  [State s (CppHdrCode b)] -> State t (CppHdrCode c)) -> 
  [State t (p CppSrcCode CppHdrCode a)] -> [State t (p CppSrcCode CppHdrCode b)]
  -> State t (p CppSrcCode CppHdrCode c)
pair2Lists srcf hdrf stv1 stv2 = do
  v1 <- sequence stv1
  let fl1 = map (toState . pfst) v1
      sl1 = map (toState . psnd) v1
  pair1List (srcf fl1) (hdrf sl1) stv2

pair3Lists :: (Pair p) => ([State r (CppSrcCode a)] -> [State s (CppSrcCode b)] 
  -> [State t (CppSrcCode c)] -> State u (CppSrcCode d)) -> 
  ([State r (CppHdrCode a)] -> [State s (CppHdrCode b)] -> 
  [State t (CppHdrCode c)] -> State u (CppHdrCode d)) -> 
  [State u (p CppSrcCode CppHdrCode a)] -> [State u (p CppSrcCode CppHdrCode b)]
  -> [State u (p CppSrcCode CppHdrCode c)] -> 
  State u (p CppSrcCode CppHdrCode d)
pair3Lists srcf hdrf stv1 stv2 stv3 = do
  v1 <- sequence stv1
  let fl1 = map (toState . pfst) v1
      sl1 = map (toState . psnd) v1
  pair2Lists (srcf fl1) (hdrf sl1) stv2 stv3 

pair1List1Val :: (Pair p) => ([State r (CppSrcCode a)] -> State s (CppSrcCode b)
  -> State t (CppSrcCode c)) -> ([State r (CppHdrCode a)] -> 
  State s (CppHdrCode b) -> State t (CppHdrCode c)) -> 
  [State t (p CppSrcCode CppHdrCode a)] -> State t (p CppSrcCode CppHdrCode b) 
  -> State t (p CppSrcCode CppHdrCode c)
pair1List1Val srcf hdrf stv1 stv2 = do
  v1 <- sequence stv1
  let fl1 = map (toState . pfst) v1
      sl1 = map (toState . psnd) v1
  pair1 (srcf fl1) (hdrf sl1) stv2

pair1Val1List :: (Pair p) => (State r (CppSrcCode a) -> [State s (CppSrcCode b)]
  -> State t (CppSrcCode c)) -> (State r (CppHdrCode a) -> 
  [State s (CppHdrCode b)] -> State t (CppHdrCode c)) -> 
  State t (p CppSrcCode CppHdrCode a) -> [State t (p CppSrcCode CppHdrCode b)] 
  -> State t (p CppSrcCode CppHdrCode c)
pair1Val1List srcf hdrf stv1 stv2 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair1List (srcf fv1) (hdrf sv1) stv2

pair2Vals1List :: (Pair p) => (State r (CppSrcCode a) -> State s (CppSrcCode b) 
  -> [State t (CppSrcCode c)] -> State u (CppSrcCode d)) -> 
  (State r (CppHdrCode a) -> State s (CppHdrCode b) -> [State t (CppHdrCode c)] 
  -> State u (CppHdrCode d)) -> State u (p CppSrcCode CppHdrCode a) -> 
  State u (p CppSrcCode CppHdrCode b) -> [State u (p CppSrcCode CppHdrCode c)] 
  -> State u (p CppSrcCode CppHdrCode d)
pair2Vals1List srcf hdrf stv1 stv2 stv3 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair1Val1List (srcf fv1) (hdrf sv1) stv2 stv3 

pair2Lists1Val :: (Pair p) => ([State r (CppSrcCode a)] -> 
  [State s (CppSrcCode b)] -> State t (CppSrcCode c) -> State u (CppSrcCode d)) 
  -> ([State r (CppHdrCode a)] -> [State s (CppHdrCode b)] -> 
  State t (CppHdrCode c) -> State u (CppHdrCode d)) -> 
  [State u (p CppSrcCode CppHdrCode a)] -> [State u (p CppSrcCode CppHdrCode b)]
  -> State u (p CppSrcCode CppHdrCode c) -> State u (p CppSrcCode CppHdrCode d)
pair2Lists1Val srcf hdrf stv1 stv2 stv3 = do
  v1 <- sequence stv1
  let fl1 = map (toState . pfst) v1
      sl1 = map (toState . psnd) v1
  pair1List1Val (srcf fl1) (hdrf sl1) stv2 stv3 

pairValListVal :: (Pair p) => (State r (CppSrcCode a) -> 
  [State s (CppSrcCode b)] -> State t (CppSrcCode c) -> State u (CppSrcCode d)) 
  -> (State r (CppHdrCode a) -> [State s (CppHdrCode b)] -> 
  State t (CppHdrCode c) -> State u (CppHdrCode d)) -> 
  State u (p CppSrcCode CppHdrCode a) -> [State u (p CppSrcCode CppHdrCode b)] 
  -> State u (p CppSrcCode CppHdrCode c) -> State u (p CppSrcCode CppHdrCode d)
pairValListVal srcf hdrf stv1 stv2 stv3 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair1List1Val (srcf fv1) (hdrf sv1) stv2 stv3 

pair3Lists1Val :: (Pair p) => ([State r (CppSrcCode a)] -> 
  [State s (CppSrcCode b)] -> [State t (CppSrcCode c)] -> State u (CppSrcCode d)
  -> State v (CppSrcCode e)) -> ([State r (CppHdrCode a)] 
  -> [State s (CppHdrCode b)] -> [State t (CppHdrCode c)] -> 
  State u (CppHdrCode d) -> State v (CppHdrCode e)) -> 
  [State v (p CppSrcCode CppHdrCode a)] -> [State v (p CppSrcCode CppHdrCode b)]
  -> [State v (p CppSrcCode CppHdrCode c)] -> 
  State v (p CppSrcCode CppHdrCode d) -> State v (p CppSrcCode CppHdrCode e)
pair3Lists1Val srcf hdrf stv1 stv2 stv3 stv4 = do
  v1 <- sequence stv1
  let fl1 = map (toState . pfst) v1
      sl1 = map (toState . psnd) v1
  pair2Lists1Val (srcf fl1) (hdrf sl1) stv2 stv3 stv4 

pairVal2ListsVal :: (Pair p) => (State r (CppSrcCode a) -> 
  [State s (CppSrcCode b)] -> [State t (CppSrcCode c)] -> State u (CppSrcCode d)
  -> State v (CppSrcCode e)) -> (State r (CppHdrCode a) 
  -> [State s (CppHdrCode b)] -> [State t (CppHdrCode c)] -> 
  State u (CppHdrCode d) -> State v (CppHdrCode e)) -> 
  State v (p CppSrcCode CppHdrCode a) -> [State v (p CppSrcCode CppHdrCode b)] 
  -> [State v (p CppSrcCode CppHdrCode c)] -> 
  State v (p CppSrcCode CppHdrCode d) -> State v (p CppSrcCode CppHdrCode e)
pairVal2ListsVal srcf hdrf stv1 stv2 stv3 stv4 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair2Lists1Val (srcf fv1) (hdrf sv1) stv2 stv3 stv4

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

instance ProgramSym CppSrcCode where
  type Program CppSrcCode = ProgData
  prog n = onStateList (onCodeList (progD n)) . map (zoom lensGStoFS)

instance RenderSym CppSrcCode
  
instance FileSym CppSrcCode where
  type RenderFile CppSrcCode = FileData
  fileDoc = G.fileDoc Source cppSrcExt top bottom

  docMod = G.docMod

  commentedMod cmnt mod = on3StateValues (\m cmt mn -> if mn then on2CodeValues 
    commentedModD m cmt else m) mod cmnt getCurrMain

instance InternalFile CppSrcCode where
  top m = on3CodeValues cppstop m (list dynamic_) endStatement
  bottom = toCode empty
  
  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym CppSrcCode where
  type Keyword CppSrcCode = Doc
  endStatement = toCode semi
  endStatementLoop = toCode empty

  include n = toCode $ text "#include" <+> doubleQuotedText (addExt cppHdrExt n)
  inherit n = onCodeValue (cppInherit n . fst) public

  list _ = toCode $ text "vector"

  blockStart = toCode lbrace
  blockEnd = toCode rbrace

  ifBodyStart = blockStart
  elseIf = toCode elseIfLabel
  
  iterForEachLabel = toCode empty
  iterInLabel = toCode empty

  commentStart = toCode doubleSlash
  blockCommentStart = toCode blockCmtStart
  blockCommentEnd = toCode blockCmtEnd
  docCommentStart = toCode docCmtStart
  docCommentEnd = blockCommentEnd

  keyDoc = unCPPSC

instance PermanenceSym CppSrcCode where
  type Permanence CppSrcCode = BindData
  static_ = toCode $ bd Static staticDocD
  dynamic_ = toCode $ bd Dynamic dynamicDocD
  
instance InternalPerm CppSrcCode where
  permDoc = bindDoc . unCPPSC
  binding = bind . unCPPSC

instance BodySym CppSrcCode where
  type Body CppSrcCode = Doc
  body = onStateList (onCodeList bodyDocD)
  bodyStatements = block
  oneLiner = G.oneLiner

  addComments s = onStateValue (on2CodeValues (addCommentsDocD s) commentStart)

  bodyDoc = unCPPSC

instance BlockSym CppSrcCode where
  type Block CppSrcCode = Doc
  block = G.block endStatement

instance InternalBlock CppSrcCode where
  blockDoc = unCPPSC
  docBlock = onStateValue toCode

instance TypeSym CppSrcCode where
  type Type CppSrcCode = TypeData
  bool = cppBoolType
  int = G.int
  float = G.double
  char = G.char
  string = G.string
  infile = cppInfileType
  outfile = cppOutfileType
  listType = G.listType
  listInnerType = G.listInnerType
  obj = G.obj
  enumType = G.enumType
  iterator = cppIterType . listType dynamic_
  void = G.void

  getType = cType . unCPPSC
  getTypeString = typeString . unCPPSC
  getTypeDoc = typeDoc . unCPPSC
  
instance InternalType CppSrcCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym CppSrcCode where
  runStrategy = G.runStrategy

  listSlice' = G.listSlice

instance UnaryOpSym CppSrcCode where
  type UnaryOp CppSrcCode = OpData
  notOp = toCode notOpDocD
  negateOp = toCode negateOpDocD
  sqrtOp = toCode sqrtOpDocD
  absOp = toCode absOpDocD
  logOp = toCode $ unOpPrec "log10"
  lnOp = toCode $ unOpPrec "log"
  expOp = toCode expOpDocD
  sinOp = toCode sinOpDocD
  cosOp = toCode cosOpDocD
  tanOp = toCode tanOpDocD
  asinOp = toCode asinOpDocD
  acosOp = toCode acosOpDocD
  atanOp = toCode atanOpDocD
  floorOp = toCode $ unOpPrec "floor"
  ceilOp = toCode $ unOpPrec "ceil"

instance BinaryOpSym CppSrcCode where
  type BinaryOp CppSrcCode = OpData
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
  powerOp = toCode powerOpDocD
  moduloOp = toCode moduloOpDocD
  andOp = toCode andOpDocD
  orOp = toCode orOpDocD

instance InternalOp CppSrcCode where
  uOpDoc = opDoc . unCPPSC
  bOpDoc = opDoc . unCPPSC
  uOpPrec = opPrec . unCPPSC
  bOpPrec = opPrec . unCPPSC

instance VariableSym CppSrcCode where
  type Variable CppSrcCode = VarData
  var = G.var
  staticVar = G.staticVar
  const = var
  extVar _ = var
  self = G.self
  enumVar = G.enumVar
  classVar = on2StateValues (\c v -> classVarCheckStatic (varFromData 
    (variableBind v) (getTypeString c ++ "::" ++ variableName v) 
    (variableType v) (cppClassVar (getTypeDoc c) (variableDoc v))))
  extClassVar = classVar
  objVar = G.objVar
  objVarSelf _ = onStateValue (\v -> mkVar ("this->"++variableName v) 
    (variableType v) (text "this->" <> variableDoc v))
  listVar = G.listVar
  listOf = G.listOf
  iterVar l t = mkStateVar l (iterator t) (text $ "(*" ++ l ++ ")")

  ($->) = objVar

  variableBind = varBind . unCPPSC
  variableName = varName . unCPPSC
  variableType = onCodeValue varType
  variableDoc = varDoc . unCPPSC

instance InternalVariable CppSrcCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CppSrcCode where
  type Value CppSrcCode = ValData
  litTrue = G.litTrue
  litFalse = G.litFalse
  litChar = G.litChar
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString

  pi = mkStateVal float (text "M_PI")

  ($:) = enumElement

  valueOf = G.valueOf
  arg n = G.arg (litInt $ n+1) argsList
  enumElement en e = mkStateVal (enumType en) (text e)
  
  argsList = G.argsList "argv"

  valueType = onCodeValue valType
  valueDoc = valDoc . unCPPSC

instance NumericExpression CppSrcCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) = binExpr divideOp
  (#%) = binExpr moduloOp
  (#^) = binExpr' powerOp

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

instance BooleanExpression CppSrcCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool
   
instance ValueExpression CppSrcCode where
  inlineIf = G.inlineIf
  funcApp = G.funcApp
  selfFuncApp c = cppSelfFuncApp (self c)
  extFuncApp _ = funcApp
  newObj = G.newObj newObjDocD'
  extNewObj _ = newObj

  exists = notNull
  notNull v = v

instance InternalValue CppSrcCode where
  inputFunc = mkStateVal string (text "std::cin")
  printFunc = mkStateVal void (text "std::cout")
  printLnFunc = mkStateVal void (text "std::cout")
  printFileFunc = on2StateValues (\v -> mkVal v . valueDoc) void
  printFileLnFunc = on2StateValues (\v -> mkVal v . valueDoc) void

  cast = cppCast

  valuePrec = valPrec . unCPPSC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CppSrcCode where
  objAccess = G.objAccess
  ($.) = G.objAccess
  selfAccess = G.selfAccess

  listIndexExists = G.listIndexExists
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf l v = funcApp "find" int [iterBegin l, iterEnd l, v] #- iterBegin l
  
instance InternalSelector CppSrcCode where
  objMethodCall' = G.objMethodCall
  objMethodCallNoParams' = G.objMethodCallNoParams


instance FunctionSym CppSrcCode where
  type Function CppSrcCode = FuncData
  func = G.func

  get = G.get
  set = G.set

  listSize v = cast int (G.listSize v)
  listAdd = G.listAdd
  listAppend = G.listAppend

  iterBegin = G.iterBegin
  iterEnd = G.iterEnd

instance SelectorFunction CppSrcCode where
  listAccess = G.listAccess
  listSet = G.listSet
  at = listAccess

instance InternalFunction CppSrcCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

  listSizeFunc = G.listSizeFunc
  listAddFunc l i v = func "insert" (listType static_ $ onStateValue valueType  
    v) [iterBegin l #+ i, v]
  listAppendFunc = G.listAppendFunc "push_back"

  iterBeginFunc t = func "begin" (iterator t) []
  iterEndFunc t = func "end" (iterator t) []

  listAccessFunc = G.listAccessFunc' "at"
  listSetFunc = G.listSetFunc cppListSetDoc

  functionType = onCodeValue funcType
  functionDoc = funcDoc . unCPPSC
  
  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance InternalStatement CppSrcCode where
  printSt nl _ = on2StateValues (\pr vl -> mkSt $ cppPrint nl pr vl)

  state = G.state
  loopState = G.loopState

  emptyState = G.emptyState
  statementDoc = fst . unCPPSC
  statementTerm = snd . unCPPSC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CppSrcCode where
  type Statement CppSrcCode = (Doc, Terminator)
  assign = G.assign Semi
  assignToListIndex = G.assignToListIndex
  multiAssign _ _ = error $ G.multiAssignError cppName
  (&=) = assign
  (&-=) = G.decrement
  (&+=) = G.increment
  (&++) = G.increment1
  (&~-) = G.decrement1

  varDec = G.varDec static_ dynamic_
  varDecDef = G.varDecDef 
  listDec n = G.listDec cppListDecDoc (litInt n)
  listDecDef = G.listDecDef cppListDecDefDoc
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew l v vs = modify (addModuleImport l) >> objDecNew v vs
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams l v = modify (addModuleImport l) >> objDecNewNoParams v
  constDecDef = G.constDecDef

  print = outDoc False Nothing printFunc
  printLn = outDoc True Nothing printLnFunc
  printStr = outDoc False Nothing printFunc . litString
  printStrLn = outDoc True Nothing printLnFunc . litString

  printFile f = outDoc False (Just f) (printFileFunc f)
  printFileLn f = outDoc True (Just f) (printFileLnFunc f)
  printFileStr f = outDoc False (Just f) (printFileFunc f) . litString
  printFileStrLn f = outDoc True (Just f) (printFileLnFunc f) . litString

  getInput v = cppInput endStatement v inputFunc
  discardInput = modify (addLangImport "limits") >> G.discardInput 
    (cppDiscardInput "\\n")
  getFileInput f v = cppInput endStatement v f
  discardFileInput f = modify (addLangImport "limits") >> G.discardFileInput 
    (cppDiscardInput " ") f

  openFileR = on2StateValues (\f v -> mkSt $ cppOpenFile "std::fstream::in" f v)
  openFileW = on2StateValues (\f v -> mkSt $ cppOpenFile "std::fstream::out" f v)
  openFileA = on2StateValues (\f v -> mkSt $ cppOpenFile "std::fstream::app" f v)
  closeFile = G.closeFile "close"

  getFileInputLine f v = valState $ funcApp "std::getline" string [f, valueOf v]
  discardFileLine f = modify (addLangImport "limits") >> onStateValue (mkSt .
    cppDiscardInput "\\n") f
  stringSplit d vnew s = let l_ss = "ss"
                             var_ss = var l_ss (obj "std::stringstream")
                             v_ss = valueOf var_ss
                             l_word = "word"
                             var_word = var l_word string
                             v_word = valueOf var_word
                         in
    modify (addLangImport "sstream") >> multi [
      valState $ valueOf vnew $. func "clear" void [],
      varDec var_ss,
      valState $ objMethodCall string v_ss "str" [s],
      varDec var_word,
      while (funcApp "std::getline" string [v_ss, v_word, litChar d]) 
        (oneLiner $ valState $ listAppend (valueOf vnew) v_word)
    ]

  stringListVals = G.stringListVals
  stringListLists = G.stringListLists

  break = toState $ mkSt breakDocD
  continue = toState $ mkSt continueDocD

  returnState = G.returnState Semi
  multiReturn _ = error $ G.multiReturnError cppName

  valState = G.valState Semi

  comment = G.comment commentStart

  free = onStateValue (mkSt . freeDocD)

  throw = G.throw cppThrowDoc Semi

  initState = G.initState
  changeState = G.changeState

  initObserverList = G.initObserverList
  addObserver = G.addObserver

  inOutCall = cppInOutCall funcApp
  selfInOutCall c = cppInOutCall (selfFuncApp c)
  extInOutCall m = cppInOutCall (extFuncApp m)

  multi = onStateList (on1CodeValue1List multiStateDocD endStatement)

instance ControlStatementSym CppSrcCode where
  ifCond = G.ifCond ifBodyStart elseIf blockEnd
  ifNoElse = G.ifNoElse
  switch = G.switch
  switchAsIf = G.switchAsIf

  ifExists _ ifBody _ = onStateValue (mkStNoEnd . bodyDoc) ifBody -- All variables are initialized in C++

  for = G.for blockStart blockEnd 
  forRange = G.forRange
  forEach i v = for (varDecDef e (iterBegin v)) (valueOf e ?!= iterEnd v) 
    (e &++)
    where e = toBasicVar i
  while = G.while blockStart blockEnd

  tryCatch = G.tryCatch cppTryCatch

  checkState l = switchAsIf (valueOf $ var l string) 
  notifyObservers = G.notifyObservers

  getFileInputAll f v = let l_line = "nextLine"
                            var_line = var l_line string
                            v_line = valueOf var_line
                        in
    multi [varDec var_line,
      while (funcApp "std::getline" string [f, v_line])
      (oneLiner $ valState $ listAppend (valueOf v) v_line)]

instance ScopeSym CppSrcCode where
  type Scope CppSrcCode = (Doc, ScopeTag)
  private = toCode (privateDocD, Priv)
  public = toCode (publicDocD, Pub)

instance InternalScope CppSrcCode where
  scopeDoc = fst . unCPPSC

instance MethodTypeSym CppSrcCode where
  type MethodType CppSrcCode = TypeData
  mType t = t
  construct = G.construct

instance ParameterSym CppSrcCode where
  type Parameter CppSrcCode = ParamData
  param = G.param paramDocD
  pointerParam = G.param cppPointerParamDoc

instance InternalParam CppSrcCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameterDoc = paramDoc . unCPPSC
  paramFromData v d = on2CodeValues pd v (toCode d)

instance MethodSym CppSrcCode where
  type Method CppSrcCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor n = G.constructor n n
  destructor n vs = 
    let i = var "i" int
        deleteStatements = map (onStateValue (onCodeValue destructSts)) vs
        loopIndexDec = varDec i
        dbody = on2StateValues (on2CodeValues emptyIfEmpty)
          (onStateList (onCodeList (vcat . map fst)) deleteStatements) $
          bodyStatements $ loopIndexDec : deleteStatements
    in pubMethod ('~':n) n void [] dbody

  docMain b = commentedFunc (docComment $ toState $ functionDox 
    "Controls the flow of the program" 
    [("argc", "Number of command-line arguments"),
    ("argv", "List of command-line arguments")] ["exit code"]) (mainFunction b)

  function = G.function
  mainFunction b = intFunc True "main" public static_ (mType int) 
    [param argc, param argv]
    (on2StateValues (on2CodeValues appendToBody) b (returnState $ litInt 0))
    where argc = var "argc" int
          argv = toState $ mkVar "argv" (typeFromData (List String) 
            "const char" (text "const char")) (text "*argv[]")

  docFunc = G.docFunc

  inOutMethod n c = cppsInOut (method n c)

  docInOutMethod n c = G.docInOutFunc (inOutMethod n c)

  inOutFunc n = cppsInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CppSrcCode where
  intMethod m n c s _ t ps b = modify (setScope (snd $ unCPPSC s) . if m then 
    setCurrMain else id) >> on3StateValues (\tp pms bod -> methodFromData (snd 
    $ unCPPSC s) $ cppsMethod n c tp pms bod blockStart blockEnd) (zoom 
    lensMStoGS t) (sequence ps) (zoom lensMStoGS b)
  intFunc m n s _ t ps b = modify (setScope (snd $ unCPPSC s) . if m then 
    setCurrMainFunc m . setCurrMain else id) >> on3StateValues (\tp pms bod -> 
    methodFromData (snd $ unCPPSC s) $ cppsFunction n tp pms bod blockStart 
    blockEnd) (zoom lensMStoGS t) (sequence ps) (zoom lensMStoGS b)
  commentedFunc = cppCommentedFunc Source
 
  methodDoc = mthdDoc . unCPPSC
  methodFromData s d = toCode $ mthd s d

instance StateVarSym CppSrcCode where
  type StateVar CppSrcCode = StateVarData
  stateVar s _ _ = onStateValue (on3CodeValues svd (onCodeValue snd s) (toCode 
    empty)) emptyState
  stateVarDef n s p v vl = on3StateValues (\vr val -> on3CodeValues svd 
    (onCodeValue snd s) (on4CodeValues (cppsStateVarDef n empty) p vr val 
    endStatement)) v vl emptyState
  constVar n s v vl = on3StateValues (\vr val -> on3CodeValues svd (onCodeValue 
    snd s) (on4CodeValues (cppsStateVarDef n (text "const")) static_ vr val 
    endStatement)) v vl emptyState
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance InternalStateVar CppSrcCode where
  stateVarDoc = stVarDoc . unCPPSC
  stateVarFromData = error "stateVarFromData unimplemented in C++"

instance ClassSym CppSrcCode where
  type Class CppSrcCode = Doc
  buildClass n _ _ vs fs = on2StateLists cppsClass (map (zoom 
    lensFStoGS) vs) (map (zoom lensFStoMS) $ fs ++ [destructor n vs])
  enum _ _ _ = toState $ toCode empty
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass _ cs = cs

instance InternalClass CppSrcCode where
  classDoc = unCPPSC
  classFromData = onStateValue toCode

instance ModuleSym CppSrcCode where
  type Module CppSrcCode = ModData
  buildModule n ls = G.buildModule n (map include ls)

instance InternalMod CppSrcCode where
  moduleDoc = modDoc . unCPPSC
  modFromData n = G.modFromData n (\d m -> toCode $ md n m d)
  updateModuleDoc f = onCodeValue (updateModDoc f)

instance BlockCommentSym CppSrcCode where
  type BlockComment CppSrcCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unCPPSC

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

instance RenderSym CppHdrCode

instance FileSym CppHdrCode where
  type RenderFile CppHdrCode = FileData
  fileDoc = G.fileDoc Header cppHdrExt top bottom
  
  docMod = G.docMod

  commentedMod cmnt mod = on3StateValues (\m cmt mn -> if mn then m else 
    on2CodeValues commentedModD m cmt) mod cmnt getCurrMain

instance InternalFile CppHdrCode where
  top m = on3CodeValues cpphtop m (list dynamic_) endStatement
  bottom = toCode $ text "#endif"
  
  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym CppHdrCode where
  type Keyword CppHdrCode = Doc
  endStatement = toCode semi
  endStatementLoop = toCode empty

  include n = toCode $ text "#include" <+> doubleQuotedText (addExt cppHdrExt n)
  inherit n = onCodeValue (cppInherit n . fst) public

  list _ = toCode $ text "vector"

  blockStart = toCode lbrace
  blockEnd = toCode rbrace

  ifBodyStart = toCode empty
  elseIf = toCode empty
  
  iterForEachLabel = toCode empty
  iterInLabel = toCode empty

  commentStart = toCode empty
  blockCommentStart = toCode blockCmtStart
  blockCommentEnd = toCode blockCmtEnd
  docCommentStart = toCode docCmtStart
  docCommentEnd = blockCommentEnd

  keyDoc = unCPPHC

instance PermanenceSym CppHdrCode where
  type Permanence CppHdrCode = BindData
  static_ = toCode $ bd Static staticDocD
  dynamic_ = toCode $ bd Dynamic dynamicDocD

instance InternalPerm CppHdrCode where
  permDoc = bindDoc . unCPPHC
  binding = bind . unCPPHC

instance BodySym CppHdrCode where
  type Body CppHdrCode = Doc
  body _ = toState $ toCode empty
  bodyStatements _ = toState $ toCode empty
  oneLiner _ = toState $ toCode empty

  addComments _ _ = toState $ toCode empty

  bodyDoc = unCPPHC

instance BlockSym CppHdrCode where
  type Block CppHdrCode = Doc
  block _ = toState $ toCode empty

instance InternalBlock CppHdrCode where
  blockDoc = unCPPHC
  docBlock = onStateValue toCode

instance TypeSym CppHdrCode where
  type Type CppHdrCode = TypeData
  bool = cppBoolType
  int = G.int
  float = G.double
  char = G.char
  string = G.string
  infile = cppInfileType
  outfile = cppOutfileType
  listType = G.listType
  listInnerType = G.listInnerType
  obj = G.obj
  enumType = G.enumType
  iterator = cppIterType . listType dynamic_
  void = G.void

  getType = cType . unCPPHC
  getTypeString = typeString . unCPPHC
  getTypeDoc = typeDoc . unCPPHC
  
instance InternalType CppHdrCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym CppHdrCode where
  runStrategy _ _ _ _ = toState $ toCode empty

  listSlice' _ _ _ _ _ = toState $ toCode empty

instance UnaryOpSym CppHdrCode where
  type UnaryOp CppHdrCode = OpData
  notOp = toCode $ od 0 empty
  negateOp = toCode $ od 0 empty
  sqrtOp = toCode $ od 0 empty
  absOp = toCode $ od 0 empty
  logOp = toCode $ od 0 empty
  lnOp = toCode $ od 0 empty
  expOp = toCode $ od 0 empty
  sinOp = toCode $ od 0 empty
  cosOp = toCode $ od 0 empty
  tanOp = toCode $ od 0 empty
  asinOp = toCode $ od 0 empty
  acosOp = toCode $ od 0 empty
  atanOp = toCode $ od 0 empty
  floorOp = toCode $ od 0 empty
  ceilOp = toCode $ od 0 empty

instance BinaryOpSym CppHdrCode where
  type BinaryOp CppHdrCode = OpData
  equalOp = toCode $ od 0 empty
  notEqualOp = toCode $ od 0 empty
  greaterOp = toCode $ od 0 empty
  greaterEqualOp = toCode $ od 0 empty
  lessOp = toCode $ od 0 empty
  lessEqualOp = toCode $ od 0 empty
  plusOp = toCode $ od 0 empty
  minusOp = toCode $ od 0 empty
  multOp = toCode $ od 0 empty
  divideOp = toCode $ od 0 empty
  powerOp = toCode $ od 0 empty
  moduloOp = toCode $ od 0 empty
  andOp = toCode $ od 0 empty
  orOp = toCode $ od 0 empty

instance InternalOp CppHdrCode where
  uOpDoc = opDoc . unCPPHC
  bOpDoc = opDoc . unCPPHC
  uOpPrec = opPrec . unCPPHC
  bOpPrec = opPrec . unCPPHC

instance VariableSym CppHdrCode where
  type Variable CppHdrCode = VarData
  var = G.var
  staticVar = G.staticVar
  const _ _ = mkStateVar "" void empty
  extVar _ _ _ = mkStateVar "" void empty
  self _ = mkStateVar "" void empty
  enumVar _ _ = mkStateVar "" void empty
  classVar _ _ = mkStateVar "" void empty
  extClassVar _ _ = mkStateVar "" void empty
  objVar _ _ = mkStateVar "" void empty
  objVarSelf _ _ = mkStateVar "" void empty
  listVar _ _ _ = mkStateVar "" void empty
  listOf _ _ = mkStateVar "" void empty
  iterVar _ _ = mkStateVar "" void empty

  ($->) _ _ = mkStateVar "" void empty
  
  variableBind = varBind . unCPPHC
  variableName = varName . unCPPHC
  variableType = onCodeValue varType
  variableDoc = varDoc . unCPPHC

instance InternalVariable CppHdrCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CppHdrCode where
  type Value CppHdrCode = ValData
  litTrue = G.litTrue
  litFalse = G.litFalse
  litChar = G.litChar
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString

  pi = mkStateVal float (text "M_PI")

  ($:) = enumElement

  valueOf = G.valueOf
  arg n = G.arg (litInt $ n+1) argsList
  enumElement en e = mkStateVal (enumType en) (text e)
  
  argsList = G.argsList "argv"

  valueType = onCodeValue valType
  valueDoc = valDoc . unCPPHC

instance NumericExpression CppHdrCode where
  (#~) _ = mkStateVal void empty
  (#/^) _ = mkStateVal void empty
  (#|) _ = mkStateVal void empty
  (#+) _ _ = mkStateVal void empty
  (#-) _ _ = mkStateVal void empty
  (#*) _ _ = mkStateVal void empty
  (#/) _ _ = mkStateVal void empty
  (#%) _ _ = mkStateVal void empty
  (#^) _ _ = mkStateVal void empty

  log _ = mkStateVal void empty
  ln _ = mkStateVal void empty
  exp _ = mkStateVal void empty
  sin _ = mkStateVal void empty
  cos _ = mkStateVal void empty
  tan _ = mkStateVal void empty
  csc _ = mkStateVal void empty
  sec _ = mkStateVal void empty
  cot _ = mkStateVal void empty
  arcsin _ = mkStateVal void empty
  arccos _ = mkStateVal void empty
  arctan _ = mkStateVal void empty
  floor _ = mkStateVal void empty
  ceil _ = mkStateVal void empty

instance BooleanExpression CppHdrCode where
  (?!) _ = mkStateVal void empty
  (?&&) _ _ = mkStateVal void empty
  (?||) _ _ = mkStateVal void empty

  (?<) _ _ = mkStateVal void empty
  (?<=) _ _ = mkStateVal void empty
  (?>) _ _ = mkStateVal void empty
  (?>=) _ _ = mkStateVal void empty
  (?==) _ _ = mkStateVal void empty
  (?!=) _ _ = mkStateVal void empty
   
instance ValueExpression CppHdrCode where
  inlineIf _ _ _ = mkStateVal void empty
  funcApp _ _ _ = mkStateVal void empty
  selfFuncApp _ _ _ _ = mkStateVal void empty
  extFuncApp _ _ _ _ = mkStateVal void empty
  newObj _ _ = mkStateVal void empty
  extNewObj _ _ _ = mkStateVal void empty

  exists _ = mkStateVal void empty
  notNull _ = mkStateVal void empty

instance InternalValue CppHdrCode where
  inputFunc = mkStateVal void empty
  printFunc = mkStateVal void empty
  printLnFunc = mkStateVal void empty
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty
  
  cast _ _ = mkStateVal void empty
  
  valuePrec = valPrec . unCPPHC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CppHdrCode where
  objAccess _ _ = mkStateVal void empty
  ($.) _ _ = mkStateVal void empty

  selfAccess _ _ = mkStateVal void empty

  listIndexExists _ _ = mkStateVal void empty
  argExists _ = mkStateVal void empty
  
  indexOf _ _ = mkStateVal void empty
  
instance InternalSelector CppHdrCode where
  objMethodCall' _ _ _ _ = mkStateVal void empty
  objMethodCallNoParams' _ _ _ = mkStateVal void empty

instance FunctionSym CppHdrCode where
  type Function CppHdrCode = FuncData
  func _ _ _ = funcFromData empty void
  
  get _ _ = mkStateVal void empty
  set _ _ _ = mkStateVal void empty

  listSize _ = mkStateVal void empty
  listAdd _ _ _ = mkStateVal void empty
  listAppend _ _ = mkStateVal void empty

  iterBegin _ = mkStateVal void empty
  iterEnd _ = mkStateVal void empty

instance SelectorFunction CppHdrCode where
  listAccess _ _ = mkStateVal void empty
  listSet _ _ _ = mkStateVal void empty
  at _ _ = mkStateVal void empty

instance InternalFunction CppHdrCode where
  getFunc _ = funcFromData empty void
  setFunc _ _ _ = funcFromData empty void

  listSizeFunc = funcFromData empty void
  listAddFunc _ _ _ = funcFromData empty void
  listAppendFunc _ = funcFromData empty void

  iterBeginFunc _ = funcFromData empty void
  iterEndFunc _ = funcFromData empty void

  listAccessFunc _ _ = funcFromData empty void
  listSetFunc _ _ _ = funcFromData empty void
  
  functionType = onCodeValue funcType
  functionDoc = funcDoc . unCPPHC
  
  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance InternalStatement CppHdrCode where
  printSt _ _ _ _ = emptyState

  state = G.state
  loopState _ = emptyState

  emptyState = G.emptyState
  statementDoc = fst . unCPPHC
  statementTerm = snd . unCPPHC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CppHdrCode where
  type Statement CppHdrCode = (Doc, Terminator)
  assign _ _ = emptyState
  assignToListIndex _ _ _ = emptyState
  multiAssign _ _ = emptyState
  (&=) _ _ = emptyState
  (&-=) _ _ = emptyState
  (&+=) _ _ = emptyState
  (&++) _ = emptyState
  (&~-) _ = emptyState

  varDec = G.varDec static_ dynamic_
  varDecDef = G.varDecDef
  listDec _ _ = emptyState
  listDecDef _ _ = emptyState
  objDecDef _ _ = emptyState
  objDecNew _ _ = emptyState
  extObjDecNew _ _ _ = emptyState
  objDecNewNoParams _ = emptyState
  extObjDecNewNoParams _ _ = emptyState
  constDecDef = G.constDecDef

  print _ = emptyState
  printLn _ = emptyState
  printStr _ = emptyState
  printStrLn _ = emptyState

  printFile _ _ = emptyState
  printFileLn _ _ = emptyState
  printFileStr _ _ = emptyState
  printFileStrLn _ _ = emptyState

  getInput _ = emptyState
  discardInput = emptyState
  getFileInput _ _ = emptyState
  discardFileInput _ = emptyState

  openFileR _ _ = emptyState
  openFileW _ _ = emptyState
  openFileA _ _ = emptyState
  closeFile _ = emptyState

  getFileInputLine _ _ = emptyState
  discardFileLine _ = emptyState
  stringSplit _ _ _ = emptyState

  stringListVals _ _ = emptyState
  stringListLists _ _ = emptyState

  break = emptyState
  continue = emptyState

  returnState _ = emptyState
  multiReturn _ = emptyState

  valState _ = emptyState

  comment _ = emptyState

  free _ = emptyState

  throw _ = emptyState

  initState _ _ = emptyState
  changeState _ _ = emptyState

  initObserverList _ _ = emptyState
  addObserver _ = emptyState

  inOutCall _ _ _ _ = emptyState
  selfInOutCall _ _ _ _ _ = emptyState
  extInOutCall _ _ _ _ _ = emptyState

  multi _ = emptyState

instance ControlStatementSym CppHdrCode where
  ifCond _ _ = emptyState
  ifNoElse _ = emptyState
  switch _ _ _ = emptyState
  switchAsIf _ _ _ = emptyState

  ifExists _ _ _ = emptyState

  for _ _ _ _ = emptyState
  forRange _ _ _ _ _ = emptyState
  forEach _ _ _ = emptyState
  while _ _ = emptyState

  tryCatch _ _ = emptyState

  checkState _ _ _ = emptyState

  notifyObservers _ _ = emptyState

  getFileInputAll _ _ = emptyState

instance ScopeSym CppHdrCode where
  type Scope CppHdrCode = (Doc, ScopeTag)
  private = toCode (privateDocD, Priv)
  public = toCode (publicDocD, Pub)

instance InternalScope CppHdrCode where
  scopeDoc = fst . unCPPHC

instance MethodTypeSym CppHdrCode where
  type MethodType CppHdrCode = TypeData
  mType t = t
  construct = G.construct

instance ParameterSym CppHdrCode where
  type Parameter CppHdrCode = ParamData
  param = onStateValue (\v -> paramFromData v (paramDocD v)) . zoom lensMStoGS
  pointerParam = onStateValue (\v -> paramFromData v (cppPointerParamDoc v)) .
    zoom lensMStoGS

instance InternalParam CppHdrCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameterDoc = paramDoc . unCPPHC
  paramFromData v d = on2CodeValues pd v (toCode d)

instance MethodSym CppHdrCode where
  type Method CppHdrCode = MethodData
  method = G.method
  getMethod c v = zoom lensMStoGS v >>= (\v' -> method (getterName $ 
    variableName v') c public dynamic_ (toState $ variableType v') [] 
    (toState $ toCode empty))
  setMethod c v = zoom lensMStoGS v >>= (\v' -> method (setterName $ 
    variableName v') c public dynamic_ void [param v] (toState $ toCode empty))
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor n = G.constructor n n
  destructor n vars = on1StateValue1List (\m vs -> toCode $ mthd Pub 
    (emptyIfEmpty (vcat (map (statementDoc . onCodeValue destructSts) vs)) 
    (methodDoc m))) (pubMethod ('~':n) n void [] (toState (toCode empty)) :: MS (CppHdrCode (Method CppHdrCode))) (map (zoom lensMStoGS) vars)

  docMain = mainFunction

  function = G.function
  mainFunction _ = getPutReturn (setScope Pub) $ toCode $ mthd Pub empty

  docFunc = G.docFunc

  inOutMethod n c = cpphInOut (method n c)

  docInOutMethod n c = G.docInOutFunc (inOutMethod n c)

  inOutFunc n = cpphInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CppHdrCode where
  intMethod m n _ s _ t ps _ = modify (setScope (snd $ unCPPHC s) . if m then 
    setCurrMain else id) >> on1StateValue1List (\tp pms -> methodFromData (snd 
    $ unCPPHC s) $ cpphMethod n tp pms endStatement) (zoom lensMStoGS t) ps
  intFunc = G.intFunc
  commentedFunc = cppCommentedFunc Header

  methodDoc = mthdDoc . unCPPHC
  methodFromData s d = toCode $ mthd s d

instance StateVarSym CppHdrCode where
  type StateVar CppHdrCode = StateVarData
  stateVar s p v = on2StateValues (\dec -> on3CodeValues svd (onCodeValue snd s)
    (toCode $ stateVarDocD empty (permDoc p) (statementDoc dec)))
    (state $ varDec v) emptyState
  stateVarDef _ s p vr vl = on2StateValues (onCodeValue . svd (snd $ unCPPHC s))
    (cpphStateVarDef empty p vr vl) emptyState
  constVar _ s vr _ = on2StateValues (\v -> on3CodeValues svd (onCodeValue snd 
    s) (on3CodeValues (constVarDocD empty) (bindDoc <$> static_) v 
    endStatement)) vr emptyState
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance InternalStateVar CppHdrCode where
  stateVarDoc = stVarDoc . unCPPHC
  stateVarFromData = error "stateVarFromData unimplemented in C++"

instance ClassSym CppHdrCode where
  type Class CppHdrCode = Doc
  buildClass n p _ vs mths = on2StateLists (\vars funcs -> cpphClass n p vars 
    funcs public private blockStart blockEnd endStatement) 
    (map (zoom lensFStoGS) vs) fs
    where fs = map (zoom lensFStoMS) $ mths ++ [destructor n vs]
  enum n es _ = cpphEnum n (enumElementsDocD es enumsEqualInts) blockStart 
    blockEnd endStatement
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass = G.commentedClass

instance InternalClass CppHdrCode where
  classDoc = unCPPHC
  classFromData = onStateValue toCode

instance ModuleSym CppHdrCode where
  type Module CppHdrCode = ModData
  buildModule n ls = G.buildModule n (map include ls)

instance InternalMod CppHdrCode where
  moduleDoc = modDoc . unCPPHC
  modFromData n = G.modFromData n (\d m -> toCode $ md n m d)
  updateModuleDoc f = onCodeValue (updateModDoc f)

instance BlockCommentSym CppHdrCode where
  type BlockComment CppHdrCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unCPPHC

-- helpers
toBasicVar :: GS (CppSrcCode (Variable CppSrcCode)) -> 
  GS (CppSrcCode (Variable CppSrcCode))
toBasicVar v = v >>= (\v' -> var (variableName v') (onStateValue variableType v))

isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

getParam :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  MS (repr (Parameter repr))
getParam v = zoom lensMStoGS v >>= (\v' -> getParamFunc ((getType . 
  variableType) v') v)
  where getParamFunc (List _) = pointerParam
        getParamFunc (Object _) = pointerParam
        getParamFunc _ = param
 
data MethodData = MthD {getMthdScp :: ScopeTag, mthdDoc :: Doc}

mthd :: ScopeTag -> Doc -> MethodData
mthd = MthD 

-- convenience
cppName :: String
cppName = "C++" 

enumsEqualInts :: Bool
enumsEqualInts = False

inc :: Doc
inc = text "#include"

cppstop :: ModData -> Doc -> Doc -> Doc
cppstop m lst end = vcat [
  if b then empty else inc <+> doubleQuotedText (addExt cppHdrExt n),
  if b then empty else blank,
  text "#define" <+> text "_USE_MATH_DEFINES", --FIXME: Only include if used (i.e. pi)
  inc <+> angles (text "algorithm"), -- find
  inc <+> angles (text "iostream"), -- cout, cin
  inc <+> angles (text "fstream"), -- ifstream, ofstream
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
  where n = name m
        b = isMainMod m

cpphtop :: ModData -> Doc -> Doc -> Doc
cpphtop m lst end = vcat [
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
  where n = name m

usingNameSpace :: Label -> Maybe Label -> Doc -> Doc
usingNameSpace n (Just m) end = text "using" <+> text n <> colon <> colon <>
  text m <> end
usingNameSpace n Nothing end = text "using namespace" <+> text n <> end

cppInherit :: Label -> Doc -> Doc
cppInherit n pub = colon <+> pub <+> text n

cppBoolType :: (RenderSym repr) => GS (repr (Type repr))
cppBoolType = toState $ typeFromData Boolean "bool" (text "bool")

cppInfileType :: (RenderSym repr) => GS (repr (Type repr))
cppInfileType = toState $ typeFromData File "ifstream" (text "ifstream")

cppOutfileType :: (RenderSym repr) => GS (repr (Type repr))
cppOutfileType = toState $ typeFromData File "ofstream" (text "ofstream")

cppIterType :: (RenderSym repr) => GS (repr (Type repr)) -> 
  GS (repr (Type repr))
cppIterType = onStateValue (\t -> typeFromData (Iterator (getType t)) 
  (getTypeString t ++ "::iterator") (text "std::" <> getTypeDoc t <> text 
  "::iterator"))

cppClassVar :: Doc -> Doc -> Doc
cppClassVar c v = c <> text "::" <> v

cppSelfFuncApp :: (RenderSym repr) => GS (repr (Variable repr)) -> Label -> 
  GS (repr (Type repr)) -> [GS (repr (Value repr))] -> GS (repr (Value repr))
cppSelfFuncApp s n t vs = s >>= 
  (\slf -> funcApp (variableName slf ++ "->" ++ n) t vs)

cppCast :: GS (CppSrcCode (Type CppSrcCode)) -> 
  GS (CppSrcCode (Value CppSrcCode)) -> GS (CppSrcCode (Value CppSrcCode))
cppCast t v = join $ on2StateValues (\tp vl -> cppCast' (getType tp) (getType $ 
  valueType vl) tp vl) t v
  where cppCast' Float String _ _ = funcApp "std::stod" float [v]
        cppCast' _ _ tp vl = mkStateVal t (castObjDocD (castDocD 
          (getTypeDoc tp)) (valueDoc vl))

cppListSetDoc :: Doc -> Doc -> Doc
cppListSetDoc i v = dot <> text "at" <> parens i <+> equals <+> v

cppListDecDoc :: (RenderSym repr) => repr (Value repr) -> Doc
cppListDecDoc n = parens (valueDoc n)

cppListDecDefDoc :: (RenderSym repr) => [repr (Value repr)] -> Doc
cppListDecDefDoc vs = braces (valueList vs)

cppPrint :: (RenderSym repr) => Bool -> repr (Value repr) -> repr (Value repr) 
  -> Doc
cppPrint newLn printFn v = valueDoc printFn <+> text "<<" <+> val (valueDoc v) 
  <+> end
  where val = if maybe False (< 9) (valuePrec v) then parens else id
        end = if newLn then text "<<" <+> text "std::endl" else empty

cppThrowDoc :: (RenderSym repr) => repr (Value repr) -> Doc
cppThrowDoc errMsg = text "throw" <> parens (valueDoc errMsg)

cppTryCatch :: (RenderSym repr) => repr (Body repr) -> repr (Body repr) -> Doc
cppTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent $ bodyDoc tb,
  rbrace <+> text "catch" <+> parens (text "...") <+> lbrace,
  indent $ bodyDoc cb,
  rbrace]

cppDiscardInput :: (RenderSym repr) => Label -> repr (Value repr) -> Doc
cppDiscardInput sep inFn = valueDoc inFn <> dot <> text "ignore" <> parens 
  (text "std::numeric_limits<std::streamsize>::max()" <> comma <+>
  quotes (text sep))

cppInput :: (RenderSym repr) => repr (Keyword repr) -> GS (repr (Variable repr))
  -> GS (repr (Value repr)) -> GS (repr (Statement repr))
cppInput end vr i = modify (addLangImport "limits") >> on2StateValues (\v inFn 
  -> mkSt $ vcat [valueDoc inFn <+> text ">>" <+> variableDoc v <> keyDoc end,
  valueDoc inFn <> dot <> 
    text "ignore(std::numeric_limits<std::streamsize>::max(), '\\n')"]) vr i

cppOpenFile :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  repr (Value repr) -> Doc
cppOpenFile mode f n = variableDoc f <> dot <> text "open" <> 
  parens (valueDoc n <> comma <+> text mode)

cppPointerParamDoc :: (RenderSym repr) => repr (Variable repr) -> Doc
cppPointerParamDoc v = getTypeDoc (variableType v) <+> text "&" <> variableDoc v

cppsMethod :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> Doc
cppsMethod n c t ps b bStart bEnd = emptyIfEmpty (bodyDoc b) $ vcat [ttype <+> 
  text c <> text "::" <> text n <> parens (parameterList ps) <+> keyDoc bStart,
  indent (bodyDoc b),
  keyDoc bEnd]
  where ttype | isDtor n = empty
              | otherwise = getTypeDoc t

cppsFunction :: (RenderSym repr) => Label -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> Doc
cppsFunction n t ps b bStart bEnd = vcat [
  getTypeDoc t <+> text n <> parens (parameterList ps) <+> keyDoc bStart,
  indent (bodyDoc b),
  keyDoc bEnd]

cpphMethod :: (RenderSym repr) => Label -> repr (Type repr) ->
  [repr (Parameter repr)] -> repr (Keyword repr) -> Doc
cpphMethod n t ps end = (if isDtor n then empty else getTypeDoc t) <+> text n 
  <> parens (parameterList ps) <> keyDoc end

cppCommentedFunc :: (RenderSym repr) => FileType -> 
  MS (repr (BlockComment repr)) -> MS (repr (Method repr)) -> 
  MS (repr (Method repr))
cppCommentedFunc ft cmt fn = do
  f <- fn
  mn <- getCurrMainFunc
  scp <- getScope
  cmnt <- cmt
  let cf = toState (methodFromData scp $ commentedItem (blockCommentDoc cmnt) $ 
        methodDoc f)
      ret Source = if mn then cf else toState f
      ret Header = if mn then toState f else cf
      ret Combined = error "Combined passed to cppCommentedFunc"
  ret ft

cppsStateVarDef :: Label -> Doc -> BindData -> VarData -> ValData -> Doc -> Doc
cppsStateVarDef n cns p vr vl end = if bind p == Static then cns <+> typeDoc 
  (varType vr) <+> text (n ++ "::") <> varDoc vr <+> equals <+> valDoc vl <>
  end else empty

cpphStateVarDef :: (RenderSym repr) => Doc -> repr (Permanence repr) -> 
  GS (repr (Variable repr)) -> GS (repr (Value repr)) -> GS Doc
cpphStateVarDef s p vr vl = onStateValue (stateVarDocD s (permDoc p) .  
  statementDoc) (state $ if binding p == Static then varDec vr else varDecDef 
  vr vl) 

cpphVarsFuncsList :: ScopeTag -> [CppHdrCode (StateVar CppHdrCode)] -> 
  [CppHdrCode (Method CppHdrCode)] -> Doc
cpphVarsFuncsList st vs fs = 
  let scopedVs = [stateVarDoc v | v <- vs, getStVarScp (unCPPHC v) == st]
      scopedFs = [methodDoc f | f <- fs, getMthdScp (unCPPHC f) == st]
  in vcat $ scopedVs ++ (if null scopedVs then empty else blank) : scopedFs

cppsClass :: [CppSrcCode (StateVar CppSrcCode)] -> 
  [CppSrcCode (Method CppSrcCode)] -> CppSrcCode (Class CppSrcCode)
cppsClass vs fs = toCode $ vcat $ vars ++ (if any (not . isEmpty) vars then 
  blank else empty) : funcs
  where vars = map stateVarDoc vs
        funcs = map methodDoc fs

cpphClass :: Label -> Maybe Label -> [CppHdrCode (StateVar CppHdrCode)] -> 
  [CppHdrCode (Method CppHdrCode)] -> CppHdrCode (Scope CppHdrCode) -> 
  CppHdrCode (Scope CppHdrCode) -> CppHdrCode (Keyword CppHdrCode) -> 
  CppHdrCode (Keyword CppHdrCode) -> CppHdrCode (Keyword CppHdrCode) -> 
  CppHdrCode (Class CppHdrCode)
cpphClass n p vars funcs pub priv bStart bEnd end = toCode $ vcat [
    classDec <+> text n <+> keyDoc (parent :: CppHdrCode (Keyword CppHdrCode)) 
    <+> keyDoc bStart,
    indentList [
      scopeDoc pub <> colon,
      indent pubs,
      blank,
      scopeDoc priv <> colon,
      indent privs],
    keyDoc bEnd <> keyDoc end]
  where pubs = cpphVarsFuncsList Pub vars funcs
        privs = cpphVarsFuncsList Priv vars funcs
        parent = case p of Nothing -> toCode empty
                           Just pn -> inherit pn

cpphEnum :: (RenderSym repr) => Label -> Doc -> repr (Keyword repr) -> 
  repr (Keyword repr) -> repr (Keyword repr) -> FS (repr (Class repr))
cpphEnum n es bStart bEnd end = classFromData $ toState $ vcat [
  text "enum" <+> text n <+> keyDoc bStart,
  indent es,
  keyDoc bEnd <> keyDoc end]

cppInOutCall :: (Label -> GS (CppSrcCode (Type CppSrcCode)) -> 
  [GS (CppSrcCode (Value CppSrcCode))] -> GS (CppSrcCode (Value CppSrcCode))) 
  -> Label -> [GS (CppSrcCode (Value CppSrcCode))] -> 
  [GS (CppSrcCode (Variable CppSrcCode))] -> 
  [GS (CppSrcCode (Variable CppSrcCode))] -> 
  GS (CppSrcCode (Statement CppSrcCode))
cppInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out)
  ins
cppInOutCall f n ins [] [out] = if null (filterOutObjs [out]) 
  then valState $ f n void (valueOf out : ins)
  else assign out $ f n (onStateValue variableType out) (valueOf out : ins)
cppInOutCall f n ins outs both = valState $ f n void (map valueOf both ++ ins 
  ++ map valueOf outs)

cppsInOut :: (CppSrcCode (Scope CppSrcCode) -> 
    CppSrcCode (Permanence CppSrcCode) -> GS (CppSrcCode (Type CppSrcCode)) -> 
    [MS (CppSrcCode (Parameter CppSrcCode))] -> 
    GS (CppSrcCode (Body CppSrcCode)) -> MS (CppSrcCode (Method CppSrcCode)))
  -> CppSrcCode (Scope CppSrcCode) -> CppSrcCode (Permanence CppSrcCode) -> 
  [GS (CppSrcCode (Variable CppSrcCode))] ->
  [GS (CppSrcCode (Variable CppSrcCode))] -> 
  [GS (CppSrcCode (Variable CppSrcCode))] -> GS (CppSrcCode (Body CppSrcCode)) 
  -> MS (CppSrcCode (Method CppSrcCode))
cppsInOut f s p ins [v] [] b = f s p (onStateValue variableType v) 
  (cppInOutParams ins [v] []) (on3StateValues (on3CodeValues surroundBody) 
  (varDec v) b (returnState $ valueOf v))
cppsInOut f s p ins [] [v] b = f s p (if null (filterOutObjs [v]) then void 
  else onStateValue variableType v) (cppInOutParams ins [] [v]) (if null 
  (filterOutObjs [v]) then b else on2StateValues (on2CodeValues appendToBody) b 
  (returnState $ valueOf v))
cppsInOut f s p ins outs both b = f s p void (cppInOutParams ins outs both) b

cpphInOut :: (CppHdrCode (Scope CppHdrCode) -> 
    CppHdrCode (Permanence CppHdrCode) -> GS (CppHdrCode (Type CppHdrCode)) -> 
    [MS (CppHdrCode (Parameter CppHdrCode))] -> 
    GS (CppHdrCode (Body CppHdrCode)) -> MS (CppHdrCode (Method CppHdrCode))) 
  -> CppHdrCode (Scope CppHdrCode) -> CppHdrCode (Permanence CppHdrCode) -> 
  [GS (CppHdrCode (Variable CppHdrCode))] -> 
  [GS (CppHdrCode (Variable CppHdrCode))] -> 
  [GS (CppHdrCode (Variable CppHdrCode))] -> GS (CppHdrCode (Body CppHdrCode)) 
  -> MS (CppHdrCode (Method CppHdrCode))
cpphInOut f s p ins [v] [] b = f s p (onStateValue variableType v) 
  (cppInOutParams ins [v] []) b
cpphInOut f s p ins [] [v] b = f s p (if null (filterOutObjs [v]) then void 
  else onStateValue variableType v) (cppInOutParams ins [] [v]) b
cpphInOut f s p ins outs both b = f s p void (cppInOutParams ins outs both) b

cppInOutParams :: (RenderSym repr) => [GS (repr (Variable repr))] -> 
  [GS (repr (Variable repr))] -> [GS (repr (Variable repr))] -> 
  [MS (repr (Parameter repr))]
cppInOutParams ins [_] [] = map getParam ins
cppInOutParams ins [] [v] = map getParam $ v : ins
cppInOutParams ins outs both = map pointerParam both ++ map getParam ins ++ 
  map pointerParam outs