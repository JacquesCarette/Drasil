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
import GOOL.Drasil.ClassInterface (Label, MSBody, VSType, SVariable, 
  SValue, MSStatement, MSParameter, SMethod, ProgramSym(..), FileSym(..), 
  PermanenceSym(..), BodySym(..), bodyStatements, oneLiner, BlockSym(..), 
  TypeSym(..), ControlBlock(..), InternalControlBlock(..), VariableSym(..), 
  ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), funcApp, selfFuncApp, extFuncApp, Selector(..), ($.), 
  InternalValueExp(..), objMethodCall, FunctionSym(..), SelectorFunction(..), 
  StatementSym(..), ControlStatement(..), switchAsIf, ScopeSym(..), 
  ParameterSym(..), MethodSym(..), pubMethod, StateVarSym(..), ClassSym(..), 
  ModuleSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, InternalFile(..),
  ImportSym(..), InternalPerm(..), InternalBody(..), InternalBlock(..), 
  InternalType(..), UnaryOpSym(..), BinaryOpSym(..), InternalOp(..), 
  InternalVariable(..), InternalValue(..), InternalFunction(..), 
  InternalStatement(..), InternalScope(..), MethodTypeSym(..), 
  InternalParam(..), InternalMethod(..), InternalStateVar(..), ParentSpec,
  InternalClass(..), InternalMod(..), BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (addExt, multiStateDocD, 
  bodyDocD, outDoc, paramDocD, stateVarDocD, constVarDocD, freeDocD, mkSt, 
  mkStNoEnd, breakDocD, continueDocD, mkStateVal, mkVal, mkStateVar, mkVar, 
  classVarCheckStatic, castDocD, castObjDocD, staticDocD, dynamicDocD, 
  privateDocD, publicDocD, classDec, dot, blockCmtStart, blockCmtEnd, 
  docCmtStart, bodyStart, bodyEnd, endStatement, commentStart, elseIfLabel, 
  blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, functionDox, 
  commentedModD, valueList, parameterList, appendToBody, surroundBody, 
  getterName, setterName)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, int, float, double, char, string, listType, 
  listInnerType, obj, funcType, void, runStrategy, listSlice, notOp, negateOp,
  sqrtOp, absOp, expOp, sinOp, cosOp, tanOp, asinOp, acosOp, atanOp, csc, sec, 
  cot, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, 
  plusOp, minusOp, multOp, divideOp, moduloOp, powerOp, andOp, orOp, var, 
  staticVar, self, objVar, listVar, arrayElem, litTrue, litFalse, litChar, 
  litDouble, litFloat, litInt, litString, litArray, valueOf, arg, argsList, 
  inlineIf, objAccess, objMethodCall, objMethodCallNoParams, call', 
  funcAppMixedArgs, selfFuncAppMixedArgs, libFuncAppMixedArgs, newObjMixedArgs, 
  libNewObjMixedArgs, lambda, func, get, set, listSize, listAdd, listAppend, 
  iterBegin, iterEnd, listAccess, listSet, getFunc, setFunc, listSizeFunc, 
  listAppendFunc, listAccessFunc', listSetFunc, state, loopState, emptyState, 
  assign, multiAssignError, decrement, increment, decrement1, increment1, 
  varDec, varDecDef, listDec, listDecDef, objDecNew, objDecNewNoParams, 
  extObjDecNew, extObjDecNewNoParams, constDecDef, funcDecDef, discardInput, 
  discardFileInput, closeFile, stringListVals, stringListLists, returnState, 
  multiReturnError, valState, comment, throw, ifCond, switch, for, forRange, 
  while, tryCatch, notifyObservers, construct, param, method, getMethod, 
  setMethod, constructor, function, docFunc, docInOutFunc, intFunc, buildClass, 
  implementingClass, docClass, commentedClass, buildModule, modFromData, 
  fileDoc, docMod, fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (unOpPrec, unExpr, 
  unExpr', typeUnExpr, binExpr, binExpr', typeBinExpr)
import GOOL.Drasil.AST (Terminator(..), ScopeTag(..), 
  Binding(..), onBinding, BindData(..), bd, FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateMod, OpData(..), od, 
  ParamData(..), pd, ProgData(..), progD, emptyProg, StateVarData(..), svd, 
  TypeData(..), td, ValData(..), vd, VarData(..), vard)
import GOOL.Drasil.Classes (Pair(..))
import GOOL.Drasil.Helpers (angles, doubleQuotedText, hicat, vibcat, 
  emptyIfEmpty, toCode, toState, onCodeValue, onStateValue, on2CodeValues, 
  on2StateValues, on3CodeValues, on3StateValues, onCodeList, onStateList, 
  on2StateLists, on1StateValue1List)
import GOOL.Drasil.State (CS, MS, VS, lensGStoFS, lensFStoCS, lensFStoMS, 
  lensCStoMS, lensCStoVS, lensMStoCS, lensMStoVS, lensVStoMS, modifyReturn, 
  revFiles, addLangImport, addLangImportVS, getLangImports, getLibImports, 
  addModuleImportVS, getModuleImports, addHeaderLangImport, 
  getHeaderLangImports, addHeaderModImport, getHeaderLibImports, 
  getHeaderModImports, addDefine, getDefines, addHeaderDefine, 
  getHeaderDefines, addUsing, getUsing, addHeaderUsing, getHeaderUsing, 
  setFileType, setClassName, getClassName, setCurrMain, getCurrMain, 
  getClassMap, setScope, getScope, setCurrMainFunc, getCurrMainFunc)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,pi,const,log,exp,mod)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (State, modify)
import Data.List (sort)
import qualified Data.Map as Map (lookup)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), hcat, brackets, 
  braces, parens, comma, empty, equals, vcat, lbrace, rbrace, quotes, colon, 
  isEmpty)

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
    modify revFiles
    toState $ pair p1 (toCode emptyProg)

instance (Pair p) => RenderSym (p CppSrcCode CppHdrCode)

instance (Pair p) => FileSym (p CppSrcCode CppHdrCode) where
  type RenderFile (p CppSrcCode CppHdrCode) = FileData
  fileDoc = pair1 fileDoc fileDoc

  docMod d a dt = pair1 (docMod d a dt) (docMod d a dt)

instance (Pair p) => InternalFile (p CppSrcCode CppHdrCode) where
  top m = pair (top $ pfst m) (top $ psnd m)
  bottom = pair bottom bottom
  
  commentedMod = pair2 commentedMod commentedMod

  fileFromData fp = pair1 (fileFromData fp) (fileFromData fp)

instance (Pair p) => ImportSym (p CppSrcCode CppHdrCode) where
  type Import (p CppSrcCode CppHdrCode) = Doc
  langImport n = pair (langImport n) (langImport n)
  modImport n = pair (modImport n) (modImport n)

  importDoc i = importDoc $ pfst i

instance (Pair p) => PermanenceSym (p CppSrcCode CppHdrCode) where
  type Permanence (p CppSrcCode CppHdrCode) = BindData
  static = pair static static
  dynamic = pair dynamic dynamic

instance (Pair p) => InternalPerm (p CppSrcCode CppHdrCode) where
  permDoc p = permDoc $ pfst p
  binding p = binding $ pfst p

instance (Pair p) => BodySym (p CppSrcCode CppHdrCode) where
  type Body (p CppSrcCode CppHdrCode) = Doc
  body = pair1List body body

  addComments s = pair1 (addComments s) (addComments s)

instance (Pair p) => InternalBody (p CppSrcCode CppHdrCode) where
  bodyDoc b = bodyDoc $ pfst b
  docBody d = on2StateValues pair (docBody d) (docBody d)
  multiBody = pair1List multiBody multiBody

instance (Pair p) => BlockSym (p CppSrcCode CppHdrCode) where
  type Block (p CppSrcCode CppHdrCode) = Doc
  block = pair1List block block

instance (Pair p) => InternalBlock (p CppSrcCode CppHdrCode) where
  blockDoc b = blockDoc $ pfst b
  docBlock d = on2StateValues pair (docBlock d) (docBlock d)
  multiBlock = pair1List multiBlock multiBlock

instance (Pair p) => TypeSym (p CppSrcCode CppHdrCode) where
  type Type (p CppSrcCode CppHdrCode) = TypeData
  bool = on2StateValues pair bool bool
  int = on2StateValues pair int int
  float = on2StateValues pair float float
  double = on2StateValues pair double double
  char = on2StateValues pair char char
  string = on2StateValues pair string string
  infile = on2StateValues pair infile infile
  outfile = on2StateValues pair outfile outfile
  listType = pair1 listType listType
  arrayType = pair1 arrayType arrayType
  listInnerType = pair1 listInnerType listInnerType
  obj t = on2StateValues pair (obj t) (obj t)
  -- enumType t = on2StateValues pair (enumType t) (enumType t)
  funcType = pair1List1Val funcType funcType
  iterator = pair1 iterator iterator
  void = on2StateValues pair void void

  getType s = getType $ pfst s
  getTypeString s = getTypeString $ pfst s
  
instance (Pair p) => InternalType (p CppSrcCode CppHdrCode) where
  getTypeDoc s = getTypeDoc $ pfst s
  typeFromData t s d = pair (typeFromData t s d) (typeFromData t s d)

instance (Pair p) => ControlBlock (p CppSrcCode CppHdrCode) where
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

instance (Pair p) => InternalControlBlock (p CppSrcCode CppHdrCode) where
  listSlice' b e s vr vl = pair2 
    (listSlice' (fmap (onStateValue pfst) b) (fmap (onStateValue pfst) e) 
      (fmap (onStateValue pfst) s))
    (listSlice' (fmap (onStateValue psnd) b) (fmap (onStateValue psnd) e) 
      (fmap (onStateValue psnd) s)) 
    (zoom lensMStoVS vr) (zoom lensMStoVS vl)

instance (Pair p) => UnaryOpSym (p CppSrcCode CppHdrCode) where
  type UnaryOp (p CppSrcCode CppHdrCode) = OpData
  notOp = on2StateValues pair notOp notOp
  negateOp = on2StateValues pair negateOp negateOp
  sqrtOp = on2StateValues pair sqrtOp sqrtOp
  absOp = on2StateValues pair absOp absOp
  logOp = on2StateValues pair logOp logOp
  lnOp = on2StateValues pair lnOp lnOp
  expOp = on2StateValues pair expOp expOp
  sinOp = on2StateValues pair sinOp sinOp
  cosOp = on2StateValues pair cosOp cosOp
  tanOp = on2StateValues pair tanOp tanOp
  asinOp = on2StateValues pair asinOp asinOp
  acosOp = on2StateValues pair acosOp acosOp
  atanOp = on2StateValues pair atanOp atanOp
  floorOp = on2StateValues pair floorOp floorOp
  ceilOp = on2StateValues pair ceilOp ceilOp

instance (Pair p) => BinaryOpSym (p CppSrcCode CppHdrCode) where
  type BinaryOp (p CppSrcCode CppHdrCode) = OpData
  equalOp = on2StateValues pair equalOp equalOp
  notEqualOp = on2StateValues pair notEqualOp notEqualOp
  greaterOp = on2StateValues pair greaterOp greaterOp
  greaterEqualOp = on2StateValues pair greaterEqualOp greaterEqualOp
  lessOp = on2StateValues pair lessOp lessOp
  lessEqualOp = on2StateValues pair lessEqualOp lessEqualOp
  plusOp = on2StateValues pair plusOp plusOp
  minusOp = on2StateValues pair minusOp minusOp
  multOp = on2StateValues pair multOp multOp
  divideOp = on2StateValues pair divideOp divideOp
  powerOp = on2StateValues pair powerOp powerOp
  moduloOp = on2StateValues pair moduloOp moduloOp
  andOp = on2StateValues pair andOp andOp
  orOp = on2StateValues pair orOp orOp

instance (Pair p) => InternalOp (p CppSrcCode CppHdrCode) where
  uOpDoc o = uOpDoc $ pfst o
  bOpDoc o = bOpDoc $ pfst o
  uOpPrec o = uOpPrec $ pfst o
  bOpPrec o = bOpPrec $ pfst o
  
  uOpFromData p d = on2StateValues pair (uOpFromData p d) (uOpFromData p d)
  bOpFromData p d = on2StateValues pair (bOpFromData p d) (bOpFromData p d)

instance (Pair p) => VariableSym (p CppSrcCode CppHdrCode) where
  type Variable (p CppSrcCode CppHdrCode) = VarData
  var n = pair1 (var n) (var n)
  staticVar n = pair1 (staticVar n) (staticVar n)
  const n = pair1 (const n) (const n)
  extVar l n = pair1 (extVar l n) (extVar l n)
  self = on2StateValues pair self self
  -- enumVar e en = on2StateValues pair (enumVar e en) (enumVar e en)
  classVar = pair2 classVar classVar
  extClassVar = pair2 extClassVar extClassVar
  objVar = pair2 objVar objVar
  objVarSelf = pair1 objVarSelf objVarSelf
  listVar n = pair1 (listVar n) (listVar n)
  arrayElem i = pair1 (arrayElem i) (arrayElem i)
  iterVar l = pair1 (iterVar l) (iterVar l)

  variableName v = variableName $ pfst v
  variableType v = pair (variableType $ pfst v) (variableType $ psnd v)

instance (Pair p) => InternalVariable (p CppSrcCode CppHdrCode) where
  variableBind v = variableBind $ pfst v
  variableDoc v = variableDoc $ pfst v
  varFromData b n t d = pair (varFromData b n (pfst t) d) 
    (varFromData b n (psnd t) d)

instance (Pair p) => ValueSym (p CppSrcCode CppHdrCode) where
  type Value (p CppSrcCode CppHdrCode) = ValData
  litTrue = on2StateValues pair litTrue litTrue
  litFalse = on2StateValues pair litFalse litFalse
  litChar c = on2StateValues pair (litChar c) (litChar c)
  litDouble v = on2StateValues pair (litDouble v) (litDouble v)
  litFloat v = on2StateValues pair (litFloat v) (litFloat v)
  litInt v =on2StateValues  pair (litInt v) (litInt v)
  litString s = on2StateValues pair (litString s) (litString s)
  litArray = pair1Val1List litArray litArray
  litList = pair1Val1List litList litList

  pi = on2StateValues pair pi pi

  -- ($:) l1 l2 = on2StateValues pair (($:) l1 l2) (($:) l1 l2)

  valueOf = pair1 valueOf valueOf
  arg n = on2StateValues pair (arg n) (arg n)
  -- enumElement en e = on2StateValues pair (enumElement en e) (enumElement en e)
  
  argsList = on2StateValues pair argsList argsList

  valueType v = pair (valueType $ pfst v) (valueType $ psnd v)

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

  funcAppMixedArgs n t pas nas = pair1Val3Lists
    (\tp pars ns nars -> funcAppMixedArgs n tp pars (zip ns nars)) 
    (\tp pars ns nars -> funcAppMixedArgs n tp pars (zip ns nars)) 
    t pas (map fst nas) (map snd nas)
  selfFuncAppMixedArgs n t pas nas = pair1Val3Lists
    (\tp pars ns nars -> selfFuncAppMixedArgs n tp pars (zip ns nars)) 
    (\tp pars ns nars -> selfFuncAppMixedArgs n tp pars (zip ns nars)) 
    t pas (map fst nas) (map snd nas)
  extFuncAppMixedArgs l n t pas nas = pair1Val3Lists
    (\tp pars ns nars -> extFuncAppMixedArgs l n tp pars (zip ns nars)) 
    (\tp pars ns nars -> extFuncAppMixedArgs l n tp pars (zip ns nars)) 
    t pas (map fst nas) (map snd nas)
  libFuncAppMixedArgs l n t pas nas = pair1Val3Lists
    (\tp pars ns nars -> extFuncAppMixedArgs l n tp pars (zip ns nars)) 
    (\tp pars ns nars -> extFuncAppMixedArgs l n tp pars (zip ns nars)) 
    t pas (map fst nas) (map snd nas)
  newObjMixedArgs t pas nas = pair1Val3Lists
    (\tp pars ns nars -> newObjMixedArgs tp pars (zip ns nars)) 
    (\tp pars ns nars -> newObjMixedArgs tp pars (zip ns nars)) 
    t pas (map fst nas) (map snd nas)
  extNewObjMixedArgs l t pas nas = pair1Val3Lists
    (\tp pars ns nars -> extNewObjMixedArgs l tp pars (zip ns nars)) 
    (\tp pars ns nars -> extNewObjMixedArgs l tp pars (zip ns nars)) 
    t pas (map fst nas) (map snd nas)
  libNewObjMixedArgs l t pas nas = pair1Val3Lists
    (\tp pars ns nars -> extNewObjMixedArgs l tp pars (zip ns nars)) 
    (\tp pars ns nars -> extNewObjMixedArgs l tp pars (zip ns nars)) 
    t pas (map fst nas) (map snd nas)

  lambda = pair1List1Val lambda lambda

  notNull = pair1 notNull notNull
  
instance (Pair p) => InternalValue (p CppSrcCode CppHdrCode) where
  inputFunc = on2StateValues pair inputFunc inputFunc
  printFunc = on2StateValues pair printFunc printFunc
  printLnFunc = on2StateValues pair printLnFunc printLnFunc
  printFileFunc = pair1 printFileFunc printFileFunc
  printFileLnFunc = pair1 printFileLnFunc printFileLnFunc

  cast = pair2 cast cast

  call l n t o pas nas = pair1Val3Lists
    (\tp pars ns nars -> call l n tp o pars (zip ns nars)) 
    (\tp pars ns nars -> call l n tp o pars (zip ns nars)) 
    t pas (map fst nas) (map snd nas)

  valuePrec v = valuePrec $ pfst v
  valueDoc v = valueDoc $ pfst v
  valFromData p t d = pair (valFromData p (pfst t) d) (valFromData p (psnd t) d)

instance (Pair p) => Selector (p CppSrcCode CppHdrCode) where
  objAccess = pair2 objAccess objAccess

  argExists i = on2StateValues pair (argExists i) (argExists i)
  
  indexOf = pair2 indexOf indexOf

instance (Pair p) => InternalValueExp (p CppSrcCode CppHdrCode) where
  objMethodCallMixedArgs' f t o pas nas = pair2Vals3Lists
    (\tp ob pars ns nars -> objMethodCallMixedArgs' f tp ob pars (zip ns nars)) 
    (\tp ob pars ns nars -> objMethodCallMixedArgs' f tp ob pars (zip ns nars)) 
    t o pas (map fst nas) (map snd nas)
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
  printSt nl f p v = pair2
    (printSt nl (fmap (onStateValue pfst) f)) 
    (printSt nl (fmap (onStateValue psnd) f)) 
    (zoom lensMStoVS p) (zoom lensMStoVS v)

  multiAssign vrs vls = pair2Lists multiAssign multiAssign 
    (map (zoom lensMStoVS) vrs) (map (zoom lensMStoVS) vls)
  multiReturn = pair1List multiReturn multiReturn . map (zoom lensMStoVS)
    
  state = pair1 state state
  loopState = pair1 loopState loopState

  emptyState = on2StateValues pair emptyState emptyState
  statementDoc s = statementDoc $ pfst s
  statementTerm s = statementTerm $ pfst s
  
  stateFromData d t = pair (stateFromData d t) (stateFromData d t)

instance (Pair p) => StatementSym (p CppSrcCode CppHdrCode) where
  type Statement (p CppSrcCode CppHdrCode) = (Doc, Terminator)
  assign vr vl = pair2 assign assign (zoom lensMStoVS vr) (zoom lensMStoVS vl)
  (&-=) vr vl = pair2 (&-=) (&-=) (zoom lensMStoVS vr) (zoom lensMStoVS vl)
  (&+=) vr vl = pair2 (&+=) (&+=) (zoom lensMStoVS vr) (zoom lensMStoVS vl)
  (&++) vl = pair1 (&++) (&++) (zoom lensMStoVS vl)
  (&--) vl = pair1 (&--) (&--) (zoom lensMStoVS vl)

  varDec vr = pair1 varDec varDec (zoom lensMStoVS vr)
  varDecDef vr vl = pair2 varDecDef varDecDef (zoom lensMStoVS vr) 
    (zoom lensMStoVS vl)
  listDec n vr = pair1 (listDec n) (listDec n) (zoom lensMStoVS vr)
  listDecDef vr vs = pair1Val1List listDecDef listDecDef (zoom lensMStoVS vr) 
    (map (zoom lensMStoVS) vs)
  arrayDec n vr = pair1 (arrayDec n) (arrayDec n) (zoom lensMStoVS vr)
  arrayDecDef vr vs = pair1Val1List arrayDecDef arrayDecDef (zoom lensMStoVS vr)
    (map (zoom lensMStoVS) vs)
  objDecDef o v = pair2 objDecDef objDecDef (zoom lensMStoVS o) 
    (zoom lensMStoVS v)
  objDecNew vr vs = pair1Val1List objDecNew objDecNew (zoom lensMStoVS vr) 
    (map (zoom lensMStoVS) vs)
  extObjDecNew lib vr vs = pair1Val1List (extObjDecNew lib) (extObjDecNew lib) 
    (zoom lensMStoVS vr) (map (zoom lensMStoVS) vs)
  objDecNewNoParams = pair1 objDecNewNoParams objDecNewNoParams . 
    zoom lensMStoVS
  extObjDecNewNoParams lib = pair1 
    (extObjDecNewNoParams lib) 
    (extObjDecNewNoParams lib) . zoom lensMStoVS
  constDecDef vr vl = pair2 constDecDef constDecDef (zoom lensMStoVS vr) 
    (zoom lensMStoVS vl)
  funcDecDef v ps r = pairValListVal funcDecDef funcDecDef (zoom lensMStoVS v) 
    (map (zoom lensMStoVS) ps) (zoom lensMStoVS r)

  print = pair1 print print . zoom lensMStoVS
  printLn = pair1 printLn printLn . zoom lensMStoVS
  printStr s = on2StateValues pair (printStr s) (printStr s)
  printStrLn s = on2StateValues pair (printStrLn s) (printStrLn s)

  printFile f v = pair2 printFile printFile (zoom lensMStoVS f) 
    (zoom lensMStoVS v)
  printFileLn f v = pair2 printFileLn printFileLn (zoom lensMStoVS f) 
    (zoom lensMStoVS v)
  printFileStr f s = pair1 (`printFileStr` s) (`printFileStr` s) 
    (zoom lensMStoVS f)
  printFileStrLn f s = pair1 (`printFileStrLn` s) (`printFileStrLn` s) 
    (zoom lensMStoVS f)

  getInput = pair1 getInput getInput . zoom lensMStoVS
  discardInput = on2StateValues pair discardInput discardInput
  getFileInput f v = pair2 getFileInput getFileInput (zoom lensMStoVS f) 
    (zoom lensMStoVS v)
  discardFileInput = pair1 discardFileInput discardFileInput . zoom lensMStoVS

  openFileR f v = pair2 openFileR openFileR (zoom lensMStoVS f) 
    (zoom lensMStoVS v)
  openFileW f v = pair2 openFileW openFileW (zoom lensMStoVS f) 
    (zoom lensMStoVS v)
  openFileA f v = pair2 openFileA openFileA (zoom lensMStoVS f) 
    (zoom lensMStoVS v)
  closeFile = pair1 closeFile closeFile . zoom lensMStoVS

  getFileInputLine f v = pair2 getFileInputLine getFileInputLine 
    (zoom lensMStoVS f) (zoom lensMStoVS v)
  discardFileLine = pair1 discardFileLine discardFileLine . zoom lensMStoVS
  stringSplit d vnew s = pair2 (stringSplit d) (stringSplit d) 
    (zoom lensMStoVS vnew) (zoom lensMStoVS s)

  stringListVals vars sl = pair1List1Val stringListVals stringListVals
    (map (zoom lensMStoVS) vars) (zoom lensMStoVS sl)
  stringListLists lsts sl = pair1List1Val stringListLists stringListLists
    (map (zoom lensMStoVS) lsts) (zoom lensMStoVS sl)

  break = on2StateValues pair break break
  continue = on2StateValues pair continue continue

  returnState = pair1 returnState returnState . zoom lensMStoVS

  valState = pair1 valState valState . zoom lensMStoVS

  comment cmt = on2StateValues pair (comment cmt) (comment cmt)

  free = pair1 free free . zoom lensMStoVS

  throw errMsg = on2StateValues pair (throw errMsg) (throw errMsg)

  inOutCall n is os bs = pair3Lists (inOutCall n) (inOutCall n) 
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)
  selfInOutCall n is os bs = pair3Lists (selfInOutCall n) (selfInOutCall n)
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)
  extInOutCall m n is os bs = pair3Lists (extInOutCall m n) (extInOutCall m n) 
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)

  multi = pair1List multi multi

instance (Pair p) => ControlStatement (p CppSrcCode CppHdrCode) where
  ifCond bs = pair2Lists1Val
    (\cs bods -> ifCond (zip cs bods)) 
    (\cs bods -> ifCond (zip cs bods)) 
    (map (zoom lensMStoVS . fst) bs) (map snd bs)
  switch v cs = pairVal2ListsVal 
    (\s cv cb -> switch s (zip cv cb))
    (\s cv cb -> switch s (zip cv cb))
    (zoom lensMStoVS v) (map (zoom lensMStoVS . fst) cs) (map snd cs)

  ifExists v = pair3 ifExists ifExists (zoom lensMStoVS v)

  for i initv = pair4 for for i (zoom lensMStoVS initv)
  forRange i initv finalv stepv = pair5 forRange forRange (zoom lensMStoVS i) 
    (zoom lensMStoVS initv) (zoom lensMStoVS finalv) (zoom lensMStoVS stepv)
  forEach e v = pair3 forEach forEach (zoom lensMStoVS e) (zoom lensMStoVS v)
  while v = pair2 while while (zoom lensMStoVS v)

  tryCatch = pair2 tryCatch tryCatch

  checkState l vs = pair2Lists1Val
    (\sts bods -> checkState l (zip sts bods))
    (\sts bods -> checkState l (zip sts bods)) 
    (map (zoom lensMStoVS . fst) vs) (map snd vs)

  notifyObservers f t = pair2 notifyObservers notifyObservers 
    (zoom lensMStoVS f) (zoom lensMStoVS t)

  getFileInputAll f v = pair2 getFileInputAll getFileInputAll 
    (zoom lensMStoVS f) (zoom lensMStoVS v)

instance (Pair p) => ScopeSym (p CppSrcCode CppHdrCode) where
  type Scope (p CppSrcCode CppHdrCode) = (Doc, ScopeTag)
  private = pair private private
  public = pair public public

instance (Pair p) => InternalScope (p CppSrcCode CppHdrCode) where
  scopeDoc s = scopeDoc $ pfst s
  scopeFromData s d = pair (scopeFromData s d) (scopeFromData s d)

instance (Pair p) => MethodTypeSym (p CppSrcCode CppHdrCode) where
  type MethodType (p CppSrcCode CppHdrCode) = TypeData
  mType = pair1 mType mType . zoom lensMStoVS
  construct n = on2StateValues pair (construct n) (construct n)

instance (Pair p) => ParameterSym (p CppSrcCode CppHdrCode) where
  type Parameter (p CppSrcCode CppHdrCode) = ParamData
  param = pair1 param param . zoom lensMStoVS
  pointerParam = pair1 pointerParam pointerParam . zoom lensMStoVS

instance (Pair p) => InternalParam (p CppSrcCode CppHdrCode) where
  parameterName p = parameterName $ pfst p
  parameterType p = pair (parameterType $ pfst p) (parameterType $ psnd p)
  parameterDoc p = parameterDoc $ pfst p
  paramFromData v d = pair (paramFromData (pfst v) d) (paramFromData (psnd v) d)

instance (Pair p) => MethodSym (p CppSrcCode CppHdrCode) where
  type Method (p CppSrcCode CppHdrCode) = MethodData
  method n s p t = pairValListVal
    (method n (pfst s) (pfst p)) (method n (psnd s) (psnd p)) 
    (zoom lensMStoVS t)
  getMethod = pair1 getMethod getMethod . zoom lensMStoVS
  setMethod = pair1 setMethod setMethod . zoom lensMStoVS
  constructor ps is = pair3Lists1Val 
    (\pms ivars ivals -> constructor pms (zip ivars ivals))
    (\pms ivars ivals -> constructor pms (zip ivars ivals)) 
    ps (map (zoom lensMStoVS . fst) is) (map (zoom lensMStoVS . snd) is)

  docMain = pair1 docMain docMain

  function n s p t = pairValListVal 
    (function n (pfst s) (pfst p)) (function n (psnd s) (psnd p)) 
    (zoom lensMStoVS t)
  mainFunction = pair1 mainFunction mainFunction

  docFunc desc pComms rComm = pair1 (docFunc desc pComms rComm) 
    (docFunc desc pComms rComm)

  inOutMethod n s p is os bs = pair3Lists1Val 
    (inOutMethod n (pfst s) (pfst p)) (inOutMethod n (psnd s) (psnd p)) 
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)

  docInOutMethod n s p desc is os bs = pair3Lists1Val
    (\ins outs both -> docInOutMethod n (pfst s) (pfst p) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (\ins outs both -> docInOutMethod n (psnd s) (psnd p) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (map (zoom lensMStoVS . snd) is) (map (zoom lensMStoVS . snd) os) 
    (map (zoom lensMStoVS . snd) bs)

  inOutFunc n s p is os bs = pair3Lists1Val
    (inOutFunc n (pfst s) (pfst p)) (inOutFunc n (psnd s) (psnd p))
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)

  docInOutFunc n s p desc is os bs = pair3Lists1Val 
    (\ins outs both -> docInOutFunc n (pfst s) (pfst p) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (\ins outs both -> docInOutFunc n (psnd s) (psnd p) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (map (zoom lensMStoVS . snd) is) (map (zoom lensMStoVS . snd) os) 
    (map (zoom lensMStoVS . snd) bs)
  
instance (Pair p) => InternalMethod (p CppSrcCode CppHdrCode) where
  intMethod m n s p = pairValListVal
    (intMethod m n (pfst s) (pfst p)) (intMethod m n (psnd s) (psnd p))
  intFunc m n s p = pairValListVal
    (intFunc m n (pfst s) (pfst p)) (intFunc m n (psnd s) (psnd p))
  commentedFunc = pair2 commentedFunc commentedFunc
  
  destructor = pair1List destructor destructor . map (zoom lensMStoCS)
    
  methodDoc m = methodDoc $ pfst m
  methodFromData s d = pair (methodFromData s d) (methodFromData s d)

instance (Pair p) => StateVarSym (p CppSrcCode CppHdrCode) where
  type StateVar (p CppSrcCode CppHdrCode) = StateVarData
  stateVar s p = pair1 (stateVar (pfst s) (pfst p)) (stateVar (psnd s) (psnd p))
    . zoom lensCStoVS
  stateVarDef n s p vr vl = pair2
    (stateVarDef n (pfst s) (pfst p)) 
    (stateVarDef n (psnd s) (psnd p)) (zoom lensCStoVS vr) (zoom lensCStoVS vl)
  constVar n s vr vl = pair2 (constVar n (pfst s)) (constVar n (psnd s))
    (zoom lensCStoVS vr) (zoom lensCStoVS vl)

instance (Pair p) => InternalStateVar (p CppSrcCode CppHdrCode) where
  stateVarDoc v = stateVarDoc $ pfst v
  stateVarFromData d = on2StateValues pair (stateVarFromData d) 
    (stateVarFromData d)

instance (Pair p) => ClassSym (p CppSrcCode CppHdrCode) where
  type Class (p CppSrcCode CppHdrCode) = Doc
  buildClass n p vs fs = modify (setClassName n) >> pair2Lists 
    (buildClass n p) (buildClass n p)
    vs (map (zoom lensCStoMS) fs)
  -- enum l ls s = modify (setClassName l) >> on2StateValues pair 
  --   (enum l ls $ pfst s) (enum l ls $ psnd s)
  extraClass n p vs fs = modify (setClassName n) >> pair2Lists 
    (extraClass n p) (extraClass n p)
    vs (map (zoom lensCStoMS) fs)
  implementingClass n is vs fs = modify (setClassName n) >> pair2Lists
    (implementingClass n is) (implementingClass n is)
    vs (map (zoom lensCStoMS) fs)

  docClass d = pair1 (docClass d) (docClass d)

instance (Pair p) => InternalClass (p CppSrcCode CppHdrCode) where
  intClass n s i vs fs = pair2Lists 
    (intClass n (pfst s) (pfst i)) (intClass n (psnd s) (psnd i)) 
    vs (map (zoom lensCStoMS) fs)

  inherit n = pair (inherit n) (inherit n)
  implements is = pair (implements is) (implements is)

  commentedClass = pair2 commentedClass commentedClass

  classDoc c = classDoc $ pfst c
  classFromData = pair1 classFromData classFromData

instance (Pair p) => ModuleSym (p CppSrcCode CppHdrCode) where
  type Module (p CppSrcCode CppHdrCode) = ModData
  buildModule n is ms = pair2Lists (buildModule n is) (buildModule n is) 
    (map (zoom lensFStoMS) ms) . map (zoom lensFStoCS)
  
instance (Pair p) => InternalMod (p CppSrcCode CppHdrCode) where
  moduleDoc m = moduleDoc $ pfst m
  modFromData n d = on2StateValues pair (modFromData n d) (modFromData n d)
  updateModuleDoc f m = pair 
    (updateModuleDoc f $ pfst m) (updateModuleDoc f $ psnd m)

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

pair1Val3Lists :: (Pair p) => (State r (CppSrcCode a) -> 
  [State s (CppSrcCode b)] -> [State t (CppSrcCode c)] -> 
  [State u (CppSrcCode d)] -> State v (CppSrcCode e)) -> (State r (CppHdrCode a)
  -> [State s (CppHdrCode b)] -> [State t (CppHdrCode c)] -> 
  [State u (CppHdrCode d)] -> State v (CppHdrCode e)) -> 
  State v (p CppSrcCode CppHdrCode a) -> [State v (p CppSrcCode CppHdrCode b)]
  -> [State v (p CppSrcCode CppHdrCode c)] -> 
  [State v (p CppSrcCode CppHdrCode d)] -> State v (p CppSrcCode CppHdrCode e)
pair1Val3Lists srcf hdrf stv1 stv2 stv3 stv4 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair3Lists (srcf fv1) (hdrf sv1) stv2 stv3 stv4

pair2Vals3Lists :: (Pair p) => (State r (CppSrcCode a) -> 
  State s (CppSrcCode b) -> [State t (CppSrcCode c)] -> 
  [State u (CppSrcCode d)] -> [State v (CppSrcCode e)] -> 
  State w (CppSrcCode f)) -> (State r (CppHdrCode a) -> State s (CppHdrCode b) 
  -> [State t (CppHdrCode c)] -> [State u (CppHdrCode d)] -> 
  [State v (CppHdrCode e)] -> State w (CppHdrCode f)) ->
  State w (p CppSrcCode CppHdrCode a) -> State w (p CppSrcCode CppHdrCode b) ->
  [State w (p CppSrcCode CppHdrCode c)] -> [State w (p CppSrcCode CppHdrCode d)]
  -> [State w (p CppSrcCode CppHdrCode e)] -> 
  State w (p CppSrcCode CppHdrCode f)
pair2Vals3Lists srcf hdrf stv1 stv2 stv3 stv4 stv5 = do
  v1 <- stv1
  let fv1 = toState $ pfst v1
      sv1 = toState $ psnd v1
  pair1Val3Lists (srcf fv1) (hdrf sv1) stv2 stv3 stv4 stv5

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
  fileDoc m = modify (setFileType Source) >> G.fileDoc cppSrcExt top bottom m

  docMod = G.docMod cppSrcExt

instance InternalFile CppSrcCode where
  top _ = toCode empty
  bottom = toCode empty

  commentedMod cmnt mod = on3StateValues (\m cmt mn -> if mn then on2CodeValues 
    commentedModD m cmt else m) mod cmnt getCurrMain
  
  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance ImportSym CppSrcCode where
  type Import CppSrcCode = Doc
  langImport n = toCode $ inc <+> angles (text n)
  modImport n = toCode $ inc <+> doubleQuotedText (addExt cppHdrExt 
    n)

  importDoc = unCPPSC

instance PermanenceSym CppSrcCode where
  type Permanence CppSrcCode = BindData
  static = toCode $ bd Static staticDocD
  dynamic = toCode $ bd Dynamic dynamicDocD
  
instance InternalPerm CppSrcCode where
  permDoc = bindDoc . unCPPSC
  binding = bind . unCPPSC

instance BodySym CppSrcCode where
  type Body CppSrcCode = Doc
  body = onStateList (onCodeList bodyDocD)

  addComments s = onStateValue (onCodeValue (addCommentsDocD s commentStart))

instance InternalBody CppSrcCode where
  bodyDoc = unCPPSC
  docBody = onStateValue toCode
  multiBody = G.multiBody 

instance BlockSym CppSrcCode where
  type Block CppSrcCode = Doc
  block = G.block

instance InternalBlock CppSrcCode where
  blockDoc = unCPPSC
  docBlock = onStateValue toCode
  multiBlock = G.multiBlock

instance TypeSym CppSrcCode where
  type Type CppSrcCode = TypeData
  bool = cppBoolType
  int = G.int
  float = G.float
  double = G.double
  char = G.char
  string = modify (addUsing "string" . addLangImportVS "string") >> G.string
  infile = modify (addUsing "ifstream") >> cppInfileType
  outfile = modify (addUsing "ofstream") >> cppOutfileType
  listType t = modify (addUsing vec . addLangImportVS vec) >> G.listType vec t
    where vec = "vector"
  arrayType = cppArrayType
  listInnerType = G.listInnerType
  obj n = zoom lensVStoMS getClassName >>= (\cn -> if cn == n then G.obj n else 
    getClassMap >>= (\cm -> maybe id ((>>) . modify . addModuleImportVS) 
    (Map.lookup n cm) (G.obj n)))
  -- enumType = G.enumType
  funcType = G.funcType
  iterator t = modify (addLangImportVS "iterator") >> (cppIterType . listType) t
  void = G.void

  getType = cType . unCPPSC
  getTypeString = typeString . unCPPSC
  
instance InternalType CppSrcCode where
  getTypeDoc = typeDoc . unCPPSC
  typeFromData t s d = toCode $ td t s d

instance ControlBlock CppSrcCode where
  runStrategy = G.runStrategy

instance InternalControlBlock CppSrcCode where
  listSlice' = G.listSlice

instance UnaryOpSym CppSrcCode where
  type UnaryOp CppSrcCode = OpData
  notOp = G.notOp
  negateOp = G.negateOp
  sqrtOp = addMathHImport G.sqrtOp
  absOp = addMathHImport G.absOp
  logOp = addMathHImport $ unOpPrec "log10"
  lnOp = addMathHImport $ unOpPrec "log"
  expOp = addMathHImport G.expOp
  sinOp = addMathHImport G.sinOp
  cosOp = addMathHImport G.cosOp
  tanOp = addMathHImport G.tanOp
  asinOp = addMathHImport G.asinOp
  acosOp = addMathHImport G.acosOp
  atanOp = addMathHImport G.atanOp
  floorOp = addMathHImport $ unOpPrec "floor"
  ceilOp = addMathHImport $ unOpPrec "ceil"

instance BinaryOpSym CppSrcCode where
  type BinaryOp CppSrcCode = OpData
  equalOp = G.equalOp
  notEqualOp = G.notEqualOp
  greaterOp = G.greaterOp
  greaterEqualOp = G.greaterEqualOp
  lessOp = G.lessOp
  lessEqualOp = G.lessEqualOp
  plusOp = G.plusOp
  minusOp = G.minusOp
  multOp = G.multOp
  divideOp = G.divideOp
  powerOp = addMathHImport G.powerOp
  moduloOp = G.moduloOp
  andOp = G.andOp
  orOp = G.orOp

instance InternalOp CppSrcCode where
  uOpDoc = opDoc . unCPPSC
  bOpDoc = opDoc . unCPPSC
  uOpPrec = opPrec . unCPPSC
  bOpPrec = opPrec . unCPPSC
  
  uOpFromData p d = toState $ toCode $ od p d
  bOpFromData p d = toState $ toCode $ od p d

instance VariableSym CppSrcCode where
  type Variable CppSrcCode = VarData
  var = G.var
  staticVar = G.staticVar
  const = var
  extVar l n t = modify (addModuleImportVS l) >> var n t
  self = G.self
  -- enumVar = G.enumVar
  classVar = on2StateValues (\c v -> classVarCheckStatic (varFromData 
    (variableBind v) (getTypeString c ++ "::" ++ variableName v) 
    (variableType v) (cppClassVar (getTypeDoc c) (variableDoc v))))
  extClassVar c v = join $ on2StateValues (\t cm -> maybe id ((>>) . modify . 
    addModuleImportVS) (Map.lookup (getTypeString t) cm) $ 
    classVar (toState t) v) c getClassMap
  objVar = G.objVar
  objVarSelf = onStateValue (\v -> mkVar ("this->"++variableName v) 
    (variableType v) (text "this->" <> variableDoc v))
  listVar = G.listVar
  arrayElem i = G.arrayElem (litInt i)
  iterVar l t = mkStateVar l (iterator t) (text $ "(*" ++ l ++ ")")

  variableName = varName . unCPPSC
  variableType = onCodeValue varType

instance InternalVariable CppSrcCode where
  variableBind = varBind . unCPPSC
  variableDoc = varDoc . unCPPSC
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CppSrcCode where
  type Value CppSrcCode = ValData
  litTrue = G.litTrue
  litFalse = G.litFalse
  litChar = G.litChar
  litDouble = G.litDouble
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = G.litArray
  litList _ _ = error $ "List literals not supported in " ++ cppName

  pi = modify (addDefine "_USE_MATH_DEFINES") >> addMathHImport (mkStateVal 
    double (text "M_PI"))

  -- ($:) = enumElement

  valueOf = G.valueOf
  arg n = G.arg (litInt $ n+1) argsList
  -- enumElement en e = mkStateVal (enumType en) (text e)
  
  argsList = G.argsList "argv"

  valueType = onCodeValue valType

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
  csc = G.csc
  sec = G.sec
  cot = G.cot
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

  funcAppMixedArgs = G.funcAppMixedArgs
  selfFuncAppMixedArgs = G.selfFuncAppMixedArgs (text "->") self
  extFuncAppMixedArgs l n t vs ns = modify (addModuleImportVS l) >> 
    funcAppMixedArgs n t vs ns
  libFuncAppMixedArgs = G.libFuncAppMixedArgs
  newObjMixedArgs = G.newObjMixedArgs ""
  extNewObjMixedArgs l t vs ns = modify (addModuleImportVS l) >> 
    newObjMixedArgs t vs ns
  libNewObjMixedArgs = G.libNewObjMixedArgs

  lambda = G.lambda cppLambda

  notNull v = v

instance InternalValue CppSrcCode where
  inputFunc = addIOStreamImport $ mkStateVal string (text "std::cin")
  printFunc = addIOStreamImport $ mkStateVal void (text "std::cout")
  printLnFunc = addIOStreamImport $ mkStateVal void (text "std::cout")
  printFileFunc = on2StateValues (\v -> mkVal v . valueDoc) void
  printFileLnFunc = on2StateValues (\v -> mkVal v . valueDoc) void

  cast = cppCast

  call = G.call' cppName

  valuePrec = valPrec . unCPPSC
  valueDoc = val . unCPPSC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CppSrcCode where
  objAccess = G.objAccess

  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf l v = addAlgorithmImportVS $ funcApp "find" int 
    [iterBegin l, iterEnd l, v] #- iterBegin l
  
instance InternalValueExp CppSrcCode where
  objMethodCallMixedArgs' = G.objMethodCall
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

instance InternalFunction CppSrcCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

  listSizeFunc = G.listSizeFunc
  listAddFunc l i v = func "insert" (listType $ onStateValue valueType v) 
    [iterBegin l #+ i, v]
  listAppendFunc = G.listAppendFunc "push_back"

  iterBeginFunc t = func "begin" (iterator t) []
  iterEndFunc t = func "end" (iterator t) []

  listAccessFunc = G.listAccessFunc' "at"
  listSetFunc = G.listSetFunc cppListSetDoc

  functionType = onCodeValue fType
  functionDoc = funcDoc . unCPPSC
  
  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance InternalStatement CppSrcCode where
  printSt nl _ = cppPrint nl

  multiAssign _ _ = error $ G.multiAssignError cppName
  multiReturn _ = error $ G.multiReturnError cppName

  state = G.state
  loopState = G.loopState

  emptyState = G.emptyState
  statementDoc = fst . unCPPSC
  statementTerm = snd . unCPPSC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CppSrcCode where
  type Statement CppSrcCode = (Doc, Terminator)
  assign = G.assign Semi
  (&-=) = G.decrement
  (&+=) = G.increment
  (&++) = G.increment1
  (&--) = G.decrement1

  varDec = G.varDec static dynamic empty
  varDecDef = G.varDecDef 
  listDec n = G.listDec cppListDecDoc (litInt n)
  listDecDef = G.listDecDef cppListDecDefDoc
  arrayDec n vr = zoom lensMStoVS $ on2StateValues (\sz v -> mkSt $ getTypeDoc 
    (variableType v) <+> variableDoc v <> brackets (valueDoc sz)) 
    (litInt n :: SValue CppSrcCode) vr
  arrayDecDef vr vals = on2StateValues (\vdc vs -> mkSt $ statementDoc vdc <+> 
    equals <+> braces (valueList vs)) (arrayDec (toInteger $ length vals) vr) 
    (mapM (zoom lensMStoVS) vals)
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = G.extObjDecNew
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams = G.extObjDecNewNoParams
  constDecDef = G.constDecDef
  funcDecDef = G.funcDecDef

  print = outDoc False Nothing printFunc
  printLn = outDoc True Nothing printLnFunc
  printStr = outDoc False Nothing printFunc . litString
  printStrLn = outDoc True Nothing printLnFunc . litString

  printFile f = outDoc False (Just f) (printFileFunc f)
  printFileLn f = outDoc True (Just f) (printFileLnFunc f)
  printFileStr f = outDoc False (Just f) (printFileFunc f) . litString
  printFileStrLn f = outDoc True (Just f) (printFileLnFunc f) . litString

  getInput v = cppInput v inputFunc
  discardInput = addAlgorithmImport $ addLimitsImport $ G.discardInput 
    (cppDiscardInput "\\n")
  getFileInput f v = cppInput v f
  discardFileInput f = addAlgorithmImport $ addLimitsImport $ 
    G.discardFileInput (cppDiscardInput " ") f

  openFileR f' v' = zoom lensMStoVS $ on2StateValues (\f v -> mkSt $ 
    cppOpenFile "std::fstream::in" f v) f' v'
  openFileW f' v' = zoom lensMStoVS $ on2StateValues (\f v -> mkSt $ 
    cppOpenFile "std::fstream::out" f v) f' v' 
  openFileA f' v' = zoom lensMStoVS $ on2StateValues (\f v -> mkSt $ 
    cppOpenFile "std::fstream::app" f v) f' v'
  closeFile = G.closeFile "close" 

  getFileInputLine f v = valState $ funcApp "std::getline" string [f, valueOf v]
  discardFileLine f = addLimitsImport $ zoom lensMStoVS $ onStateValue (mkSt .
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

  valState = G.valState Semi

  comment = G.comment commentStart

  free = onStateValue (mkSt . freeDocD) . zoom lensMStoVS

  throw = G.throw cppThrowDoc Semi

  inOutCall = cppInOutCall funcApp
  selfInOutCall = cppInOutCall selfFuncApp
  extInOutCall m = cppInOutCall (extFuncApp m)

  multi = onStateList (onCodeList multiStateDocD)

instance ControlStatement CppSrcCode where
  ifCond = G.ifCond bodyStart elseIfLabel bodyEnd
  switch = G.switch

  ifExists _ ifBody _ = onStateValue (mkStNoEnd . bodyDoc) ifBody -- All variables are initialized in C++

  for = G.for bodyStart bodyEnd 
  forRange = G.forRange
  forEach i v = for (varDecDef e (iterBegin v)) (valueOf e ?!= iterEnd v) 
    (e &++)
    where e = toBasicVar i
  while = G.while bodyStart bodyEnd

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
  scopeFromData s d = toCode (d, s)

instance MethodTypeSym CppSrcCode where
  type MethodType CppSrcCode = TypeData
  mType = zoom lensMStoVS
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
  constructor = cppConstructor

  docMain b = commentedFunc (docComment $ toState $ functionDox 
    "Controls the flow of the program" 
    [("argc", "Number of command-line arguments"),
    ("argv", "List of command-line arguments")] ["exit code"]) (mainFunction b)

  function = G.function
  mainFunction b = intFunc True "main" public static (mType int) 
    [param argc, param argv]
    (on2StateValues (on2CodeValues appendToBody) b (returnState $ litInt 0))
    where argc = var "argc" int
          argv = toState $ mkVar "argv" (typeFromData (List String) 
            "const char" (text "const char")) (text "*argv[]")

  docFunc = G.docFunc

  inOutMethod n = cppsInOut (method n)

  docInOutMethod n = G.docInOutFunc (inOutMethod n)

  inOutFunc n = cppsInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CppSrcCode where
  intMethod m n s _ t ps b = modify (setScope (snd $ unCPPSC s) . if m then 
    setCurrMain else id) >> (\c tp pms bod -> methodFromData 
    (snd $ unCPPSC s) $ cppsMethod [] n c tp pms bod) <$> getClassName <*> t 
    <*> sequence ps <*> b
  intFunc m n s _ t ps b = modify (setScope (snd $ unCPPSC s) . if m then 
    setCurrMainFunc m . setCurrMain else id) >> on3StateValues (\tp pms bod -> 
    methodFromData (snd $ unCPPSC s) $ cppsFunction n tp pms bod) t 
    (sequence ps) b
  commentedFunc = cppCommentedFunc Source
  
  destructor vs = 
    let i = var "i" int
        deleteStatements = map (onStateValue (onCodeValue destructSts) . 
          zoom lensMStoCS) vs
        loopIndexDec = varDec i
        dbody = on2StateValues (on2CodeValues emptyIfEmpty)
          (onStateList (onCodeList (vcat . map fst)) deleteStatements) $
          bodyStatements $ loopIndexDec : deleteStatements
    in getClassName >>= (\n -> pubMethod ('~':n) void [] dbody)
 
  methodDoc = mthdDoc . unCPPSC
  methodFromData s d = toCode $ mthd s d

instance StateVarSym CppSrcCode where
  type StateVar CppSrcCode = StateVarData
  stateVar s _ _ = onStateValue (on3CodeValues svd (onCodeValue snd s) (toCode 
    empty)) $ zoom lensCStoMS emptyState
  stateVarDef n s p v vl' = on3StateValues (\vr vl -> on3CodeValues svd 
    (onCodeValue snd s) (cppsStateVarDef n empty <$> p <*> vr <*> vl)) 
    (zoom lensCStoVS v) (zoom lensCStoVS vl') (zoom lensCStoMS emptyState)
  constVar n s v vl' = on3StateValues (\vr vl -> on3CodeValues svd (onCodeValue 
    snd s) (cppsStateVarDef n (text "const") <$> static <*> vr <*> vl)) 
    (zoom lensCStoVS v) (zoom lensCStoVS vl') (zoom lensCStoMS emptyState)

instance InternalStateVar CppSrcCode where
  stateVarDoc = stVar . unCPPSC
  stateVarFromData = error "stateVarFromData unimplemented in C++"

instance ClassSym CppSrcCode where
  type Class CppSrcCode = Doc
  buildClass = G.buildClass
  -- enum _ _ _ = toState $ toCode empty
  extraClass = buildClass
  implementingClass = G.implementingClass

  docClass = G.docClass

instance InternalClass CppSrcCode where
  intClass n _ _ vs fs = modify (setClassName n) >> on2StateLists cppsClass 
    vs (map (zoom lensCStoMS) $ fs ++ [destructor vs])

  inherit n = onCodeValue (cppInherit n . fst) public
  implements is = onCodeValue ((\p -> colon <+> hcat (map ((p <+>) . text) is)) 
    . fst) public

  commentedClass _ cs = cs

  classDoc = unCPPSC
  classFromData d = d

instance ModuleSym CppSrcCode where
  type Module CppSrcCode = ModData
  buildModule n is ms cs = G.buildModule n ((\ds lis libis mis us mn -> vibcat [
    if mn && length ms + length cs == 1 then empty else importDoc $ mi n,
    vcat (map ((text "#define" <+>) . text) ds),
    vcat (map (importDoc . li) lis),
    vcat (map (importDoc . mi) (sort (is ++ libis) ++ mis)),
    vcat (map (\i -> usingNameSpace "std" (Just i)) us)]) 
    <$> getDefines <*> getLangImports <*> getLibImports <*> getModuleImports 
    <*> getUsing <*> getCurrMain) (toState empty) ms cs
    where mi, li :: Label -> CppSrcCode (Import CppSrcCode)
          mi = modImport
          li = langImport

instance InternalMod CppSrcCode where
  moduleDoc = modDoc . unCPPSC
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance BlockCommentSym CppSrcCode where
  type BlockComment CppSrcCode = Doc
  blockComment lns = toCode $ blockCmtDoc lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ docCmtDoc lns docCmtStart 
    blockCmtEnd)

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
  fileDoc m = modify (setFileType Header) >> G.fileDoc cppHdrExt top bottom m
  
  docMod = G.docMod cppHdrExt

instance InternalFile CppHdrCode where
  top = onCodeValue cpphtop
  bottom = toCode $ text "#endif"
  
  commentedMod cmnt mod = on2StateValues (\m cmt -> if isEmpty (moduleDoc $ 
    onCodeValue fileMod m) then m else on2CodeValues commentedModD m cmt) 
    mod cmnt

  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance ImportSym CppHdrCode where
  type Import CppHdrCode = Doc
  langImport n = toCode $ inc <+> angles (text n)
  modImport n = toCode $ inc <+> doubleQuotedText (addExt cppHdrExt 
    n)

  importDoc = unCPPHC

instance PermanenceSym CppHdrCode where
  type Permanence CppHdrCode = BindData
  static = toCode $ bd Static staticDocD
  dynamic = toCode $ bd Dynamic dynamicDocD

instance InternalPerm CppHdrCode where
  permDoc = bindDoc . unCPPHC
  binding = bind . unCPPHC

instance BodySym CppHdrCode where
  type Body CppHdrCode = Doc
  body _ = toState $ toCode empty

  addComments _ _ = toState $ toCode empty

instance InternalBody CppHdrCode where

  bodyDoc = unCPPHC
  docBody = onStateValue toCode
  multiBody = G.multiBody 

instance BlockSym CppHdrCode where
  type Block CppHdrCode = Doc
  block _ = toState $ toCode empty

instance InternalBlock CppHdrCode where
  blockDoc = unCPPHC
  docBlock = onStateValue toCode
  multiBlock = G.multiBlock

instance TypeSym CppHdrCode where
  type Type CppHdrCode = TypeData
  bool = cppBoolType
  int = G.int
  float = G.float
  double = G.double
  char = G.char
  string = modify (addHeaderUsing "string" . addHeaderLangImport "string") >> 
    G.string
  infile = modify (addHeaderUsing "ifstream") >> cppInfileType
  outfile = modify (addHeaderUsing "ofstream") >> cppOutfileType
  listType t = modify (addHeaderUsing vec . addHeaderLangImport vec) >> 
    G.listType vec t
    where vec = "vector"
  arrayType = cppArrayType
  listInnerType = G.listInnerType
  obj n = getClassMap >>= (\cm -> maybe id ((>>) . modify . addHeaderModImport) 
    (Map.lookup n cm) $ G.obj n)
  -- enumType = G.enumType
  funcType = G.funcType
  iterator t = modify (addHeaderLangImport "iterator") >> 
    (cppIterType . listType) t
  void = G.void

  getType = cType . unCPPHC
  getTypeString = typeString . unCPPHC
  
instance InternalType CppHdrCode where
  getTypeDoc = typeDoc . unCPPHC
  typeFromData t s d = toCode $ td t s d

instance ControlBlock CppHdrCode where
  runStrategy _ _ _ _ = toState $ toCode empty

instance InternalControlBlock CppHdrCode where
  listSlice' _ _ _ _ _ = toState $ toCode empty

instance UnaryOpSym CppHdrCode where
  type UnaryOp CppHdrCode = OpData
  notOp = uOpFromData 0 empty
  negateOp = uOpFromData 0 empty
  sqrtOp = uOpFromData 0 empty
  absOp = uOpFromData 0 empty
  logOp = uOpFromData 0 empty
  lnOp = uOpFromData 0 empty
  expOp = uOpFromData 0 empty
  sinOp = uOpFromData 0 empty
  cosOp = uOpFromData 0 empty
  tanOp = uOpFromData 0 empty
  asinOp = uOpFromData 0 empty
  acosOp = uOpFromData 0 empty
  atanOp = uOpFromData 0 empty
  floorOp = uOpFromData 0 empty
  ceilOp = uOpFromData 0 empty

instance BinaryOpSym CppHdrCode where
  type BinaryOp CppHdrCode = OpData
  equalOp = bOpFromData 0 empty
  notEqualOp = bOpFromData 0 empty
  greaterOp = bOpFromData 0 empty
  greaterEqualOp = bOpFromData 0 empty
  lessOp = bOpFromData 0 empty
  lessEqualOp = bOpFromData 0 empty
  plusOp = bOpFromData 0 empty
  minusOp = bOpFromData 0 empty
  multOp = bOpFromData 0 empty
  divideOp = bOpFromData 0 empty
  powerOp = bOpFromData 0 empty
  moduloOp = bOpFromData 0 empty
  andOp = bOpFromData 0 empty
  orOp = bOpFromData 0 empty

instance InternalOp CppHdrCode where
  uOpDoc = opDoc . unCPPHC
  bOpDoc = opDoc . unCPPHC
  uOpPrec = opPrec . unCPPHC
  bOpPrec = opPrec . unCPPHC
  
  uOpFromData p d = toState $ toCode $ od p d
  bOpFromData p d = toState $ toCode $ od p d

instance VariableSym CppHdrCode where
  type Variable CppHdrCode = VarData
  var = G.var
  staticVar = G.staticVar
  const _ _ = mkStateVar "" void empty
  extVar _ _ _ = mkStateVar "" void empty
  self = mkStateVar "" void empty
  -- enumVar _ _ = mkStateVar "" void empty
  classVar _ _ = mkStateVar "" void empty
  extClassVar _ _ = mkStateVar "" void empty
  objVar = G.objVar
  objVarSelf _ = mkStateVar "" void empty
  listVar _ _ = mkStateVar "" void empty
  arrayElem _ _ = mkStateVar "" void empty
  iterVar _ _ = mkStateVar "" void empty
  
  variableName = varName . unCPPHC
  variableType = onCodeValue varType

instance InternalVariable CppHdrCode where
  variableBind = varBind . unCPPHC
  variableDoc = varDoc . unCPPHC
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CppHdrCode where
  type Value CppHdrCode = ValData
  litTrue = G.litTrue
  litFalse = G.litFalse
  litChar = G.litChar
  litDouble = G.litDouble
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = G.litArray
  litList _ _ = error $ "List literals not supported in " ++ cppName

  pi = modify (addHeaderDefine "_USE_MATH_DEFINES" . addHeaderLangImport 
    "math.h") >> mkStateVal double (text "M_PI")

  -- ($:) = enumElement

  valueOf = G.valueOf
  arg n = G.arg (litInt $ n+1) argsList
  -- enumElement en e = mkStateVal (enumType en) (text e)
  
  argsList = G.argsList "argv"

  valueType = onCodeValue valType

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

  funcAppMixedArgs _ _ _ _ = mkStateVal void empty
  selfFuncAppMixedArgs _ _ _ _ = mkStateVal void empty
  extFuncAppMixedArgs _ _ _ _ _ = mkStateVal void empty
  libFuncAppMixedArgs _ _ _ _ _ = mkStateVal void empty
  newObjMixedArgs _ _ _ = mkStateVal void empty
  extNewObjMixedArgs _ _ _ _ = mkStateVal void empty
  libNewObjMixedArgs _ _ _ _ = mkStateVal void empty

  lambda _ _ = mkStateVal void empty

  notNull _ = mkStateVal void empty

instance InternalValue CppHdrCode where
  inputFunc = mkStateVal void empty
  printFunc = mkStateVal void empty
  printLnFunc = mkStateVal void empty
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty
  
  cast _ _ = mkStateVal void empty
  
  call _ _ _ _ _ _ = mkStateVal void empty

  valuePrec = valPrec . unCPPHC
  valueDoc = val . unCPPHC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CppHdrCode where
  objAccess _ _ = mkStateVal void empty
  
  argExists _ = mkStateVal void empty
  
  indexOf _ _ = mkStateVal void empty
  
instance InternalValueExp CppHdrCode where
  objMethodCallMixedArgs' _ _ _ _ _ = mkStateVal void empty
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
  
  functionType = onCodeValue fType
  functionDoc = funcDoc . unCPPHC
  
  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance InternalStatement CppHdrCode where
  printSt _ _ _ _ = emptyState
  
  multiAssign _ _ = emptyState
  multiReturn _ = emptyState

  state = G.state
  loopState _ = emptyState

  emptyState = G.emptyState
  statementDoc = fst . unCPPHC
  statementTerm = snd . unCPPHC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CppHdrCode where
  type Statement CppHdrCode = (Doc, Terminator)
  assign _ _ = emptyState
  (&-=) _ _ = emptyState
  (&+=) _ _ = emptyState
  (&++) _ = emptyState
  (&--) _ = emptyState

  varDec = G.varDec static dynamic empty
  varDecDef = G.varDecDef
  listDec _ _ = emptyState
  listDecDef _ _ = emptyState
  arrayDec _ _ = emptyState
  arrayDecDef _ _ = emptyState
  objDecDef _ _ = emptyState
  objDecNew _ _ = emptyState
  extObjDecNew _ _ _ = emptyState
  objDecNewNoParams _ = emptyState
  extObjDecNewNoParams _ _ = emptyState
  constDecDef = G.constDecDef
  funcDecDef _ _ _ = emptyState

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

  valState _ = emptyState

  comment _ = emptyState

  free _ = emptyState

  throw _ = emptyState

  inOutCall _ _ _ _ = emptyState
  selfInOutCall _ _ _ _ = emptyState
  extInOutCall _ _ _ _ _ = emptyState

  multi _ = emptyState

instance ControlStatement CppHdrCode where
  ifCond _ _ = emptyState
  switch _ _ _ = emptyState

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
  scopeFromData s d = toCode (d, s)

instance MethodTypeSym CppHdrCode where
  type MethodType CppHdrCode = TypeData
  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym CppHdrCode where
  type Parameter CppHdrCode = ParamData
  param = onStateValue (\v -> paramFromData v (paramDocD v)) . zoom lensMStoVS
  pointerParam = onStateValue (\v -> paramFromData v (cppPointerParamDoc v)) .
    zoom lensMStoVS

instance InternalParam CppHdrCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameterDoc = paramDoc . unCPPHC
  paramFromData v d = on2CodeValues pd v (toCode d)

instance MethodSym CppHdrCode where
  type Method CppHdrCode = MethodData
  method = G.method
  getMethod v = zoom lensMStoVS v >>= (\v' -> method (getterName $ variableName 
    v') public dynamic (toState $ variableType v') [] (toState $ toCode empty))
  setMethod v = zoom lensMStoVS v >>= (\v' -> method (setterName $ variableName 
    v') public dynamic void [param v] (toState $ toCode empty))
  constructor ps is b = getClassName >>= (\n -> G.constructor n ps is b)

  docMain = mainFunction

  function = G.function
  mainFunction _ = modifyReturn (setScope Pub) $ toCode $ mthd Pub empty

  docFunc = G.docFunc

  inOutMethod n = cpphInOut (method n)

  docInOutMethod n = G.docInOutFunc (inOutMethod n)

  inOutFunc n = cpphInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CppHdrCode where
  intMethod _ n s _ t ps _ = modify (setScope (snd $ unCPPHC s)) >> 
    on1StateValue1List (\tp pms -> methodFromData (snd $ unCPPHC s) $ 
    cpphMethod n tp pms) t ps
  intFunc = G.intFunc
  commentedFunc = cppCommentedFunc Header

  destructor vars = on1StateValue1List (\m vs -> toCode $ mthd Pub 
    (emptyIfEmpty (vcat (map (statementDoc . onCodeValue destructSts) vs)) 
    (methodDoc m))) (getClassName >>= (\n -> pubMethod ('~':n) void [] 
    (toState (toCode empty)) :: SMethod CppHdrCode)) 
    (map (zoom lensMStoCS) vars)

  methodDoc = mthdDoc . unCPPHC
  methodFromData s d = toCode $ mthd s d

instance StateVarSym CppHdrCode where
  type StateVar CppHdrCode = StateVarData
  stateVar s p v = on2StateValues (\dec -> on3CodeValues svd (onCodeValue snd s)
    (toCode $ stateVarDocD empty (permDoc p) (statementDoc dec)))
    (zoom lensCStoMS $ state $ G.varDec static dynamic (text "&") v) 
    (zoom lensCStoMS emptyState)
  stateVarDef _ s p vr vl = on2StateValues (onCodeValue . svd (snd $ unCPPHC s))
    (cpphStateVarDef empty p vr vl) (zoom lensCStoMS emptyState)
  constVar _ s vr _ = on2StateValues (on3CodeValues svd (onCodeValue snd s) . 
    on2CodeValues (constVarDocD empty endStatement) (bindDoc <$> static))
    (zoom lensCStoVS vr) (zoom lensCStoMS emptyState)

instance InternalStateVar CppHdrCode where
  stateVarDoc = stVar . unCPPHC
  stateVarFromData = error "stateVarFromData unimplemented in C++"

instance ClassSym CppHdrCode where
  type Class CppHdrCode = Doc
  buildClass = G.buildClass
  -- enum n es _ = modify (setClassName n) >> cpphEnum n (enumElementsDocD es 
  --   enumsEqualInts) bodyStart bodyEnd endStatement
  extraClass = buildClass
  implementingClass = G.implementingClass

  docClass = G.docClass

instance InternalClass CppHdrCode where
  intClass n _ i vs mths = modify (setClassName n) >> on2StateLists 
    (\vars funcs -> cpphClass n i vars funcs public private) vs fs
    where fs = map (zoom lensCStoMS) $ mths ++ [destructor vs]

  inherit n = onCodeValue (cppInherit n . fst) public
  implements is = onCodeValue ((\p -> colon <+> hcat (map ((p <+>) . text) is)) 
    . fst) public

  commentedClass = G.commentedClass

  classDoc = unCPPHC
  classFromData d = d

instance ModuleSym CppHdrCode where
  type Module CppHdrCode = ModData
  buildModule n is = G.buildModule n ((\ds lis libis mis us -> vibcat [
    vcat (map ((text "#define" <+>) . text) ds),
    vcat (map (importDoc . li) lis),
    vcat (map (importDoc . mi) (sort (is ++ libis) ++ mis)),
    vcat (map (\i -> usingNameSpace "std" (Just i)) us)]) 
    <$> getHeaderDefines <*> getHeaderLangImports <*> getHeaderLibImports <*> 
    getHeaderModImports <*> getHeaderUsing) (toState empty)
    where mi, li :: Label -> CppHdrCode (Import CppHdrCode)
          mi = modImport
          li = langImport

instance InternalMod CppHdrCode where
  moduleDoc = modDoc . unCPPHC
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance BlockCommentSym CppHdrCode where
  type BlockComment CppHdrCode = Doc
  blockComment lns = toCode $ blockCmtDoc lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ docCmtDoc lns docCmtStart 
    blockCmtEnd)

  blockCommentDoc = unCPPHC

-- helpers
toBasicVar :: SVariable CppSrcCode -> 
  SVariable CppSrcCode
toBasicVar v = v >>= (\v' -> var (variableName v') (onStateValue variableType v))

isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

getParam :: (RenderSym repr) => SVariable repr -> 
  MSParameter repr
getParam v = zoom lensMStoVS v >>= (\v' -> getParamFunc ((getType . 
  variableType) v') v)
  where getParamFunc (List _) = pointerParam
        getParamFunc (Object _) = pointerParam
        getParamFunc _ = param
 
data MethodData = MthD {getMthdScp :: ScopeTag, mthdDoc :: Doc}

mthd :: ScopeTag -> Doc -> MethodData
mthd = MthD 

algorithmImport :: String
algorithmImport = "algorithm"

addAlgorithmImport :: MS a -> MS a
addAlgorithmImport = (>>) $ modify (addLangImport algorithmImport)

addAlgorithmImportVS :: VS a -> VS a
addAlgorithmImportVS = (>>) $ modify (addLangImportVS algorithmImport)

addFStreamImport :: a -> VS a
addFStreamImport = modifyReturn (addLangImportVS "fstream")

addIOStreamImport :: VS a -> VS a
addIOStreamImport = (>>) $ modify (addLangImportVS "iostream")

addMathHImport :: VS a -> VS a
addMathHImport = (>>) $ modify (addLangImportVS "math.h")

addLimitsImport :: MS a -> MS a
addLimitsImport = (>>) $ modify (addLangImport "limits")

-- convenience
cppName :: String
cppName = "C++" 

-- enumsEqualInts :: Bool
-- enumsEqualInts = False

inc :: Doc
inc = text "#include"

cpphtop :: ModData -> Doc
cpphtop m = vcat [
  text "#ifndef" <+> text n <> text "_h",
  text "#define" <+> text n <> text "_h"]
  where n = name m

usingNameSpace :: Label -> Maybe Label -> Doc
usingNameSpace n (Just m) = text "using" <+> text n <> colon <> colon <>
  text m <> endStatement
usingNameSpace n Nothing = text "using namespace" <+> text n <> endStatement

cppInherit :: Maybe Label -> Doc -> Doc
cppInherit n pub = maybe empty ((colon <+> pub <+>) . text) n

cppBoolType :: (RenderSym repr) => VSType repr
cppBoolType = toState $ typeFromData Boolean "bool" (text "bool")

cppInfileType :: (RenderSym repr) => VSType repr
cppInfileType = addFStreamImport $ typeFromData File "ifstream" 
  (text "ifstream")

cppOutfileType :: (RenderSym repr) => VSType repr
cppOutfileType = addFStreamImport $ typeFromData File "ofstream" 
  (text "ofstream")

cppArrayType :: (RenderSym repr) => VSType repr -> VSType repr
cppArrayType = onStateValue (\t -> typeFromData (Array (getType t)) 
  (getTypeString t) (getTypeDoc t))

cppIterType :: (RenderSym repr) => VSType repr -> VSType repr
cppIterType = onStateValue (\t -> typeFromData (Iterator (getType t)) 
  (getTypeString t ++ "::iterator") (text "std::" <> getTypeDoc t <> text 
  "::iterator"))

cppClassVar :: Doc -> Doc -> Doc
cppClassVar c v = c <> text "::" <> v

cppLambda :: (RenderSym repr) => [repr (Variable repr)] -> repr (Value repr) -> 
  Doc
cppLambda ps ex = text "[]" <+> parens (hicat (text ",") $ zipWith (<+>) 
  (map (getTypeDoc . variableType) ps) (map variableDoc ps)) <+> text "->" <+> 
  bodyStart <> text "return" <+> valueDoc ex <> endStatement <> bodyEnd

cppCast :: VSType CppSrcCode -> SValue CppSrcCode -> SValue CppSrcCode
cppCast t v = join $ on2StateValues (\tp vl -> cppCast' (getType tp) (getType $ 
  valueType vl) tp vl) t v
  where cppCast' Double String _ _ = funcApp "std::stod" double [v]
        cppCast' Float String _ _ = funcApp "std::stof" float [v]
        cppCast' _ _ tp vl = mkStateVal t (castObjDocD (castDocD 
          (getTypeDoc tp)) (valueDoc vl))

cppListSetDoc :: Doc -> Doc -> Doc
cppListSetDoc i v = dot <> text "at" <> parens i <+> equals <+> v

cppListDecDoc :: (RenderSym repr) => repr (Value repr) -> Doc
cppListDecDoc n = parens (valueDoc n)

cppListDecDefDoc :: (RenderSym repr) => [repr (Value repr)] -> Doc
cppListDecDefDoc vs = braces (valueList vs)

cppPrint :: (RenderSym repr) => Bool -> SValue repr -> SValue repr -> 
  MSStatement repr
cppPrint newLn  pf vl = zoom lensMStoVS $ on3StateValues (\e printFn v -> mkSt 
  $ valueDoc printFn <+> text "<<" <+> pars v (valueDoc v) <+> e) end pf vl
  where pars v = if maybe False (< 9) (valuePrec v) then parens else id
        end = if newLn then addIOStreamImport (toState $ text "<<" <+> 
          text "std::endl") else toState empty

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

cppInput :: (RenderSym repr) => SVariable repr -> SValue repr -> 
  MSStatement repr
cppInput vr i = addAlgorithmImport $ addLimitsImport $ zoom lensMStoVS $ 
  on2StateValues (\v inFn -> mkSt $ vcat [valueDoc inFn <+> text ">>" <+> 
  variableDoc v <> endStatement, valueDoc inFn <> dot <> 
    text "ignore(std::numeric_limits<std::streamsize>::max(), '\\n')"]) vr i

cppOpenFile :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  repr (Value repr) -> Doc
cppOpenFile mode f n = variableDoc f <> dot <> text "open" <> 
  parens (valueDoc n <> comma <+> text mode)

cppPointerParamDoc :: (RenderSym repr) => repr (Variable repr) -> Doc
cppPointerParamDoc v = getTypeDoc (variableType v) <+> text "&" <> variableDoc v

cppsMethod :: [Doc] -> Label -> Label -> CppSrcCode (MethodType CppSrcCode) 
  -> [CppSrcCode (Parameter CppSrcCode)] -> CppSrcCode (Body CppSrcCode) -> Doc
cppsMethod is n c t ps b = emptyIfEmpty (bodyDoc b <> initList) $ 
  vcat [ttype <+> text c <> text "::" <> text n <> parens (parameterList ps) 
  <+> emptyIfEmpty initList (colon <+> initList) <+> bodyStart,
  indent (bodyDoc b),
  bodyEnd]
  where ttype | isDtor n = empty
              | otherwise = getTypeDoc t
        initList = hicat (text ", ") is

cppConstructor :: [MSParameter CppSrcCode] -> [(SVariable CppSrcCode, 
  SValue CppSrcCode)] -> MSBody CppSrcCode -> SMethod CppSrcCode
cppConstructor ps is b = getClassName >>= (\n -> join $ (\tp pms ivars ivals 
  bod -> if null is then G.constructor n ps is b else modify (setScope Pub) >> 
  toState (methodFromData Pub (cppsMethod (zipWith (\ivar ival -> variableDoc 
  ivar <> parens (valueDoc ival)) ivars ivals) n n tp pms bod))) <$> construct 
  n <*> sequence ps <*> mapM (zoom lensMStoVS . fst) is <*> mapM (zoom 
  lensMStoVS . snd) is <*> b)

cppsFunction :: (RenderSym repr) => Label -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> Doc
cppsFunction n t ps b = vcat [
  getTypeDoc t <+> text n <> parens (parameterList ps) <+> bodyStart,
  indent (bodyDoc b),
  bodyEnd]

cpphMethod :: (RenderSym repr) => Label -> repr (Type repr) ->
  [repr (Parameter repr)] -> Doc
cpphMethod n t ps = (if isDtor n then empty else getTypeDoc t) <+> text n 
  <> parens (parameterList ps) <> endStatement

cppCommentedFunc :: (RenderSym repr) => FileType -> 
  MS (repr (BlockComment repr)) -> SMethod repr -> SMethod repr
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

cppsStateVarDef :: Label -> Doc -> BindData -> VarData -> ValData -> Doc
cppsStateVarDef n cns p vr vl = onBinding (bind p) (cns <+> typeDoc 
  (varType vr) <+> text (n ++ "::") <> varDoc vr <+> equals <+> val vl <>
  endStatement) empty

cpphStateVarDef :: (RenderSym repr) => Doc -> repr (Permanence repr) -> 
  SVariable repr -> SValue repr -> CS Doc
cpphStateVarDef s p vr vl = onStateValue (stateVarDocD s (permDoc p) .  
  statementDoc) (zoom lensCStoMS $ state $ onBinding (binding p) (varDec 
  vr) (varDecDef vr vl)) 

cpphVarsFuncsList :: ScopeTag -> [CppHdrCode (StateVar CppHdrCode)] -> 
  [CppHdrCode (Method CppHdrCode)] -> Doc
cpphVarsFuncsList st vs fs = 
  let scopedVs = [stateVarDoc v | v <- vs, getStVarScp (unCPPHC v) == st]
      scopedFs = [methodDoc f | f <- fs, getMthdScp (unCPPHC f) == st]
  in vcat $ scopedVs ++ (if null scopedVs then empty else blank) : scopedFs

cppsClass :: [CppSrcCode (StateVar CppSrcCode)] -> 
  [CppSrcCode (Method CppSrcCode)] -> CppSrcCode (Class CppSrcCode)
cppsClass vs fs = toCode $ vibcat $ vcat vars : funcs
  where vars = map stateVarDoc vs
        funcs = map methodDoc fs

cpphClass :: Label -> CppHdrCode ParentSpec -> 
  [CppHdrCode (StateVar CppHdrCode)] -> [CppHdrCode (Method CppHdrCode)] -> 
  CppHdrCode (Scope CppHdrCode) -> CppHdrCode (Scope CppHdrCode) -> 
  CppHdrCode (Class CppHdrCode)
cpphClass n ps vars funcs pub priv = onCodeValue (\p -> vcat [
    classDec <+> text n <+> p <+> bodyStart,
    indentList [
      scopeDoc pub <> colon,
      indent pubs,
      blank,
      scopeDoc priv <> colon,
      indent privs],
    bodyEnd <> endStatement]) ps
  where pubs = cpphVarsFuncsList Pub vars funcs
        privs = cpphVarsFuncsList Priv vars funcs

-- cpphEnum :: (RenderSym repr) => Label -> Doc -> repr (Keyword repr) -> 
--   repr (Keyword repr) -> repr (Keyword repr) -> SClass repr
-- cpphEnum n es bStart bEnd end = classFromData $ toState $ vcat [
--   text "enum" <+> text n <+> keyDoc bStart,
--   indent es,
--   keyDoc bEnd <> keyDoc end]

cppInOutCall :: (Label -> VSType CppSrcCode -> [SValue CppSrcCode] -> 
  SValue CppSrcCode) -> Label -> [SValue CppSrcCode] -> [SVariable CppSrcCode] 
  -> [SVariable CppSrcCode] -> MSStatement CppSrcCode
cppInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out)
  ins
cppInOutCall f n ins [] [out] = assign out $ f n (onStateValue variableType out)
  (valueOf out : ins)
cppInOutCall f n ins outs both = valState $ f n void (map valueOf both ++ ins 
  ++ map valueOf outs)

cppsInOut :: (CppSrcCode (Scope CppSrcCode) -> 
    CppSrcCode (Permanence CppSrcCode) -> VSType CppSrcCode -> 
    [MSParameter CppSrcCode] -> MSBody CppSrcCode -> SMethod CppSrcCode)
  -> CppSrcCode (Scope CppSrcCode) -> CppSrcCode (Permanence CppSrcCode) -> 
  [SVariable CppSrcCode] -> [SVariable CppSrcCode] -> [SVariable CppSrcCode] -> 
  MSBody CppSrcCode -> SMethod CppSrcCode
cppsInOut f s p ins [v] [] b = f s p (onStateValue variableType v) 
  (cppInOutParams ins [v] []) (on3StateValues (on3CodeValues surroundBody) 
  (varDec v) b (returnState $ valueOf v))
cppsInOut f s p ins [] [v] b = f s p (onStateValue variableType v) 
  (cppInOutParams ins [] [v]) (on2StateValues (on2CodeValues appendToBody) b 
  (returnState $ valueOf v))
cppsInOut f s p ins outs both b = f s p void (cppInOutParams ins outs both) b

cpphInOut :: (CppHdrCode (Scope CppHdrCode) -> 
    CppHdrCode (Permanence CppHdrCode) -> VSType CppHdrCode -> 
    [MSParameter CppHdrCode] -> MSBody CppHdrCode -> SMethod CppHdrCode) 
  -> CppHdrCode (Scope CppHdrCode) -> CppHdrCode (Permanence CppHdrCode) -> 
  [SVariable CppHdrCode] -> [SVariable CppHdrCode] -> [SVariable CppHdrCode] -> 
  MSBody CppHdrCode -> SMethod CppHdrCode
cpphInOut f s p ins [v] [] b = f s p (onStateValue variableType v) 
  (cppInOutParams ins [v] []) b
cpphInOut f s p ins [] [v] b = f s p (onStateValue variableType v) 
  (cppInOutParams ins [] [v]) b
cpphInOut f s p ins outs both b = f s p void (cppInOutParams ins outs both) b

cppInOutParams :: (RenderSym repr) => [SVariable repr] -> [SVariable repr] -> 
  [SVariable repr] -> [MSParameter repr]
cppInOutParams ins [_] [] = map getParam ins
cppInOutParams ins [] [v] = map getParam $ v : ins
cppInOutParams ins outs both = map pointerParam both ++ map getParam ins ++ 
  map pointerParam outs