{-# LANGUAGE TypeFamilies, Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ code is contained in this module
module GOOL.Drasil.LanguageRenderer.CppRenderer (
  -- * C++ Code Configuration -- defines syntax of all C++ code
  CppSrcCode(..), CppHdrCode(..), CppCode(..), unCPPC, cppName, cppVersion
) where

import Utils.Drasil (blank, indent, indentList)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, MSBody, VSType, SVariable, SValue, 
  VSFunction, MSStatement, MSParameter, SMethod, CSStateVar, NamedArgs, OOProg, 
  ProgramSym(..), FileSym(..), PermanenceSym(..), BodySym(..), bodyStatements, 
  oneLiner, BlockSym(..), TypeSym(..), TypeElim(..), VariableSym(..), 
  VariableElim(..), ValueSym(..), Argument(..), Literal(..), MathConstant(..), 
  VariableValue(..), CommandLineArgs(..), NumericExpression(..), 
  BooleanExpression(..), Comparison(..), ValueExpression(..), funcApp, 
  selfFuncApp, extFuncApp, InternalValueExp(..), objMethodCall, FunctionSym(..),
  ($.), GetSet(..), List(..), InternalList(..), StatementSym(..), 
  AssignStatement(..), DeclStatement(..), IOStatement(..), StringStatement(..), 
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..), switchAsIf, 
  StatePattern(..), ObserverPattern(..), StrategyPattern(..), ScopeSym(..), 
  ParameterSym(..), MethodSym(..), pubMethod, StateVarSym(..), ClassSym(..), 
  ModuleSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(..), ImportSym(..), 
  ImportElim, PermElim(binding), RenderBody(..), BodyElim, RenderBlock(..), 
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..), 
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind), 
  RenderValue(..), ValueElim(valuePrec), InternalGetSet(..), 
  InternalListFunc(..), RenderFunction(..), FunctionElim(functionType), 
  InternalAssignStmt(..), InternalIOStmt(..), InternalControlStmt(..), 
  RenderStatement(..), StatementElim(statementTerm), RenderScope(..), ScopeElim, 
  MSMthdType, MethodTypeSym(..), RenderParam(..), 
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim, 
  StateVarElim, ParentSpec, RenderClass(..), ClassElim, RenderMod(..), 
  ModuleElim, BlockCommentSym(..), BlockCommentElim)
import qualified GOOL.Drasil.RendererClasses as RC (import', perm, body, block,
  type', uOp, bOp, variable, value, function, statement, scope, parameter,
  method, stateVar, class', module', blockComment')
import GOOL.Drasil.LanguageRenderer (addExt, classDec, dot, blockCmtStart, 
  blockCmtEnd, docCmtStart, bodyStart, bodyEnd, endStatement, commentStart, 
  returnLabel, elseIfLabel, tryLabel, catchLabel, throwLabel, array', constDec',
  listSep', argc, argv, constDec, mainFunc, containing, functionDox, valueList, 
  parameterList, appendToBody, surroundBody, getterName, setterName)
import qualified GOOL.Drasil.LanguageRenderer as R (this', this, sqrt, fabs,
  log10, log, exp, sin, cos, tan, asin, acos, atan, floor, ceil, pow, multiStmt,
  body, param, stateVar, constVar, cast, castObj, static, dynamic, break, 
  continue, private, public, blockCmt, docCmt, addComments, commentedMod, 
  commentedItem)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd, 
  mkStateVal, mkVal, mkStateVar, mkVar, VSOp, mkOp, unOpPrec, powerPrec, 
  unExpr, unExpr', typeUnExpr, binExpr, binExpr', typeBinExpr)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, listInnerType, obj, negateOp, csc, sec, 
  cot, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, 
  plusOp, minusOp, multOp, divideOp, moduloOp, var, staticVar, objVar,
  arrayElem, litChar, litDouble, litInt, litString, valueOf, arg, argsList, 
  objAccess, objMethodCall, funcAppMixedArgs, selfFuncAppMixedArgs, 
  newObjMixedArgs, lambda, func, get, set, listAdd, listAppend, listAccess, 
  listSet, getFunc, setFunc, listAppendFunc, stmt, loopStmt, emptyStmt, assign, 
  subAssign, increment, objDecNew, print, closeFile, returnStmt, valStmt, 
  comment, throw, ifCond, tryCatch, construct, param, method, getMethod, 
  setMethod, function, buildClass, implementingClass, commentedClass, 
  modFromData, fileDoc, fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (classVarCheckStatic)
import qualified GOOL.Drasil.LanguageRenderer.CommonPseudoOO as CP (int,
  constructor, doxFunc, doxClass, doxMod, funcType, buildModule, litArray, 
  call', listSizeFunc, listAccessFunc', string, constDecDef, docInOutFunc, 
  listSetFunc, extraClass)
import qualified GOOL.Drasil.LanguageRenderer.CLike as C (charRender, float, 
  double, char, listType, void, notOp, andOp, orOp, self, litTrue, litFalse, 
  litFloat, inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, 
  increment1, decrement1, varDec, varDecDef, listDec, extObjDecNew, switch, 
  for, while, intFunc, multiAssignError, multiReturnError, multiTypeError)
import qualified GOOL.Drasil.LanguageRenderer.Macros as M (runStrategy, 
  listSlice, stringListVals, stringListLists, forRange, notifyObservers)
import GOOL.Drasil.AST (Terminator(..), ScopeTag(..), Binding(..), onBinding, 
  BindData(..), bd, FileType(..), FileData(..), fileD, FuncData(..), fd, 
  ModData(..), md, updateMod, OpData(..), ParamData(..), pd, ProgData(..), 
  progD, emptyProg, StateVarData(..), svd, TypeData(..), td, ValData(..), vd, 
  VarData(..), vard)
import GOOL.Drasil.Classes (Pair(..))
import GOOL.Drasil.Helpers (angles, doubleQuotedText, hicat, vibcat, 
  emptyIfEmpty, toCode, toState, onCodeValue, onStateValue, on2CodeValues, 
  on2StateValues, on3CodeValues, on3StateValues, onCodeList, onStateList, 
  on2StateLists, on2StateWrapped)
import GOOL.Drasil.State (CS, MS, VS, lensGStoFS, lensFStoCS, lensFStoMS, 
  lensCStoMS, lensCStoVS, lensMStoCS, lensCStoFS, lensMStoVS, lensVStoMS, 
  modifyReturn, revFiles, addLangImport, addLangImportVS, getLangImports, 
  getLibImports, addModuleImportVS, getModuleImports, addHeaderLangImport, 
  getHeaderLangImports, addHeaderModImport, getHeaderLibImports, 
  getHeaderModImports, addDefine, getDefines, addHeaderDefine, 
  getHeaderDefines, addUsing, getUsing, addHeaderUsing, getHeaderUsing, 
  setFileType, getModuleName, setModuleName, setClassName, getClassName, setCurrMain, 
  getCurrMain, getClassMap, setScope, getScope, setCurrMainFunc, getCurrMainFunc,
  addIter, getIter, resetIter)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,pi,const,log,exp,mod,max)
import Control.Lens.Zoom (zoom)
import Control.Monad (join)
import Control.Monad.State (State, modify)
import Data.Composition ((.:))
import Data.List (sort)
import qualified Data.Map as Map (lookup)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($$), hcat, brackets, 
  braces, parens, empty, equals, vcat, lbrace, rbrace, colon, isEmpty, quotes)

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

instance (Pair p) => OOProg (p CppSrcCode CppHdrCode)

instance (Pair p) => ProgramSym (p CppSrcCode CppHdrCode) where
  type Program (p CppSrcCode CppHdrCode) = ProgData
  prog n mods = do
    m <-  mapM (zoom lensGStoFS) mods
    let fm = map pfst m
        sm = map (hdrToSrc . psnd) m
    p1 <- prog n $ map pure sm ++ map pure fm
    modify revFiles
    pure $ pair p1 (toCode emptyProg)

instance (Pair p) => RenderSym (p CppSrcCode CppHdrCode)

instance (Pair p) => FileSym (p CppSrcCode CppHdrCode) where
  type File (p CppSrcCode CppHdrCode) = FileData
  fileDoc = pair1 fileDoc fileDoc

  docMod d a dt = pair1 (docMod d a dt) (docMod d a dt)

instance (Pair p) => RenderFile (p CppSrcCode CppHdrCode) where
  top m = pair (top $ pfst m) (top $ psnd m)
  bottom = pair bottom bottom
  
  commentedMod = pair2 commentedMod commentedMod

  fileFromData fp = pair1 (fileFromData fp) (fileFromData fp)

instance (Pair p) => ImportSym (p CppSrcCode CppHdrCode) where
  type Import (p CppSrcCode CppHdrCode) = Doc
  langImport n = pair (langImport n) (langImport n)
  modImport n = pair (modImport n) (modImport n)

instance (Pair p) => ImportElim (p CppSrcCode CppHdrCode) where
  import' i = RC.import' $ pfst i

instance (Pair p) => PermanenceSym (p CppSrcCode CppHdrCode) where
  type Permanence (p CppSrcCode CppHdrCode) = BindData
  static = pair static static
  dynamic = pair dynamic dynamic

instance (Pair p) => PermElim (p CppSrcCode CppHdrCode) where
  perm p = RC.perm $ pfst p
  binding p = binding $ pfst p

instance (Pair p) => BodySym (p CppSrcCode CppHdrCode) where
  type Body (p CppSrcCode CppHdrCode) = Doc
  body = pair1List body body

  addComments s = pair1 (addComments s) (addComments s)

instance (Pair p) => RenderBody (p CppSrcCode CppHdrCode) where
  multiBody = pair1List multiBody multiBody

instance (Pair p) => BodyElim (p CppSrcCode CppHdrCode) where
  body b = RC.body $ pfst b

instance (Pair p) => BlockSym (p CppSrcCode CppHdrCode) where
  type Block (p CppSrcCode CppHdrCode) = Doc
  block = pair1List block block

instance (Pair p) => RenderBlock (p CppSrcCode CppHdrCode) where
  multiBlock = pair1List multiBlock multiBlock

instance (Pair p) => BlockElim (p CppSrcCode CppHdrCode) where
  block b = RC.block $ pfst b

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
  funcType = pair1List1Val funcType funcType
  void = on2StateValues pair void void

instance (Pair p) => TypeElim (p CppSrcCode CppHdrCode) where
  getType s = getType $ pfst s
  getTypeString s = getTypeString $ pfst s
  
instance (Pair p) => RenderType (p CppSrcCode CppHdrCode) where
  multiType = pair1List multiType multiType
  typeFromData t s d = on2StateValues pair (typeFromData t s d) (typeFromData t s d)

instance (Pair p) => InternalTypeElim (p CppSrcCode CppHdrCode) where
  type' s = RC.type' $ pfst s

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

instance (Pair p) => OpElim (p CppSrcCode CppHdrCode) where
  uOp o = RC.uOp $ pfst o
  bOp o = RC.bOp $ pfst o
  uOpPrec o = uOpPrec $ pfst o
  bOpPrec o = bOpPrec $ pfst o

instance (Pair p) => VariableSym (p CppSrcCode CppHdrCode) where
  type Variable (p CppSrcCode CppHdrCode) = VarData
  var n = pair1 (var n) (var n)
  staticVar n = pair1 (staticVar n) (staticVar n)
  constant n = pair1 (constant n) (constant n)
  extVar l n = pair1 (extVar l n) (extVar l n)
  self = on2StateValues pair self self
  classVar = pair2 classVar classVar
  extClassVar = pair2 extClassVar extClassVar
  objVar = pair2 objVar objVar
  objVarSelf = pair1 objVarSelf objVarSelf
  arrayElem i = pair1 (arrayElem i) (arrayElem i)

instance (Pair p) => VariableElim (p CppSrcCode CppHdrCode) where
  variableName v = variableName $ pfst v
  variableType v = pair (variableType $ pfst v) (variableType $ psnd v)

instance (Pair p) => InternalVarElim (p CppSrcCode CppHdrCode) where
  variableBind v = variableBind $ pfst v
  variable v = RC.variable $ pfst v

instance (Pair p) => RenderVariable (p CppSrcCode CppHdrCode) where
  varFromData b n t' d = pair1 (\t ->varFromData b n t d) 
    (\t -> varFromData b n t d) t'

instance (Pair p) => ValueSym (p CppSrcCode CppHdrCode) where
  type Value (p CppSrcCode CppHdrCode) = ValData
  valueType v = pair (valueType $ pfst v) (valueType $ psnd v)

instance (Pair p) => Argument (p CppSrcCode CppHdrCode) where
  pointerArg = pair1 pointerArg pointerArg

instance (Pair p) => Literal (p CppSrcCode CppHdrCode) where
  litTrue = on2StateValues pair litTrue litTrue
  litFalse = on2StateValues pair litFalse litFalse
  litChar c = on2StateValues pair (litChar c) (litChar c)
  litDouble v = on2StateValues pair (litDouble v) (litDouble v)
  litFloat v = on2StateValues pair (litFloat v) (litFloat v)
  litInt v =on2StateValues  pair (litInt v) (litInt v)
  litString s = on2StateValues pair (litString s) (litString s)
  litArray = pair1Val1List litArray litArray
  litList = pair1Val1List litList litList

instance (Pair p) => MathConstant (p CppSrcCode CppHdrCode) where
  pi = on2StateValues pair pi pi

instance (Pair p) => VariableValue (p CppSrcCode CppHdrCode) where
  valueOf = pair1 valueOf valueOf
  
instance (Pair p) => CommandLineArgs (p CppSrcCode CppHdrCode) where
  arg n = on2StateValues pair (arg n) (arg n)
  argsList = on2StateValues pair argsList argsList
  argExists i = on2StateValues pair (argExists i) (argExists i)

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

instance (Pair p) => Comparison (p CppSrcCode CppHdrCode) where
  (?<) = pair2 (?<) (?<)
  (?<=) = pair2 (?<=) (?<=)
  (?>) = pair2 (?>) (?>)
  (?>=) = pair2 (?>=) (?>=)
  (?==) = pair2 (?==) (?==)
  (?!=) = pair2 (?!=) (?!=)
  
instance (Pair p) => ValueExpression (p CppSrcCode CppHdrCode) where
  inlineIf = pair3 inlineIf inlineIf

  funcAppMixedArgs n = pair1Val3Lists (funcAppMixedArgs n) (funcAppMixedArgs n)
  selfFuncAppMixedArgs n = pair1Val3Lists
    (selfFuncAppMixedArgs n) 
    (selfFuncAppMixedArgs n) 
  extFuncAppMixedArgs l n = pair1Val3Lists
    (extFuncAppMixedArgs l n) 
    (extFuncAppMixedArgs l n) 
  libFuncAppMixedArgs l n = pair1Val3Lists
    (extFuncAppMixedArgs l n) 
    (extFuncAppMixedArgs l n)
  newObjMixedArgs = pair1Val3Lists newObjMixedArgs newObjMixedArgs
  extNewObjMixedArgs l = pair1Val3Lists
    (extNewObjMixedArgs l) 
    (extNewObjMixedArgs l)
  libNewObjMixedArgs l = pair1Val3Lists 
    (extNewObjMixedArgs l) 
    (extNewObjMixedArgs l)

  lambda = pair1List1Val lambda lambda

  notNull = pair1 notNull notNull
  
instance (Pair p) => RenderValue (p CppSrcCode CppHdrCode) where
  inputFunc = on2StateValues pair inputFunc inputFunc
  printFunc = on2StateValues pair printFunc printFunc
  printLnFunc = on2StateValues pair printLnFunc printLnFunc
  printFileFunc = pair1 printFileFunc printFileFunc
  printFileLnFunc = pair1 printFileLnFunc printFileLnFunc

  cast = pair2 cast cast

  call l o n = pair1Val3Lists (call l o n) (call l o n)

  valFromData p t' d = pair1 (\t -> valFromData p t d) 
    (\t -> valFromData p t d) t'

instance (Pair p) => ValueElim (p CppSrcCode CppHdrCode) where
  valuePrec v = valuePrec $ pfst v
  value v = RC.value $ pfst v

instance (Pair p) => InternalValueExp (p CppSrcCode CppHdrCode) where
  objMethodCallMixedArgs' f = pair2Vals3Lists
    (objMethodCallMixedArgs' f)
    (objMethodCallMixedArgs' f)

instance (Pair p) => FunctionSym (p CppSrcCode CppHdrCode) where
  type Function (p CppSrcCode CppHdrCode) = FuncData
  func l = pair1Val1List (func l) (func l)
  objAccess = pair2 objAccess objAccess
  
instance (Pair p) => GetSet (p CppSrcCode CppHdrCode) where
  get = pair2 get get
  set = pair3 set set

instance (Pair p) => List (p CppSrcCode CppHdrCode) where
  listSize = pair1 listSize listSize
  listAdd = pair3 listAdd listAdd
  listAppend = pair2 listAppend listAppend
  listAccess = pair2 listAccess listAccess
  listSet = pair3 listSet listSet
  indexOf = pair2 indexOf indexOf

instance (Pair p) => InternalList (p CppSrcCode CppHdrCode) where
  listSlice' b e s vr vl = pair2 
    (listSlice' (fmap (onStateValue pfst) b) (fmap (onStateValue pfst) e) 
      (fmap (onStateValue pfst) s))
    (listSlice' (fmap (onStateValue psnd) b) (fmap (onStateValue psnd) e) 
      (fmap (onStateValue psnd) s)) 
    (zoom lensMStoVS vr) (zoom lensMStoVS vl)

instance (Pair p) => InternalGetSet (p CppSrcCode CppHdrCode) where  
  getFunc = pair1 getFunc getFunc
  setFunc = pair3 setFunc setFunc

instance (Pair p) => InternalListFunc (p CppSrcCode CppHdrCode) where  
  listSizeFunc = on2StateValues pair listSizeFunc listSizeFunc
  listAddFunc = pair3 listAddFunc listAddFunc
  listAppendFunc = pair1 listAppendFunc listAppendFunc
  listAccessFunc = pair2 listAccessFunc listAccessFunc
  listSetFunc = pair3 listSetFunc listSetFunc

instance (Pair p) => RenderFunction (p CppSrcCode CppHdrCode) where  
  funcFromData d = pair1 (funcFromData d) (funcFromData d)
  
instance (Pair p) => FunctionElim (p CppSrcCode CppHdrCode) where  
  functionType f = pair (functionType $ pfst f) (functionType $ psnd f)
  function f = RC.function $ pfst f

instance (Pair p) => InternalAssignStmt (p CppSrcCode CppHdrCode) where
  multiAssign vrs vls = pair2Lists multiAssign multiAssign 
    (map (zoom lensMStoVS) vrs) (map (zoom lensMStoVS) vls)
    
instance (Pair p) => InternalIOStmt (p CppSrcCode CppHdrCode) where
  -- Another Maybe/State combination
  printSt nl f p v = pair2
    (printSt nl (fmap (onStateValue pfst) f)) 
    (printSt nl (fmap (onStateValue psnd) f)) 
    (zoom lensMStoVS p) (zoom lensMStoVS v)

instance (Pair p) => InternalControlStmt (p CppSrcCode CppHdrCode) where
  multiReturn = pair1List multiReturn multiReturn . map (zoom lensMStoVS)
    
instance (Pair p) => RenderStatement (p CppSrcCode CppHdrCode) where
  stmt = pair1 stmt stmt
  loopStmt = pair1 loopStmt loopStmt

  emptyStmt = on2StateValues pair emptyStmt emptyStmt
  
  stmtFromData d t = on2StateValues pair (stmtFromData d t) (stmtFromData d t)

instance (Pair p) => StatementElim (p CppSrcCode CppHdrCode) where
  statement s = RC.statement $ pfst s
  statementTerm s = statementTerm $ pfst s

instance (Pair p) => StatementSym (p CppSrcCode CppHdrCode) where
  type Statement (p CppSrcCode CppHdrCode) = (Doc, Terminator)
  valStmt = pair1 valStmt valStmt . zoom lensMStoVS
  multi = pair1List multi multi

instance (Pair p) => AssignStatement (p CppSrcCode CppHdrCode) where
  assign vr vl = pair2 assign assign (zoom lensMStoVS vr) (zoom lensMStoVS vl)
  (&-=) vr vl = pair2 (&-=) (&-=) (zoom lensMStoVS vr) (zoom lensMStoVS vl)
  (&+=) vr vl = pair2 (&+=) (&+=) (zoom lensMStoVS vr) (zoom lensMStoVS vl)
  (&++) vl = pair1 (&++) (&++) (zoom lensMStoVS vl)
  (&--) vl = pair1 (&--) (&--) (zoom lensMStoVS vl)

instance (Pair p) => DeclStatement (p CppSrcCode CppHdrCode) where
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
  constDecDef vr vl = pair2 constDecDef constDecDef (zoom lensMStoVS vr) 
    (zoom lensMStoVS vl)
  funcDecDef v ps = pairValListVal funcDecDef funcDecDef (zoom lensMStoVS v) 
    (map (zoom lensMStoVS) ps)

instance (Pair p) => IOStatement (p CppSrcCode CppHdrCode) where
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
  getFileInputAll f v = pair2 getFileInputAll getFileInputAll 
    (zoom lensMStoVS f) (zoom lensMStoVS v)

instance (Pair p) => StringStatement (p CppSrcCode CppHdrCode) where
  stringSplit d vnew s = pair2 (stringSplit d) (stringSplit d) 
    (zoom lensMStoVS vnew) (zoom lensMStoVS s)

  stringListVals vars sl = pair1List1Val stringListVals stringListVals
    (map (zoom lensMStoVS) vars) (zoom lensMStoVS sl)
  stringListLists lsts sl = pair1List1Val stringListLists stringListLists
    (map (zoom lensMStoVS) lsts) (zoom lensMStoVS sl)

instance (Pair p) => FuncAppStatement (p CppSrcCode CppHdrCode) where
  inOutCall n is os bs = pair3Lists (inOutCall n) (inOutCall n) 
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)
  selfInOutCall n is os bs = pair3Lists (selfInOutCall n) (selfInOutCall n)
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)
  extInOutCall m n is os bs = pair3Lists (extInOutCall m n) (extInOutCall m n) 
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)

instance (Pair p) => CommentStatement (p CppSrcCode CppHdrCode) where
  comment cmt = on2StateValues pair (comment cmt) (comment cmt)

instance (Pair p) => ControlStatement (p CppSrcCode CppHdrCode) where
  break = on2StateValues pair break break  
  continue = on2StateValues pair continue continue

  returnStmt = pair1 returnStmt returnStmt . zoom lensMStoVS

  throw errMsg = on2StateValues pair (throw errMsg) (throw errMsg)

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
  forEach e' v b = do 
    e <- zoom lensMStoVS e'
    let le = variableName e 
    modify (addIter le) 
    loop <- pair3 forEach forEach (zoom lensMStoVS e') (zoom lensMStoVS v) b
    modify (resetIter le)
    toState loop
  while v = pair2 while while (zoom lensMStoVS v)

  tryCatch = pair2 tryCatch tryCatch

instance (Pair p) => StatePattern (p CppSrcCode CppHdrCode) where
  checkState l vs = pair2Lists1Val
    (\sts bods -> checkState l (zip sts bods))
    (\sts bods -> checkState l (zip sts bods)) 
    (map (zoom lensMStoVS . fst) vs) (map snd vs)

instance (Pair p) => ObserverPattern (p CppSrcCode CppHdrCode) where
  notifyObservers f t = pair2 notifyObservers notifyObservers 
    (zoom lensMStoVS f) (zoom lensMStoVS t)

instance (Pair p) => StrategyPattern (p CppSrcCode CppHdrCode) where
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

instance (Pair p) => ScopeSym (p CppSrcCode CppHdrCode) where
  type Scope (p CppSrcCode CppHdrCode) = (Doc, ScopeTag)
  private = pair private private
  public = pair public public

instance (Pair p) => RenderScope (p CppSrcCode CppHdrCode) where
  scopeFromData s d = pair (scopeFromData s d) (scopeFromData s d)
  
instance (Pair p) => ScopeElim (p CppSrcCode CppHdrCode) where
  scope s = RC.scope $ pfst s

instance (Pair p) => MethodTypeSym (p CppSrcCode CppHdrCode) where
  type MethodType (p CppSrcCode CppHdrCode) = TypeData
  mType = pair1 mType mType . zoom lensMStoVS
  construct n = on2StateValues pair (construct n) (construct n)

instance (Pair p) => ParameterSym (p CppSrcCode CppHdrCode) where
  type Parameter (p CppSrcCode CppHdrCode) = ParamData
  param = pair1 param param . zoom lensMStoVS
  pointerParam = pair1 pointerParam pointerParam . zoom lensMStoVS

instance (Pair p) => RenderParam (p CppSrcCode CppHdrCode) where
  paramFromData v' d = pair1 (`paramFromData` d) (`paramFromData` d) (zoom lensMStoVS v')

instance (Pair p) => ParamElim (p CppSrcCode CppHdrCode) where
  parameterName p = parameterName $ pfst p
  parameterType p = pair (parameterType $ pfst p) (parameterType $ psnd p)
  parameter p = RC.parameter $ pfst p

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

  function n s t = pairValListVal 
    (function n (pfst s)) (function n (psnd s)) 
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

  inOutFunc n s is os bs = pair3Lists1Val
    (inOutFunc n (pfst s)) (inOutFunc n (psnd s))
    (map (zoom lensMStoVS) is) (map (zoom lensMStoVS) os) 
    (map (zoom lensMStoVS) bs)

  docInOutFunc n s desc is os bs = pair3Lists1Val 
    (\ins outs both -> docInOutFunc n (pfst s) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (\ins outs both -> docInOutFunc n (psnd s) desc (zip (map fst 
      is) ins) (zip (map fst os) outs) (zip (map fst bs) both))
    (map (zoom lensMStoVS . snd) is) (map (zoom lensMStoVS . snd) os) 
    (map (zoom lensMStoVS . snd) bs)
  
instance (Pair p) => RenderMethod (p CppSrcCode CppHdrCode) where
  intMethod m n s p = pairValListVal
    (intMethod m n (pfst s) (pfst p)) (intMethod m n (psnd s) (psnd p))
  intFunc m n s p = pairValListVal
    (intFunc m n (pfst s) (pfst p)) (intFunc m n (psnd s) (psnd p))
  commentedFunc = pair2 commentedFunc commentedFunc
  
  destructor = pair1List destructor destructor . map (zoom lensMStoCS)
  
  mthdFromData s d = on2StateValues pair (mthdFromData s d) (mthdFromData s d)
  
instance (Pair p) => MethodElim (p CppSrcCode CppHdrCode) where
  method m = RC.method $ pfst m

instance (Pair p) => StateVarSym (p CppSrcCode CppHdrCode) where
  type StateVar (p CppSrcCode CppHdrCode) = StateVarData
  stateVar s p = pair1 (stateVar (pfst s) (pfst p)) (stateVar (psnd s) (psnd p))
    . zoom lensCStoVS
  stateVarDef s p vr vl = pair2
    (stateVarDef (pfst s) (pfst p)) 
    (stateVarDef (psnd s) (psnd p)) (zoom lensCStoVS vr) (zoom lensCStoVS vl)
  constVar s vr vl = pair2 (constVar (pfst s)) (constVar (psnd s))
    (zoom lensCStoVS vr) (zoom lensCStoVS vl)

instance (Pair p) => StateVarElim (p CppSrcCode CppHdrCode) where
  stateVar v = RC.stateVar $ pfst v

instance (Pair p) => ClassSym (p CppSrcCode CppHdrCode) where
  type Class (p CppSrcCode CppHdrCode) = Doc
  buildClass p vs fs = do
    n <- zoom lensCStoFS getModuleName
    modify (setClassName n)
    pair2Lists (buildClass p) (buildClass p) vs (map (zoom lensCStoMS) fs)
  extraClass n p vs fs = modify (setClassName n) >> pair2Lists 
    (extraClass n p) (extraClass n p)
    vs (map (zoom lensCStoMS) fs)
  implementingClass n is vs fs = modify (setClassName n) >> pair2Lists
    (implementingClass n is) (implementingClass n is)
    vs (map (zoom lensCStoMS) fs)

  docClass d = pair1 (docClass d) (docClass d)

instance (Pair p) => RenderClass (p CppSrcCode CppHdrCode) where
  intClass n s i vs fs = pair2Lists 
    (intClass n (pfst s) (pfst i)) (intClass n (psnd s) (psnd i)) 
    vs (map (zoom lensCStoMS) fs)

  inherit n = pair (inherit n) (inherit n)
  implements is = pair (implements is) (implements is)

  commentedClass = pair2 commentedClass commentedClass
  
instance (Pair p) => ClassElim (p CppSrcCode CppHdrCode) where
  class' c = RC.class' $ pfst c

instance (Pair p) => ModuleSym (p CppSrcCode CppHdrCode) where
  type Module (p CppSrcCode CppHdrCode) = ModData
  buildModule n is ms cs= do 
    modify (setModuleName n)
    pair2Lists (buildModule n is) (buildModule n is) 
        (map (zoom lensFStoMS) ms) (map (zoom lensFStoCS)cs)
  
instance (Pair p) => RenderMod (p CppSrcCode CppHdrCode) where
  modFromData n d = on2StateValues pair (modFromData n d) (modFromData n d)
  updateModuleDoc f m = pair 
    (updateModuleDoc f $ pfst m) (updateModuleDoc f $ psnd m)
    
instance (Pair p) => ModuleElim (p CppSrcCode CppHdrCode) where
  module' m = RC.module' $ pfst m

instance (Pair p) => BlockCommentSym (p CppSrcCode CppHdrCode) where
  type BlockComment (p CppSrcCode CppHdrCode) = Doc
  blockComment lns = pair (blockComment lns) (blockComment lns)
  docComment lns = on2StateValues pair (docComment lns) (docComment lns)

instance (Pair p) => BlockCommentElim (p CppSrcCode CppHdrCode) where
  blockComment' c = RC.blockComment' $ pfst c

-- Helpers for pair instance

type SrcState s a = State s (CppSrcCode a)
type HdrState s a = State s (CppHdrCode a)
type PairState s p a = State s (p CppSrcCode CppHdrCode a) 

pair1 :: (Pair p) => (SrcState r a -> SrcState s b) -> (HdrState r a -> 
  HdrState s b) -> PairState s p a -> PairState s p b
pair1 srcf hdrf stv = do
  v <- stv
  let fp = pure $ pfst v
      sp = pure $ psnd v
  p1 <- srcf fp
  p2 <- hdrf sp
  pure $ pair p1 p2

pair2 :: (Pair p) => (SrcState r a -> SrcState s b -> SrcState t c) -> 
  (HdrState r a -> HdrState s b -> HdrState t c) -> PairState t p a -> 
  PairState t p b -> PairState t p c
pair2 srcf hdrf stv1 stv2 = do
  v1 <- stv1
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
  pair1 (srcf fv1) (hdrf sv1) stv2

pair3 :: (Pair p) => (SrcState r a -> SrcState s b -> SrcState t c -> 
  SrcState u d) -> (HdrState r a -> HdrState s b -> HdrState t c -> 
  HdrState u d) -> PairState u p a -> PairState u p b -> PairState u p c -> 
  PairState u p d
pair3 srcf hdrf stv1 stv2 stv3 = do
  v1 <- stv1
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
  pair2 (srcf fv1) (hdrf sv1) stv2 stv3

pair4 :: (Pair p) => (SrcState r a -> SrcState s b -> SrcState t c -> 
  SrcState u d -> SrcState v e) -> (HdrState r a -> HdrState s b -> HdrState t c
  -> HdrState u d -> HdrState v e) -> PairState v p a -> PairState v p b -> 
  PairState v p c -> PairState v p d -> PairState v p e
pair4 srcf hdrf stv1 stv2 stv3 stv4 = do
  v1 <- stv1
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
  pair3 (srcf fv1) (hdrf sv1) stv2 stv3 stv4

pair5 :: (Pair p) => (SrcState r a -> SrcState s b -> SrcState t c -> 
  SrcState u d -> SrcState v e -> SrcState w f) -> (HdrState r a -> HdrState s b
  -> HdrState t c -> HdrState u d -> HdrState v e -> HdrState w f) -> 
  PairState w p a -> PairState w p b -> PairState w p c -> PairState w p d -> 
  PairState w p e -> PairState w p f
pair5 srcf hdrf stv1 stv2 stv3 stv4 stv5 = do
  v1 <- stv1
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
  pair4 (srcf fv1) (hdrf sv1) stv2 stv3 stv4 stv5
  
pair1List :: (Pair p) => ([SrcState r a] -> SrcState s b) -> ([HdrState r a] -> 
  HdrState s b) -> [PairState s p a] -> PairState s p b
pair1List srcf hdrf stv = do
  v <- sequence stv
  let fl = map (pure . pfst) v
      sl = map (pure . psnd) v
  p1 <- srcf fl
  p2 <- hdrf sl
  pure $ pair p1 p2

pair2Lists :: (Pair p) => ([SrcState r a] -> [SrcState s b] -> SrcState t c) -> 
  ([HdrState r a] -> [HdrState s b] -> HdrState t c) -> [PairState t p a] -> 
  [PairState t p b] -> PairState t p c
pair2Lists srcf hdrf stv1 stv2 = do
  v1 <- sequence stv1
  let fl1 = map (pure . pfst) v1
      sl1 = map (pure . psnd) v1
  pair1List (srcf fl1) (hdrf sl1) stv2

pair3Lists :: (Pair p) => ([SrcState r a] -> [SrcState s b] -> [SrcState t c] ->
  SrcState u d) -> ([HdrState r a] -> [HdrState s b] -> [HdrState t c] -> 
  HdrState u d) -> [PairState u p a] -> [PairState u p b] -> [PairState u p c] 
  -> PairState u p d
pair3Lists srcf hdrf stv1 stv2 stv3 = do
  v1 <- sequence stv1
  let fl1 = map (pure . pfst) v1
      sl1 = map (pure . psnd) v1
  pair2Lists (srcf fl1) (hdrf sl1) stv2 stv3 

pair1List1Val :: (Pair p) => ([SrcState r a] -> SrcState s b -> SrcState t c) ->
  ([HdrState r a] -> HdrState s b -> HdrState t c) -> [PairState t p a] -> 
  PairState t p b -> PairState t p c
pair1List1Val srcf hdrf stv1 stv2 = do
  v1 <- sequence stv1
  let fl1 = map (pure . pfst) v1
      sl1 = map (pure . psnd) v1
  pair1 (srcf fl1) (hdrf sl1) stv2

pair1Val1List :: (Pair p) => (SrcState r a -> [SrcState s b] -> SrcState t c) ->
  (HdrState r a -> [HdrState s b] -> HdrState t c) -> PairState t p a -> 
  [PairState t p b] -> PairState t p c
pair1Val1List srcf hdrf stv1 stv2 = do
  v1 <- stv1
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
  pair1List (srcf fv1) (hdrf sv1) stv2

pair2Lists1Val :: (Pair p) => ([SrcState r a] -> [SrcState s b] -> SrcState t c 
  -> SrcState u d) -> ([HdrState r a] -> [HdrState s b] -> HdrState t c -> 
  HdrState u d) -> [PairState u p a] -> [PairState u p b] -> PairState u p c -> 
  PairState u p d
pair2Lists1Val srcf hdrf stv1 stv2 stv3 = do
  v1 <- sequence stv1
  let fl1 = map (pure . pfst) v1
      sl1 = map (pure . psnd) v1
  pair1List1Val (srcf fl1) (hdrf sl1) stv2 stv3 

pairValListVal :: (Pair p) => (SrcState r a -> [SrcState s b] -> SrcState t c ->
  SrcState u d) -> (HdrState r a -> [HdrState s b] -> HdrState t c -> 
  HdrState u d) -> PairState u p a -> [PairState u p b] -> PairState u p c -> 
  PairState u p d
pairValListVal srcf hdrf stv1 stv2 stv3 = do
  v1 <- stv1
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
  pair1List1Val (srcf fv1) (hdrf sv1) stv2 stv3 

pair3Lists1Val :: (Pair p) => ([SrcState r a] -> [SrcState s b] -> 
  [SrcState t c] -> SrcState u d -> SrcState v e) -> ([HdrState r a] -> 
  [HdrState s b] -> [HdrState t c] -> HdrState u d -> HdrState v e) -> 
  [PairState v p a] -> [PairState v p b] -> [PairState v p c] -> PairState v p d
  -> PairState v p e
pair3Lists1Val srcf hdrf stv1 stv2 stv3 stv4 = do
  v1 <- sequence stv1
  let fl1 = map (pure . pfst) v1
      sl1 = map (pure . psnd) v1
  pair2Lists1Val (srcf fl1) (hdrf sl1) stv2 stv3 stv4 

pair1Val3Lists :: (Pair p) => (SrcState r a -> [SrcState s b] -> 
  [(SrcState t c, SrcState u d)] -> SrcState v e) -> (HdrState r a -> 
  [HdrState s b] -> [(HdrState t c, HdrState u d)] -> HdrState v e) -> 
  PairState v p a -> [PairState v p b] -> [(PairState v p c, PairState v p d)] 
  -> PairState v p e
pair1Val3Lists srcf hdrf stv1 stv2 stv34 = do
  v1 <- stv1
  v2 <- sequence stv2
  v3 <- mapM fst stv34
  v4 <- mapM snd stv34
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
      fv2 = map (pure . pfst) v2
      sv2 = map (pure . psnd) v2
      fv3 = map (pure . pfst) v3
      sv3 = map (pure . psnd) v3
      fv4 = map (pure . pfst) v4
      sv4 = map (pure . psnd) v4
  p1 <- srcf fv1 fv2 (zip fv3 fv4)
  p2 <- hdrf sv1 sv2 (zip sv3 sv4)
  pure $ pair p1 p2

pair2Vals3Lists :: (Pair p) => (SrcState r a -> SrcState s b -> [SrcState t c] 
  -> [(SrcState u d, SrcState v e)] -> SrcState w f) -> (HdrState r a -> 
  HdrState s b -> [HdrState t c] -> [(HdrState u d, HdrState v e)] -> 
  HdrState w f) -> PairState w p a -> PairState w p b -> [PairState w p c] -> 
  [(PairState w p d, PairState w p e)] -> PairState w p f
pair2Vals3Lists srcf hdrf stv1 stv2 stv3 stv45 = do
  v1 <- stv1
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
  pair1Val3Lists (srcf fv1) (hdrf sv1) stv2 stv3 stv45

pairVal2ListsVal :: (Pair p) => (SrcState r a -> [SrcState s b] -> 
  [SrcState t c] -> SrcState u d -> SrcState v e) -> (HdrState r a -> 
  [HdrState s b] -> [HdrState t c] -> HdrState u d -> HdrState v e) -> 
  PairState v p a -> [PairState v p b] -> [PairState v p c] -> PairState v p d 
  -> PairState v p e
pairVal2ListsVal srcf hdrf stv1 stv2 stv3 stv4 = do
  v1 <- stv1
  let fv1 = pure $ pfst v1
      sv1 = pure $ psnd v1
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
  CPPSC x >>= f = f x

instance ProgramSym CppSrcCode where
  type Program CppSrcCode = ProgData
  prog n = onStateList (onCodeList (progD n)) . map (zoom lensGStoFS)

instance RenderSym CppSrcCode
  
instance FileSym CppSrcCode where
  type File CppSrcCode = FileData
  fileDoc m = do
    modify (setFileType Source)
    G.fileDoc cppSrcExt top bottom m

  docMod = CP.doxMod cppSrcExt

instance RenderFile CppSrcCode where
  top _ = toCode empty
  bottom = toCode empty

  commentedMod = on3StateValues (\mn m cmt -> if mn then on2CodeValues 
    R.commentedMod m cmt else m) getCurrMain
  
  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym CppSrcCode where
  type Import CppSrcCode = Doc
  langImport n = toCode $ inc <+> angles (text n)
  modImport n = toCode $ inc <+> doubleQuotedText (addExt cppHdrExt 
    n)

instance ImportElim CppSrcCode where
  import' = unCPPSC

instance PermanenceSym CppSrcCode where
  type Permanence CppSrcCode = BindData
  static = toCode $ bd Static R.static
  dynamic = toCode $ bd Dynamic R.dynamic
  
instance PermElim CppSrcCode where
  perm = bindDoc . unCPPSC
  binding = bind . unCPPSC

instance BodySym CppSrcCode where
  type Body CppSrcCode = Doc
  body = onStateList (onCodeList R.body)

  addComments s = onStateValue (onCodeValue (R.addComments s commentStart))

instance RenderBody CppSrcCode where
  multiBody = G.multiBody 

instance BodyElim CppSrcCode where
  body = unCPPSC

instance BlockSym CppSrcCode where
  type Block CppSrcCode = Doc
  block = G.block

instance RenderBlock CppSrcCode where
  multiBlock = G.multiBlock

instance BlockElim CppSrcCode where
  block = unCPPSC

instance TypeSym CppSrcCode where
  type Type CppSrcCode = TypeData
  bool = cppBoolType
  int = CP.int
  float = C.float
  double = C.double
  char = C.char
  string = do 
    modify (addUsing cppString . addLangImportVS cppString)
    CP.string
  infile = do
    modify (addUsing cppInfile)
    cppInfileType
  outfile = do
    modify (addUsing cppOutfile) 
    cppOutfileType
  listType t = do
    modify (addUsing vector . addLangImportVS vector)
    C.listType vector t
  arrayType = cppArrayType
  listInnerType = G.listInnerType
  obj n = do 
    cn <- zoom lensVStoMS getClassName
    if cn == n then G.obj n else 
      getClassMap >>= (\cm -> maybe id ((>>) . modify . addModuleImportVS) 
        (Map.lookup n cm) (G.obj n))
  funcType = CP.funcType
  void = C.void

instance TypeElim CppSrcCode where
  getType = cType . unCPPSC
  getTypeString = typeString . unCPPSC
  
instance RenderType CppSrcCode where
  multiType _ = error $ C.multiTypeError cppName
  typeFromData t s d = toState (toCode $ td t s d)

instance InternalTypeElim CppSrcCode where
  type' = typeDoc . unCPPSC
  
instance UnaryOpSym CppSrcCode where
  type UnaryOp CppSrcCode = OpData
  notOp = C.notOp
  negateOp = G.negateOp
  sqrtOp = cppSqrtOp
  absOp = cppAbsOp
  logOp = cppLogOp
  lnOp = cppLnOp
  expOp = cppExpOp
  sinOp = cppSinOp
  cosOp = cppCosOp
  tanOp = cppTanOp
  asinOp = cppAsinOp
  acosOp = cppAcosOp
  atanOp = cppAtanOp
  floorOp = cppFloorOp
  ceilOp = cppCeilOp

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
  powerOp = addMathHImport cppPowerOp
  moduloOp = G.moduloOp
  andOp = C.andOp
  orOp = C.orOp

instance OpElim CppSrcCode where
  uOp = opDoc . unCPPSC
  bOp = opDoc . unCPPSC
  uOpPrec = opPrec . unCPPSC
  bOpPrec = opPrec . unCPPSC

instance VariableSym CppSrcCode where
  type Variable CppSrcCode = VarData
  var = G.var
  staticVar = G.staticVar
  constant = var
  extVar l n t = modify (addModuleImportVS l) >> var n t
  self = C.self
  classVar c' v'= do 
    c <- c'
    v <- v'
    vfd <- varFromData 
      (variableBind v) (getTypeString c `nmSpcAccess` variableName v) 
      (toState $ variableType v) (cppClassVar (RC.type' c) (RC.variable v))
    toState $ classVarCheckStatic vfd
  extClassVar c v = do
    t <- c
    cm <- getClassMap
    maybe id ((>>) . modify . addModuleImportVS) 
      (Map.lookup (getTypeString t) cm) $ classVar (pure t) v
  objVar = G.objVar
  objVarSelf v' = do 
    v <- v' 
    mkVar (R.this ++ ptrAccess ++ variableName v)
      (variableType v) (R.this' <> ptrAccess' <> RC.variable v)
  arrayElem i = G.arrayElem (litInt i)

instance VariableElim CppSrcCode where
  variableName = varName . unCPPSC
  variableType = onCodeValue varType

instance InternalVarElim CppSrcCode where
  variableBind = varBind . unCPPSC
  variable = varDoc . unCPPSC

instance RenderVariable CppSrcCode where
  varFromData b n t' d = do 
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym CppSrcCode where
  type Value CppSrcCode = ValData
  valueType = onCodeValue valType

instance Argument CppSrcCode where
  pointerArg = id

instance Literal CppSrcCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = C.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = CP.litArray braces
  litList _ _ = error $ "List literals not supported in " ++ cppName

instance MathConstant CppSrcCode where
  pi = do
    modify (addDefine mathDefines)
    addMathHImport (mkStateVal double cppPi)

instance VariableValue CppSrcCode where
  valueOf vr' = do 
    vr <- vr'
    its <- zoom lensVStoMS getIter
    let namevr = variableName vr 
    if namevr `elem` its then 
      mkStateVal (iterator $ toState $ variableType vr) 
        (parens $ cppDeref <> RC.variable vr)
      else G.valueOf vr'

instance CommandLineArgs CppSrcCode where
  arg n = G.arg (litInt $ n+1) argsList
  argsList = G.argsList argv
  argExists i = listSize argsList ?> litInt (fromIntegral $ i+1)

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

instance Comparison CppSrcCode where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool
   
instance ValueExpression CppSrcCode where
  inlineIf = C.inlineIf

  funcAppMixedArgs = G.funcAppMixedArgs
  selfFuncAppMixedArgs = G.selfFuncAppMixedArgs ptrAccess' self
  extFuncAppMixedArgs l n t vs ns = do
    modify (addModuleImportVS l)
    funcAppMixedArgs n t vs ns
  libFuncAppMixedArgs = C.libFuncAppMixedArgs
  newObjMixedArgs = G.newObjMixedArgs ""
  extNewObjMixedArgs l t vs ns = do
    modify (addModuleImportVS l)
    newObjMixedArgs t vs ns
  libNewObjMixedArgs = C.libNewObjMixedArgs

  lambda = G.lambda cppLambda

  notNull v = v

instance RenderValue CppSrcCode where
  inputFunc = addIOStreamImport $ mkStateVal string (text cin)
  printFunc = addIOStreamImport $ mkStateVal void (text cout)
  printLnFunc = addIOStreamImport $ mkStateVal void (text cout)
  printFileFunc = on2StateWrapped (\vt w -> mkVal vt (RC.value w)) void
  printFileLnFunc = on2StateWrapped (\vt w -> mkVal vt (RC.value w)) void

  cast = cppCast

  call = CP.call' cppName

  valFromData p t' d = do 
    t <- t'
    toState $ on2CodeValues (vd p) t (toCode d)
  
instance ValueElim CppSrcCode where
  valuePrec = valPrec . unCPPSC
  value = val . unCPPSC

instance InternalValueExp CppSrcCode where
  objMethodCallMixedArgs' = G.objMethodCall

instance FunctionSym CppSrcCode where
  type Function CppSrcCode = FuncData
  func = G.func
  objAccess = G.objAccess

instance GetSet CppSrcCode where
  get = G.get
  set = G.set

instance List CppSrcCode where
  listSize v = cast int (C.listSize v)
  listAdd = G.listAdd
  listAppend = G.listAppend 
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf l v = addAlgorithmImportVS $ cppIndexFunc l v #- iterBegin l
    
instance InternalList CppSrcCode where
  listSlice' = M.listSlice

instance InternalGetSet CppSrcCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

instance InternalListFunc CppSrcCode where
  listSizeFunc = CP.listSizeFunc
  listAddFunc = cppListAddFunc
  listAppendFunc = G.listAppendFunc cppListAppend
  listAccessFunc = CP.listAccessFunc' cppListAccess
  listSetFunc = CP.listSetFunc cppListSetDoc

instance RenderFunction CppSrcCode where
  funcFromData d = onStateValue (onCodeValue (`fd` d))
  
instance FunctionElim CppSrcCode where
  functionType = onCodeValue fType
  function = funcDoc . unCPPSC

instance InternalAssignStmt CppSrcCode where
  multiAssign _ _ = error $ C.multiAssignError cppName

instance InternalIOStmt CppSrcCode where
  printSt nl _ = cppPrint nl

instance InternalControlStmt CppSrcCode where
  multiReturn _ = error $ C.multiReturnError cppName

instance RenderStatement CppSrcCode where
  stmt = G.stmt
  loopStmt = G.loopStmt

  emptyStmt = G.emptyStmt

  stmtFromData d t = toState $ toCode (d, t)
  
instance StatementElim CppSrcCode where
  statement = fst . unCPPSC
  statementTerm = snd . unCPPSC

instance StatementSym CppSrcCode where
  type Statement CppSrcCode = (Doc, Terminator)
  valStmt = G.valStmt Semi
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement CppSrcCode where
  assign = G.assign Semi
  (&-=) = G.subAssign Semi
  (&+=) = G.increment
  (&++) = C.increment1
  (&--) = C.decrement1

instance DeclStatement CppSrcCode where
  varDec = C.varDec static dynamic empty
  varDecDef = C.varDecDef Semi 
  listDec n = C.listDec cppListDecDoc (litInt n)
  listDecDef = cppListDecDef cppListDecDefDoc
  arrayDec n vr = do
    let sz' = litInt n :: SValue CppSrcCode
    sz <- zoom lensMStoVS sz'
    v <- zoom lensMStoVS vr 
    mkStmt $ RC.type' (variableType v) <+> RC.variable v <> 
      brackets (RC.value sz)
  arrayDecDef vr vals = do
    vdc <- arrayDec (toInteger $ length vals) vr
    vs <- mapM (zoom lensMStoVS) vals
    mkStmt $ RC.statement vdc <+> equals <+> braces (valueList vs)
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = C.extObjDecNew
  constDecDef = CP.constDecDef
  funcDecDef = cppFuncDecDef

instance IOStatement CppSrcCode where
  print      = G.print False Nothing printFunc
  printLn    = G.print True  Nothing printLnFunc
  printStr   = G.print False Nothing printFunc   . litString
  printStrLn = G.print True  Nothing printLnFunc . litString

  printFile f      = G.print False (Just f) (printFileFunc f)
  printFileLn f    = G.print True  (Just f) (printFileLnFunc f)
  printFileStr f   = G.print False (Just f) (printFileFunc f)   . litString
  printFileStrLn f = G.print True  (Just f) (printFileLnFunc f) . litString

  getInput v = cppInput v inputFunc
  discardInput = addAlgorithmImport $ addLimitsImport $ cppDiscardInput '\n' 
    inputFunc
  getFileInput f v = cppInput v f
  discardFileInput f = addAlgorithmImport $ addLimitsImport $ 
    cppDiscardInput ' ' f

  openFileR = cppOpenFile cppR
  openFileW = cppOpenFile cppW
  openFileA = cppOpenFile cppA
  closeFile = G.closeFile cppClose

  getFileInputLine f v = valStmt $ getLineFunc f (valueOf v)
  discardFileLine f = addLimitsImport $ cppDiscardInput '\n' f
  getFileInputAll f v = let l_line = "nextLine"
                            var_line = var l_line string
                            v_line = valueOf var_line
                        in
    multi [varDec var_line,
      while (getLineFunc f v_line)
        (oneLiner $ valStmt $ listAppend (valueOf v) v_line)]

instance StringStatement CppSrcCode where
  stringSplit d vnew s = let l_ss = "ss"
                             var_ss = var l_ss (obj stringstream)
                             v_ss = valueOf var_ss
                             l_word = "word"
                             var_word = var l_word string
                             v_word = valueOf var_word
                         in
    modify (addLangImport sstream) >> multi [
      valStmt $ valueOf vnew $. clearFunc,
      varDec var_ss,
      valStmt $ strFunc v_ss s,
      varDec var_word,
      while (getLine3ArgFunc v_ss v_word d) 
        (oneLiner $ valStmt $ listAppend (valueOf vnew) v_word)
    ]

  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FuncAppStatement CppSrcCode where
  inOutCall = cppInOutCall funcApp
  selfInOutCall = cppInOutCall selfFuncApp
  extInOutCall m = cppInOutCall (extFuncApp m)

instance CommentStatement CppSrcCode where
  comment = G.comment commentStart

instance ControlStatement CppSrcCode where
  break = mkStmt R.break
  continue = mkStmt R.continue

  returnStmt = G.returnStmt Semi

  throw = G.throw cppThrowDoc Semi

  ifCond = G.ifCond parens bodyStart elseIfLabel bodyEnd
  switch = C.switch parens break

  ifExists _ ifBody _ = do 
    ifb <- ifBody 
    mkStmtNoEnd (RC.body ifb) -- All variables are initialized in C++

  for = C.for bodyStart bodyEnd 
  forRange = M.forRange
  forEach i v b = do 
    e <- zoom lensMStoVS i
    let l = variableName e
        t = toState $ variableType e
        iterI = var l (iterator t) 
    for (varDecDef iterI (iterBegin v)) (setIterVar iterI ?!= iterEnd v) 
      (iterI &++) b
  while = C.while parens bodyStart bodyEnd

  tryCatch = G.tryCatch cppTryCatch

instance StatePattern CppSrcCode where 
  checkState l = switchAsIf (valueOf $ var l string) 

instance ObserverPattern CppSrcCode where
  notifyObservers = M.notifyObservers

instance StrategyPattern CppSrcCode where
  runStrategy = M.runStrategy

instance ScopeSym CppSrcCode where
  type Scope CppSrcCode = (Doc, ScopeTag)
  private = toCode (R.private, Priv)
  public = toCode (R.public, Pub)

instance RenderScope CppSrcCode where
  scopeFromData s d = toCode (d, s)
  
instance ScopeElim CppSrcCode where
  scope = fst . unCPPSC

instance MethodTypeSym CppSrcCode where
  type MethodType CppSrcCode = TypeData
  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym CppSrcCode where
  type Parameter CppSrcCode = ParamData
  param = G.param R.param
  pointerParam = G.param cppPointerParamDoc

instance RenderParam CppSrcCode where
  paramFromData v' d = do 
    v <- zoom lensMStoVS v' 
    toState $ on2CodeValues pd v (toCode d) 
  
instance ParamElim CppSrcCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unCPPSC

instance MethodSym CppSrcCode where
  type Method CppSrcCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor = cppConstructor

  docMain b = commentedFunc (docComment $ toState $ functionDox mainDesc 
    [(argc, argcDesc), (argv, argvDesc)] [mainReturnDesc]) (mainFunction b)

  function = G.function
  mainFunction b = intFunc True mainFunc public static (mType int) 
    [param argcVar, param argvVar]
    (on2StateValues (on2CodeValues appendToBody) b (returnStmt $ litInt 0))
    where argcVar = var argc int
          argvVar = do 
            t <- typeFromData (List String) 
              (constDec ++ " " ++ C.charRender) (constDec' <+> text 
              C.charRender)
            mkVar argv t (cppDeref <> text argv <> array')

  docFunc = CP.doxFunc

  inOutMethod n s p = cppsInOut (method n s p)

  docInOutMethod n s p = CP.docInOutFunc (inOutMethod n s p)

  inOutFunc n s = cppsInOut (function n s)

  docInOutFunc n s = CP.docInOutFunc (inOutFunc n s)

instance RenderMethod CppSrcCode where
  intMethod m n s _ t ps b = do
    modify (if m then setCurrMain else id)
    c <- getClassName
    cppsIntFunc (cppsMethod [] n c) s t ps b
  intFunc m n s _ t ps b = do
    modify (if m then setCurrMainFunc m . setCurrMain else id)
    cppsIntFunc (cppsFunction n) s t ps b
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
    
  mthdFromData s d = toState $ toCode $ mthd s d
  
instance MethodElim CppSrcCode where
  method = mthdDoc . unCPPSC

instance StateVarSym CppSrcCode where
  type StateVar CppSrcCode = StateVarData
  stateVar s _ _ = onStateValue (on3CodeValues svd (onCodeValue snd s) (toCode 
    empty)) $ zoom lensCStoMS emptyStmt
  stateVarDef = cppsStateVarDef empty
  constVar s = cppsStateVarDef constDec' s static
  
instance StateVarElim CppSrcCode where
  stateVar = stVar . unCPPSC

instance ClassSym CppSrcCode where
  type Class CppSrcCode = Doc
  buildClass = G.buildClass
  extraClass = CP.extraClass
  implementingClass = G.implementingClass

  docClass = CP.doxClass

instance RenderClass CppSrcCode where
  intClass n _ _ vs fs = do
    modify (setClassName n)
    on2StateLists cppsClass vs (map (zoom lensCStoMS) $ fs ++ [destructor vs])

  inherit n = onCodeValue (cppInherit n . fst) public
  implements is = onCodeValue ((\p -> colon <+> hcat (map ((p <+>) . text) is)) 
    . fst) public

  commentedClass _ cs = cs
  
instance ClassElim CppSrcCode where
  class' = unCPPSC

instance ModuleSym CppSrcCode where
  type Module CppSrcCode = ModData
  buildModule n is ms cs = CP.buildModule n (do
    ds <- getDefines
    lis <- getLangImports
    libis <- getLibImports
    mis <- getModuleImports
    us <- getUsing
    mn <- getCurrMain
    pure $ vibcat [
      if mn && length ms + length cs == 1 then empty else RC.import' $ mi n,
      vcat (map ((define <+>) . text) ds),
      vcat (map (RC.import' . li) lis),
      vcat (map (RC.import' . mi) (sort (is ++ libis) ++ mis)),
      vcat (map (usingNameSpace std . Just) us)]) 
    (pure empty) (pure empty) ms cs
    where mi, li :: Label -> CppSrcCode (Import CppSrcCode)
          mi = modImport
          li = langImport

instance RenderMod CppSrcCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)
  
instance ModuleElim CppSrcCode where
  module' = modDoc . unCPPSC

instance BlockCommentSym CppSrcCode where
  type BlockComment CppSrcCode = Doc
  blockComment lns = toCode $ R.blockCmt lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns docCmtStart 
    blockCmtEnd)

instance BlockCommentElim CppSrcCode where
  blockComment' = unCPPSC

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
  CPPHC x >>= f = f x

instance RenderSym CppHdrCode

instance FileSym CppHdrCode where
  type File CppHdrCode = FileData
  fileDoc m = do
    modify (setFileType Header) 
    G.fileDoc cppHdrExt top bottom m
  
  docMod = CP.doxMod cppHdrExt

instance RenderFile CppHdrCode where
  top = onCodeValue cpphtop
  bottom = toCode endif
  
  commentedMod = on2StateValues (\m cmt -> if isEmpty (RC.module' $ 
    onCodeValue fileMod m) then m else on2CodeValues R.commentedMod m cmt)

  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym CppHdrCode where
  type Import CppHdrCode = Doc
  langImport n = toCode $ inc <+> angles (text n)
  modImport n = toCode $ inc <+> doubleQuotedText (addExt cppHdrExt n)

instance ImportElim CppHdrCode where
  import' = unCPPHC

instance PermanenceSym CppHdrCode where
  type Permanence CppHdrCode = BindData
  static = toCode $ bd Static R.static
  dynamic = toCode $ bd Dynamic R.dynamic

instance PermElim CppHdrCode where
  perm = bindDoc . unCPPHC
  binding = bind . unCPPHC

instance BodySym CppHdrCode where
  type Body CppHdrCode = Doc
  body _ = toState $ toCode empty

  addComments _ _ = toState $ toCode empty

instance RenderBody CppHdrCode where
  multiBody = G.multiBody 

instance BodyElim CppHdrCode where
  body = unCPPHC

instance BlockSym CppHdrCode where
  type Block CppHdrCode = Doc
  block _ = toState $ toCode empty

instance RenderBlock CppHdrCode where
  multiBlock = G.multiBlock

instance BlockElim CppHdrCode where
  block = unCPPHC

instance TypeSym CppHdrCode where
  type Type CppHdrCode = TypeData
  bool = cppBoolType
  int = CP.int
  float = C.float
  double = C.double
  char = C.char
  string = do
    modify (addHeaderUsing cppString . addHeaderLangImport cppString) 
    CP.string
  infile = do
    modify (addHeaderUsing cppInfile)
    cppInfileType
  outfile = do
    modify (addHeaderUsing cppOutfile)
    cppOutfileType
  listType t = do
    modify (addHeaderUsing vector . addHeaderLangImport vector)
    C.listType vector t
  arrayType = cppArrayType
  listInnerType = G.listInnerType
  obj n = getClassMap >>= (\cm -> maybe id ((>>) . modify . addHeaderModImport) 
    (Map.lookup n cm) $ G.obj n)
  funcType = CP.funcType
  void = C.void

instance TypeElim CppHdrCode where
  getType = cType . unCPPHC
  getTypeString = typeString . unCPPHC
  
instance RenderType CppHdrCode where
  multiType _ = error $ C.multiTypeError cppName
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim CppHdrCode where
  type' = typeDoc . unCPPHC

instance UnaryOpSym CppHdrCode where
  type UnaryOp CppHdrCode = OpData
  notOp = mkOp 0 empty
  negateOp = mkOp 0 empty
  sqrtOp = mkOp 0 empty
  absOp = mkOp 0 empty
  logOp = mkOp 0 empty
  lnOp = mkOp 0 empty
  expOp = mkOp 0 empty
  sinOp = mkOp 0 empty
  cosOp = mkOp 0 empty
  tanOp = mkOp 0 empty
  asinOp = mkOp 0 empty
  acosOp = mkOp 0 empty
  atanOp = mkOp 0 empty
  floorOp = mkOp 0 empty
  ceilOp = mkOp 0 empty

instance BinaryOpSym CppHdrCode where
  type BinaryOp CppHdrCode = OpData
  equalOp = mkOp 0 empty
  notEqualOp = mkOp 0 empty
  greaterOp = mkOp 0 empty
  greaterEqualOp = mkOp 0 empty
  lessOp = mkOp 0 empty
  lessEqualOp = mkOp 0 empty
  plusOp = mkOp 0 empty
  minusOp = mkOp 0 empty
  multOp = mkOp 0 empty
  divideOp = mkOp 0 empty
  powerOp = mkOp 0 empty
  moduloOp = mkOp 0 empty
  andOp = mkOp 0 empty
  orOp = mkOp 0 empty

instance OpElim CppHdrCode where
  uOp = opDoc . unCPPHC
  bOp = opDoc . unCPPHC
  uOpPrec = opPrec . unCPPHC
  bOpPrec = opPrec . unCPPHC

instance VariableSym CppHdrCode where
  type Variable CppHdrCode = VarData
  var = G.var
  staticVar = G.staticVar
  constant _ _ = mkStateVar "" void empty
  extVar _ _ _ = mkStateVar "" void empty
  self = mkStateVar "" void empty
  classVar _ _ = mkStateVar "" void empty
  extClassVar _ _ = mkStateVar "" void empty
  objVar = G.objVar
  objVarSelf _ = mkStateVar "" void empty
  arrayElem _ _ = mkStateVar "" void empty
  
instance VariableElim CppHdrCode where
  variableName = varName . unCPPHC
  variableType = onCodeValue varType

instance InternalVarElim CppHdrCode where
  variableBind = varBind . unCPPHC
  variable = varDoc . unCPPHC

instance RenderVariable CppHdrCode where
  varFromData b n t' d = do 
    t <- t' 
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym CppHdrCode where
  type Value CppHdrCode = ValData
  valueType = onCodeValue valType

instance Argument CppHdrCode where
  pointerArg = id

instance Literal CppHdrCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = C.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = CP.litArray braces
  litList _ _ = error $ "List literals not supported in " ++ cppName

instance MathConstant CppHdrCode where
  pi = do
    modify (addHeaderDefine mathDefines . addHeaderLangImport mathh) 
    mkStateVal double cppPi

instance VariableValue CppHdrCode where
  valueOf = G.valueOf

instance CommandLineArgs CppHdrCode where
  arg n = G.arg (litInt $ n+1) argsList
  argsList = G.argsList argv
  argExists _ = mkStateVal void empty

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

instance Comparison CppHdrCode where
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

instance RenderValue CppHdrCode where
  inputFunc = mkStateVal void empty
  printFunc = mkStateVal void empty
  printLnFunc = mkStateVal void empty
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty
  
  cast _ _ = mkStateVal void empty
  
  call _ _ _ _ _ _ = mkStateVal void empty

  valFromData p t' d = do 
    t <- t' 
    toState $ on2CodeValues (vd p) t (toCode d)
  
instance ValueElim CppHdrCode where
  valuePrec = valPrec . unCPPHC
  value = val . unCPPHC
  
instance InternalValueExp CppHdrCode where
  objMethodCallMixedArgs' _ _ _ _ _ = mkStateVal void empty

instance FunctionSym CppHdrCode where
  type Function CppHdrCode = FuncData
  func _ _ _ = funcFromData empty void
  objAccess _ _ = mkStateVal void empty

instance GetSet CppHdrCode where
  get _ _ = mkStateVal void empty
  set _ _ _ = mkStateVal void empty

instance List CppHdrCode where
  listSize _ = mkStateVal void empty
  listAdd _ _ _ = mkStateVal void empty
  listAppend _ _ = mkStateVal void empty
  listAccess _ _ = mkStateVal void empty
  listSet _ _ _ = mkStateVal void empty
  indexOf _ _ = mkStateVal void empty
  
instance InternalList CppHdrCode where
  listSlice' _ _ _ _ _ = toState $ toCode empty

instance InternalGetSet CppHdrCode where
  getFunc _ = funcFromData empty void
  setFunc _ _ _ = funcFromData empty void

instance InternalListFunc CppHdrCode where
  listSizeFunc = funcFromData empty void
  listAddFunc _ _ _ = funcFromData empty void
  listAppendFunc _ = funcFromData empty void
  listAccessFunc _ _ = funcFromData empty void
  listSetFunc _ _ _ = funcFromData empty void
  
instance RenderFunction CppHdrCode where
  funcFromData d = onStateValue (onCodeValue (`fd` d))
  
instance FunctionElim CppHdrCode where
  functionType = onCodeValue fType
  function = funcDoc . unCPPHC

instance InternalAssignStmt CppHdrCode where
  multiAssign _ _ = emptyStmt

instance InternalIOStmt CppHdrCode where
  printSt _ _ _ _ = emptyStmt
  
instance InternalControlStmt CppHdrCode where
  multiReturn _ = emptyStmt

instance RenderStatement CppHdrCode where
  stmt = G.stmt
  loopStmt _ = emptyStmt

  emptyStmt = G.emptyStmt
  
  stmtFromData d t = toState $ toCode (d, t)
  
instance StatementElim CppHdrCode where
  statement = fst . unCPPHC
  statementTerm = snd . unCPPHC

instance StatementSym CppHdrCode where
  type Statement CppHdrCode = (Doc, Terminator)
  valStmt _ = emptyStmt
  multi _ = emptyStmt

instance AssignStatement CppHdrCode where
  assign _ _ = emptyStmt
  (&-=) _ _ = emptyStmt
  (&+=) _ _ = emptyStmt
  (&++) _ = emptyStmt
  (&--) _ = emptyStmt

instance DeclStatement CppHdrCode where
  varDec = C.varDec static dynamic empty
  varDecDef = C.varDecDef Semi
  listDec _ _ = emptyStmt
  listDecDef _ _ = emptyStmt
  arrayDec _ _ = emptyStmt
  arrayDecDef _ _ = emptyStmt
  objDecDef _ _ = emptyStmt
  objDecNew _ _ = emptyStmt
  extObjDecNew _ _ _ = emptyStmt
  constDecDef = CP.constDecDef
  funcDecDef _ _ _ = emptyStmt

instance IOStatement CppHdrCode where
  print _ = emptyStmt
  printLn _ = emptyStmt
  printStr _ = emptyStmt
  printStrLn _ = emptyStmt

  printFile _ _ = emptyStmt
  printFileLn _ _ = emptyStmt
  printFileStr _ _ = emptyStmt
  printFileStrLn _ _ = emptyStmt

  getInput _ = emptyStmt
  discardInput = emptyStmt
  getFileInput _ _ = emptyStmt
  discardFileInput _ = emptyStmt

  openFileR _ _ = emptyStmt
  openFileW _ _ = emptyStmt
  openFileA _ _ = emptyStmt
  closeFile _ = emptyStmt

  getFileInputLine _ _ = emptyStmt
  discardFileLine _ = emptyStmt
  getFileInputAll _ _ = emptyStmt

instance StringStatement CppHdrCode where
  stringSplit _ _ _ = emptyStmt

  stringListVals _ _ = emptyStmt
  stringListLists _ _ = emptyStmt

instance FuncAppStatement CppHdrCode where
  inOutCall _ _ _ _ = emptyStmt
  selfInOutCall _ _ _ _ = emptyStmt
  extInOutCall _ _ _ _ _ = emptyStmt

instance CommentStatement CppHdrCode where
  comment _ = emptyStmt

instance ControlStatement CppHdrCode where
  break = emptyStmt
  continue = emptyStmt

  returnStmt _ = emptyStmt

  throw _ = emptyStmt

  ifCond _ _ = emptyStmt
  switch _ _ _ = emptyStmt

  ifExists _ _ _ = emptyStmt

  for _ _ _ _ = emptyStmt
  forRange _ _ _ _ _ = emptyStmt
  forEach _ _ _ = emptyStmt
  while _ _ = emptyStmt

  tryCatch _ _ = emptyStmt

instance StatePattern CppHdrCode where
  checkState _ _ _ = emptyStmt

instance ObserverPattern CppHdrCode where
  notifyObservers _ _ = emptyStmt

instance StrategyPattern CppHdrCode where
  runStrategy _ _ _ _ = toState $ toCode empty

instance ScopeSym CppHdrCode where
  type Scope CppHdrCode = (Doc, ScopeTag)
  private = toCode (R.private, Priv)
  public = toCode (R.public, Pub)

instance RenderScope CppHdrCode where
  scopeFromData s d = toCode (d, s)
  
instance ScopeElim CppHdrCode where
  scope = fst . unCPPHC

instance MethodTypeSym CppHdrCode where
  type MethodType CppHdrCode = TypeData
  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym CppHdrCode where
  type Parameter CppHdrCode = ParamData
  param v' = do 
    v <- zoom lensMStoVS v'
    paramFromData v' (R.param v)
  pointerParam v' = do 
    v <- zoom lensMStoVS v' 
    paramFromData v' (cppPointerParamDoc v)

instance RenderParam CppHdrCode where
  paramFromData v' d = do 
    v <- zoom lensMStoVS v' 
    toState $ on2CodeValues pd v (toCode d) 
  
instance ParamElim CppHdrCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unCPPHC

instance MethodSym CppHdrCode where
  type Method CppHdrCode = MethodData
  method = G.method
  getMethod v = zoom lensMStoVS v >>= (\v' -> method (getterName $ variableName 
    v') public dynamic (toState $ variableType v') [] (toState $ toCode empty))
  setMethod v = zoom lensMStoVS v >>= (\v' -> method (setterName $ variableName 
    v') public dynamic void [param v] (toState $ toCode empty))
  constructor ps is b = getClassName >>= (\n -> CP.constructor n ps is b)

  docMain = mainFunction

  function = G.function
  mainFunction _ = modifyReturn (setScope Pub) $ toCode $ mthd Pub empty

  docFunc = CP.doxFunc

  inOutMethod n s p = cpphInOut (method n s p)

  docInOutMethod n s p = CP.docInOutFunc (inOutMethod n s p)

  inOutFunc n s = cpphInOut (function n s)

  docInOutFunc n s = CP.docInOutFunc (inOutFunc n s)

instance RenderMethod CppHdrCode where
  intMethod _ n s _ t ps _ = do
    modify (setScope (snd $ unCPPHC s)) 
    tp <- t
    pms <- sequence ps
    pure $ toCode $ mthd (snd $ unCPPHC s) $ cpphMethod n tp pms
  intFunc = C.intFunc
  commentedFunc = cppCommentedFunc Header

  destructor vars = do
    n <- getClassName
    m <- pubMethod ('~':n) void [] (pure (toCode empty)) :: SMethod CppHdrCode
    vs <- mapM (zoom lensMStoCS) vars
    pure $ toCode $ mthd Pub (emptyIfEmpty 
      (vcat (map (RC.statement . onCodeValue destructSts) vs)) (RC.method m))
      
  mthdFromData s d = toState $ toCode $ mthd s d
  
instance MethodElim CppHdrCode where
  method = mthdDoc . unCPPHC

instance StateVarSym CppHdrCode where
  type StateVar CppHdrCode = StateVarData
  stateVar s p v = do
    dec <- zoom lensCStoMS $ stmt $ C.varDec static dynamic (text "&") v
    emptS <- zoom lensCStoMS emptyStmt
    pure $ on3CodeValues svd (onCodeValue snd s)
      (toCode $ R.stateVar empty (RC.perm p) (RC.statement dec)) emptS
  stateVarDef s p vr vl = on2StateValues (onCodeValue . svd (snd $ unCPPHC s))
    (cpphStateVarDef empty p vr vl) (zoom lensCStoMS emptyStmt)
  constVar s vr _ = on2StateValues (on3CodeValues svd (onCodeValue snd s) . 
    on2CodeValues (R.constVar empty endStatement) (bindDoc <$> static))
    (zoom lensCStoVS vr) (zoom lensCStoMS emptyStmt)
  
instance StateVarElim CppHdrCode where
  stateVar = stVar . unCPPHC

instance ClassSym CppHdrCode where
  type Class CppHdrCode = Doc
  buildClass = G.buildClass
  extraClass = CP.extraClass
  implementingClass = G.implementingClass

  docClass = CP.doxClass

instance RenderClass CppHdrCode where
  intClass n _ i vs mths = do
    modify (setClassName n)
    vars <- sequence vs
    funcs <- sequence fs
    pure $ cpphClass n i vars funcs public private
    where fs = map (zoom lensCStoMS) $ mths ++ [destructor vs]

  inherit n = onCodeValue (cppInherit n . fst) public
  implements is = onCodeValue ((\p -> colon <+> hcat (map ((p <+>) . text) is)) 
    . fst) public

  commentedClass = G.commentedClass
  
instance ClassElim CppHdrCode where
  class' = unCPPHC

instance ModuleSym CppHdrCode where
  type Module CppHdrCode = ModData
  buildModule n is = CP.buildModule n (do
    ds <- getHeaderDefines
    lis <- getHeaderLangImports
    libis <- getHeaderLibImports
    mis <- getHeaderModImports
    us <- getHeaderUsing
    pure $ vibcat [
      vcat (map ((define <+>) . text) ds),
      vcat (map (RC.import' . li) lis),
      vcat (map (RC.import' . mi) (sort (is ++ libis) ++ mis)),
      vcat (map (usingNameSpace std . Just) us)]) 
    (pure empty) (pure empty)
    where mi, li :: Label -> CppHdrCode (Import CppHdrCode)
          mi = modImport
          li = langImport

instance RenderMod CppHdrCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)
  
instance ModuleElim CppHdrCode where
  module' = modDoc . unCPPHC

instance BlockCommentSym CppHdrCode where
  type BlockComment CppHdrCode = Doc
  blockComment lns = toCode $ R.blockCmt lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns docCmtStart 
    blockCmtEnd)

instance BlockCommentElim CppHdrCode where
  blockComment' = unCPPHC

-- helpers
isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

getParam :: (RenderSym r) => SVariable r -> MSParameter r
getParam v = zoom lensMStoVS v >>= (\v' -> getParamFunc ((getType . 
  variableType) v') v)
  where getParamFunc (List _) = pointerParam
        getParamFunc (Object _) = pointerParam
        getParamFunc _ = param
 
data MethodData = MthD {getMthdScp :: ScopeTag, mthdDoc :: Doc}

mthd :: ScopeTag -> Doc -> MethodData
mthd = MthD 

addAlgorithmImport :: MS a -> MS a 
addAlgorithmImport v = do
  modify (addLangImport algorithm)
  v

addAlgorithmImportVS :: VS a -> VS a
addAlgorithmImportVS v = do
  modify (addLangImportVS algorithm)
  v

addFStreamImport :: a -> VS a
addFStreamImport = modifyReturn (addLangImportVS fstream)

addIOStreamImport :: VS a -> VS a
addIOStreamImport v = do
  modify (addLangImportVS iostream)
  v

addMathHImport :: VS a -> VS a
addMathHImport v = do
  modify (addLangImportVS mathh)
  v

addLimitsImport :: MS a -> MS a
addLimitsImport v = do
  modify (addLangImport limits)
  v

setIterVar :: RenderSym r=> SVariable r -> SValue r
setIterVar = G.valueOf

iterator :: RenderSym r => VSType r -> VSType r
iterator t = do 
    modify (addLangImportVS cppIterator)
    cppIterType $ listType t

iterBegin :: SValue CppSrcCode -> SValue CppSrcCode
iterBegin v = v $. cppIterBeginFunc (G.listInnerType $ onStateValue valueType v)

iterEnd :: SValue CppSrcCode -> SValue CppSrcCode
iterEnd v = v $. cppIterEndFunc (G.listInnerType $ onStateValue valueType v)

-- convenience
cppName, cppVersion :: String
cppName = "C++"
cppVersion = "gcc 10.1"

guard, inc, ifndef, define, defineSuffix, endif, using, namespace, cppPtr, 
  cppDeref, streamL, streamR, cppLambdaDec, cppLambdaSep, catchAll, cppPi,
  ptrAccess' :: Doc
guard = text "#"
inc = guard <> text "include"
ifndef = guard <> text "ifndef"
define = guard <> text "define"
defineSuffix = text "_h"
endif = guard <> text "endif"
using = text "using"
namespace = text "namespace"
cppPtr = text "&"
cppDeref = text "*"
streamL = text "<<"
streamR = text ">>"
cppLambdaDec = text "[]"
cppLambdaSep = text "->"
catchAll = text "..."
cppPi = text "M_PI"
ptrAccess' = text ptrAccess

nmSpc, ptrAccess, std, algorithm, cppString, vector, sstream, stringstream, 
  fstream, iostream, limits, mathh, cppBool, cppInfile, cppOutfile, 
  cppIterator, cppOpen, stod, stof, cppIgnore, numLimits, streamsize, max, 
  endl, cin, cout, cppIndex, cppListAccess, cppListAdd, cppListAppend, 
  cppIterBegin, cppIterEnd, cppR, cppW, cppA, cppGetLine, cppClose, cppClear, 
  cppStr, mathDefines :: String
nmSpc = "::"
ptrAccess = "->"
std = "std"
algorithm = "algorithm"
cppString = "string"
vector = "vector"
fstream = "fstream"
iostream = "iostream"
sstream = "sstream"
stringstream = stdAccess "stringstream"
limits = "limits"
mathh = "math.h"
cppBool = "bool"
cppInfile = "ifstream"
cppOutfile = "ofstream"
cppIterator = "iterator"
cppOpen = "open"
stod = stdAccess "stod"
stof = stdAccess "stof"
cppIgnore = "ignore"
numLimits = stdAccess "numeric_limits"
streamsize = stdAccess "streamsize"
max = "max"
endl = stdAccess "endl"
cin = stdAccess "cin"
cout = stdAccess "cout"
cppIndex= "find"
cppListAccess = "at"
cppListAdd = "insert"
cppListAppend = "push_back"
cppIterBegin = "begin"
cppIterEnd = "end"
cppR = stdAccess (fstream `nmSpcAccess` "in")
cppW = stdAccess (fstream `nmSpcAccess` "out")
cppA = stdAccess (fstream `nmSpcAccess` "app")
cppGetLine = stdAccess "getline"
cppClose = "close"
cppClear = "clear"
cppStr = "str"
mathDefines = "_USE_MATH_DEFINES"

nmSpcAccess :: String -> String -> String
nmSpcAccess ns e = ns ++ nmSpc ++ e 

nmSpcAccess' :: Doc -> Doc -> Doc
nmSpcAccess' ns e = ns <> text nmSpc <> e

stdAccess :: String -> String
stdAccess = nmSpcAccess std

stdAccess' :: Doc -> Doc
stdAccess' = nmSpcAccess' (text std)

mainDesc, argcDesc, argvDesc, mainReturnDesc :: String
mainDesc = "Controls the flow of the program" 
argcDesc = "Number of command-line arguments"
argvDesc = "List of command-line arguments"
mainReturnDesc = "exit code"

cppSqrtOp :: (Monad r) => VSOp r
cppSqrtOp = cppUnaryMath R.sqrt

cppAbsOp :: (Monad r) => VSOp r
cppAbsOp = cppUnaryMath R.fabs

cppLogOp :: (Monad r) => VSOp r
cppLogOp = cppUnaryMath R.log10

cppLnOp :: (Monad r) => VSOp r
cppLnOp = cppUnaryMath R.log

cppExpOp :: (Monad r) => VSOp r
cppExpOp = cppUnaryMath R.exp

cppSinOp :: (Monad r) => VSOp r
cppSinOp = cppUnaryMath R.sin

cppCosOp :: (Monad r) => VSOp r
cppCosOp = cppUnaryMath R.cos

cppTanOp :: (Monad r) => VSOp r
cppTanOp = cppUnaryMath R.tan

cppAsinOp :: (Monad r) => VSOp r
cppAsinOp = cppUnaryMath R.asin

cppAcosOp :: (Monad r) => VSOp r
cppAcosOp = cppUnaryMath R.acos

cppAtanOp :: (Monad r) => VSOp r
cppAtanOp = cppUnaryMath R.atan

cppFloorOp :: (Monad r) => VSOp r
cppFloorOp = cppUnaryMath R.floor

cppCeilOp :: (Monad r) => VSOp r
cppCeilOp = cppUnaryMath R.ceil

cppUnaryMath :: (Monad r) => String -> VSOp r
cppUnaryMath = addMathHImport . unOpPrec 

cppPowerOp :: (Monad r) => VSOp r
cppPowerOp = powerPrec R.pow

getLineFunc :: SValue CppSrcCode -> SValue CppSrcCode -> SValue CppSrcCode
getLineFunc f v = funcApp cppGetLine string [f, v]

getLine3ArgFunc :: SValue CppSrcCode -> SValue CppSrcCode -> Char -> 
  SValue CppSrcCode
getLine3ArgFunc s v d = funcApp cppGetLine string [s, v, litChar d]

clearFunc :: VSFunction CppSrcCode
clearFunc = func cppClear void []

strFunc :: SValue CppSrcCode -> SValue CppSrcCode -> SValue CppSrcCode
strFunc v s = objMethodCall string v cppStr [s]

cppIndexFunc :: SValue CppSrcCode -> SValue CppSrcCode -> SValue CppSrcCode
cppIndexFunc l v = funcApp cppIndex int [iterBegin l, iterEnd l, v]

cppListAddFunc :: SValue CppSrcCode -> SValue CppSrcCode -> SValue CppSrcCode 
  -> VSFunction CppSrcCode
cppListAddFunc l i v = func cppListAdd (onStateValue valueType l) 
    [iterBegin l #+ i, v]

cppIterBeginFunc :: VSType CppSrcCode -> VSFunction CppSrcCode
cppIterBeginFunc t = func cppIterBegin (iterator t) []

cppIterEndFunc :: VSType CppSrcCode -> VSFunction CppSrcCode
cppIterEndFunc t = func cppIterEnd (iterator t) []

cppListDecDef :: (RenderSym r) => ([r (Value r)] -> Doc) -> SVariable r -> 
  [SValue r] -> MSStatement r
cppListDecDef f v vls = do 
  vdc <- varDec v
  vs <- zoom lensMStoVS $ sequence vls
  mkStmt (RC.statement vdc <> f vs)

cpphtop :: ModData -> Doc
cpphtop m = vcat [
  ifndef <+> text n <> defineSuffix,
  define <+> text n <> defineSuffix]
  where n = name m

usingNameSpace :: Label -> Maybe Label -> Doc
usingNameSpace n (Just m) = using <+> text n <> colon <> colon <>
  text m <> endStatement
usingNameSpace n Nothing = using <+> namespace <+> text n <> endStatement

cppInherit :: Maybe Label -> Doc -> Doc
cppInherit n pub = maybe empty ((colon <+> pub <+>) . text) n

cppBoolType :: (RenderSym r) => VSType r
cppBoolType = typeFromData Boolean cppBool (text cppBool)

cppInfileType :: (RenderSym r) => VSType r
cppInfileType = do 
  t <- typeFromData InFile cppInfile (text cppInfile)
  addFStreamImport t

cppOutfileType :: (RenderSym r) => VSType r
cppOutfileType = do 
  t <- typeFromData OutFile cppOutfile (text cppOutfile)
  addFStreamImport t

cppArrayType :: (RenderSym r) => VSType r -> VSType r
cppArrayType t' = do 
  t <- t' 
  typeFromData (Array (getType t)) (getTypeString t) (RC.type' t)

cppIterType :: (RenderSym r) => VSType r -> VSType r
cppIterType t' = do 
  t <- t' 
  typeFromData (getType t) 
    (getTypeString t `nmSpcAccess` cppIterator) (stdAccess' (RC.type' t) 
    `nmSpcAccess'` text cppIterator)

cppClassVar :: Doc -> Doc -> Doc
cppClassVar c v = c `nmSpcAccess'` v

cppLambda :: (RenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
cppLambda ps ex = cppLambdaDec <+> parens (hicat listSep' $ zipWith (<+>) 
  (map (RC.type' . variableType) ps) (map RC.variable ps)) <+> cppLambdaSep <+> 
  bodyStart <> returnLabel <+> RC.value ex <> endStatement <> bodyEnd

stodFunc :: SValue CppSrcCode -> SValue CppSrcCode
stodFunc v = funcApp stod double [v]

stofFunc :: SValue CppSrcCode -> SValue CppSrcCode
stofFunc v = funcApp stof float [v]

ignoreFunc :: Char -> SValue CppSrcCode -> SValue CppSrcCode
ignoreFunc sep inFn = objMethodCall void inFn cppIgnore [maxFunc, litChar sep]

maxFunc :: SValue CppSrcCode
maxFunc = funcApp ((numLimits `containing` streamsize) `nmSpcAccess` max) int []

cppCast :: VSType CppSrcCode -> SValue CppSrcCode -> SValue CppSrcCode
cppCast = join .: on2StateValues (\t v -> cppCast' (getType t) (getType $ 
  valueType v) t v)
  where cppCast' Double String _ v = stodFunc (toState v)
        cppCast' Float String _ v = stofFunc (toState v)
        cppCast' _ _ t v = mkStateVal (toState t) (R.castObj (R.cast (RC.type' 
          t)) (RC.value v))

cppListSetDoc :: Doc -> Doc -> Doc
cppListSetDoc i v = dot <> text cppListAccess <> parens i <+> equals <+> v

cppListDecDoc :: (RenderSym r) => r (Value r) -> Doc
cppListDecDoc n = parens (RC.value n)

cppListDecDefDoc :: (RenderSym r) => [r (Value r)] -> Doc
cppListDecDefDoc vs = braces (valueList vs)

cppFuncDecDef :: (RenderSym r) => SVariable r -> [SVariable r] -> MSBody r -> 
  MSStatement r
cppFuncDecDef v ps bod = do
  vr <- zoom lensMStoVS v
  pms <- mapM (zoom lensMStoVS) ps
  b <- bod
  mkStmt $ RC.type' (variableType vr) <+> RC.variable vr <+> equals <+>
    cppLambdaDec <+> parens (hicat listSep' $ zipWith (<+>) (map (RC.type' . 
    variableType) pms) (map RC.variable pms)) <+> cppLambdaSep <+> bodyStart $$ 
    indent (RC.body b) $$ bodyEnd

cppPrint :: (RenderSym r) => Bool -> SValue r -> SValue r -> MSStatement r
cppPrint newLn pf vl = do
  e <- zoom lensMStoVS end
  printFn <- zoom lensMStoVS pf
  v <- zoom lensMStoVS vl
  mkStmt $ RC.value printFn <+> streamL <+> pars v (RC.value v) <+> e
  where pars v = if maybe False (< 9) (valuePrec v) then parens else id
        end = if newLn then addIOStreamImport (pure $ streamL <+> text endl) 
          else pure empty

cppThrowDoc :: (RenderSym r) => r (Value r) -> Doc
cppThrowDoc errMsg = throwLabel <> parens (RC.value errMsg)

cppTryCatch :: (RenderSym r) => r (Body r) -> r (Body r) -> Doc
cppTryCatch tb cb = vcat [
  tryLabel <+> lbrace,
  indent $ RC.body tb,
  rbrace <+> catchLabel <+> parens catchAll <+> lbrace,
  indent $ RC.body cb,
  rbrace]

cppDiscardInput :: Char -> SValue CppSrcCode -> MSStatement CppSrcCode
cppDiscardInput sep inFn = valStmt $ ignoreFunc sep inFn

cppInput :: SVariable CppSrcCode -> SValue CppSrcCode -> MSStatement CppSrcCode
cppInput vr i = addAlgorithmImport $ addLimitsImport $ do
  v <- zoom lensMStoVS vr
  inFn <- zoom lensMStoVS i
  multi [mkStmt (RC.value inFn <+> streamR <+> RC.variable v), 
    valStmt $ ignoreFunc '\n' i]

cppOpenFile :: (RenderSym r) => Label -> SVariable r -> SValue r -> MSStatement r
cppOpenFile mode f n = valStmt $ objMethodCall void (valueOf f) cppOpen [n, 
  mkStateVal void $ text mode]

cppPointerParamDoc :: (RenderSym r) => r (Variable r) -> Doc
cppPointerParamDoc v = RC.type' (variableType v) <+> cppPtr <> RC.variable v

cppsMethod :: [Doc] -> Label -> Label -> CppSrcCode (MethodType CppSrcCode) 
  -> [CppSrcCode (Parameter CppSrcCode)] -> CppSrcCode (Body CppSrcCode) -> Doc
cppsMethod is n c t ps b = emptyIfEmpty (RC.body b <> initList) $ 
  vcat [ttype <+> text (c `nmSpcAccess` n) <> parens (parameterList 
    ps) <+> emptyIfEmpty initList (colon <+> initList) <+> bodyStart,
  indent (RC.body b),
  bodyEnd]
  where ttype | isDtor n = empty
              | otherwise = RC.type' t
        initList = hicat listSep' is

cppConstructor :: [MSParameter CppSrcCode] -> NamedArgs CppSrcCode -> 
  MSBody CppSrcCode -> SMethod CppSrcCode
cppConstructor ps is b = getClassName >>= (\n -> join $ (\tp pms ivars ivals 
  bod -> if null is then CP.constructor n ps is b else modify (setScope Pub) >> 
  toState (toCode $ mthd Pub (cppsMethod (zipWith (\ivar ival -> RC.variable 
  ivar <> parens (RC.value ival)) ivars ivals) n n tp pms bod))) <$> construct 
  n <*> sequence ps <*> mapM (zoom lensMStoVS . fst) is <*> mapM (zoom 
  lensMStoVS . snd) is <*> b)

cppsFunction :: Label -> CppSrcCode (Type CppSrcCode) -> 
  [CppSrcCode (Parameter CppSrcCode)] -> CppSrcCode (Body CppSrcCode) -> Doc
cppsFunction n t ps b = vcat [
  RC.type' t <+> text n <> parens (parameterList ps) <+> bodyStart,
  indent (RC.body b),
  bodyEnd]
  
cppsIntFunc :: (CppSrcCode (Type CppSrcCode) -> 
  [CppSrcCode (Parameter CppSrcCode)] -> CppSrcCode (Body CppSrcCode) -> Doc) 
  -> CppSrcCode (Scope CppSrcCode) -> MSMthdType CppSrcCode -> 
  [MSParameter CppSrcCode] -> MSBody CppSrcCode -> SMethod CppSrcCode
cppsIntFunc f s t ps b = do
  modify (setScope (snd $ unCPPSC s))
  tp <- t
  pms <- sequence ps
  toCode . mthd (snd $ unCPPSC s) . f tp pms <$> b

cpphMethod :: (RenderSym r) => Label -> r (Type r) -> [r (Parameter r)] -> Doc
cpphMethod n t ps = (if isDtor n then empty else RC.type' t) <+> text n 
  <> parens (parameterList ps) <> endStatement

cppCommentedFunc :: (RenderSym r, Monad r) => FileType -> 
  MS (r (BlockComment r)) -> MS (r MethodData) -> MS (r MethodData)
cppCommentedFunc ft cmt fn = do
  f <- fn
  mn <- getCurrMainFunc
  scp <- getScope
  cmnt <- cmt
  let cf = pure (onCodeValue (mthd scp . R.commentedItem 
        (RC.blockComment' cmnt) . mthdDoc) f)
      ret Source = if mn then cf else pure f
      ret Header = if mn then pure f else cf
      ret Combined = error "Combined passed to cppCommentedFunc"
  ret ft

cppsStateVarDef :: Doc -> CppSrcCode (Scope CppSrcCode) -> 
  CppSrcCode (Permanence CppSrcCode) -> SVariable CppSrcCode -> 
  SValue CppSrcCode -> CSStateVar CppSrcCode
cppsStateVarDef cns s p vr' vl' = do
  vr <- zoom lensCStoVS vr'
  vl <- zoom lensCStoVS vl'
  n <- zoom lensCStoMS getClassName
  emptS <- zoom lensCStoMS emptyStmt
  pure $ on3CodeValues svd (onCodeValue snd s) 
    (toCode $ onBinding (binding p) (cns <+> RC.type' (variableType vr) <+> 
      text n `nmSpcAccess'` RC.variable vr <+> equals <+> RC.value vl <> 
      endStatement) empty) 
    emptS

cpphStateVarDef :: (RenderSym r) => Doc -> r (Permanence r) -> SVariable r -> 
  SValue r -> CS Doc
cpphStateVarDef s p vr vl = onStateValue (R.stateVar s (RC.perm p) .  
  RC.statement) (zoom lensCStoMS $ stmt $ onBinding (binding p) (varDec 
  vr) (varDecDef vr vl)) 

cpphVarsFuncsList :: ScopeTag -> [CppHdrCode (StateVar CppHdrCode)] -> 
  [CppHdrCode (Method CppHdrCode)] -> Doc
cpphVarsFuncsList st vs fs = 
  let scopedVs = [RC.stateVar v | v <- vs, getStVarScp (unCPPHC v) == st]
      scopedFs = [RC.method f | f <- fs, getMthdScp (unCPPHC f) == st]
  in vcat $ scopedVs ++ (if null scopedVs then empty else blank) : scopedFs

cppsClass :: [CppSrcCode (StateVar CppSrcCode)] -> 
  [CppSrcCode (Method CppSrcCode)] -> CppSrcCode (Class CppSrcCode)
cppsClass vs fs = toCode $ vibcat $ vcat vars : funcs
  where vars = map RC.stateVar vs
        funcs = map RC.method fs

cpphClass :: Label -> CppHdrCode ParentSpec -> 
  [CppHdrCode (StateVar CppHdrCode)] -> [CppHdrCode (Method CppHdrCode)] -> 
  CppHdrCode (Scope CppHdrCode) -> CppHdrCode (Scope CppHdrCode) -> 
  CppHdrCode (Class CppHdrCode)
cpphClass n ps vars funcs pub priv = let 
  pubs  = cpphVarsFuncsList Pub vars funcs
  privs = cpphVarsFuncsList Priv vars funcs
  ifEmptyPubs  = emptyIfEmpty pubs
  ifEmptyPrivs = emptyIfEmpty privs
  indLi = [ifEmptyPubs (RC.scope pub <> colon), ifEmptyPubs (indent pubs),
          ifEmptyPubs (ifEmptyPrivs blank),
          ifEmptyPrivs (RC.scope priv <> colon), ifEmptyPrivs (indent privs)]
  in onCodeValue (\p -> vcat [ 
    classDec <+> text n <+> p <+> bodyStart,
    indentList indLi,
    bodyEnd <> endStatement]) ps

cppInOutCall :: (Label -> VSType CppSrcCode -> [SValue CppSrcCode] -> 
  SValue CppSrcCode) -> Label -> [SValue CppSrcCode] -> [SVariable CppSrcCode] 
  -> [SVariable CppSrcCode] -> MSStatement CppSrcCode
cppInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out)
  ins
cppInOutCall f n ins [] [out] = assign out $ f n (onStateValue variableType out)
  (valueOf out : ins)
cppInOutCall f n ins outs both = valStmt $ f n void (map valueOf both ++ ins 
  ++ map valueOf outs)

cppsInOut :: (VSType CppSrcCode -> [MSParameter CppSrcCode] -> MSBody CppSrcCode -> 
    SMethod CppSrcCode) -> 
  [SVariable CppSrcCode] -> [SVariable CppSrcCode] -> [SVariable CppSrcCode] -> 
  MSBody CppSrcCode -> SMethod CppSrcCode
cppsInOut f ins [v] [] b = f (onStateValue variableType v) 
  (cppInOutParams ins [v] []) (on3StateValues (on3CodeValues surroundBody) 
  (varDec v) b (returnStmt $ valueOf v))
cppsInOut f ins [] [v] b = f (onStateValue variableType v) 
  (cppInOutParams ins [] [v]) (on2StateValues (on2CodeValues appendToBody) b 
  (returnStmt $ valueOf v))
cppsInOut f ins outs both b = f void (cppInOutParams ins outs both) b

cpphInOut :: (VSType CppHdrCode -> [MSParameter CppHdrCode] -> MSBody CppHdrCode -> 
    SMethod CppHdrCode) -> 
  [SVariable CppHdrCode] -> [SVariable CppHdrCode] -> [SVariable CppHdrCode] -> 
  MSBody CppHdrCode -> SMethod CppHdrCode
cpphInOut f ins [v] [] b = f (onStateValue variableType v) 
  (cppInOutParams ins [v] []) b
cpphInOut f ins [] [v] b = f (onStateValue variableType v) 
  (cppInOutParams ins [] [v]) b
cpphInOut f ins outs both b = f void (cppInOutParams ins outs both) b

cppInOutParams :: (RenderSym r) => [SVariable r] -> [SVariable r] -> 
  [SVariable r] -> [MSParameter r]
cppInOutParams ins [_] [] = map getParam ins
cppInOutParams ins [] [v] = map getParam $ v : ins
cppInOutParams ins outs both = map pointerParam both ++ map getParam ins ++ 
  map pointerParam outs