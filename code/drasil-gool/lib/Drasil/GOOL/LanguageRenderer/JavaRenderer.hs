{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The logic to render Java code is contained in this module
module Drasil.GOOL.LanguageRenderer.JavaRenderer (
  -- * Java Code Configuration -- defines syntax of all Java code
  JavaCode(..), jName, jVersion
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (UnRepr(..), SharedProg, Label, MSBody,
  VSFunction, SVariable, SValue, MSParameter, SMethod, BodySym(..), oneLiner,
  BlockSym(..), TypeSym(..), TypeElim(..), getTypeString, VariableSym(..),
  VisibilitySym(..), VariableElim(..),ValueSym(..), Argument(..), Literal(..),
  MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, extFuncApp, IndexTranslator(..), Reference(..),
  Array(..), List(..), Set(..), InternalList(..), StatementSym(..),
  AssignStatement(..), (&=), DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), BinderSym(..), BinderElim(..), ControlStatement(..),
  ScopeSym(..), ParameterSym(..), MethodSym(..))
import Drasil.GOOL.InterfaceGOOL (SClass, CSStateVar, OOProg, ProgramSym(..),
  FileSym(..), ModuleSym(..), ClassSym(..), OOTypeSym(..), OOVariableSym(..),
  SelfSym(..), StateVarSym(..), AttachmentSym(..), OOValueSym, OOVariableValue,
  OOValueExpression(..), objMethodCall, selfMethodCall, newObj,
  InternalValueExp(..), OOFunctionSym(..), ($.), GetSet(..), OODeclStatement(..),
  OOFuncAppStatement(..), ObserverPattern(..), StrategyPattern(..),
  OOMethodSym(..))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  RenderBody(..), BodyElim, RenderBlock(..), BlockElim, RenderType(..),
  UnaryOpSym(..), BinaryOpSym(..), OpElim(uOpPrec, bOpPrec), RenderVariable(..),
  InternalVarElim(variableBind), RenderValue(..), ValueElim(valuePrec, valueInt),
  InternalListFunc(..), RenderFunction(..), FunctionElim(functionType),
  InternalAssignStmt(..), InternalIOStmt(..), InternalControlStmt(..),
  RenderStatement(..), StatementElim(statementTerm), RenderVisibility(..),
  VisibilityElim, MethodTypeSym(..), RenderParam(..),
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim,
  BlockCommentSym(..), BlockCommentElim, ScopeElim(..), InternalBinderElim(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (body, block, uOp,
  bOp, variable, value, function, statement, visibility, parameter, method,
  blockComment')
import Drasil.GOOL.RendererClassesOO (OORenderSym, RenderFile(..),
  PermElim(binding), InternalGetSet(..), OOMethodTypeSym(..),
  OORenderMethod(..), StateVarElim, RenderClass(..), ClassElim, RenderMod(..),
  ModuleElim)
import qualified Drasil.GOOL.RendererClassesOO as RC (perm, stateVar, class',
  module')
import Drasil.Shared.LanguageRenderer (new, elseIfLabel, forLabel, tryLabel,
  catchLabel, throwLabel, throwsLabel, importLabel, blockCmtStart, blockCmtEnd,
  docCmtStart, bodyStart, bodyEnd, endStatement, commentStart, exceptionObj',
  new', args, printLabel, exceptionObj, mainFunc, new, nullLabel, listSep,
  access, containing, mathFunc, functionDox, variableList,
  parameterList, appendToBody, surroundBody, valueList)
import qualified Drasil.Shared.LanguageRenderer as R (sqrt, abs, log10,
  log, exp, sin, cos, tan, asin, acos, atan, floor, ceil, pow, package, class',
  multiStmt, body, printFile, classVarAccess, cast, castObj,
  classLevel, instanceLevel, break, continue, private, public, blockCmt, docCmt,
  addComments, commentedMod, commentedItem)
import Drasil.GOOL.Renderers (renderType, renderParam, renderListDec)
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt, mkStateVal, mkVal,
  typeFromData, VSOp, unOpPrec, powerPrec, unExpr, unExpr', unExprNumDbl,
  typeUnExpr, binExpr, binExprNumDbl', typeBinExpr, typeFromData)
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, obj, csc, sec, cot, negateOp, equalOp,
  notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp, minusOp,
  multOp, divideOp, moduloOp, var, classVar, instanceVarAccess, arrayElem,
  litChar, litDouble, litInt, litString, valueOf, arg, argsList, objAccess,
  objMethodCall, funcAppMixedArgs, newObjMixedArgs, lambda, func, get, set,
  listAccess, getFunc, setFunc, stmt, loopStmt, emptyStmt, assign, subAssign,
  objDecNew, print, closeFile, returnStmt, valStmt, comment, throw, ifCond,
  tryCatch, construct, param, method, getMethod, setMethod, function, buildClass,
  implementingClass, commentedClass, modFromData, fileDoc, fileFromData,
  defaultOptSpace, local)
import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (docFuncRepr)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP
import qualified Drasil.Shared.LanguageRenderer.CLike as C (float, double, char,
  listType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat,
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, increment,
  increment1, decrement1, varDec, varDecDef, listDec, extObjDecNew, switch, for,
  while, intFunc, multiAssignError, multiReturnError, multiTypeError, setType)
import qualified Drasil.Shared.LanguageRenderer.Macros as M (ifExists,
  runStrategy, listSlice, stringListVals, stringListLists, forRange,
  notifyObservers)
import qualified Drasil.GOOL.LanguageRenderer.CommonGOOL as CG (classMethodCall,
  listAppend, listAdd, innerType)
import Drasil.Shared.AST (Terminator(..), VisibilityTag(..), qualName,
  FileType(..), FileData(..), fileD, FuncData(..), fd, ModData(..), md,
  updateMod, MethodData(..), mthd, updateMthd, OpData(..), ParamData(..), pd,
  ProgData(..), progD, TypeData(..), ValData(..), vd, VarData(..), vard,
  ScopeData, BinderD(..), bindFormD)
import Drasil.Shared.CodeAnalysis (Exception(..), ExceptionType(..), exception,
  stdExc, HasException(..))
import Drasil.Shared.Helpers (emptyIfNull, toCode, toState, onCodeValue,
  onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues,
  onCodeList, onStateList, on2StateWrapped)
import Drasil.Shared.State (MS, VS, lensGStoFS, lensMStoFS, lensMStoVS,
  lensVStoFS, lensVStoMS, modifyReturn, modifyReturnList, revFiles,
  addProgNameToPaths, addLangImport, addLangImportVS, addExceptionImports,
  getModuleName, setFileType, getClassName, setCurrMain, setOutputsDeclared,
  isOutputsDeclared, getExceptions, getMethodExcMap, addExceptions, useVarName,
  setVarScope)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Control.Lens.Zoom (zoom)
import Control.Monad (join)
import Control.Monad.State (modify)
import Data.Composition ((.:))
import qualified Data.Map as Map (lookup)
import Data.List (nub, intercalate, sort)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($$), parens, empty,
  equals, vcat, lbrace, rbrace, braces, colon, quotes)

import qualified Drasil.Shared.LanguageRenderer.Common as CS

jExt :: String
jExt = "java"

newtype JavaCode a = JC {unJC :: a}

instance Functor JavaCode where
  fmap f (JC x) = JC (f x)

instance Applicative JavaCode where
  pure = JC
  (JC f) <*> (JC x) = JC (f x)

instance Monad JavaCode where
  JC x >>= f = f x

instance SharedProg JavaCode TypeData Doc (Doc, Terminator)
instance OOProg JavaCode TypeData Doc (Doc, Terminator)

instance ProgramSym JavaCode TypeData Doc (Doc, Terminator) where
  type Program JavaCode = ProgData
  prog n st fs = modifyReturnList (map (zoom lensGStoFS) fs) (revFiles .
    addProgNameToPaths n) (onCodeList (progD n st . map (R.package n
    endStatement)))

instance CommonRenderSym JavaCode TypeData Doc (Doc, Terminator)
instance OORenderSym JavaCode TypeData Doc (Doc, Terminator)

instance UnRepr JavaCode contents where
  unRepr = unJC

instance FileSym JavaCode TypeData Doc (Doc, Terminator) where
  type File JavaCode = FileData
  fileDoc m = do
    modify (setFileType Combined)
    G.fileDoc jExt top bottom m

  docMod = CP.doxMod jExt

instance RenderFile JavaCode where
  top _ = toCode empty
  bottom = toCode empty

  commentedMod = on2StateValues (on2CodeValues R.commentedMod)

  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym JavaCode where
  langImport = toCode . jImport
  modImport = langImport

instance AttachmentSym JavaCode where
  type Attachment JavaCode = Doc
  classLevel = toCode R.classLevel
  instanceLevel = toCode R.instanceLevel

instance PermElim JavaCode where
  perm = unJC
  binding = error $ CP.bindingError jName

instance BodySym JavaCode TypeData (Doc, Terminator) where
  type Body JavaCode = Doc
  body = onStateList (onCodeList R.body)

  addComments s = onStateValue (onCodeValue (R.addComments s commentStart))

instance RenderBody JavaCode where
  multiBody = G.multiBody

instance BodyElim JavaCode where
  body = unJC

instance BlockSym JavaCode TypeData (Doc, Terminator) where
  type Block JavaCode = Doc
  block = G.block

instance RenderBlock JavaCode where
  multiBlock = G.multiBlock

instance BlockElim JavaCode where
  block = unJC

instance TypeSym JavaCode TypeData where
  bool = jBoolType
  int = CP.int
  float = C.float
  double = C.double
  char = C.char
  string = CP.string'
  infile = jInfileType
  outfile = jOutfileType
  referenceType = id -- Ignore reference types in "high-level" langauges for now; later on think about using boxed/unboxed types
  listType = jListType
  setType = jSetType
  arrayType = CP.arrayType
  innerType = CG.innerType
  funcType = CS.funcType -- TODO [Brandon Bosman, 05/11/2026]: fix this to work with lambda types
  void = C.void

instance TypeElim JavaCode TypeData where
  getCodeType = cType . unJC

instance OOTypeSym JavaCode TypeData where
  obj = G.obj

instance RenderType JavaCode TypeData where
  multiType _ = error $ C.multiTypeError jName

instance UnaryOpSym JavaCode where
  notOp = C.notOp
  negateOp = G.negateOp
  sqrtOp = jUnaryMath R.sqrt
  absOp = jUnaryMath R.abs
  logOp = jUnaryMath R.log10
  lnOp = jUnaryMath R.log
  expOp = jUnaryMath R.exp
  sinOp = jUnaryMath R.sin
  cosOp = jUnaryMath R.cos
  tanOp = jUnaryMath R.tan
  asinOp = jUnaryMath R.asin
  acosOp = jUnaryMath R.acos
  atanOp = jUnaryMath R.atan
  floorOp = jUnaryMath R.floor
  ceilOp = jUnaryMath R.ceil

instance BinaryOpSym JavaCode where
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
  powerOp = powerPrec $ mathFunc R.pow
  moduloOp = G.moduloOp
  andOp = C.andOp
  orOp = C.orOp

instance OpElim JavaCode where
  uOp = opDoc . unJC
  bOp = opDoc . unJC
  uOpPrec = opPrec . unJC
  bOpPrec = opPrec . unJC

instance ScopeSym JavaCode where
  global = CP.global
  mainFn = local
  local = G.local

instance ScopeElim JavaCode where
  scopeData = unJC

instance VariableSym JavaCode TypeData where
  type Variable JavaCode = VarData
  var         = G.var
  constant    = var
  extVar      = CS.extVar

instance OOVariableSym JavaCode TypeData where
  classVar = G.classVar
  classConst = classVar
  classVarAccess = CP.classVarAccess R.classVarAccess
  extClassVarAccess = classVarAccess
  instanceVarAccess = G.instanceVarAccess

instance SelfSym JavaCode TypeData where
  self = C.self

instance VariableElim JavaCode TypeData where
  variableName = varName . unJC
  variableType = onCodeValue varType

instance InternalVarElim JavaCode where
  variableBind = varBind . unJC
  variable = varDoc . unJC

instance RenderVariable JavaCode TypeData where
  varFromData b n t' d =  do
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym JavaCode TypeData where
  type Value JavaCode = ValData
  valueType = onCodeValue valType

instance OOValueSym JavaCode TypeData

instance Argument JavaCode TypeData where
  pointerArg = id

instance Literal JavaCode TypeData where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = C.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = jLitArray
  litSet = CP.litSet (text jSetOf <>) parens

  litList t es = do
    zoom lensVStoMS $ modify (if null es then id else addLangImport $ utilImport
      jArrays)
    newObj (listType t) [jAsListFunc t es | not (null es)]

instance MathConstant JavaCode TypeData where
  pi = CP.pi

instance VariableValue JavaCode TypeData where
  valueOf = G.valueOf

instance OOVariableValue JavaCode TypeData

instance CommandLineArgs JavaCode TypeData where
  arg n = G.arg (litInt n) argsList
  argsList = G.argsList args
  argExists i = listSize argsList ?> litInt (fromIntegral i)

instance NumericExpression JavaCode TypeData where
  (#~) = unExpr' negateOp
  (#/^) = unExprNumDbl sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) = binExpr divideOp
  (#%) = binExpr moduloOp
  (#^) = binExprNumDbl' powerOp

  log = unExprNumDbl logOp
  ln = unExprNumDbl lnOp
  exp = unExprNumDbl expOp
  sin = unExprNumDbl sinOp
  cos = unExprNumDbl cosOp
  tan = unExprNumDbl tanOp
  csc = G.csc
  sec = G.sec
  cot = G.cot
  arcsin = unExprNumDbl asinOp
  arccos = unExprNumDbl acosOp
  arctan = unExprNumDbl atanOp
  floor = unExpr floorOp
  ceil = unExpr ceilOp

instance BooleanExpression JavaCode TypeData where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

instance Comparison JavaCode TypeData where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = jEquality
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression JavaCode TypeData where
  inlineIf = C.inlineIf

  -- Exceptions from function/method calls should already be in the exception
  -- map from the CodeInfo pass, but it's possible that one of the higher-level
  -- functions implicitly calls these functions in the Java renderer, so we
  -- also check here to add the exceptions from the called function to the map
  funcAppMixedArgs n t vs ns = do
    addCallExcsCurrMod n
    G.funcAppMixedArgs n t vs ns
  extFuncAppMixedArgs l n t vs ns = do
    mem <- getMethodExcMap
    modify (maybe id addExceptions (Map.lookup (qualName l n) mem))
    CS.extFuncAppMixedArgs l n t vs ns
  libFuncAppMixedArgs = C.libFuncAppMixedArgs

  lambda = G.lambda jLambda

  notNull = CP.notNull nullLabel

instance OOValueExpression JavaCode TypeData where
  newObjMixedArgs ot vs ns = addConstructorCallExcsCurrMod ot (\t ->
    G.newObjMixedArgs (new ++ " ") t vs ns)
  extNewObjMixedArgs l ot vs ns = do
    t <- ot
    mem <- getMethodExcMap
    let tp = getTypeString t
    modify (maybe id addExceptions (Map.lookup (qualName l tp) mem))
    newObjMixedArgs (toState t) vs ns
  libNewObjMixedArgs = C.libNewObjMixedArgs

instance RenderValue JavaCode TypeData where
  inputFunc = modify (addLangImportVS $ utilImport jScanner) >> mkStateVal
    (obj jScanner) (parens $ new' <+> jScanner' <> parens (jSystem jStdIn))
  printFunc = mkStateVal void (jSystem (jStdOut `access` printLabel))
  printLnFunc = mkStateVal void (jSystem (jStdOut `access` jPrintLn))
  printFileFunc = on2StateWrapped (\v -> mkVal v . R.printFile printLabel .
    RC.value) void
  printFileLnFunc = on2StateWrapped (\v -> mkVal v . R.printFile jPrintLn .
    RC.value) void

  cast = jCast

  call = CP.call' jName

  valFromData p i t' d = do
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim JavaCode where
  valuePrec = valPrec . unJC
  valueInt = valInt . unJC
  value = val . unJC

instance InternalValueExp JavaCode TypeData where
  objMethodCallMixedArgs' f t o ps ns = do
    ob <- o
    mem <- getMethodExcMap
    let tp = getTypeString (valueType ob)
    modify (maybe id addExceptions (Map.lookup (qualName tp f) mem))
    G.objMethodCall f t o ps ns
  classMethodCallMixedArgs' f t c ps ns = do
    cls <- c
    mem <- getMethodExcMap
    let tp = getTypeString cls
    modify (maybe id addExceptions (Map.lookup (qualName tp f) mem))
    CG.classMethodCall f t c ps ns

instance FunctionSym JavaCode TypeData where
  type Function JavaCode = FuncData

instance OOFunctionSym JavaCode TypeData where
  func = G.func
  objAccess = G.objAccess

instance GetSet JavaCode TypeData where
  get = G.get
  set = G.set

instance IndexTranslator JavaCode TypeData where
  intToIndex = CP.intToIndex
  indexToInt = CP.indexToInt

instance Reference JavaCode TypeData where
  makeRef = id
  maybeDeref = id

instance Array JavaCode TypeData where
  arrayElem = G.arrayElem
  arrayLength arr = valueOf $ instanceVarAccess arr (var "length" int)
  arrayCopy arr = let
    arrTp = onStateValue valueType arr
    in objMethodCall arrTp arr "clone" []

instance List JavaCode TypeData (Doc, Terminator) where
  listSize = C.listSize "size"
  listAdd = CG.listAdd jListAdd
  listAppend = CG.listAppend jListAdd
  listAccess = G.listAccess
  listSet list idx vl = valStmt $ objMethodCall void list jListSet [idx, vl]
  indexOf = CP.indexOf jIndex

instance Set JavaCode TypeData where
  contains = CP.contains jContains
  setAdd = CP.setMethodCall jListAdd
  setRemove = CP.setMethodCall jListRemove
  setUnion = CP.setMethodCall jListUnion

instance InternalList JavaCode TypeData where
  listSlice' = M.listSlice

instance InternalGetSet JavaCode TypeData where
  getFunc = G.getFunc
  setFunc = G.setFunc

instance InternalListFunc JavaCode TypeData where
  listAccessFunc = CP.listAccessFunc' jListAccess

instance BinderSym JavaCode TypeData where
  binder nm tp = onCodeValue (bindFormD nm) <$> tp

instance BinderElim JavaCode TypeData where
  binderName = bindName . unJC
  binderType = onCodeValue bindType

instance InternalBinderElim JavaCode where
  binderElim = text . bindName . unJC

instance RenderFunction JavaCode TypeData where
  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance FunctionElim JavaCode TypeData where
  functionType = onCodeValue fType
  function = funcDoc . unJC

instance InternalAssignStmt JavaCode (Doc, Terminator) where
  multiAssign _ _ = error $ C.multiAssignError jName

instance InternalIOStmt JavaCode (Doc, Terminator) where
  printSt _ _ = CP.printSt

instance InternalControlStmt JavaCode (Doc, Terminator) where
  multiReturn _ = error $ C.multiReturnError jName

instance RenderStatement JavaCode (Doc, Terminator) where
  stmt = G.stmt
  loopStmt = G.loopStmt
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim JavaCode (Doc, Terminator) where
  statement = fst . unJC
  statementTerm = snd . unJC

instance StatementSym JavaCode TypeData (Doc, Terminator) where
  -- Terminator determines how statements end
  valStmt = G.valStmt Semi
  emptyStmt = G.emptyStmt
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement JavaCode TypeData (Doc, Terminator) where
  assign = G.assign Semi
  (&-=) = G.subAssign Semi
  (&+=) = C.increment
  (&++) = C.increment1
  (&--) = C.decrement1

instance DeclStatement JavaCode TypeData (Doc, Terminator) where
  varDec = C.varDec classLevel instanceLevel empty
  varDecDef = C.varDecDef Semi
  setDec = varDec
  setDecDef = varDecDef
  listDec n v scp = zoom lensMStoVS v >>= (\v' -> C.listDec (renderListDec v')
    (litInt n) v scp)
  listDecDef = CP.listDecDef
  arrayDec n = CP.arrayDec (litInt n)
  arrayDecDef = CP.arrayDecDef
  constDecDef = jConstDecDef
  funcDecDef = jFuncDecDef

instance OODeclStatement JavaCode TypeData (Doc, Terminator) where
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = C.extObjDecNew

instance IOStatement JavaCode TypeData (Doc, Terminator) where
  print      = jOut False Nothing printFunc
  printLn    = jOut True  Nothing printLnFunc
  printStr   = jOut False Nothing printFunc   . litString
  printStrLn = jOut True  Nothing printLnFunc . litString

  printFile f      = jOut False (Just f) (printFileFunc f)
  printFileLn f    = jOut True  (Just f) (printFileLnFunc f)
  printFileStr f   = jOut False (Just f) (printFileFunc f)   . litString
  printFileStrLn f = jOut True  (Just f) (printFileLnFunc f) . litString

  getInput v = v &= jInput v inputFunc
  discardInput = jDiscardInput inputFunc
  getFileInput f v = v &= jInput v f
  discardFileInput = jDiscardInput

  openFileR = CP.openFileR jOpenFileR
  openFileW = CP.openFileW jOpenFileWorA
  openFileA = CP.openFileA jOpenFileWorA
  closeFile = G.closeFile jClose

  getFileInputLine f v = v &= f $. jNextLineFunc
  discardFileLine = CP.discardFileLine jNextLine
  getFileInputAll f v = while (f $. jHasNextLineFunc)
    (oneLiner $ listAppend (valueOf v) (f $. jNextLineFunc))

instance StringStatement JavaCode TypeData (Doc, Terminator) where
  stringSplit d vnew s = do
    modify (addLangImport $ utilImport jArrays)
    ss <- zoom lensMStoVS $
      jStringSplit vnew (jAsListFunc string [s $. jSplitFunc d])
    mkStmt ss

  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FuncAppStatement JavaCode TypeData (Doc, Terminator) where
  inOutCall = jInOutCall funcApp
  extInOutCall m = jInOutCall (extFuncApp m)

instance OOFuncAppStatement JavaCode TypeData (Doc, Terminator) where
  selfInOutCall = jInOutCall selfMethodCall

instance CommentStatement JavaCode TypeData (Doc, Terminator) where
  comment = G.comment commentStart

instance ControlStatement JavaCode TypeData (Doc, Terminator) where
  break = mkStmt R.break
  continue = mkStmt R.continue

  returnStmt = G.returnStmt Semi

  throw = G.throw jThrowDoc Semi

  ifCond = G.ifCond parens bodyStart G.defaultOptSpace elseIfLabel bodyEnd empty
  switch  = C.switch parens break

  ifExists = M.ifExists

  for = C.for bodyStart bodyEnd
  forRange = M.forRange
  forEach = CP.forEach bodyStart bodyEnd forLabel colon
  while = C.while parens bodyStart bodyEnd

  tryCatch = G.tryCatch jTryCatch

  assert condition errorMessage = do
    cond <- zoom lensMStoVS condition
    errMsg <- zoom lensMStoVS errorMessage
    mkStmt (jAssert cond errMsg)

instance ObserverPattern JavaCode TypeData (Doc, Terminator) where
  notifyObservers = M.notifyObservers

instance StrategyPattern JavaCode TypeData (Doc, Terminator) where
  runStrategy = M.runStrategy

instance VisibilitySym JavaCode Doc where
  private = toCode R.private
  public = toCode R.public

instance RenderVisibility JavaCode Doc where
  visibilityFromData _ = toCode

instance VisibilityElim JavaCode Doc where
  visibility = unJC

instance MethodTypeSym JavaCode TypeData where
  type MethodType JavaCode = TypeData
  mType = zoom lensMStoVS

instance OOMethodTypeSym JavaCode TypeData where
  construct = G.construct

instance ParameterSym JavaCode TypeData where
  type Parameter JavaCode = ParamData
  param = G.param renderParam
  pointerParam = param

instance RenderParam JavaCode where
  paramFromData v' d = do
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)

instance ParamElim JavaCode TypeData where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unJC

instance MethodSym JavaCode TypeData Doc (Doc, Terminator) where
  type Method JavaCode = MethodData
  docMain = CP.docMain
  function = G.function
  mainFunction = CP.mainFunction string mainFunc
  docFunc = CP.doxFunc

  inOutFunc n s = jInOut (function n s)
  docInOutFunc n s = jDocInOut (inOutFunc n s)

instance OOMethodSym JavaCode TypeData Doc (Doc, Terminator) where
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor ps is b = getClassName >>= (\n -> CP.constructor n ps is b)

  inOutMethod n s p = jInOut (method n s p)
  docInOutMethod n s p = jDocInOut (inOutMethod n s p)

instance RenderMethod JavaCode TypeData where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)

  mthdFromData _ d = toState $ toCode $ mthd d

instance OORenderMethod JavaCode TypeData Doc where
  intMethod m n s p t ps b = do
    tp <- t
    pms <- sequence ps
    bd <- b
    mem <- zoom lensMStoVS getMethodExcMap
    es <- getExceptions
    mn <- zoom lensMStoFS getModuleName
    let excs = map (unJC . toConcreteExc) $ maybe es (nub . (++ es))
          (Map.lookup (qualName mn n) mem)
    modify ((if m then setCurrMain else id) . addExceptionImports excs)
    pure $ toCode $ mthd $ jMethod n (map exc excs) s p tp pms bd
  intFunc = C.intFunc
  destructor _ = error $ CP.destructorError jName

instance MethodElim JavaCode where
  method = mthdDoc . unJC

instance StateVarSym JavaCode TypeData Doc where
  type StateVar JavaCode = Doc
  stateVar = CP.stateVar
  stateVarDef = CP.stateVarDef
  constVar = CP.constVar (RC.perm (classLevel :: JavaCode (Attachment JavaCode)))

instance StateVarElim JavaCode where
  stateVar = unJC

instance ClassSym JavaCode TypeData Doc (Doc, Terminator) where
  type Class JavaCode = Doc
  buildClass = G.buildClass
  extraClass = jExtraClass
  implementingClass = G.implementingClass

  docClass = CP.doxClass

instance RenderClass JavaCode Doc where
  intClass = CP.intClass R.class'

  inherit n = toCode $ maybe empty ((jExtends <+>) . text) n
  implements is = toCode $ jImplements <+> text (intercalate listSep is)

  commentedClass = G.commentedClass

instance ClassElim JavaCode where
  class' = unJC

instance ModuleSym JavaCode TypeData Doc (Doc, Terminator) where
  type Module JavaCode = ModData
  buildModule n = CP.buildModule' n langImport

instance RenderMod JavaCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim JavaCode where
  module' = modDoc . unJC

instance BlockCommentSym JavaCode where
  blockComment lns = toCode $ R.blockCmt lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns docCmtStart
    blockCmtEnd)

instance BlockCommentElim JavaCode where
  blockComment' = unJC

instance HasException JavaCode where
  toConcreteExc Standard = toCode $ stdExc exceptionObj
  toConcreteExc FileNotFound = toCode $ exception (javaImport io) jFNFExc
  toConcreteExc IO = toCode $ exception (javaImport io) jIOExc

jName, jVersion :: String
jName = "Java"
jVersion = "14"

jImport :: Label -> Doc
jImport n = importLabel <+> text n <> endStatement

jBoolType :: (Monad r) => VS (r TypeData)
jBoolType = typeFromData Boolean jBool (text jBool)

jInfileType :: (Monad r) => VS (r TypeData)
jInfileType = do
  tpf <- typeFromData InFile jScanner jScanner'
  modifyReturn (addLangImportVS $ utilImport jScanner) tpf

jOutfileType :: (Monad r) => VS (r TypeData)
jOutfileType = do
  tpf <- typeFromData OutFile jPrintWriter (text jPrintWriter)
  modifyReturn (addLangImportVS $ ioImport jPrintWriter) tpf

jExtends, jImplements, jFinal, jScanner', jLambdaSep :: Doc
jExtends = text "extends"
jImplements = text "implements"
jFinal = text "final"
jScanner' = text jScanner
jLambdaSep = text "->"

arrayList, jBool, jBool', jInteger, jObject, jScanner, jContains, jPrintWriter,
  jFile, jFileWriter, jIOExc, jFNFExc, jArrays, jSet, jAsList, jSetOf, jStdIn,
  jStdOut, jPrintLn, jEquals, jParseInt, jParseDbl, jParseFloat, jIndex,
  jListAdd, jListRemove, jListUnion, jListAccess, jListSet, jClose, jNext,
  jNextLine, jNextBool, jHasNextLine, jCharAt, jSplit, io, util :: String
arrayList = "ArrayList"
jBool = "boolean"
jBool' = "Boolean"
jInteger = "Integer"
jObject = "Object"
jScanner = "Scanner"
jContains = "contains"
jPrintWriter = "PrintWriter"
jFile = "File"
jFileWriter = "FileWriter"
jIOExc = "IOException"
jFNFExc = "FileNotFoundException"
jArrays = "Arrays"
jSet = "Set"
jAsList = jArrays `access` "asList"
jSetOf = jSet `access` "of"
jStdIn = "in"
jStdOut = "out"
jPrintLn = "println"
jEquals = "equals"
jParseInt = jInteger `access` "parseInt"
jParseDbl = CP.doubleRender `access` "parseDouble"
jParseFloat = CP.floatRender `access` "parseFloat"
jIndex = "indexOf"
jListAdd = "add"
jListRemove = "remove"
jListUnion = "addAll"
jListAccess = "get"
jListSet = "set"
jClose = "close"
jNext = "next"
jNextLine = "nextLine"
jNextBool = "nextBoolean"
jHasNextLine = "hasNextLine"
jCharAt = "charAt"
jSplit = "split"
io = "io"
util = "util"

javaImport, ioImport, utilImport :: String -> String
javaImport = access "java"
ioImport = javaImport . access io
utilImport = javaImport . access util

jSystem :: String -> Doc
jSystem = text . access "System"

jUnaryMath :: (Monad r) => String -> VSOp r
jUnaryMath = unOpPrec . mathFunc

jListType :: (TypeElim r TypeData, UnRepr r TypeData, Monad r) =>
  VS (r TypeData) -> VS (r TypeData)
jListType t = do
  modify (addLangImportVS $ utilImport arrayList)
  t >>= (jListType' . getCodeType)
  where jListType' Integer = typeFromData (List Integer)
          lstInt (text lstInt)
        jListType' Float = C.listType arrayList CP.float
        jListType' Double = C.listType arrayList CP.double
        jListType' Boolean = typeFromData (List Boolean) lstBool (text lstBool)
        jListType' _ = C.listType arrayList t
        lstInt = arrayList `containing` jInteger
        lstBool = arrayList `containing` jBool'

jSetType :: (TypeElim r TypeData, UnRepr r TypeData, Monad r) =>
  VS (r TypeData) -> VS (r TypeData)
jSetType t = do
  modify (addLangImportVS $ utilImport "Set")
  t >>= (jSetType' . getCodeType)
  where jSetType' Integer = typeFromData (Set Integer)
          stInt (text stInt)
        jSetType' Float = C.setType "Set" CP.float
        jSetType' Double = C.setType "Set" CP.double
        jSetType' Boolean = typeFromData (Set Boolean) stBool (text stBool)
        jSetType' _ = C.setType "Set" t
        stInt = "Set" `containing` jInteger
        stBool = "Set" `containing` jBool'

jArrayType :: VS (JavaCode TypeData)
jArrayType = arrayType (obj jObject)

jLitArray :: VS (JavaCode TypeData) -> [SValue JavaCode] -> SValue JavaCode
jLitArray t' es' = do
  es <- sequence es'
  lt <- arrayType t'
  mkVal lt (new' <+> renderType lt
    <+> braces (valueList es))

jFileType :: (OORenderSym r TypeData vis smt) => VS (r TypeData)
jFileType = do
  tpf <- obj jFile
  modifyReturn (addLangImportVS $ ioImport jFile) tpf

jFileWriterType :: (OORenderSym r TypeData vis smt) => VS (r TypeData)
jFileWriterType = do
  tpf <- obj jFileWriter
  modifyReturn (addLangImportVS $ ioImport jFileWriter) tpf

jAsListFunc :: VS (JavaCode TypeData) -> [SValue JavaCode] -> SValue JavaCode
jAsListFunc t = funcApp jAsList (listType t)

jEqualsFunc :: SValue JavaCode -> VSFunction JavaCode
jEqualsFunc v = func jEquals bool [v]

jParseIntFunc :: SValue JavaCode -> SValue JavaCode
jParseIntFunc v = funcApp jParseInt int [v]

jParseDblFunc :: SValue JavaCode -> SValue JavaCode
jParseDblFunc v = funcApp jParseDbl double [v]

jParseFloatFunc :: SValue JavaCode -> SValue JavaCode
jParseFloatFunc v = funcApp jParseFloat float [v]

jNextFunc :: VSFunction JavaCode
jNextFunc = func jNext string []

jNextLineFunc :: VSFunction JavaCode
jNextLineFunc = func jNextLine string []

jNextBoolFunc :: VSFunction JavaCode
jNextBoolFunc = func jNextBool bool []

jHasNextLineFunc :: VSFunction JavaCode
jHasNextLineFunc = func jHasNextLine bool []

jCharAtFunc :: VSFunction JavaCode
jCharAtFunc = func jCharAt char [litInt 0]

jSplitFunc :: (OORenderSym r TypeData vis smt) => Char -> VSFunction r
jSplitFunc d = func jSplit (listType string) [litString [d]]

jEquality :: SValue JavaCode -> SValue JavaCode -> SValue JavaCode
jEquality v1 v2 = v2 >>= jEquality' . getCodeType . valueType
  where jEquality' String = objAccess v1 (jEqualsFunc v2)
        jEquality' _ = typeBinExpr equalOp bool v1 v2

jLambda :: [r BinderD] -> r (Value r) -> Doc -- Needs (CommonRenderSym r TypeData) constraint
jLambda = error "Lambdas not supported in Java (yet). See #4956 for updates." -- \ps ex -> parens (binderList ps) <+> jLambdaSep <+> RC.value ex

jCast :: VS (JavaCode TypeData) -> SValue JavaCode -> SValue JavaCode
jCast = join .: on2StateValues
  (\t v -> jCast' (getCodeType t) (getCodeType $ valueType v) t v)
  where jCast' Double String _ v = jParseDblFunc (toState v)
        jCast' Float String _ v = jParseFloatFunc (toState v)
        jCast' _ _ t v = mkStateVal (toState t) (R.castObj (R.cast (renderType t))
          (RC.value v))

jConstDecDef :: SVariable JavaCode -> JavaCode ScopeData -> SValue JavaCode ->
  MS (JavaCode (Doc, Terminator))
jConstDecDef v' scp def' = do
  v <- zoom lensMStoVS v'
  def <- zoom lensMStoVS def'
  modify $ useVarName $ variableName v
  modify $ setVarScope (variableName v) (scopeData scp)
  mkStmt $ jFinal <+> renderType (variableType v) <+>
    RC.variable v <+> equals <+> RC.value def

jFuncDecDef :: SVariable JavaCode -> JavaCode ScopeData ->
  [SVariable JavaCode] -> MSBody JavaCode -> MS (JavaCode (Doc, Terminator))
jFuncDecDef v scp ps bod = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  pms <- mapM (zoom lensMStoVS) ps
  b <- bod
  mkStmt $ renderType (variableType vr) <+> RC.variable vr <+> equals <+>
    parens (variableList pms) <+> jLambdaSep <+> bodyStart $$ indent (RC.body b)
    $$ bodyEnd

jThrowDoc :: (CommonRenderSym r TypeData vis smt) => r (Value r) -> Doc
jThrowDoc errMsg = throwLabel <+> new' <+> exceptionObj' <>
  parens (RC.value errMsg)

jTryCatch :: (CommonRenderSym r TypeData vis smt) => r (Body r) -> r (Body r) -> Doc
jTryCatch tb cb = vcat [
  tryLabel <+> lbrace,
  indent $ RC.body tb,
  rbrace <+> catchLabel <+> parens (exceptionObj' <+> text "exc") <+>
    lbrace,
  indent $ RC.body cb,
  rbrace]

jAssert :: (CommonRenderSym r TypeData vis smt) => r (Value r) -> r (Value r) -> Doc
jAssert condition errorMessage = vcat [
  text "assert" <+> RC.value condition <+> colon <+> RC.value errorMessage
  ]

jOut :: (CommonRenderSym r TypeData vis smt, TypeElim r TypeData) =>
  Bool -> Maybe (SValue r) -> SValue r -> SValue r -> MS (r smt)
jOut newLn f printFn v = zoom lensMStoVS v >>= jOut' . getCodeType . valueType
  where jOut' (List (Object _)) = G.print newLn f printFn v
        jOut' (List _) = printSt newLn f printFn v
        jOut' (Array _) = do
          zoom lensMStoVS $ modify (addLangImportVS $ utilImport jArrays)
          printSt newLn f printFn (extFuncApp jArrays "toString" string [v])
        jOut' _ = G.print newLn f printFn v

jDiscardInput :: SValue JavaCode -> MS (JavaCode (Doc, Terminator))
jDiscardInput inFn = valStmt $ inFn $. jNextFunc

jInput :: SVariable JavaCode -> SValue JavaCode -> SValue JavaCode
jInput vr inFn = do
  v <- vr
  let jInput' Integer = jParseIntFunc $ inFn $. jNextLineFunc
      jInput' Float = jParseFloatFunc $ inFn $. jNextLineFunc
      jInput' Double = jParseDblFunc $ inFn $. jNextLineFunc
      jInput' Boolean = inFn $. jNextBoolFunc
      jInput' String = inFn $. jNextLineFunc
      jInput' Char = (inFn $. jNextFunc) $. jCharAtFunc
      jInput' _ = error "Attempt to read value of unreadable type"
  jInput' (getCodeType $ variableType v)

jOpenFileR :: (OORenderSym r TypeData vis smt) => SValue r -> VS (r TypeData) -> SValue r
jOpenFileR n t = newObj t [newObj jFileType [n]]

jOpenFileWorA :: (OORenderSym r TypeData vis smt) => SValue r -> VS (r TypeData) ->
  SValue r -> SValue r
jOpenFileWorA n t wa = newObj t
  [newObj jFileWriterType [newObj jFileType [n], wa]]

jStringSplit :: SVariable JavaCode -> SValue JavaCode -> VS Doc
jStringSplit = on2StateValues (\vnew s -> RC.variable vnew <+> equals <+>
  new' <+> renderType (variableType vnew) <> parens (RC.value s))

jMethod :: Label -> [String] -> JavaCode Doc ->
  JavaCode (Attachment JavaCode) -> JavaCode TypeData ->
  [JavaCode (Parameter JavaCode)] -> JavaCode (Body JavaCode) -> Doc
jMethod n es s p t ps b = vcat [
  RC.visibility s <+> RC.perm p <+> renderType t <+> text n <>
    parens (parameterList ps) <+> emptyIfNull es (throwsLabel <+>
    text (intercalate listSep (sort es))) <+> lbrace,
  indent $ RC.body b,
  rbrace]

outputs :: SVariable JavaCode
outputs = var "outputs" jArrayType

jAssignFromArray :: Integer -> [SVariable JavaCode] -> [MS (JavaCode (Doc, Terminator))]
jAssignFromArray _ [] = []
jAssignFromArray c (v:vs) = (v &= cast (onStateValue variableType v)
  (valueOf $ arrayElem (valueOf outputs) (litInt c))) : jAssignFromArray (c+1) vs

jInOutCall :: (Label -> VS (JavaCode TypeData) -> [SValue JavaCode] ->
  SValue JavaCode) -> Label -> [SValue JavaCode] -> [SVariable JavaCode] ->
  [SVariable JavaCode] -> MS (JavaCode (Doc, Terminator))
jInOutCall f n ins [] [] = valStmt $ f n void ins
jInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out)
  ins
jInOutCall f n ins [] [out] = assign out $ f n (onStateValue variableType out)
  (valueOf out : ins)
jInOutCall f n ins outs both = fCall rets
  where rets = both ++ outs
        fCall [x] = assign x $ f n (onStateValue variableType x)
          (map valueOf both ++ ins)
        fCall xs = isOutputsDeclared >>= (\odec -> modify setOutputsDeclared >>
          multi ((if odec then assign else (`varDecDef` local)) outputs
          (f n jArrayType (map valueOf both ++ ins)) : jAssignFromArray 0 xs))

jInOut :: (VS (JavaCode TypeData) -> [MSParameter JavaCode] -> MSBody JavaCode ->
  SMethod JavaCode) -> [SVariable JavaCode] -> [SVariable JavaCode]
  -> [SVariable JavaCode] -> MSBody JavaCode -> SMethod JavaCode
jInOut f ins [] [] b = f void (map param ins) b
jInOut f ins [v] [] b = f (onStateValue variableType v) (map param ins)
  (on3StateValues (on3CodeValues surroundBody) (varDec v local) b (returnStmt $
  valueOf v))
jInOut f ins [] [v] b = f (onStateValue variableType v)
  (map param $ v : ins) (on2StateValues (on2CodeValues appendToBody) b
  (returnStmt $ valueOf v))
jInOut f ins outs both b = f (returnTp rets)
  (map param $ both ++ ins) (on3StateValues (on3CodeValues surroundBody) decls
  b (returnSt rets))
  where returnTp [x] = onStateValue variableType x
        returnTp _ = jArrayType
        returnSt [x] = returnStmt $ valueOf x
        returnSt _ = multi (arrayDec (toInteger $ length rets) outputs local
          : assignArray 0 (map valueOf rets)
          ++ [returnStmt (valueOf outputs)])
        assignArray :: Integer -> [SValue JavaCode] -> [MS (JavaCode (Doc, Terminator))]
        assignArray _ [] = []
        assignArray c (v:vs) =
          (arrayElem (valueOf outputs) (litInt c) &= v)
          : assignArray (c+1) vs
        decls = multi $ map (`varDec` local) outs
        rets = both ++ outs

jDocInOut :: (CommonRenderSym r TypeData vis smt) => ([SVariable r] ->
  [SVariable r] -> [SVariable r] -> MSBody r -> SMethod r) -> String ->
  [(String, SVariable r)] -> [(String, SVariable r)] ->
  [(String, SVariable r)] -> MSBody r -> SMethod r
jDocInOut f desc is [] [] b = docFuncRepr functionDox desc (map fst is) []
  (f (map snd is) [] [] b)
jDocInOut f desc is [o] [] b = docFuncRepr functionDox desc (map fst is)
  [fst o] (f (map snd is) [snd o] [] b)
jDocInOut f desc is [] [both] b = docFuncRepr functionDox desc (map fst (both :
  is)) [fst both] (f (map snd is) [] [snd both] b)
jDocInOut f desc is os bs b = docFuncRepr  functionDox desc (map fst $ bs ++ is)
  rets (f (map snd is) (map snd os) (map snd bs) b)
  where rets = "array containing the following values:" : map fst bs ++
          map fst os

jExtraClass :: (OORenderSym r TypeData vis smt) => Label -> Maybe Label ->
  [CSStateVar r] -> [SMethod r] -> [SMethod r] -> SClass r
jExtraClass n = intClass n (visibilityFromData Priv empty) . inherit

addCallExcsCurrMod :: String -> VS ()
addCallExcsCurrMod n = do
  cm <- zoom lensVStoFS getModuleName
  mem <- getMethodExcMap
  modify (maybe id addExceptions (Map.lookup (qualName cm n) mem))

addConstructorCallExcsCurrMod :: (UnRepr r TypeData) =>
  VS (r TypeData) -> (VS (r TypeData) -> SValue r) -> SValue r
addConstructorCallExcsCurrMod ot f = do
  t <- ot
  cm <- zoom lensVStoFS getModuleName
  mem <- getMethodExcMap
  let tp = getTypeString t
  modify (maybe id addExceptions (Map.lookup (qualName cm tp) mem))
  f (pure t)
