{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The logic to render C# code is contained in this module
module Drasil.GOOL.LanguageRenderer.CSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..), csName, csVersion
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (UnRepr(..), SharedProg, Label, MSBody,
  VSType, VSFunction, SVariable, SValue, MSStatement, MSParameter, SMethod,
  BodySym(..), oneLiner, BlockSym(..), TypeSym(..), getCodeType, getTypeString,
  VariableSym(..), VisibilitySym(..), VariableElim(..), ValueSym(..),
  Argument(..), Literal(..), litZero, MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), funcApp, extFuncApp, IndexTranslator(..),
  Array(..), List(..), Set(..), InternalList(..), ThunkSym(..), VectorType(..),
  VectorDecl(..), VectorThunk(..), VectorExpression(..), ThunkAssign(..),
  StatementSym(..), AssignStatement(..), (&=), DeclStatement(..),
  IOStatement(..), StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), BinderSym(..), BinderElim(..), ControlStatement(..),
  ScopeSym(..), ParameterSym(..), MethodSym(..))
import Drasil.GOOL.InterfaceGOOL (OOProg, ProgramSym(..), FileSym(..),
  ModuleSym(..), ClassSym(..), OOTypeSym(..), OOVariableSym(..), SelfSym(..),
  InstanceVarSelfSym(..), StateVarSym(..), AttachmentSym(..), OOValueSym,
  OOVariableValue, OOValueExpression(..), selfMethodCall, newObj,
  InternalValueExp(..), objMethodCall, objMethodCallNoParams, OOFunctionSym(..),
  ($.), GetSet(..), OODeclStatement(..), OOFuncAppStatement(..),
  ObserverPattern(..), StrategyPattern(..), OOMethodSym(..))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  ImportElim, RenderBody(..), BodyElim, RenderBlock(..), BlockElim,
  RenderType(..), UnaryOpSym(..), BinaryOpSym(..), OpElim(uOpPrec, bOpPrec),
  RenderVariable(..), InternalVarElim(variableBind), RenderValue(..),
  ValueElim(valuePrec, valueInt), InternalListFunc(..), RenderFunction(..),
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..),
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm),
  RenderVisibility(..), VisibilityElim, MethodTypeSym(..), RenderParam(..),
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim,
  BlockCommentSym(..), BlockCommentElim, ScopeElim(..), InternalBinderElim(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (import', body, block,
  uOp, bOp, variable, value, function, statement, visibility, parameter, method,
  blockComment')
import Drasil.GOOL.RendererClassesOO (OORenderSym, RenderFile(..),
  PermElim(binding), InternalGetSet(..), OOMethodTypeSym(..),
  OORenderMethod(..), StateVarElim, RenderClass(..), ClassElim, RenderMod(..),
  ModuleElim)
import qualified Drasil.GOOL.RendererClassesOO as RC (perm, stateVar, class',
  module')
import Drasil.GOOL.Renderers (renderType, renderParam, renderMethod,
  renderListDec)
import Drasil.Shared.LanguageRenderer (new, dot, blockCmtStart, blockCmtEnd,
  docCmtStart, bodyStart, bodyEnd, endStatement, commentStart, elseIfLabel,
  inLabel, tryLabel, catchLabel, throwLabel, exceptionObj', new', listSep',
  args, nullLabel, listSep, access, containing, mathFunc, valueList,
  variableList, binderList, appendToBody, surroundBody)
import qualified Drasil.Shared.LanguageRenderer as R (class', multiStmt, body,
  printFile, classVarAccess, cast, listSetFunc, castObj, classLevel,
  instanceLevel, break, continue, private, public, blockCmt, docCmt, addComments,
  commentedMod, commentedItem)
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt,  mkStmtNoEnd,
  mkStateVal, mkVal, typeFromData, VSOp, unOpPrec, powerPrec, unExpr, unExpr',
  unExprNumDbl, typeUnExpr, binExpr, binExprNumDbl', typeBinExpr)
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, listInnerType, obj, csc, sec, cot, negateOp,
  equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp,
  minusOp, multOp, divideOp, moduloOp, var, classVar, instanceVarAccess,
  arrayElem, litChar, litDouble, litInt, litString, valueOf, arg, argsList,
  objAccess, objMethodCall, call, funcAppMixedArgs, newObjMixedArgs, lambda,
  func, get, set, listAccess, listSet, getFunc, setFunc, stmt, loopStmt,
  emptyStmt, assign, subAssign, objDecNew, print, closeFile, returnStmt, valStmt,
  comment, throw, ifCond, tryCatch, construct, param, method, getMethod,
  setMethod, function, buildClass, implementingClass, commentedClass,
  modFromData, fileDoc, fileFromData, defaultOptSpace, local)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (
  arrayDec, arrayDecDef, arrayType, bindingError, buildModule', classVarAccess,
  constVar, constructor, contains, destructorError, discardFileLine, docInOutFunc,
  docMain, doubleRender, doxClass, doxFunc, doxMod, extraClass, forEach, global,
  implements, indexOf, indexToInt, inherit, int, intClass, intToIndex,
  listDecDef, mainFunction, notNull, instanceVarSelf, openFileA, openFileR, openFileW,
  pi, printSt, setMethodCall, stateVar, stateVarDef, string)
import qualified Drasil.GOOL.LanguageRenderer.CommonGOOL as CG (constDecDef,
  classMethodCall, listAppend, listAdd)

import qualified Drasil.Shared.LanguageRenderer.CLike as C (setType, float, double, char,
  listType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat,
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize', increment,
  increment1, decrement1, varDec, varDecDef, listDec, extObjDecNew, switch, for,
  while, intFunc, multiAssignError, multiReturnError, multiTypeError)
import qualified Drasil.Shared.LanguageRenderer.Macros as M (ifExists,
  runStrategy, listSlice, stringListVals, stringListLists, forRange,
  notifyObservers)
import Drasil.Shared.AST (Terminator(..), FileType(..), FileData(..), fileD,
  FuncData(..), fd, ModData(..), md, updateMod, MethodData(..), mthd,
  updateMthd, OpData(..), ParamData(..), pd, updateParam, ProgData(..), progD,
  TypeData(..), ValData(..), vd, updateValDoc, AttachmentTag(..), VarData(..),
  vard, CommonThunk, pureValue, vectorize, vectorize2, sumComponents,
  commonVecIndex, commonThunkElim, commonThunkDim, ScopeData, BinderD(..),
  bindFormD)
import Drasil.Shared.Helpers (angles, hicat, toCode, toState, onCodeValue,
  onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues,
  on2StateWrapped, onCodeList, onStateList)
import Drasil.Shared.State (VS, lensGStoFS, lensMStoVS, modifyReturn, revFiles,
  addLangImport, addLangImportVS, setFileType, getClassName, setCurrMain,
  useVarName, genLoopIndex, setVarScope)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Control.Lens.Zoom (zoom)
import Control.Monad (join)
import Control.Monad.State (modify)
import Data.Composition ((.:))
import Data.List (intercalate)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($$), parens, empty,
  equals, vcat, lbrace, rbrace, braces, colon, space, quotes, semi)
import qualified Drasil.Shared.LanguageRenderer.Common as CS (
  extFuncAppMixedArgs, extVar, listAccessFunc, listSetFunc)

csExt :: String
csExt = "cs"

newtype CSharpCode a = CSC {unCSC :: a} deriving Eq

instance Functor CSharpCode where
  fmap f (CSC x) = CSC (f x)

instance Applicative CSharpCode where
  pure = CSC
  (CSC f) <*> (CSC x) = CSC (f x)

instance Monad CSharpCode where
  CSC x >>= f = f x

instance SharedProg CSharpCode
instance OOProg CSharpCode

instance ProgramSym CSharpCode where
  type Program CSharpCode = ProgData
  prog n st files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n st) fs

instance CommonRenderSym CSharpCode
instance OORenderSym CSharpCode

instance UnRepr CSharpCode contents where
  unRepr = unCSC

instance FileSym CSharpCode where
  type File CSharpCode = FileData
  fileDoc m = do
    modify (setFileType Combined)
    G.fileDoc csExt top bottom m

  docMod = CP.doxMod csExt

instance RenderFile CSharpCode where
  top _ = toCode empty
  bottom = toCode empty

  commentedMod = on2StateValues (on2CodeValues R.commentedMod)

  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym CSharpCode where
  type Import CSharpCode = Doc
  langImport = toCode . csImport
  modImport = langImport

instance ImportElim CSharpCode where
  import' = unCSC

instance AttachmentSym CSharpCode where
  type Attachment CSharpCode = Doc
  classLevel = toCode R.classLevel
  instanceLevel = toCode R.instanceLevel

instance PermElim CSharpCode where
  perm = unCSC
  binding = error $ CP.bindingError csName

instance BodySym CSharpCode where
  type Body CSharpCode = Doc
  body = onStateList (onCodeList R.body)

  addComments s = onStateValue (onCodeValue (R.addComments s commentStart))

instance RenderBody CSharpCode where
  multiBody = G.multiBody

instance BodyElim CSharpCode where
  body = unCSC

instance BlockSym CSharpCode where
  type Block CSharpCode = Doc
  block = G.block

instance RenderBlock CSharpCode where
  multiBlock = G.multiBlock

instance BlockElim CSharpCode where
  block = unCSC

instance TypeSym CSharpCode where
  bool = addSystemImport csBoolType
  int = CP.int
  float = C.float
  double = C.double
  char = C.char
  string = CP.string
  infile = csInfileType
  outfile = csOutfileType
  listType t = do
    modify (addLangImportVS csGeneric)
    C.listType csList t
  arrayType = CP.arrayType
  setType t = do
    modify (addLangImportVS csGeneric)
    C.setType csSet t
  listInnerType = G.listInnerType
  funcType = csFuncType
  void = C.void

instance OOTypeSym CSharpCode where
  obj = G.obj

instance RenderType CSharpCode where
  multiType _ = error $ C.multiTypeError csName

instance UnaryOpSym CSharpCode where
  notOp = C.notOp
  negateOp = G.negateOp
  sqrtOp = csUnaryMath "Sqrt"
  absOp = csUnaryMath "Abs"
  logOp = csUnaryMath "Log10"
  lnOp = csUnaryMath "Log"
  expOp = csUnaryMath "Exp"
  sinOp = csUnaryMath "Sin"
  cosOp = csUnaryMath "Cos"
  tanOp = csUnaryMath "Tan"
  asinOp = csUnaryMath "Asin"
  acosOp = csUnaryMath "Acos"
  atanOp = csUnaryMath "Atan"
  floorOp = csUnaryMath "Floor"
  ceilOp = csUnaryMath "Ceiling"

instance BinaryOpSym CSharpCode where
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
  powerOp = addSystemImport $ powerPrec $ mathFunc "Pow"
  moduloOp = G.moduloOp
  andOp = C.andOp
  orOp = C.orOp

instance OpElim CSharpCode where
  uOp = opDoc . unCSC
  bOp = opDoc . unCSC
  uOpPrec = opPrec . unCSC
  bOpPrec = opPrec . unCSC

instance ScopeSym CSharpCode where
  global = CP.global
  mainFn = local
  local = G.local

instance ScopeElim CSharpCode where
  scopeData = unCSC

instance VariableSym CSharpCode where
  type Variable CSharpCode = VarData
  var         = G.var
  constant    = var
  extVar      = CS.extVar

instance OOVariableSym CSharpCode where
  classVar = G.classVar
  classConst = classVar
  classVarAccess = CP.classVarAccess R.classVarAccess
  extClassVarAccess = classVarAccess
  instanceVarAccess = G.instanceVarAccess

instance SelfSym CSharpCode where
  self = C.self

instance InstanceVarSelfSym CSharpCode where
  instanceVarSelf = CP.instanceVarSelf

instance VariableElim CSharpCode where
  variableName = varName . unCSC
  variableType = onCodeValue varType

instance InternalVarElim CSharpCode where
  variableBind = varBind . unCSC
  variable = varDoc . unCSC

instance RenderVariable CSharpCode where
  varFromData b n t' d = do
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym CSharpCode where
  type Value CSharpCode = ValData
  valueType = onCodeValue valType

instance OOValueSym CSharpCode

instance Argument CSharpCode where
  pointerArg = id

instance Literal CSharpCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = C.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = csLitList arrayType
  litSet = csLitList setType
  litList = csLitList listType

instance MathConstant CSharpCode where
  pi = CP.pi

instance VariableValue CSharpCode where
  valueOf = G.valueOf

instance OOVariableValue CSharpCode

instance CommandLineArgs CSharpCode where
  arg n = G.arg (litInt n) argsList
  argsList = G.argsList args
  argExists i = listSize argsList ?> litInt (fromIntegral i)

instance NumericExpression CSharpCode where
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

instance BooleanExpression CSharpCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

instance Comparison CSharpCode where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression CSharpCode where
  inlineIf = C.inlineIf

  funcAppMixedArgs = G.funcAppMixedArgs
  extFuncAppMixedArgs = CS.extFuncAppMixedArgs
  libFuncAppMixedArgs = C.libFuncAppMixedArgs

  lambda = G.lambda csLambda

  notNull = CP.notNull nullLabel

instance OOValueExpression CSharpCode where
  selfMethodCallMixedArgs fn tp = objMethodCallMixedArgs' fn tp (valueOf self)
  newObjMixedArgs = G.newObjMixedArgs (new ++ " ")
  extNewObjMixedArgs _ = newObjMixedArgs
  libNewObjMixedArgs = C.libNewObjMixedArgs

instance RenderValue CSharpCode where
  inputFunc = addSystemImport csReadLineFunc
  printFunc = addSystemImport $ mkStateVal void (text $ csConsole `access`
    csWrite)
  printLnFunc = addSystemImport $ mkStateVal void (text $ csConsole `access`
    csWriteLine)
  printFileFunc w' = on2StateWrapped (\w vt ->
    mkVal vt . R.printFile csWrite . RC.value $ w) w' void
  printFileLnFunc w' = on2StateWrapped (\w vt ->
    mkVal vt . R.printFile csWriteLine . RC.value $ w) w' void

  cast = csCast

  call = G.call csNamedArgSep

  valFromData p i t' d = do
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim CSharpCode where
  valuePrec = valPrec . unCSC
  valueInt = valInt . unCSC
  value = val . unCSC

instance InternalValueExp CSharpCode where
  objMethodCallMixedArgs' = G.objMethodCall
  classMethodCallMixedArgs' = CG.classMethodCall

instance FunctionSym CSharpCode where
  type Function CSharpCode = FuncData

instance OOFunctionSym CSharpCode where
  func = G.func
  objAccess = G.objAccess

instance GetSet CSharpCode where
  get = G.get
  set = G.set

instance IndexTranslator CSharpCode where
  intToIndex = CP.intToIndex
  indexToInt = CP.indexToInt

instance Array CSharpCode where
  arrayElem = G.arrayElem
  arrayLength arr = valueOf $ instanceVarAccess arr (var "Length" int)
  arrayCopy arr = let
    arrTp = onStateValue valueType arr
    in cast arrTp (objMethodCall arrTp arr "Clone" [])

instance List CSharpCode where
  listSize = C.listSize' csListSize
  listAdd = CG.listAdd csListAdd
  listAppend = CG.listAppend csListAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = CP.indexOf csIndex

instance Set CSharpCode where
  contains = CP.contains csContains
  setAdd = CP.setMethodCall csListAppend
  setRemove = CP.setMethodCall csListRemove
  setUnion = CP.setMethodCall csUnionWith

instance InternalList CSharpCode where
  listSlice' = M.listSlice

instance InternalGetSet CSharpCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

instance InternalListFunc CSharpCode where
  listAccessFunc = CS.listAccessFunc
  -- Hack! This produces a statement and calls it a value
  listSetFunc = CS.listSetFunc R.listSetFunc

instance BinderSym CSharpCode where
  binder nm tp = onCodeValue (bindFormD nm) <$> tp

instance BinderElim CSharpCode where
  binderName = bindName . unCSC
  binderType = onCodeValue bindType

instance InternalBinderElim CSharpCode where
  binderElim = text . bindName . unCSC

instance ThunkSym CSharpCode where
  type Thunk CSharpCode = CommonThunk VS

instance ThunkAssign CSharpCode where
  thunkAssign v t = do
    iName <- genLoopIndex
    let
      i = var iName int
      dim = fmap pure $ t >>= commonThunkDim (fmap unCSC . listSize . fmap pure) . unCSC
      loopInit = zoom lensMStoVS (fmap unCSC t) >>= commonThunkElim
        (const emptyStmt) (const $ assign v $ litZero $ fmap variableType v)
      loopBody = zoom lensMStoVS (fmap unCSC t) >>= commonThunkElim
        (valStmt . listSet (valueOf v) (valueOf i) . vecIndex (valueOf i) . pure . pure)
        ((v &+=) . vecIndex (valueOf i) . pure . pure)
    multi [loopInit,
      forRange i (litInt 0) dim (litInt 1) $ body [block [loopBody]]]

instance VectorType CSharpCode where
  vecType = listType

instance VectorDecl CSharpCode where
  vecDec = listDec
  vecDecDef = listDecDef

instance VectorThunk CSharpCode where
  vecThunk = pure . pure . pureValue . fmap unCSC . valueOf

instance VectorExpression CSharpCode where
  vecScale k = fmap $ fmap $ vectorize (fmap unCSC . (k #*) . fmap pure)
  vecAdd = liftA2 $ liftA2 $ vectorize2 (\v1 v2 -> fmap unCSC $ fmap pure v1 #+ fmap pure v2)
  vecIndex i = (>>= fmap pure . commonVecIndex (fmap unCSC . flip listAccess i . fmap pure) . unCSC)
  vecDot = liftA2 $ liftA2 $ fmap sumComponents <$> vectorize2 (\v1 v2 -> fmap unCSC $ fmap pure v1 #* fmap pure v2)

instance RenderFunction CSharpCode where
  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance FunctionElim CSharpCode where
  functionType = onCodeValue fType
  function = funcDoc . unCSC

instance InternalAssignStmt CSharpCode where
  multiAssign _ _ = error $ C.multiAssignError csName

instance InternalIOStmt CSharpCode where
  printSt _ _ = CP.printSt

instance InternalControlStmt CSharpCode where
  multiReturn _ = error $ C.multiReturnError csName

instance RenderStatement CSharpCode where
  stmt = G.stmt
  loopStmt = G.loopStmt
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim CSharpCode where
  statement = fst . unCSC
  statementTerm = snd . unCSC

instance StatementSym CSharpCode where
  type Statement CSharpCode = (Doc, Terminator)
  valStmt = G.valStmt Semi
  emptyStmt = G.emptyStmt
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement CSharpCode where
  assign = G.assign Semi
  (&-=) = G.subAssign Semi
  (&+=) = C.increment
  (&++) = C.increment1
  (&--) = C.decrement1

instance DeclStatement CSharpCode where
  varDec v scp = zoom lensMStoVS v >>= (\v' -> csVarDec (variableBind v') $
    C.varDec classLevel instanceLevel empty v scp)
  varDecDef = C.varDecDef Semi
  setDec = varDec
  setDecDef = varDecDef
  listDec n v scp = zoom lensMStoVS v >>= (\v' -> C.listDec (renderListDec v')
    (litInt n) v scp)
  listDecDef = CP.listDecDef
  arrayDec n = CP.arrayDec (litInt n)
  arrayDecDef = CP.arrayDecDef
  constDecDef = CG.constDecDef
  funcDecDef = csFuncDecDef

instance OODeclStatement CSharpCode where
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = C.extObjDecNew

instance IOStatement CSharpCode where
  print      = csPrint False Nothing printFunc
  printLn    = csPrint True  Nothing printLnFunc
  printStr   = csPrint False Nothing printFunc   . litString
  printStrLn = csPrint True  Nothing printLnFunc . litString

  printFile f      = csPrint False (Just f) (printFileFunc f)
  printFileLn f    = csPrint True  (Just f) (printFileLnFunc f)
  printFileStr f   = csPrint False (Just f) (printFileFunc f)   . litString
  printFileStrLn f = csPrint True  (Just f) (printFileLnFunc f) . litString

  getInput v = v &= csInput (onStateValue variableType v) inputFunc
  discardInput = csDiscardInput inputFunc
  getFileInput f v = v &= csInput (onStateValue variableType v) (csFileInput f)
  discardFileInput f = valStmt $ csFileInput f

  openFileR = CP.openFileR csOpenFileR
  openFileW = CP.openFileW csOpenFileWorA
  openFileA = CP.openFileA csOpenFileWorA
  closeFile = G.closeFile csClose

  getFileInputLine = getFileInput
  discardFileLine = CP.discardFileLine csReadLine
  getFileInputAll f v = while ((f $. funcFromData (dot <> text csEOS) bool) ?!)
    (oneLiner $ valStmt $ listAppend (valueOf v) (csFileInput f))

instance StringStatement CSharpCode where
  stringSplit d vnew s = assign vnew $ newObj (listType string)
    [s $. csSplitFunc d]

  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FuncAppStatement CSharpCode where
  inOutCall = csInOutCall funcApp
  extInOutCall m = csInOutCall (extFuncApp m)

instance OOFuncAppStatement CSharpCode where
  selfInOutCall = csInOutCall selfMethodCall

instance CommentStatement CSharpCode where
  comment = G.comment commentStart

instance ControlStatement CSharpCode where
  break =  mkStmt R.break
  continue =  mkStmt R.continue

  returnStmt = G.returnStmt Semi

  throw msg = do
    modify (addLangImport csSystem)
    G.throw csThrowDoc Semi msg

  ifCond = G.ifCond parens bodyStart G.defaultOptSpace elseIfLabel bodyEnd empty
  switch = C.switch parens break

  ifExists = M.ifExists

  for = C.for bodyStart bodyEnd
  forRange = M.forRange
  forEach = CP.forEach bodyStart bodyEnd csForEach inLabel
  while = C.while parens bodyStart bodyEnd

  tryCatch = G.tryCatch csTryCatch

  assert condition errorMessage = do
    modify (addLangImport csDiagnostics)
    cond <- zoom lensMStoVS condition
    errMsg <- zoom lensMStoVS errorMessage
    mkStmtNoEnd (csAssert cond errMsg)

instance ObserverPattern CSharpCode where
  notifyObservers = M.notifyObservers

instance StrategyPattern CSharpCode where
  runStrategy = M.runStrategy

instance VisibilitySym CSharpCode where
  type Visibility CSharpCode = Doc
  private = toCode R.private
  public = toCode R.public

instance RenderVisibility CSharpCode where
  visibilityFromData _ = toCode

instance VisibilityElim CSharpCode where
  visibility = unCSC

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypeData
  mType = zoom lensMStoVS

instance OOMethodTypeSym CSharpCode where
  construct = G.construct

instance ParameterSym CSharpCode where
  type Parameter CSharpCode = ParamData
  param = G.param renderParam
  pointerParam = param

instance RenderParam CSharpCode where
  paramFromData v' d = do
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)

instance ParamElim CSharpCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unCSC

instance MethodSym CSharpCode where
  type Method CSharpCode = MethodData
  docMain = CP.docMain
  function = G.function
  mainFunction = CP.mainFunction string csMain
  docFunc = CP.doxFunc

  inOutFunc n s = csInOut (function n s)
  docInOutFunc n s = CP.docInOutFunc (inOutFunc n s)

instance OOMethodSym CSharpCode where
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor ps is b = getClassName >>= (\n -> CP.constructor n ps is b)

  inOutMethod n s p = csInOut (method n s p)
  docInOutMethod n s p = CP.docInOutFunc (inOutMethod n s p)

instance RenderMethod CSharpCode where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)

  mthdFromData _ d = toState $ toCode $ mthd d

instance OORenderMethod CSharpCode where
  intMethod m n s p t ps b = do
    modify (if m then setCurrMain else id)
    tp <- t
    pms <- sequence ps
    toCode . mthd . renderMethod n s p tp pms <$> b
  intFunc = C.intFunc
  destructor _ = error $ CP.destructorError csName

instance MethodElim CSharpCode where
  method = mthdDoc . unCSC

instance StateVarSym CSharpCode where
  type StateVar CSharpCode = Doc
  stateVar = CP.stateVar
  stateVarDef = CP.stateVarDef
  constVar = CP.constVar empty

instance StateVarElim CSharpCode where
  stateVar = unCSC

instance ClassSym CSharpCode where
  type Class CSharpCode = Doc
  buildClass = G.buildClass
  extraClass = CP.extraClass
  implementingClass = G.implementingClass

  docClass = CP.doxClass

instance RenderClass CSharpCode where
  intClass = CP.intClass R.class'

  inherit = CP.inherit
  implements = CP.implements

  commentedClass = G.commentedClass

instance ClassElim CSharpCode where
  class' = unCSC

instance ModuleSym CSharpCode where
  type Module CSharpCode = ModData
  buildModule n = CP.buildModule' n langImport

instance RenderMod CSharpCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim CSharpCode where
  module' = modDoc . unCSC

instance BlockCommentSym CSharpCode where
  blockComment lns = toCode $ R.blockCmt lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns docCmtStart
    blockCmtEnd)

instance BlockCommentElim CSharpCode where
  blockComment' = unCSC

addSystemImport :: VS a -> VS a
addSystemImport = (>>) $ modify (addLangImportVS csSystem)

csName, csVersion :: String
csName = "C#"
csVersion = "6.0"

csImport :: Label -> Doc
csImport n = text ("using " ++ n) <> endStatement

csBoolType :: (Monad r) => VSType r
csBoolType = typeFromData Boolean csBool (text csBool)

csFuncType :: [VSType CSharpCode] -> VSType CSharpCode -> VSType CSharpCode
csFuncType ps r = do
  pts <- sequence ps
  rt <- r
  typeFromData (Func (map getCodeType pts) (getCodeType rt))
    (csFunc `containing` intercalate listSep (map getTypeString $ pts ++ [rt]))
    (text csFunc <> angles (hicat listSep' $ map renderType $ pts ++ [rt]))

csForEach, csNamedArgSep, csLambdaSep :: Doc
csForEach = text "foreach"
csNamedArgSep = colon <> space
csLambdaSep = text "=>"

csSystem, csConsole, csGeneric, csDiagnostics, csIO, csList, csSet, csInt, csFloat, csBool,
  csChar, csParse, csReader, csWriter, csReadLine, csWrite, csWriteLine,
  csIndex, csContains, csListAdd, csListSize, csListAppend, csListRemove,
  csClose, csEOS, csSplit, csMain, csFunc, csUnionWith :: String
csSystem = "System"
csConsole = "Console"
csGeneric = csSysAccess $ "Collections" `access` "Generic"
csDiagnostics = csSysAccess "Diagnostics"
csIO = csSysAccess "IO"
csList = "List"
csSet = "HashSet"
csInt = "Int32"
csFloat = "Single"
csBool = "Boolean"
csChar = "Char"
csParse = "Parse"
csReader = "StreamReader"
csWriter = "StreamWriter"
csReadLine = "ReadLine"
csWrite = "Write"
csWriteLine = "WriteLine"
csIndex = "IndexOf"
csContains = "Contains"
csListAdd = "Insert"
csListSize = "Count"
csListAppend = "Add"
csListRemove = "Remove"
csClose = "Close"
csEOS = "EndOfStream"
csSplit = "Split"
csMain = "Main"
csFunc = "Func"
csUnionWith = "UnionWith"

csSysAccess :: String -> String
csSysAccess = access csSystem

csUnaryMath :: (Monad r) => String -> VSOp r
csUnaryMath = addSystemImport . unOpPrec . mathFunc

csInfileType :: (Monad r) => VSType r
csInfileType = join $ modifyReturn (addLangImportVS csIO) $
  typeFromData InFile csReader (text csReader)

csOutfileType :: (Monad r) => VSType r
csOutfileType = join $ modifyReturn (addLangImportVS csIO) $
  typeFromData OutFile csWriter (text csWriter)

csLitList :: (VSType CSharpCode -> VSType CSharpCode) -> VSType CSharpCode ->
  [SValue CSharpCode] -> SValue CSharpCode
csLitList f t' es' = do
  es <- sequence es'
  lt <- f t'
  mkVal lt (new' <+> renderType lt
    <+> braces (valueList es))

csLambda :: (CommonRenderSym r) => [r BinderD] -> r (Value r) -> Doc
csLambda ps ex = parens (binderList ps) <+> csLambdaSep <+> RC.value ex

csReadLineFunc :: SValue CSharpCode
csReadLineFunc = extFuncApp csConsole csReadLine string []

csIntParse :: SValue CSharpCode -> SValue CSharpCode
csIntParse v = extFuncApp csInt csParse int [v]

csFloatParse :: SValue CSharpCode -> SValue CSharpCode
csFloatParse v = extFuncApp csFloat csParse float [v]

csDblParse :: SValue CSharpCode -> SValue CSharpCode
csDblParse v = extFuncApp CP.doubleRender csParse double [v]

csBoolParse :: SValue CSharpCode -> SValue CSharpCode
csBoolParse v = extFuncApp csBool csParse bool [v]

csCharParse :: SValue CSharpCode -> SValue CSharpCode
csCharParse v = extFuncApp csChar csParse char [v]

csSplitFunc :: Char -> VSFunction CSharpCode
csSplitFunc d = func csSplit (listType string) [litChar d]

csCast :: VSType CSharpCode -> SValue CSharpCode -> SValue CSharpCode
csCast = join .: on2StateValues (\t v -> csCast' (getCodeType t) (getCodeType $
  valueType v) t v)
  where csCast' Double String _ v = csDblParse (toState v)
        csCast' Float String _ v = csFloatParse (toState v)
        csCast' _ _ t v = mkStateVal (toState t) (R.castObj (R.cast
          (renderType t)) (RC.value v))

-- This implementation generates a statement lambda to define the function.
-- C# 7 supports local functions, which would be a cleaner way to implement
-- this, but the mcs compiler used in our Travis builds does not yet support
-- all features of C# 7, so we cannot generate local functions.
-- If support for local functions is added to mcs in the future, this
-- should be re-written to generate a local function.
csFuncDecDef :: SVariable CSharpCode -> CSharpCode ScopeData ->
  [SVariable CSharpCode] -> MSBody CSharpCode -> MSStatement CSharpCode
csFuncDecDef v scp ps bod = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  pms <- mapM (zoom lensMStoVS) ps
  t <- zoom lensMStoVS $ funcType (map (pure . variableType) pms)
    (pure $ variableType vr)
  b <- bod
  modify (addLangImport csSystem)
  mkStmt $ renderType t <+> text (variableName vr) <+> equals <+>
    parens (variableList pms) <+> csLambdaSep <+> bodyStart $$
    indent (RC.body b) $$ bodyEnd

csThrowDoc :: (CommonRenderSym r) => r (Value r) -> Doc
csThrowDoc errMsg = throwLabel <+> new' <+> exceptionObj' <>
  parens (RC.value errMsg)

csTryCatch :: (CommonRenderSym r) => r (Body r) -> r (Body r) -> Doc
csTryCatch tb cb = vcat [
  tryLabel <+> lbrace,
  indent $ RC.body tb,
  rbrace <+> catchLabel <+>
    lbrace,
  indent $ RC.body cb,
  rbrace]

csAssert :: (CommonRenderSym r) => r (Value r) -> r (Value r) -> Doc
csAssert condition errorMessage = vcat [
  text "Debug.Assert(" <+> RC.value condition <+> text "," <+> RC.value errorMessage <> text ")" <> semi
  ]

csDiscardInput :: SValue CSharpCode -> MSStatement CSharpCode
csDiscardInput = valStmt

csFileInput :: (OORenderSym r) => SValue r -> SValue r
csFileInput f = objMethodCallNoParams string f csReadLine

csInput :: VSType CSharpCode -> SValue CSharpCode -> SValue CSharpCode
csInput tp inFn = do
  t <- tp
  csInputImport (getCodeType t) (csInput' (getCodeType t) inFn)
  where csInput' Integer = csIntParse
        csInput' Float = csFloatParse
        csInput' Double = csDblParse
        csInput' Boolean = csBoolParse
        csInput' String = id
        csInput' Char = csCharParse
        csInput' _ = error "Attempt to read value of unreadable type"
        csInputImport t = if t `elem` [Integer, Float, Double, Boolean, Char]
          then addSystemImport else id

csOpenFileR :: (OORenderSym r) => SValue r -> VSType r -> SValue r
csOpenFileR n r = newObj r [n]

csOpenFileWorA :: (OORenderSym r) => SValue r -> VSType r -> SValue r -> SValue r
csOpenFileWorA n w a = newObj w [n, a]

csRef :: Doc -> Doc
csRef p = text "ref" <+> p

csOut :: Doc -> Doc
csOut p = text "out" <+> p

csInOutCall :: (Label -> VSType CSharpCode -> [SValue CSharpCode] ->
  SValue CSharpCode) -> Label -> [SValue CSharpCode] -> [SVariable CSharpCode]
  -> [SVariable CSharpCode] -> MSStatement CSharpCode
csInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out)
  ins
csInOutCall f n ins [] [out] = assign out $ f n (onStateValue variableType out)
  (valueOf out : ins)
csInOutCall f n ins outs both = valStmt $ f n void (map (onStateValue
  (onCodeValue (updateValDoc csRef)) . valueOf) both ++ ins ++ map
  (onStateValue (onCodeValue (updateValDoc csOut)) . valueOf) outs)

csVarDec :: AttachmentTag -> MSStatement CSharpCode -> MSStatement CSharpCode
csVarDec ClassLevel _ = error "ClassLevel variables can't be declared locally to a function in C#. Use stateVar to make a ClassLevel state variable instead."
csVarDec InstanceLevel d = d

csInOut :: (VSType CSharpCode -> [MSParameter CSharpCode] -> MSBody CSharpCode ->
    SMethod CSharpCode) ->
  [SVariable CSharpCode] -> [SVariable CSharpCode] -> [SVariable CSharpCode] ->
  MSBody CSharpCode -> SMethod CSharpCode
csInOut f ins [v] [] b = f (onStateValue variableType v) (map param ins)
  (on3StateValues (on3CodeValues surroundBody) (varDec v local) b (returnStmt $
  valueOf v))
csInOut f ins [] [v] b = f (onStateValue variableType v)
  (map param $ v : ins) (on2StateValues (on2CodeValues appendToBody) b
  (returnStmt $ valueOf v))
csInOut f ins outs both b = f void (map (onStateValue (onCodeValue
  (updateParam csRef)) . param) both ++ map param ins ++ map (onStateValue
  (onCodeValue (updateParam csOut)) . param) outs) b

csPrint :: (CommonRenderSym r, UnRepr r TypeData) => Bool -> Maybe (SValue r) -> SValue r ->
  SValue r -> MSStatement r
csPrint newLn f printFn v = zoom lensMStoVS v >>= csPrint' . getCodeType . valueType
  where csPrint' (Array _) = multi [printStr "[",
          print $ extFuncApp "string" "Join" string [litString ", ", v],
          printMaybeNewLn $ litString "]"]
        csPrint' _ = G.print newLn f printFn v
        printMaybeNewLn = if newLn then printLn else print
