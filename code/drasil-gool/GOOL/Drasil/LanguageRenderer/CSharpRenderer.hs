{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# code is contained in this module
module GOOL.Drasil.LanguageRenderer.CSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..)
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym, FileSym(..),
  InternalFile(..), KeywordSym(..), ImportSym(..), PermanenceSym(..), 
  InternalPerm(..), BodySym(..), InternalBody(..), BlockSym(..), 
  InternalBlock(..), ControlBlockSym(..), InternalControlBlock(..), TypeSym(..),
  InternalType(..), UnaryOpSym(..), BinaryOpSym(..), InternalOp(..), 
  VariableSym(..), InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  InternalValueExp(..), objMethodCall, objMethodCallNoParams, FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), InternalParam(..), MethodSym(..), 
  InternalMethod(..), StateVarSym(..), InternalStateVar(..), ClassSym(..), 
  InternalClass(..), ModuleSym(..), InternalMod(..), BlockCommentSym(..), 
  ODEInfo(..), ODEOptions(..), ODEMethod(..))
import GOOL.Drasil.LanguageRenderer (classDocD, multiStateDocD, bodyDocD, 
  outDoc, printFileDocD, destructorError, paramDocD, methodDocD, listDecDocD, 
  mkSt, mkStNoEnd, breakDocD, continueDocD, mkStateVal, mkVal, mkVar, 
  classVarDocD, objVarDocD, funcDocD, castDocD, listSetFuncDocD, castObjDocD, 
  staticDocD, dynamicDocD, bindingError, privateDocD, publicDocD, dot, 
  blockCmtStart, blockCmtEnd, docCmtStart, doubleSlash, elseIfLabel, inLabel, 
  blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, commentedModD, 
  variableList, appendToBody, surroundBody)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  oneLiner, multiBody, block, multiBlock, bool, int, float, double, char, 
  string, listType, arrayType, listInnerType, obj, funcType, void, 
  runStrategy, listSlice, notOp, csc, sec, cot, negateOp, equalOp, notEqualOp, 
  greaterOp, greaterEqualOp, lessOp, lessEqualOp,plusOp, minusOp, multOp, 
  divideOp, moduloOp, andOp, orOp, var, staticVar, extVar, self, 
  classVar, objVarSelf, listVar, listOf, arrayElem, iterVar, pi, litTrue, 
  litFalse, litChar, litDouble, litFloat, litInt, litString, litList, valueOf, 
  arg, argsList, inlineIf, objAccess, objMethodCall, 
  objMethodCallNoParams, selfAccess, listIndexExists, indexOf, call, funcApp, 
  funcAppMixedArgs, selfFuncApp, selfFuncAppMixedArgs, extFuncApp, 
  extFuncAppMixedArgs, libFuncApp, libFuncAppMixedArgs, newObj, 
  newObjMixedArgs, libNewObj, libNewObjMixedArgs, lambda, notNull, func, get, 
  set, listSize, listAdd, listAppend, iterBegin, iterEnd, listAccess, listSet, 
  getFunc, setFunc, listAddFunc, listAppendFunc, iterBeginError, iterEndError, 
  listAccessFunc, listSetFunc, printSt, state, loopState, emptyState, assign, 
  assignToListIndex, multiAssignError, decrement, increment, decrement1, 
  increment1, varDec, varDecDef, listDec, listDecDef', arrayDec, arrayDecDef, 
  objDecNew, objDecNewNoParams, extObjDecNew, extObjDecNewNoParams, 
  constDecDef, discardInput, openFileR, openFileW, openFileA, closeFile, 
  discardFileLine, stringListVals, stringListLists, returnState, 
  multiReturnError, valState, comment, freeError, throw, initState, 
  changeState, initObserverList, addObserver, ifCond, ifNoElse, switch, 
  switchAsIf, ifExists, for, forRange, forEach, while, tryCatch, checkState, 
  notifyObservers, construct, param, method, getMethod, setMethod, privMethod, 
  pubMethod, constructor, docMain, function, mainFunction, docFunc, 
  docInOutFunc, intFunc, stateVar, stateVarDef, constVar, privMVar, pubMVar, 
  pubGVar, buildClass, implementingClass, docClass, commentedClass, 
  intClass, buildModule', modFromData, fileDoc, docMod, fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (unOpPrec, unExpr, 
  unExpr', unExprNumDbl, typeUnExpr, powerPrec, binExpr, binExprNumDbl', 
  typeBinExpr)
import GOOL.Drasil.AST (Terminator(..), ScopeTag(..), FileType(..), 
  FileData(..), fileD, FuncData(..), fd, ModData(..), md, updateModDoc, 
  MethodData(..), mthd, updateMthdDoc, OpData(..), od, ParamData(..), pd, 
  updateParamDoc, ProgData(..), progD, TypeData(..), td, ValData(..), vd, 
  updateValDoc, Binding(..), VarData(..), vard)
import GOOL.Drasil.Helpers (toCode, toState, onCodeValue, onStateValue, 
  on2CodeValues, on2StateValues, on3CodeValues, on3StateValues, onCodeList, 
  onStateList, on1CodeValue1List)
import GOOL.Drasil.State (MS, VS, lensGStoFS, lensMStoVS, modifyReturn, 
  addLangImport, addLangImportVS, addLibImport, setFileType, getClassName, 
  setCurrMain, setODEDepVars, getODEDepVars)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (modify)
import Data.List (elemIndex, intercalate)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($$), parens, empty,
  semi, vcat, lbrace, rbrace, colon, space)

csExt :: String
csExt = "cs"

newtype CSharpCode a = CSC {unCSC :: a} deriving Eq

instance Functor CSharpCode where
  fmap f (CSC x) = CSC (f x)

instance Applicative CSharpCode where
  pure = CSC
  (CSC f) <*> (CSC x) = CSC (f x)

instance Monad CSharpCode where
  return = CSC
  CSC x >>= f = f x

instance ProgramSym CSharpCode where
  type Program CSharpCode = ProgData
  prog n = onStateList (onCodeList (progD n)) . map (zoom lensGStoFS)

instance RenderSym CSharpCode

instance FileSym CSharpCode where
  type RenderFile CSharpCode = FileData
  fileDoc m = modify (setFileType Combined) >> G.fileDoc csExt top bottom m

  docMod = G.docMod csExt

  commentedMod cmt m = on2StateValues (on2CodeValues commentedModD) m cmt

instance InternalFile CSharpCode where
  top _ = toCode empty
  bottom = toCode empty

  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym CSharpCode where
  type Keyword CSharpCode = Doc
  endStatement = toCode semi
  endStatementLoop = toCode empty

  inherit n = toCode $ colon <+> text n
  implements is = toCode $ colon <+> text (intercalate ", " is)

  list = toCode $ text "List"

  blockStart = toCode lbrace
  blockEnd = toCode rbrace

  ifBodyStart = blockStart
  elseIf = toCode elseIfLabel
  
  iterForEachLabel = toCode $ text "foreach"
  iterInLabel = toCode inLabel

  commentStart = toCode doubleSlash
  blockCommentStart = toCode blockCmtStart
  blockCommentEnd = toCode blockCmtEnd
  docCommentStart = toCode docCmtStart
  docCommentEnd = blockCommentEnd

  keyFromDoc = toCode
  keyDoc = unCSC

instance ImportSym CSharpCode where
  type Import CSharpCode = Doc
  langImport n = toCode $ csImport n endStatement
  modImport = langImport

  importDoc = unCSC

instance PermanenceSym CSharpCode where
  type Permanence CSharpCode = Doc
  static = toCode staticDocD
  dynamic = toCode dynamicDocD

instance InternalPerm CSharpCode where
  permDoc = unCSC
  binding = error $ bindingError csName

instance BodySym CSharpCode where
  type Body CSharpCode = Doc
  body = onStateList (onCodeList bodyDocD)
  bodyStatements = block
  oneLiner = G.oneLiner

  addComments s = onStateValue (on2CodeValues (addCommentsDocD s) commentStart)

instance InternalBody CSharpCode where
  bodyDoc = unCSC
  docBody = onStateValue toCode
  multiBody = G.multiBody 

instance BlockSym CSharpCode where
  type Block CSharpCode = Doc
  block = G.block endStatement

instance InternalBlock CSharpCode where
  blockDoc = unCSC
  docBlock = onStateValue toCode
  multiBlock = G.multiBlock

instance TypeSym CSharpCode where
  type Type CSharpCode = TypeData
  bool = addSystemImport G.bool
  int = G.int
  float = G.float
  double = G.double
  char = G.char
  string = G.string
  infile = csInfileType
  outfile = csOutfileType
  listType t = modify (addLangImportVS "System.Collections.Generic") >> 
    G.listType list t
  arrayType = G.arrayType
  listInnerType = G.listInnerType
  obj = G.obj
  -- enumType = G.enumType
  funcType = G.funcType
  iterator t = t
  void = G.void

  getType = cType . unCSC
  getTypeString = typeString . unCSC
  getTypeDoc = typeDoc . unCSC
  
instance InternalType CSharpCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym CSharpCode where
  runStrategy = G.runStrategy

  solveODE info opts = modify (addLibImport "Microsoft.Research.Oslo" . 
    addLangImport "System.Linq") >> 
    multiBlock [
      block [
        objDecNewNoParams optsVar,
        objVar optsVar (var "AbsoluteTolerance" float) &= absTol opts,
        objVar optsVar (var "AbsoluteTolerance" float) &= relTol opts],
      block [
        varDecDef sol (extFuncApp "Ode" (csODEMethod $ solveMethod opts) odeT 
        [tInit info, 
        newObj vec [initVal info], 
        lambda [iv, dv] (newObj vec [dv >>= (\dpv -> modify (setODEDepVars 
          [variableName dpv]) >> ode info)]),
        valueOf optsVar])],
      block [
        varDecDef points (objMethodCallNoParams spArray 
        (objMethodCall void (valueOf sol) "SolveFromToStep" 
          [tInit info, tFinal info, stepSize opts]) "ToArray"),
        listDecDef dv [],
        forEach sp (valueOf points) 
          (oneLiner $ valState $ listAppend (valueOf dv) (valueOf $ 
          objVar sp (var "X" (listInnerType $ onStateValue variableType dv))))]
    ]
    where optsVar = var "opts" (obj "Options")
          iv = indepVar info
          dv = depVar info
          odeT = obj "IEnumerable<SolPoint>"
          vec = obj "Vector"
          sol = var "sol" odeT
          spArray = arrayType (obj "SolPoint")
          points = var "points" spArray
          sp = var "sp" (obj "SolPoint")

instance InternalControlBlock CSharpCode where
  listSlice' = G.listSlice

instance UnaryOpSym CSharpCode where
  type UnaryOp CSharpCode = OpData
  notOp = G.notOp
  negateOp = G.negateOp
  sqrtOp = addSystemImport $ unOpPrec "Math.Sqrt"
  absOp = addSystemImport $ unOpPrec "Math.Abs"
  logOp = addSystemImport $ unOpPrec "Math.Log10"
  lnOp = addSystemImport $ unOpPrec "Math.Log"
  expOp = addSystemImport $ unOpPrec "Math.Exp"
  sinOp = addSystemImport $ unOpPrec "Math.Sin"
  cosOp = addSystemImport $ unOpPrec "Math.Cos"
  tanOp = addSystemImport $ unOpPrec "Math.Tan"
  asinOp = addSystemImport $ unOpPrec "Math.Asin"
  acosOp = addSystemImport $ unOpPrec "Math.Acos"
  atanOp = addSystemImport $ unOpPrec "Math.Atan"
  floorOp = addSystemImport $ unOpPrec "Math.Floor"
  ceilOp = addSystemImport $ unOpPrec "Math.Ceiling"

instance BinaryOpSym CSharpCode where
  type BinaryOp CSharpCode = OpData
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
  powerOp = addSystemImport $ powerPrec "Math.Pow"
  moduloOp = G.moduloOp
  andOp = G.andOp
  orOp = G.orOp

instance InternalOp CSharpCode where
  uOpDoc = opDoc . unCSC
  bOpDoc = opDoc . unCSC
  uOpPrec = opPrec . unCSC
  bOpPrec = opPrec . unCSC
  
  uOpFromData p d = toState $ toCode $ od p d
  bOpFromData p d = toState $ toCode $ od p d

instance VariableSym CSharpCode where
  type Variable CSharpCode = VarData
  var = G.var
  staticVar = G.staticVar
  const = var
  extVar = G.extVar
  self = G.self
  -- enumVar = G.enumVar
  classVar = G.classVar classVarDocD
  extClassVar = classVar
  objVar = on2StateValues csObjVar
  objVarSelf = G.objVarSelf
  listVar  = G.listVar
  listOf = G.listOf
  arrayElem i = G.arrayElem (litInt i)
  iterVar = G.iterVar

  ($->) = objVar

  variableBind = varBind . unCSC
  variableName = varName . unCSC
  variableType = onCodeValue varType
  variableDoc = varDoc . unCSC

instance InternalVariable CSharpCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CSharpCode where
  type Value CSharpCode = ValData
  litTrue = G.litTrue
  litFalse = G.litFalse
  litChar = G.litChar
  litDouble = G.litDouble
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = G.litList arrayType
  litList = G.litList listType

  pi = G.pi

  -- ($:) = enumElement

  valueOf v = join $ on2StateValues (\dvs vr -> maybe (G.valueOf v) (listAccess 
    (G.valueOf v) . litInt . toInteger) (elemIndex (variableName vr) dvs)) 
    getODEDepVars v
  arg n = G.arg (litInt n) argsList
  -- enumElement = G.enumElement
  
  argsList = G.argsList "args"

  valueType = onCodeValue valType
  valueDoc = valDoc . unCSC

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

  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool
  
instance ValueExpression CSharpCode where
  inlineIf = G.inlineIf
  funcApp = G.funcApp
  funcAppNamedArgs n t = funcAppMixedArgs n t []
  funcAppMixedArgs = G.funcAppMixedArgs
  selfFuncApp = G.selfFuncApp
  selfFuncAppMixedArgs = G.selfFuncAppMixedArgs dot self
  extFuncApp = G.extFuncApp
  extFuncAppMixedArgs = G.extFuncAppMixedArgs
  libFuncApp = G.libFuncApp
  libFuncAppMixedArgs = G.libFuncAppMixedArgs
  newObj = G.newObj
  newObjMixedArgs = G.newObjMixedArgs "new "
  extNewObj _ = newObj
  extNewObjMixedArgs _ = newObjMixedArgs
  libNewObj = G.libNewObj
  libNewObjMixedArgs = G.libNewObjMixedArgs

  lambda = G.lambda csLambda

  exists = notNull
  notNull = G.notNull

instance InternalValue CSharpCode where
  inputFunc = addSystemImport $ mkStateVal string (text "Console.ReadLine()")
  printFunc = addSystemImport $ mkStateVal void (text "Console.Write")
  printLnFunc = addSystemImport $ mkStateVal void (text "Console.WriteLine")
  printFileFunc = on2StateValues (\v -> mkVal v . printFileDocD "Write" . 
    valueDoc) void
  printFileLnFunc = on2StateValues (\v -> mkVal v . printFileDocD "WriteLine" . 
    valueDoc) void
  
  cast = csCast

  call = G.call (colon <> space)
  
  valuePrec = valPrec . unCSC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CSharpCode where
  objAccess = G.objAccess
  ($.) = objAccess

  selfAccess = G.selfAccess

  listIndexExists = G.listIndexExists
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf = G.indexOf "IndexOf"
  
instance InternalValueExp CSharpCode where
  objMethodCallMixedArgs' = G.objMethodCall 
  objMethodCallNoParams' = G.objMethodCallNoParams

instance FunctionSym CSharpCode where
  type Function CSharpCode = FuncData
  func = G.func

  get = G.get
  set = G.set

  listSize = G.listSize
  listAdd = G.listAdd
  listAppend = G.listAppend

  iterBegin = G.iterBegin
  iterEnd = G.iterEnd

instance SelectorFunction CSharpCode where
  listAccess = G.listAccess
  listSet = G.listSet
  at = listAccess

instance InternalFunction CSharpCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

  listSizeFunc = funcFromData (funcDocD (text "Count")) int
  listAddFunc _ = G.listAddFunc "Insert"
  listAppendFunc = G.listAppendFunc "Add"

  iterBeginFunc _ = error $ G.iterBeginError csName
  iterEndFunc _ = error $ G.iterEndError csName

  listAccessFunc = G.listAccessFunc
  listSetFunc = G.listSetFunc listSetFuncDocD 
    
  functionType = onCodeValue fType
  functionDoc = funcDoc . unCSC

  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance InternalStatement CSharpCode where
  printSt _ _ = G.printSt

  state = G.state
  loopState = G.loopState

  emptyState = G.emptyState
  statementDoc = fst . unCSC
  statementTerm = snd . unCSC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CSharpCode where
  type Statement CSharpCode = (Doc, Terminator)
  assign = G.assign Semi
  assignToListIndex = G.assignToListIndex
  multiAssign _ _ = error $ G.multiAssignError csName
  (&=) = assign
  (&-=) = G.decrement
  (&+=) = G.increment
  (&++) = G.increment1
  (&~-) = G.decrement1

  varDec v = zoom lensMStoVS v >>= (\v' -> csVarDec (variableBind v') $ 
    G.varDec static dynamic v)
  varDecDef = G.varDecDef
  listDec n v = zoom lensMStoVS v >>= (\v' -> G.listDec (listDecDocD v') 
    (litInt n) v)
  listDecDef = G.listDecDef'
  arrayDec n = G.arrayDec (litInt n)
  arrayDecDef = G.arrayDecDef
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = G.extObjDecNew
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams = G.extObjDecNewNoParams
  constDecDef = G.constDecDef
  funcDecDef = csFuncDecDef blockStart blockEnd

  print = outDoc False Nothing printFunc
  printLn = outDoc True Nothing printLnFunc
  printStr = outDoc False Nothing printFunc . litString
  printStrLn = outDoc True Nothing printLnFunc . litString

  printFile f = outDoc False (Just f) (printFileFunc f)
  printFileLn f = outDoc True (Just f) (printFileLnFunc f)
  printFileStr f = outDoc False (Just f) (printFileFunc f) . litString
  printFileStrLn f = outDoc True (Just f) (printFileLnFunc f) . litString

  getInput v = v &= csInput (onStateValue variableType v) inputFunc
  discardInput = G.discardInput csDiscardInput
  getFileInput f v = v &= csInput (onStateValue variableType v) (csFileInput f)
  discardFileInput f = valState $ csFileInput f

  openFileR = G.openFileR csOpenFileR
  openFileW = G.openFileW csOpenFileWorA
  openFileA = G.openFileA csOpenFileWorA
  closeFile = G.closeFile "Close"

  getFileInputLine = getFileInput
  discardFileLine = G.discardFileLine "ReadLine"
  stringSplit d vnew s = assign vnew $ newObj (listType string) 
    [s $. func "Split" (listType string) [litChar d]]

  stringListVals = G.stringListVals
  stringListLists = G.stringListLists

  break = toState $ mkSt breakDocD
  continue = toState $ mkSt continueDocD

  returnState = G.returnState Semi
  multiReturn _ = error $ G.multiReturnError csName 

  valState = G.valState Semi

  comment = G.comment commentStart

  free _ = error $ G.freeError csName -- could set variable to null? Might be misleading.

  throw msg = modify (addLangImport "System") >> G.throw csThrowDoc Semi msg

  initState = G.initState
  changeState = G.changeState

  initObserverList = G.initObserverList
  addObserver = G.addObserver

  inOutCall = csInOutCall funcApp
  selfInOutCall = csInOutCall selfFuncApp
  extInOutCall m = csInOutCall (extFuncApp m)

  multi = onStateList (on1CodeValue1List multiStateDocD endStatement)

instance ControlStatementSym CSharpCode where
  ifCond = G.ifCond ifBodyStart elseIf blockEnd
  ifNoElse = G.ifNoElse
  switch = G.switch
  switchAsIf = G.switchAsIf

  ifExists = G.ifExists

  for = G.for blockStart blockEnd
  forRange = G.forRange
  forEach = G.forEach blockStart blockEnd iterForEachLabel iterInLabel 
  while = G.while blockStart blockEnd

  tryCatch = G.tryCatch csTryCatch

  checkState = G.checkState
  notifyObservers = G.notifyObservers

  getFileInputAll f v = while ((f $. funcFromData (text ".EndOfStream") bool) 
    ?!) (oneLiner $ valState $ listAppend (valueOf v) (csFileInput f))

instance ScopeSym CSharpCode where
  type Scope CSharpCode = Doc
  private = toCode privateDocD
  public = toCode publicDocD

instance InternalScope CSharpCode where
  scopeDoc = unCSC
  scopeFromData _ = toCode

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypeData
  mType = zoom lensMStoVS 
  construct = G.construct

instance ParameterSym CSharpCode where
  type Parameter CSharpCode = ParamData
  param = G.param paramDocD
  pointerParam = param

instance InternalParam CSharpCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameterDoc = paramDoc . unCSC
  paramFromData v d = on2CodeValues pd v (toCode d)

instance MethodSym CSharpCode where
  type Method CSharpCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor ps is b = getClassName >>= (\n -> G.constructor n ps is b)
  destructor _ = error $ destructorError csName

  docMain = G.docMain
 
  function = G.function
  mainFunction = G.mainFunction string "Main"

  docFunc = G.docFunc

  inOutMethod n = csInOut (method n)

  docInOutMethod n = G.docInOutFunc (inOutMethod n)

  inOutFunc n = csInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CSharpCode where
  intMethod m n s p t ps b = modify (if m then setCurrMain else id) >> 
    on3StateValues (\tp pms bd -> methodFromData Pub $ methodDocD n s p tp pms 
    bd) t (sequence ps) b
  intFunc = G.intFunc
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthdDoc) m 
    (onStateValue (onCodeValue commentedItem) cmt)
  
  methodDoc = mthdDoc . unCSC
  methodFromData _ = toCode . mthd

instance StateVarSym CSharpCode where
  type StateVar CSharpCode = Doc
  stateVar = G.stateVar
  stateVarDef _ = G.stateVarDef
  constVar _ = G.constVar empty
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance InternalStateVar CSharpCode where
  stateVarDoc = unCSC
  stateVarFromData = onStateValue toCode

instance ClassSym CSharpCode where
  type Class CSharpCode = Doc
  buildClass = G.buildClass
  -- enum = G.enum
  extraClass = buildClass
  implementingClass = G.implementingClass

  docClass = G.docClass

  commentedClass = G.commentedClass

instance InternalClass CSharpCode where
  intClass = G.intClass classDocD
  classDoc = unCSC
  classFromData = onStateValue toCode

instance ModuleSym CSharpCode where
  type Module CSharpCode = ModData
  buildModule n = G.buildModule' n langImport
  
instance InternalMod CSharpCode where
  moduleDoc = modDoc . unCSC
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateModDoc f)

instance BlockCommentSym CSharpCode where
  type BlockComment CSharpCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unCSC

addSystemImport :: VS a -> VS a
addSystemImport = (>>) $ modify (addLangImportVS "System")

csName :: String
csName = "C#"

csODEMethod :: ODEMethod -> String
csODEMethod RK45 = "RK547M"
csODEMethod BDF = "GearBDF"
csODEMethod _ = error "Chosen ODE method unavailable in C#"

csImport :: Label -> CSharpCode (Keyword CSharpCode) -> Doc
csImport n end = text ("using " ++ n) <> keyDoc end

csInfileType :: (RenderSym repr) => VS (repr (Type repr))
csInfileType = modifyReturn (addLangImportVS "System.IO") $ 
  typeFromData File "StreamReader" (text "StreamReader")

csOutfileType :: (RenderSym repr) => VS (repr (Type repr))
csOutfileType = modifyReturn (addLangImportVS "System.IO") $ 
  typeFromData File "StreamWriter" (text "StreamWriter")

csLambda :: (RenderSym repr) => [repr (Variable repr)] -> repr (Value repr) -> 
  Doc
csLambda ps ex = parens (variableList ps) <+> text "=>" <+> valueDoc ex

csCast :: VS (CSharpCode (Type CSharpCode)) -> 
  VS (CSharpCode (Value CSharpCode)) -> VS (CSharpCode (Value CSharpCode))
csCast t v = join $ on2StateValues (\tp vl -> csCast' (getType tp) (getType $ 
  valueType vl) tp vl) t v
  where csCast' Double String _ _ = funcApp "Double.Parse" double [v]
        csCast' Float String _ _ = funcApp "Single.Parse" float [v]
        csCast' _ _ tp vl = mkStateVal t (castObjDocD (castDocD (getTypeDoc 
          tp)) (valueDoc vl))

csFuncDecDef :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) 
  -> VS (repr (Variable repr)) -> [VS (repr (Variable repr))] -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
csFuncDecDef bStart bEnd v ps r = on3StateValues (\vr pms b -> mkStNoEnd $ 
  getTypeDoc (variableType vr) <+> text (variableName vr) <> parens 
  (variableList pms) <+> keyDoc bStart $$ indent (bodyDoc b) $$ keyDoc bEnd) 
  (zoom lensMStoVS v) (mapM (zoom lensMStoVS) ps) (oneLiner $ returnState r)

csThrowDoc :: (RenderSym repr) => repr (Value repr) -> Doc
csThrowDoc errMsg = text "throw new" <+> text "Exception" <> 
  parens (valueDoc errMsg)

csTryCatch :: (RenderSym repr) => repr (Body repr) -> repr (Body repr) -> Doc
csTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent $ bodyDoc tb,
  rbrace <+> text "catch" <+> 
    lbrace,
  indent $ bodyDoc cb,
  rbrace]

csDiscardInput :: (RenderSym repr) => repr (Value repr) -> Doc
csDiscardInput = valueDoc

csFileInput :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Value repr))
csFileInput = onStateValue (\f -> mkVal (valueType f) (valueDoc f <> dot <> 
  text "ReadLine()"))

csInput :: (RenderSym repr) => VS (repr (Type repr)) -> VS (repr (Value repr)) 
  -> VS (repr (Value repr))
csInput tp inF = tp >>= (\t -> csInputImport (getType t) $ onStateValue (\inFn 
  -> mkVal t $ text (csInput' (getType t)) <> parens (valueDoc inFn)) inF)
  where csInput' Integer = "Int32.Parse"
        csInput' Float = "Single.Parse"
        csInput' Double = "Double.Parse"
        csInput' Boolean = "Boolean.Parse"
        csInput' String = ""
        csInput' Char = "Char.Parse"
        csInput' _ = error "Attempt to read value of unreadable type"
        csInputImport t = if t `elem` [Integer, Float, Double, Boolean, Char] 
          then addSystemImport else id

csOpenFileR :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr))
csOpenFileR n r = newObj r [n]

csOpenFileWorA :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))
csOpenFileWorA n w a = newObj w [n, a] 

csRef :: Doc -> Doc
csRef p = text "ref" <+> p

csOut :: Doc -> Doc
csOut p = text "out" <+> p

csInOutCall :: (Label -> VS (CSharpCode (Type CSharpCode)) -> 
  [VS (CSharpCode (Value CSharpCode))] -> VS (CSharpCode (Value CSharpCode)))
  -> Label -> [VS (CSharpCode (Value CSharpCode))] -> 
  [VS (CSharpCode (Variable CSharpCode))] -> 
  [VS (CSharpCode (Variable CSharpCode))] -> 
  MS (CSharpCode (Statement CSharpCode))
csInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out) 
  ins
csInOutCall f n ins [] [out] = assign out $ f n (onStateValue variableType out) 
  (valueOf out : ins)
csInOutCall f n ins outs both = valState $ f n void (map (onStateValue 
  (onCodeValue (updateValDoc csRef)) . valueOf) both ++ ins ++ map 
  (onStateValue (onCodeValue (updateValDoc csOut)) . valueOf) outs)

csVarDec :: Binding -> MS (CSharpCode (Statement CSharpCode)) -> 
  MS (CSharpCode (Statement CSharpCode))
csVarDec Static _ = error "Static variables can't be declared locally to a function in C#. Use stateVar to make a static state variable instead."
csVarDec Dynamic d = d

csObjVar :: (RenderSym repr) => repr (Variable repr) -> repr (Variable repr) -> 
  repr (Variable repr)
csObjVar o v = csObjVar' (variableBind v)
  where csObjVar' Static = error 
          "Cannot use objVar to access static variables through an object in C#"
        csObjVar' Dynamic = mkVar (variableName o ++ "." ++ variableName v) 
          (variableType v) (objVarDocD (variableDoc o) (variableDoc v))

csInOut :: (CSharpCode (Scope CSharpCode) -> CSharpCode (Permanence CSharpCode) 
    -> VS (CSharpCode (Type CSharpCode)) -> 
    [MS (CSharpCode (Parameter CSharpCode))] 
    -> MS (CSharpCode (Body CSharpCode)) -> MS (CSharpCode (Method CSharpCode)))
  -> CSharpCode (Scope CSharpCode) -> CSharpCode (Permanence CSharpCode) -> 
  [VS (CSharpCode (Variable CSharpCode))] -> 
  [VS (CSharpCode (Variable CSharpCode))] -> 
  [VS (CSharpCode (Variable CSharpCode))] -> 
  MS (CSharpCode (Body CSharpCode)) -> MS (CSharpCode (Method CSharpCode))
csInOut f s p ins [v] [] b = f s p (onStateValue variableType v) (map param ins)
  (on3StateValues (on3CodeValues surroundBody) (varDec v) b (returnState $ 
  valueOf v))
csInOut f s p ins [] [v] b = f s p (onStateValue variableType v) 
  (map param $ v : ins) (on2StateValues (on2CodeValues appendToBody) b 
  (returnState $ valueOf v))
csInOut f s p ins outs both b = f s p void (map (onStateValue (onCodeValue 
  (updateParamDoc csRef)) . param) both ++ map param ins ++ map (onStateValue 
  (onCodeValue (updateParamDoc csOut)) . param) outs) b
