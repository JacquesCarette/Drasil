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
  InternalFile(..), KeywordSym(..), PermanenceSym(..), InternalPerm(..), 
  BodySym(..), BlockSym(..), InternalBlock(..), ControlBlockSym(..), 
  TypeSym(..), InternalType(..), UnaryOpSym(..), BinaryOpSym(..), 
  InternalOp(..), VariableSym(..), InternalVariable(..), ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  InternalValue(..), Selector(..), InternalSelector(..), FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), InternalParam(..), MethodSym(..), 
  InternalMethod(..), StateVarSym(..), InternalStateVar(..), ClassSym(..), 
  InternalClass(..), ModuleSym(..), InternalMod(..), BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (classDocD, multiStateDocD, bodyDocD, 
  outDoc, printFileDocD, destructorError, paramDocD, methodDocD, listDecDocD, 
  listDecDefDocD, mkSt, breakDocD, continueDocD, mkStateVal, mkVal, mkVar, 
  classVarDocD, objVarDocD, newObjDocD, funcDocD, castDocD, listSetFuncDocD, 
  castObjDocD, staticDocD, dynamicDocD, bindingError, privateDocD, publicDocD, 
  dot, new, blockCmtStart, blockCmtEnd, docCmtStart, doubleSlash, elseIfLabel, 
  inLabel, blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, 
  commentedModD, appendToBody, surroundBody, filterOutObjs)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  oneLiner, block, bool, int, double, char, string, listType, listInnerType,
  obj, enumType, void, runStrategy, listSlice, notOp, negateOp, equalOp, 
  notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp, minusOp, 
  multOp, divideOp, moduloOp, andOp, orOp, var, staticVar, extVar, self, 
  enumVar, classVar, objVarSelf, listVar, listOf, iterVar, pi, litTrue, 
  litFalse, litChar, litFloat, litInt, litString, valueOf, arg, enumElement, 
  argsList, inlineIf, objAccess, objMethodCall, objMethodCallNoParams, 
  selfAccess, listIndexExists, indexOf, funcApp, selfFuncApp, extFuncApp, 
  newObj, notNull, func, get, set, listSize, listAdd, listAppend, iterBegin, 
  iterEnd, listAccess, listSet, getFunc, setFunc, listAddFunc, listAppendFunc, 
  iterBeginError, iterEndError, listAccessFunc, listSetFunc, printSt, state, 
  loopState, emptyState, assign, assignToListIndex, multiAssignError, 
  decrement, increment, decrement1, increment1, varDec, varDecDef, listDec, 
  listDecDef', objDecNew, objDecNewNoParams,constDecDef, discardInput,
  openFileR, openFileW, openFileA, closeFile, discardFileLine, stringListVals, 
  stringListLists, returnState, multiReturnError, valState, comment, freeError, 
  throw, initState, changeState, initObserverList, addObserver, ifCond, 
  ifNoElse, switch, switchAsIf, ifExists, for, forRange, forEach, while, 
  tryCatch, checkState, notifyObservers, construct, param, method, getMethod, 
  setMethod, privMethod, pubMethod, constructor, docMain, function, 
  mainFunction, docFunc, docInOutFunc, intFunc, stateVar, stateVarDef, constVar,
  privMVar, pubMVar, pubGVar, buildClass, enum, privClass, pubClass, docClass, 
  commentedClass, buildModule', modFromData, fileDoc, docMod, fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (unOpPrec, unExpr, unExpr', typeUnExpr, powerPrec, binExpr, binExpr', typeBinExpr)
import GOOL.Drasil.Data (Terminator(..), ScopeTag(..), FileType(..), 
  FileData(..), fileD, FuncData(..), fd, ModData(..), md, updateModDoc, 
  MethodData(..), mthd, updateMthdDoc, OpData(..), od, ParamData(..), pd, 
  updateParamDoc, ProgData(..), progD, TypeData(..), td, ValData(..), vd, 
  updateValDoc, Binding(..), VarData(..), vard)
import GOOL.Drasil.Helpers (toCode, toState, onCodeValue, onStateValue, 
  on2CodeValues, on2StateValues, on3CodeValues, on3StateValues, onCodeList, 
  onStateList, on1CodeValue1List)
import GOOL.Drasil.State (GS, MS, lensGStoFS, lensMStoGS, addLangImport, 
  setCurrMain)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (modify)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, comma, empty,
  semi, vcat, lbrace, rbrace, colon)

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
  fileDoc = G.fileDoc Combined csExt top bottom

  docMod = G.docMod

  commentedMod cmt m = on2StateValues (on2CodeValues commentedModD) m cmt

instance InternalFile CSharpCode where
  top _ = on2CodeValues cstop endStatement (include "")
  bottom = toCode empty

  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym CSharpCode where
  type Keyword CSharpCode = Doc
  endStatement = toCode semi
  endStatementLoop = toCode empty

  include _ = toCode $ text "using"
  inherit n = toCode $ colon <+> text n

  list _ = toCode $ text "List"

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

  keyDoc = unCSC

instance PermanenceSym CSharpCode where
  type Permanence CSharpCode = Doc
  static_ = toCode staticDocD
  dynamic_ = toCode dynamicDocD

instance InternalPerm CSharpCode where
  permDoc = unCSC
  binding = error $ bindingError csName

instance BodySym CSharpCode where
  type Body CSharpCode = Doc
  body = onStateList (onCodeList bodyDocD)
  bodyStatements = block
  oneLiner = G.oneLiner

  addComments s = onStateValue (on2CodeValues (addCommentsDocD s) commentStart)

  bodyDoc = unCSC

instance BlockSym CSharpCode where
  type Block CSharpCode = Doc
  block = G.block endStatement

instance InternalBlock CSharpCode where
  blockDoc = unCSC
  docBlock = onStateValue toCode

instance TypeSym CSharpCode where
  type Type CSharpCode = TypeData
  bool = G.bool
  int = G.int
  float = G.double
  char = G.char
  string = G.string
  infile = csInfileType
  outfile = csOutfileType
  listType = G.listType
  listInnerType = G.listInnerType
  obj = G.obj
  enumType = G.enumType
  iterator t = t
  void = G.void

  getType = cType . unCSC
  getTypeString = typeString . unCSC
  getTypeDoc = typeDoc . unCSC
  
instance InternalType CSharpCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym CSharpCode where
  runStrategy = G.runStrategy

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
  enumVar = G.enumVar
  classVar = G.classVar classVarDocD
  extClassVar = classVar
  objVar = on2StateValues csObjVar
  objVarSelf = G.objVarSelf
  listVar  = G.listVar
  listOf = G.listOf
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
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString

  pi = G.pi

  ($:) = enumElement

  valueOf = G.valueOf
  arg n = G.arg (litInt n) argsList
  enumElement = G.enumElement
  
  argsList = G.argsList "args"

  valueType = onCodeValue valType
  valueDoc = valDoc . unCSC

instance NumericExpression CSharpCode where
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
  selfFuncApp c = G.selfFuncApp (self c)
  extFuncApp = G.extFuncApp
  newObj = G.newObj newObjDocD
  extNewObj _ = newObj

  exists = notNull
  notNull = G.notNull

instance InternalValue CSharpCode where
  inputFunc = mkStateVal string (text "Console.ReadLine()")
  printFunc = mkStateVal void (text "Console.Write")
  printLnFunc = mkStateVal void (text "Console.WriteLine")
  printFileFunc = on2StateValues (\v -> mkVal v . printFileDocD "Write" . 
    valueDoc) void
  printFileLnFunc = on2StateValues (\v -> mkVal v . printFileDocD "WriteLine" . 
    valueDoc) void
  
  cast = csCast
  
  valuePrec = valPrec . unCSC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CSharpCode where
  objAccess = G.objAccess
  ($.) = objAccess

  selfAccess = G.selfAccess

  listIndexExists = G.listIndexExists
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf = G.indexOf "IndexOf"
  
instance InternalSelector CSharpCode where
  objMethodCall' = G.objMethodCall
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
    
  functionType = onCodeValue funcType
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

  varDec v = v >>= (\v' -> csVarDec (variableBind v') $ G.varDec static_ 
    dynamic_ v)
  varDecDef = G.varDecDef
  listDec n v = v >>= (\v' -> G.listDec (listDecDocD v') (litInt n) v)
  listDecDef = G.listDecDef' listDecDefDocD
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew _ = objDecNew
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams _ = objDecNewNoParams
  constDecDef = G.constDecDef

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
  stringSplit d vnew s = assign vnew $ newObj (listType dynamic_ string) 
    [s $. func "Split" (listType static_ string) [litChar d]]

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
  selfInOutCall c = csInOutCall (selfFuncApp c)
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

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypeData
  mType t = t
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
  constructor n = G.constructor n n
  destructor _ _ = error $ destructorError csName

  docMain = G.docMain
 
  function = G.function
  mainFunction = G.mainFunction string "Main"

  docFunc = G.docFunc

  inOutMethod n c = csInOut (method n c)

  docInOutMethod n c = G.docInOutFunc (inOutMethod n c)

  inOutFunc n = csInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CSharpCode where
  intMethod m n _ s p t ps b = modify (if m then setCurrMain else id) >> 
    on3StateValues (\tp pms bd -> methodFromData Pub $ methodDocD n s p tp pms 
    bd) (zoom lensMStoGS t) (sequence ps) (zoom lensMStoGS b)
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
  buildClass = G.buildClass classDocD inherit
  enum = G.enum
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass = G.commentedClass

instance InternalClass CSharpCode where
  classDoc = unCSC
  classFromData = onStateValue toCode

instance ModuleSym CSharpCode where
  type Module CSharpCode = ModData
  buildModule n _ = G.buildModule' n
  
instance InternalMod CSharpCode where
  moduleDoc = modDoc . unCSC
  modFromData n = G.modFromData n (\d m -> toCode $ md n m d)
  updateModuleDoc f = onCodeValue (updateModDoc f)

instance BlockCommentSym CSharpCode where
  type BlockComment CSharpCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unCSC

addSystemImport :: GS a -> GS a
addSystemImport = (>>) $ modify (addLangImport "System")

csName :: String
csName = "C#"

cstop :: Doc -> Doc -> Doc
cstop end inc = vcat [
  inc <+> text "System" <> end, -- Console
  inc <+> text "System.IO" <> end,
  inc <+> text "System.Collections.Generic" <> end]

csInfileType :: (RenderSym repr) => GS (repr (Type repr))
csInfileType = toState $ typeFromData File "StreamReader" (text "StreamReader")

csOutfileType :: (RenderSym repr) => GS (repr (Type repr))
csOutfileType = toState $ typeFromData File "StreamWriter" (text "StreamWriter")

csCast :: GS (CSharpCode (Type CSharpCode)) -> 
  GS (CSharpCode (Value CSharpCode)) -> GS (CSharpCode (Value CSharpCode))
csCast t v = join $ on2StateValues (\tp vl -> csCast' (getType tp) (getType $ 
  valueType vl) tp vl) t v
  where csCast' Float String _ _ = funcApp "Double.Parse" float [v]
        csCast' _ _ tp vl = mkStateVal t (castObjDocD (castDocD (getTypeDoc 
          tp)) (valueDoc vl))

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

csFileInput :: (RenderSym repr) => GS (repr (Value repr)) -> 
  GS (repr (Value repr))
csFileInput = onStateValue (\f -> mkVal (valueType f) (valueDoc f <> dot <> 
  text "ReadLine()"))

csInput :: (RenderSym repr) => GS (repr (Type repr)) -> GS (repr (Value repr)) 
  -> GS (repr (Value repr))
csInput = on2StateValues (\t inFn -> mkVal t $ text (csInput' (getType t)) <> 
  parens (valueDoc inFn))
  where csInput' Integer = "Int32.Parse"
        csInput' Float = "Double.Parse"
        csInput' Boolean = "Boolean.Parse"
        csInput' String = ""
        csInput' Char = "Char.Parse"
        csInput' _ = error "Attempt to read value of unreadable type"

csOpenFileR :: (RenderSym repr) => GS (repr (Value repr)) -> 
  GS (repr (Type repr)) -> GS (repr (Value repr))
csOpenFileR = on2StateValues (\n r -> mkVal r $ new <+> getTypeDoc r <> parens 
  (valueDoc n))

csOpenFileWorA :: (RenderSym repr) => GS (repr (Value repr)) -> 
  GS (repr (Type repr)) -> GS (repr (Value repr)) -> GS (repr (Value repr))
csOpenFileWorA = on3StateValues (\n w a -> mkVal w $ new <+> getTypeDoc w <> 
  parens (valueDoc n <> comma <+> valueDoc a))

csRef :: Doc -> Doc
csRef p = text "ref" <+> p

csOut :: Doc -> Doc
csOut p = text "out" <+> p

csInOutCall :: (Label -> GS (CSharpCode (Type CSharpCode)) -> 
  [GS (CSharpCode (Value CSharpCode))] -> GS (CSharpCode (Value CSharpCode)))
  -> Label -> [GS (CSharpCode (Value CSharpCode))] -> 
  [GS (CSharpCode (Variable CSharpCode))] -> 
  [GS (CSharpCode (Variable CSharpCode))] -> 
  GS (CSharpCode (Statement CSharpCode))
csInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out) 
  ins
csInOutCall f n ins [] [out] = if null (filterOutObjs [out])
  then valState $ f n void (valueOf out : ins) 
  else assign out $ f n (onStateValue variableType out) (valueOf out : ins)
csInOutCall f n ins outs both = valState $ f n void (map (onStateValue 
  (onCodeValue (updateValDoc csRef)) . valueOf) both ++ ins ++ map 
  (onStateValue (onCodeValue (updateValDoc csOut)) . valueOf) outs)

csVarDec :: Binding -> GS (CSharpCode (Statement CSharpCode)) -> 
  GS (CSharpCode (Statement CSharpCode))
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
    -> GS (CSharpCode (Type CSharpCode)) -> 
    [MS (CSharpCode (Parameter CSharpCode))] 
    -> GS (CSharpCode (Body CSharpCode)) -> MS (CSharpCode (Method CSharpCode)))
  -> CSharpCode (Scope CSharpCode) -> CSharpCode (Permanence CSharpCode) -> 
  [GS (CSharpCode (Variable CSharpCode))] -> 
  [GS (CSharpCode (Variable CSharpCode))] -> 
  [GS (CSharpCode (Variable CSharpCode))] -> 
  GS (CSharpCode (Body CSharpCode)) -> MS (CSharpCode (Method CSharpCode))
csInOut f s p ins [v] [] b = f s p (onStateValue variableType v) (map param ins)
  (on3StateValues (on3CodeValues surroundBody) (varDec v) b (returnState $ 
  valueOf v))
csInOut f s p ins [] [v] b = f s p (if null (filterOutObjs [v]) then void 
  else onStateValue variableType v) (map param $ v : ins) (if null 
  (filterOutObjs [v]) then b else on2StateValues (on2CodeValues appendToBody) b 
  (returnState $ valueOf v))
csInOut f s p ins outs both b = f s p void (map (onStateValue (onCodeValue 
  (updateParamDoc csRef)) . param) both ++ map param ins ++ map (onStateValue 
  (onCodeValue (updateParamDoc csOut)) . param) outs) b
