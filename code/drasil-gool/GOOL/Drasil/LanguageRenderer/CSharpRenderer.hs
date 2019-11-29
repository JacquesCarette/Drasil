{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# code is contained in this module
module GOOL.Drasil.LanguageRenderer.CSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..)
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym(..), 
  InternalFile(..), KeywordSym(..), PermanenceSym(..), InternalPerm(..), 
  BodySym(..), BlockSym(..), InternalBlock(..), ControlBlockSym(..), 
  TypeSym(..), InternalType(..), UnaryOpSym(..), BinaryOpSym(..), InternalOp(..),
  VariableSym(..), InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), MethodTypeSym(..), ParameterSym(..), 
  MethodSym(..), InternalMethod(..), StateVarSym(..), InternalStateVar(..), 
  ClassSym(..), InternalClass(..), ModuleSym(..), InternalMod(..), 
  BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (classDocD, multiStateDocD, bodyDocD, 
  oneLinerD, outDoc, printFileDocD, boolTypeDocD, intTypeDocD, charTypeDocD, 
  stringTypeDocD, typeDocD, enumTypeDocD, listTypeDocD, listInnerTypeD, 
  voidDocD, destructorError, paramDocD, paramListDocD, mkParam, methodDocD, 
  runStrategyD, listSliceD, checkStateD, notifyObserversD, listDecDocD, 
  listDecDefDocD, mkSt, stringListVals', stringListLists', printStD, stateD, 
  loopStateD, emptyStateD, assignD, assignToListIndexD, multiAssignError, 
  decrementD, incrementD, decrement1D, increment1D, constDecDefD, discardInputD,
  openFileRD, openFileWD, openFileAD, closeFileD, discardFileLineD, breakDocD, 
  continueDocD, returnD, multiReturnError, valStateD, freeError, throwD, 
  initStateD, changeStateD, initObserverListD, addObserverD, ifNoElseD, switchD,
  switchAsIfD, ifExistsD, forRangeD, tryCatchD, unOpPrec, notOpDocD, 
  negateOpDocD, unExpr, unExpr', typeUnExpr, powerPrec, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', typeBinExpr, mkVal, 
  mkVar, litTrueD, litFalseD, litCharD, litFloatD, litIntD, litStringD, 
  classVarDocD, objVarDocD, inlineIfD, newObjDocD, varD, staticVarD, 
  extVarD, selfD, enumVarD, classVarD, objVarSelfD, listVarD, listOfD, iterVarD,
  valueOfD, argD, enumElementD, argsListD, objAccessD, objMethodCallD, 
  objMethodCallNoParamsD, selfAccessD, listIndexExistsD, indexOfD, funcAppD, 
  selfFuncAppD, extFuncAppD, newObjD,notNullD, funcDocD, castDocD, 
  listSetFuncDocD, castObjDocD, funcD, getD, setD, listSizeD, listAddD, 
  listAppendD, iterBeginD, iterEndD, listAccessD, listSetD, getFuncD, setFuncD, 
  listAddFuncD, listAppendFuncD, iterBeginError, iterEndError, listAccessFuncD, 
  listSetFuncD, staticDocD, dynamicDocD, bindingError, privateDocD, publicDocD, 
  dot, new, blockCmtStart, blockCmtEnd, docCmtStart, doubleSlash, elseIfLabel, 
  inLabel, blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, 
  commentedModD, appendToBody, surroundBody, filterOutObjs)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  fileFromData, block, pi, varDec, varDecDef, listDec, listDecDef, objDecNew, 
  objDecNewNoParams, construct, comment, ifCond, for, forEach, while, method, 
  getMethod, setMethod,privMethod, pubMethod, constructor, docMain, function, 
  mainFunction, docFunc, docInOutFunc, intFunc, stateVar, stateVarDef, constVar,
  privMVar, pubMVar, pubGVar, buildClass, enum, privClass, pubClass, docClass, 
  commentedClass, buildModule', modFromData, fileDoc, docMod)
import GOOL.Drasil.Data (Terminator(..), FileType(..), FileData(..), fileD,
  FuncData(..), fd, ModData(..), md, updateModDoc, MethodData(..), mthd, 
  updateMthdDoc, OpData(..), ParamData(..), updateParamDoc, ProgData(..), progD,
  TypeData(..), td, ValData(..), vd, updateValDoc, Binding(..), VarData(..), 
  vard)
import GOOL.Drasil.Helpers (toCode, toState, onCodeValue, onStateValue, 
  on2CodeValues, on2StateValues, on3CodeValues, on5CodeValues, 
  onCodeList, onStateList, on1CodeValue1List, checkParams)
import GOOL.Drasil.State (MS, lensGStoFS, initialState, initialFS, getPutReturn,
  setCurrMain, setParameters)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad.State (evalState)
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

instance RenderSym CSharpCode where
  type RenderFile CSharpCode = FileData
  fileDoc code = G.fileDoc Combined csExt (top $ evalState code (initialState,
    initialFS)) bottom code

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
  body = onCodeList bodyDocD
  bodyStatements = block
  oneLiner = oneLinerD

  addComments s = on2CodeValues (addCommentsDocD s) commentStart

  bodyDoc = unCSC

instance BlockSym CSharpCode where
  type Block CSharpCode = Doc
  block = G.block endStatement

instance InternalBlock CSharpCode where
  blockDoc = unCSC
  docBlock = toCode

instance TypeSym CSharpCode where
  type Type CSharpCode = TypeData
  bool = toCode boolTypeDocD
  int = toCode intTypeDocD
  float = toCode csFloatTypeDoc
  char = toCode charTypeDocD
  string = toCode stringTypeDocD
  infile = toCode csInfileTypeDoc
  outfile = toCode csOutfileTypeDoc
  listType p st = on2CodeValues listTypeDocD st (list p)
  listInnerType = listInnerTypeD
  obj t = toCode $ typeDocD t
  enumType t = toCode $ enumTypeDocD t
  iterator t = t
  void = toCode voidDocD

  getType = cType . unCSC
  getTypeString = typeString . unCSC
  getTypeDoc = typeDoc . unCSC
  
instance InternalType CSharpCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym CSharpCode where
  runStrategy = runStrategyD

  listSlice = listSliceD

instance UnaryOpSym CSharpCode where
  type UnaryOp CSharpCode = OpData
  notOp = toCode notOpDocD
  negateOp = toCode negateOpDocD
  sqrtOp = toCode $ unOpPrec "Math.Sqrt"
  absOp = toCode $ unOpPrec "Math.Abs"
  logOp = toCode $ unOpPrec "Math.Log10"
  lnOp = toCode $ unOpPrec "Math.Log"
  expOp = toCode $ unOpPrec "Math.Exp"
  sinOp = toCode $ unOpPrec "Math.Sin"
  cosOp = toCode $ unOpPrec "Math.Cos"
  tanOp = toCode $ unOpPrec "Math.Tan"
  asinOp = toCode $ unOpPrec "Math.Asin"
  acosOp = toCode $ unOpPrec "Math.Acos"
  atanOp = toCode $ unOpPrec "Math.Atan"
  floorOp = toCode $ unOpPrec "Math.Floor"
  ceilOp = toCode $ unOpPrec "Math.Ceiling"

instance BinaryOpSym CSharpCode where
  type BinaryOp CSharpCode = OpData
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
  powerOp = toCode $ powerPrec "Math.Pow"
  moduloOp = toCode moduloOpDocD
  andOp = toCode andOpDocD
  orOp = toCode orOpDocD

instance InternalOp CSharpCode where
  uOpDoc = opDoc . unCSC
  bOpDoc = opDoc . unCSC
  uOpPrec = opPrec . unCSC
  bOpPrec = opPrec . unCSC

instance VariableSym CSharpCode where
  type Variable CSharpCode = VarData
  var = varD
  staticVar = staticVarD
  const = var
  extVar = extVarD
  self = selfD
  enumVar = enumVarD
  classVar = classVarD classVarDocD
  extClassVar = classVar
  objVar = csObjVar
  objVarSelf = objVarSelfD
  listVar  = listVarD
  listOf = listOfD 
  iterVar = iterVarD

  ($->) = objVar

  variableBind = varBind . unCSC
  variableName = varName . unCSC
  variableType = onCodeValue varType
  variableDoc = varDoc . unCSC

instance InternalVariable CSharpCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CSharpCode where
  type Value CSharpCode = ValData
  litTrue = litTrueD
  litFalse = litFalseD
  litChar = litCharD
  litFloat = litFloatD
  litInt = litIntD
  litString = litStringD

  pi = G.pi

  ($:) = enumElement

  valueOf = valueOfD
  arg n = argD (litInt n) argsList
  enumElement = enumElementD
  
  argsList = argsListD "args"

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
  inlineIf = on3CodeValues inlineIfD
  funcApp = funcAppD
  selfFuncApp c = selfFuncAppD (self c)
  extFuncApp = extFuncAppD
  newObj = newObjD newObjDocD
  extNewObj _ = newObj

  exists = notNull
  notNull = notNullD

instance InternalValue CSharpCode where
  inputFunc = mkVal string (text "Console.ReadLine()")
  printFunc = mkVal void (text "Console.Write")
  printLnFunc = mkVal void (text "Console.WriteLine")
  printFileFunc f = mkVal void (printFileDocD "Write" (valueDoc f))
  printFileLnFunc f = mkVal void (printFileDocD "WriteLine" (valueDoc f))
  
  cast = csCast
  
  valuePrec = valPrec . unCSC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CSharpCode where
  objAccess = objAccessD
  ($.) = objAccess

  objMethodCall = objMethodCallD
  objMethodCallNoParams = objMethodCallNoParamsD

  selfAccess = selfAccessD

  listIndexExists = listIndexExistsD
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf = indexOfD "IndexOf"

instance FunctionSym CSharpCode where
  type Function CSharpCode = FuncData
  func = funcD

  get = getD
  set = setD

  listSize = listSizeD
  listAdd = listAddD
  listAppend = listAppendD

  iterBegin = iterBeginD
  iterEnd = iterEndD

instance SelectorFunction CSharpCode where
  listAccess = listAccessD
  listSet = listSetD
  at = listAccess

instance InternalFunction CSharpCode where
  getFunc = getFuncD
  setFunc = setFuncD

  listSizeFunc = on2CodeValues fd int (toCode $ funcDocD (text "Count"))
  listAddFunc _ = listAddFuncD "Insert"
  listAppendFunc = listAppendFuncD "Add"

  iterBeginFunc _ = error $ iterBeginError csName
  iterEndFunc _ = error $ iterEndError csName

  listAccessFunc = listAccessFuncD
  listSetFunc = listSetFuncD listSetFuncDocD 
    
  functionType = onCodeValue funcType
  functionDoc = funcDoc . unCSC

  funcFromData t d = on2CodeValues fd t (toCode d)

instance InternalStatement CSharpCode where
  printSt _ p v _ = printStD p v

  state = stateD
  loopState = loopStateD

  emptyState = emptyStateD
  statementDoc = fst . unCSC
  statementTerm = snd . unCSC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CSharpCode where
  type Statement CSharpCode = (Doc, Terminator)
  assign = assignD Semi
  assignToListIndex = assignToListIndexD
  multiAssign _ _ = error $ multiAssignError csName
  (&=) = assign
  (&-=) = decrementD
  (&+=) = incrementD
  (&++) = increment1D
  (&~-) = decrement1D

  varDec v = csVarDec (variableBind v) $ G.varDec static_ dynamic_ v
  varDecDef = G.varDecDef
  listDec n v = G.listDec (listDecDocD v) (litInt n) v
  listDecDef v = G.listDecDef (listDecDefDocD v) v
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew _ = objDecNew
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams _ = objDecNewNoParams
  constDecDef = constDecDefD

  print v = outDoc False printFunc v Nothing
  printLn v = outDoc True printLnFunc v Nothing
  printStr s = outDoc False printFunc (litString s) Nothing
  printStrLn s = outDoc True printLnFunc (litString s) Nothing

  printFile f v = outDoc False (printFileFunc f) v (Just f)
  printFileLn f v = outDoc True (printFileLnFunc f) v (Just f)
  printFileStr f s = outDoc False (printFileFunc f) (litString s) (Just f)
  printFileStrLn f s = outDoc True (printFileLnFunc f) (litString s) (Just f)

  getInput v = v &= csInput (variableType v) inputFunc
  discardInput = discardInputD csDiscardInput
  getFileInput f v = v &= csInput (variableType v) (csFileInput f)
  discardFileInput f = valState $ csFileInput f

  openFileR = openFileRD csOpenFileR
  openFileW = openFileWD csOpenFileWorA
  openFileA = openFileAD csOpenFileWorA
  closeFile = closeFileD "Close"

  getFileInputLine = getFileInput
  discardFileLine = discardFileLineD "ReadLine"
  stringSplit d vnew s = assign vnew $ newObj (listType dynamic_ string) 
    [s $. func "Split" (listType static_ string) [litChar d]]

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = mkSt breakDocD
  continue = mkSt continueDocD

  returnState = returnD Semi
  multiReturn _ = error $ multiReturnError csName 

  valState = valStateD Semi

  comment = G.comment commentStart

  free _ = error $ freeError csName -- could set variable to null? Might be misleading.

  throw = throwD csThrowDoc Semi

  initState = initStateD
  changeState = changeStateD

  initObserverList = initObserverListD
  addObserver = addObserverD

  inOutCall = csInOutCall funcApp
  selfInOutCall c = csInOutCall (selfFuncApp c)
  extInOutCall m = csInOutCall (extFuncApp m)

  multi = on1CodeValue1List multiStateDocD endStatement

instance ControlStatementSym CSharpCode where
  ifCond = G.ifCond ifBodyStart elseIf blockEnd
  ifNoElse = ifNoElseD
  switch = switchD
  switchAsIf = switchAsIfD

  ifExists = ifExistsD

  for = G.for blockStart blockEnd
  forRange = forRangeD
  forEach = G.forEach blockStart blockEnd iterForEachLabel iterInLabel 
  while = G.while blockStart blockEnd

  tryCatch = tryCatchD csTryCatch

  checkState = checkStateD
  notifyObservers = notifyObserversD

  getFileInputAll f v = while ((f $. on2CodeValues fd bool (toCode $ text 
    ".EndOfStream")) ?!) (oneLiner $ valState $ listAppend (valueOf v) 
    (csFileInput f))

instance ScopeSym CSharpCode where
  type Scope CSharpCode = Doc
  private = toCode privateDocD
  public = toCode publicDocD

instance InternalScope CSharpCode where
  scopeDoc = unCSC

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypeData
  mType t = t
  construct = toCode . G.construct

instance ParameterSym CSharpCode where
  type Parameter CSharpCode = ParamData
  param = onCodeValue (mkParam paramDocD)
  pointerParam = param

  parameterType = variableType . onCodeValue paramVar

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
  intMethod m n _ s p t ps b = getPutReturn (setParameters (map unCSC ps) . 
    if m then setCurrMain else id) $ onCodeValue mthd (on5CodeValues 
    (methodDocD n) s p t (onCodeList (paramListDocD . checkParams n) ps) b)
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
  stateVarFromData = toState . toCode

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
  updateModuleDoc f = onStateValue (onCodeValue (updateModDoc f))

instance BlockCommentSym CSharpCode where
  type BlockComment CSharpCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unCSC

csName :: String
csName = "C#"

cstop :: Doc -> Doc -> Doc
cstop end inc = vcat [
  inc <+> text "System" <> end,
  inc <+> text "System.IO" <> end,
  inc <+> text "System.Collections" <> end,
  inc <+> text "System.Collections.Generic" <> end]

csFloatTypeDoc :: TypeData
csFloatTypeDoc = td Float "double" (text "double") -- Same as Java, maybe make a common function

csInfileTypeDoc :: TypeData
csInfileTypeDoc = td File "StreamReader" (text "StreamReader")

csOutfileTypeDoc :: TypeData
csOutfileTypeDoc = td File "StreamWriter" (text "StreamWriter")

csCast :: CSharpCode (Type CSharpCode) -> CSharpCode (Value CSharpCode) -> 
  CSharpCode (Value CSharpCode)
csCast t v = csCast' (getType t) (getType $ valueType v)
  where csCast' Float String = funcApp "Double.Parse" float [v]
        csCast' _ _ = mkVal t $ castObjDocD (castDocD (getTypeDoc t)) 
          (valueDoc v)

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

csFileInput :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
csFileInput f = mkVal (valueType f) (valueDoc f <> dot <> text "ReadLine()")

csInput :: (RenderSym repr) => repr (Type repr) -> repr (Value repr) -> 
  repr (Value repr)
csInput t inFn = mkVal t $ text (csInput' (getType t)) <> 
  parens (valueDoc inFn)
  where csInput' Integer = "Int32.Parse"
        csInput' Float = "Double.Parse"
        csInput' Boolean = "Boolean.Parse"
        csInput' String = ""
        csInput' Char = "Char.Parse"
        csInput' _ = error "Attempt to read value of unreadable type"

csOpenFileR :: (RenderSym repr) => repr (Value repr) -> repr (Type repr) -> 
  repr (Value repr)
csOpenFileR n r = valFromData Nothing r $ new <+> getTypeDoc r <> 
  parens (valueDoc n)

csOpenFileWorA :: (RenderSym repr) => repr (Value repr) -> repr (Type repr) -> 
  repr (Value repr) -> repr (Value repr)
csOpenFileWorA n w a = valFromData Nothing w $ new <+> getTypeDoc w <> 
  parens (valueDoc n <> comma <+> valueDoc a)

csRef :: Doc -> Doc
csRef p = text "ref" <+> p

csOut :: Doc -> Doc
csOut p = text "out" <+> p

csInOutCall :: (Label -> CSharpCode (Type CSharpCode) -> 
  [CSharpCode (Value CSharpCode)] -> CSharpCode (Value CSharpCode)) -> Label -> 
  [CSharpCode (Value CSharpCode)] -> [CSharpCode (Variable CSharpCode)] -> 
  [CSharpCode (Variable CSharpCode)] -> CSharpCode (Statement CSharpCode)
csInOutCall f n ins [out] [] = assign out $ f n (variableType out) ins
csInOutCall f n ins [] [out] = if null (filterOutObjs [out])
  then valState $ f n void (valueOf out : ins) 
  else assign out $ f n (variableType out) (valueOf out : ins)
csInOutCall f n ins outs both = valState $ f n void (map (onCodeValue 
  (updateValDoc csRef) . valueOf) both ++ ins ++ map (onCodeValue (updateValDoc 
  csOut) . valueOf) outs)

csVarDec :: Binding -> CSharpCode (Statement CSharpCode) -> 
  CSharpCode (Statement CSharpCode)
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
    -> CSharpCode (Type CSharpCode) -> [CSharpCode (Parameter CSharpCode)] -> 
    CSharpCode (Body CSharpCode) -> 
    MS (CSharpCode (Method CSharpCode)))
  -> CSharpCode (Scope CSharpCode) -> CSharpCode (Permanence CSharpCode) -> 
  [CSharpCode (Variable CSharpCode)] -> [CSharpCode (Variable CSharpCode)] -> 
  [CSharpCode (Variable CSharpCode)] -> CSharpCode (Body CSharpCode) -> 
  MS (CSharpCode (Method CSharpCode))
csInOut f s p ins [v] [] b = f s p (variableType v) (map param ins)
  (on3CodeValues surroundBody (varDec v) b (returnState $ valueOf v))
csInOut f s p ins [] [v] b = f s p (if null (filterOutObjs [v]) then void 
  else variableType v) (map param $ v : ins) (if null (filterOutObjs [v]) then 
  b else on2CodeValues appendToBody b (returnState $ valueOf v))
csInOut f s p ins outs both b = f s p void (map (onCodeValue (updateParamDoc 
  csRef) . param) both ++ map param ins ++ map (onCodeValue (updateParamDoc 
  csOut) . param) outs) b
