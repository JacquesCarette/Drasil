{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java code is contained in this module
module GOOL.Drasil.LanguageRenderer.JavaRenderer (
  -- * Java Code Configuration -- defines syntax of all Java code
  JavaCode(..)
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym(..), 
  InternalFile(..), KeywordSym(..), PermanenceSym(..), InternalPerm(..), 
  BodySym(..), BlockSym(..), InternalBlock(..), ControlBlockSym(..), 
  TypeSym(..), InternalType(..), UnaryOpSym(..), BinaryOpSym(..), 
  VariableSym(..), InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), MethodTypeSym(..), ParameterSym(..), 
  MethodSym(..), InternalMethod(..), StateVarSym(..), InternalStateVar(..), 
  ClassSym(..), InternalClass(..), ModuleSym(..), InternalMod(..), 
  BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (packageDocD, classDocD, multiStateDocD, 
  bodyDocD, oneLinerD, outDoc, printFileDocD, boolTypeDocD, intTypeDocD, 
  charTypeDocD, typeDocD, enumTypeDocD, listTypeDocD, listInnerTypeD, voidDocD, 
  destructorError, paramDocD, paramListDocD, mkParam, runStrategyD, listSliceD, 
  checkStateD, notifyObserversD, listDecDocD, mkSt, stringListVals', 
  stringListLists', printStD, stateD, loopStateD, emptyStateD, assignD, 
  assignToListIndexD, multiAssignError, decrementD, incrementD, decrement1D, 
  increment1D, discardInputD, discardFileInputD, openFileRD, openFileWD, 
  openFileAD, closeFileD, discardFileLineD, breakD, continueD, returnD, 
  multiReturnError, valStateD, freeError, throwD, initStateD, changeStateD, 
  initObserverListD, addObserverD, ifNoElseD, switchD, switchAsIfD, ifExistsD, 
  forRangeD, tryCatchD, unOpPrec, notOpDocD, negateOpDocD, unExpr, unExpr', 
  typeUnExpr, powerPrec, equalOpDocD, notEqualOpDocD, greaterOpDocD, 
  greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, 
  multOpDocD, divideOpDocD, moduloOpDocD, andOpDocD, orOpDocD, binExpr, 
  binExpr', typeBinExpr, mkVal, litTrueD, litFalseD, litCharD, litFloatD, 
  litIntD, litStringD, classVarDocD, inlineIfD, newObjDocD, varD, 
  staticVarD, extVarD, selfD, enumVarD, classVarD, objVarD, objVarSelfD, 
  listVarD, listOfD, iterVarD, valueOfD, argD, enumElementD, argsListD, 
  objAccessD, objMethodCallD, objMethodCallNoParamsD, selfAccessD, 
  listIndexExistsD, indexOfD, funcAppD, selfFuncAppD, extFuncAppD, newObjD, 
  notNullD, castDocD, castObjDocD, funcD, getD, setD, listSizeD, listAddD, 
  listAppendD, iterBeginD, iterEndD, listAccessD, listSetD, getFuncD, setFuncD, 
  listSizeFuncD, listAddFuncD, listAppendFuncD, iterBeginError, iterEndError, 
  listAccessFuncD', staticDocD, dynamicDocD, bindingError, privateDocD, 
  publicDocD, dot, new, elseIfLabel, forLabel, blockCmtStart, blockCmtEnd, 
  docCmtStart, doubleSlash, blockCmtDoc, docCmtDoc, commentedItem, 
  addCommentsDocD, commentedModD, docFuncRepr, valueList, appendToBody, 
  surroundBody, intValue, filterOutObjs)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  fileFromData, block, pi, varDec, varDecDef, listDec, listDecDef, objDecNew, 
  objDecNewNoParams, construct, comment, ifCond, for, forEach, while, method, 
  getMethod, setMethod,privMethod, pubMethod, constructor, docMain, function, 
  mainFunction, docFunc, intFunc, stateVar, stateVarDef, constVar, privMVar, 
  pubMVar, pubGVar, buildClass, enum, privClass, pubClass, docClass, 
  commentedClass, buildModule', modFromData, fileDoc, docMod)
import GOOL.Drasil.Data (Terminator(..), FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateModDoc, MethodData(..), mthd, 
  updateMthdDoc, OpData(..), ParamData(..), ProgData(..), progD, TypeData(..), 
  td, ValData(..), vd, VarData(..), vard)
import GOOL.Drasil.Helpers (angles, emptyIfNull, toCode, toState, onCodeValue, onStateValue, on2CodeValues, on2StateValues, on3CodeValues,
  liftA4, liftA5, liftList, lift1List, checkParams)
import GOOL.Drasil.State (MS, lensMStoGS, initialState, putAfter, getPutReturn, 
  getPutReturnList, addProgNameToPaths, setMain, setCurrMain, setParameters)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Control.Lens (over)
import Control.Applicative (Applicative)
import Control.Monad.State (evalState)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, space, 
  equals, semi, vcat, lbrace, rbrace, render, colon, comma, render)

jExt :: String
jExt = "java"

newtype JavaCode a = JC {unJC :: a}

instance Functor JavaCode where
  fmap f (JC x) = JC (f x)

instance Applicative JavaCode where
  pure = JC
  (JC f) <*> (JC x) = JC (f x)

instance Monad JavaCode where
  return = JC
  JC x >>= f = f x

instance ProgramSym JavaCode where
  type Program JavaCode = ProgData
  prog n fs = getPutReturnList (map (putAfter $ setCurrMain False) fs) 
    (addProgNameToPaths n) (lift1List (\end -> progD n . 
    map (packageDocD n end)) endStatement)

instance RenderSym JavaCode where
  type RenderFile JavaCode = FileData 
  fileDoc code = G.fileDoc Combined jExt (top $ evalState code initialState) 
    bottom code

  docMod = G.docMod

  commentedMod cmt m = on2StateValues (on2CodeValues commentedModD) m cmt

instance InternalFile JavaCode where
  top _ = on3CodeValues jtop endStatement (include "") (list static_)
  bottom = toCode empty
  
  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym JavaCode where
  type Keyword JavaCode = Doc
  endStatement = toCode semi
  endStatementLoop = toCode empty

  include _ = toCode $ text "import"
  inherit n = toCode $ text "extends" <+> text n

  list _ = toCode $ text "ArrayList"

  blockStart = toCode lbrace
  blockEnd = toCode rbrace

  ifBodyStart = blockStart
  elseIf = toCode elseIfLabel
  
  iterForEachLabel = toCode forLabel
  iterInLabel = toCode colon

  commentStart = toCode doubleSlash
  blockCommentStart = toCode blockCmtStart
  blockCommentEnd = toCode blockCmtEnd
  docCommentStart = toCode docCmtStart
  docCommentEnd = blockCommentEnd

  keyDoc = unJC

instance PermanenceSym JavaCode where
  type Permanence JavaCode = Doc
  static_ = toCode staticDocD
  dynamic_ = toCode dynamicDocD

instance InternalPerm JavaCode where
  permDoc = unJC
  binding = error $ bindingError jName

instance BodySym JavaCode where
  type Body JavaCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner = oneLinerD

  addComments s = on2CodeValues (addCommentsDocD s) commentStart

  bodyDoc = unJC

instance BlockSym JavaCode where
  type Block JavaCode = Doc
  block = G.block endStatement

instance InternalBlock JavaCode where
  blockDoc = unJC
  docBlock = toCode

instance TypeSym JavaCode where
  type Type JavaCode = TypeData
  bool = toCode boolTypeDocD
  int = toCode intTypeDocD
  float = toCode jFloatTypeDocD
  char = toCode charTypeDocD
  string = toCode jStringTypeDoc
  infile = toCode jInfileTypeDoc
  outfile = toCode jOutfileTypeDoc
  listType p st = on2CodeValues jListType st (list p)
  listInnerType = listInnerTypeD
  obj t = toCode $ typeDocD t
  enumType t = toCode $ enumTypeDocD t
  iterator t = t
  void = toCode voidDocD

  getType = cType . unJC
  getTypeString = typeString . unJC
  getTypeDoc = typeDoc . unJC
  
instance InternalType JavaCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym JavaCode where
  runStrategy = runStrategyD

  listSlice = listSliceD

instance UnaryOpSym JavaCode where
  type UnaryOp JavaCode = OpData
  notOp = toCode notOpDocD
  negateOp = toCode negateOpDocD
  sqrtOp = toCode $ unOpPrec "Math.sqrt"
  absOp = toCode $ unOpPrec "Math.abs"
  logOp = toCode $ unOpPrec "Math.log10"
  lnOp = toCode $ unOpPrec "Math.log"
  expOp = toCode $ unOpPrec "Math.exp"
  sinOp = toCode $ unOpPrec "Math.sin"
  cosOp = toCode $ unOpPrec "Math.cos"
  tanOp = toCode $ unOpPrec "Math.tan"
  asinOp = toCode $ unOpPrec "Math.asin"
  acosOp = toCode $ unOpPrec "Math.acos"
  atanOp = toCode $ unOpPrec "Math.atan"
  floorOp = toCode $ unOpPrec "Math.floor"
  ceilOp = toCode $ unOpPrec "Math.ceil"

instance BinaryOpSym JavaCode where
  type BinaryOp JavaCode = OpData
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
  powerOp = toCode $ powerPrec "Math.pow"
  moduloOp = toCode moduloOpDocD
  andOp = toCode andOpDocD
  orOp = toCode orOpDocD

instance VariableSym JavaCode where
  type Variable JavaCode = VarData
  var = varD
  staticVar = staticVarD
  const = var
  extVar = extVarD
  self = selfD
  enumVar = enumVarD
  classVar = classVarD classVarDocD
  extClassVar = classVar
  objVar = objVarD
  objVarSelf = objVarSelfD
  listVar = listVarD
  listOf = listOfD
  iterVar = iterVarD

  ($->) = objVar

  variableBind = varBind . unJC
  variableName = varName . unJC
  variableType = onCodeValue varType
  variableDoc = varDoc . unJC
  
instance InternalVariable JavaCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym JavaCode where
  type Value JavaCode = ValData
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
  valueDoc = valDoc . unJC

instance NumericExpression JavaCode where
  (#~) = on2CodeValues unExpr' negateOp
  (#/^) = on2CodeValues unExpr sqrtOp
  (#|) = on2CodeValues unExpr absOp
  (#+) = on3CodeValues binExpr plusOp
  (#-) = on3CodeValues binExpr minusOp
  (#*) = on3CodeValues binExpr multOp
  (#/) = on3CodeValues binExpr divideOp
  (#%) = on3CodeValues binExpr moduloOp
  (#^) = on3CodeValues binExpr' powerOp

  log = on2CodeValues unExpr logOp
  ln = on2CodeValues unExpr lnOp
  exp = on2CodeValues unExpr expOp
  sin = on2CodeValues unExpr sinOp
  cos = on2CodeValues unExpr cosOp
  tan = on2CodeValues unExpr tanOp
  csc v = litFloat 1.0 #/ sin v
  sec v = litFloat 1.0 #/ cos v
  cot v = litFloat 1.0 #/ tan v
  arcsin = on2CodeValues unExpr asinOp
  arccos = on2CodeValues unExpr acosOp
  arctan = on2CodeValues unExpr atanOp
  floor = on2CodeValues unExpr floorOp
  ceil = on2CodeValues unExpr ceilOp

instance BooleanExpression JavaCode where
  (?!) = on3CodeValues typeUnExpr notOp bool
  (?&&) = liftA4 typeBinExpr andOp bool
  (?||) = liftA4 typeBinExpr orOp bool

  (?<) = liftA4 typeBinExpr lessOp bool
  (?<=) = liftA4 typeBinExpr lessEqualOp bool
  (?>) = liftA4 typeBinExpr greaterOp bool
  (?>=) = liftA4 typeBinExpr greaterEqualOp bool
  (?==) = jEquality
  (?!=) = liftA4 typeBinExpr notEqualOp bool
  
instance ValueExpression JavaCode where
  inlineIf = on3CodeValues inlineIfD
  funcApp = funcAppD
  selfFuncApp c = selfFuncAppD (self c)
  extFuncApp = extFuncAppD
  newObj = newObjD newObjDocD
  extNewObj _ = newObj

  exists = notNull
  notNull = notNullD

instance InternalValue JavaCode where
  inputFunc = on2CodeValues mkVal (obj "Scanner") (toCode $ parens (
    text "new Scanner(System.in)"))
  printFunc = on2CodeValues mkVal void (toCode $ text "System.out.print")
  printLnFunc = on2CodeValues mkVal void (toCode $ text "System.out.println")
  printFileFunc f = on2CodeValues mkVal void (onCodeValue (printFileDocD 
    "print") f)
  printFileLnFunc f = on2CodeValues mkVal void (onCodeValue (printFileDocD 
    "println") f)
  
  cast = jCast
  
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector JavaCode where
  objAccess = objAccessD
  ($.) = objAccess

  objMethodCall = objMethodCallD
  objMethodCallNoParams = objMethodCallNoParamsD

  selfAccess = selfAccessD

  listIndexExists = listIndexExistsD
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf = indexOfD "indexOf"

instance FunctionSym JavaCode where
  type Function JavaCode = FuncData
  func = funcD

  get = getD
  set = setD

  listSize = listSizeD
  listAdd = listAddD
  listAppend = listAppendD

  iterBegin = iterBeginD
  iterEnd = iterEndD

instance SelectorFunction JavaCode where
  listAccess = listAccessD
  listSet = listSetD
  at = listAccess

instance InternalFunction JavaCode where
  getFunc = getFuncD
  setFunc = setFuncD

  listSizeFunc = listSizeFuncD
  listAddFunc _ = listAddFuncD "add"
  listAppendFunc = listAppendFuncD "add"

  iterBeginFunc _ = error $ iterBeginError jName
  iterEndFunc _ = error $ iterEndError jName
  
  listAccessFunc = listAccessFuncD' "get"
  listSetFunc v i toVal = func "set" (valueType v) [intValue i, toVal]

  functionType = onCodeValue funcType
  functionDoc = funcDoc . unJC

  funcFromData t d = on2CodeValues fd t (toCode d)

instance InternalStatement JavaCode where
  printSt _ p v _ = printStD p v

  state = stateD
  loopState = loopStateD

  emptyState = emptyStateD
  statementDoc = fst . unJC
  statementTerm = snd . unJC
  
  stateFromData d t = toCode (d, t)

instance StatementSym JavaCode where
  -- Terminator determines how statements end
  type Statement JavaCode = (Doc, Terminator)
  assign = assignD Semi
  assignToListIndex = assignToListIndexD
  multiAssign _ _ = error $ multiAssignError jName
  (&=) = assign
  (&-=) = decrementD
  (&+=) = incrementD
  (&++) = increment1D
  (&~-) = decrement1D

  varDec = G.varDec static_ dynamic_
  varDecDef = G.varDecDef
  listDec n v = G.listDec (listDecDocD v) (litInt n) v
  listDecDef v = G.listDecDef (jListDecDef v) v
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew _ = objDecNew
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams _ = objDecNewNoParams
  constDecDef v def = mkSt <$> on2CodeValues jConstDecDef v def

  print v = jOut False printFunc v Nothing
  printLn v = jOut True printLnFunc v Nothing
  printStr s = jOut False printFunc (litString s) Nothing
  printStrLn s = jOut True printLnFunc (litString s) Nothing

  printFile f v = jOut False (printFileFunc f) v (Just f)
  printFileLn f v = jOut True (printFileLnFunc f) v (Just f)
  printFileStr f s = jOut False (printFileFunc f) (litString s) (Just f)
  printFileStrLn f s = jOut True (printFileLnFunc f) (litString s) (Just f)

  getInput v = v &= on2CodeValues jInput (variableType v) inputFunc
  discardInput = discardInputD jDiscardInput
  getFileInput f v = v &= on2CodeValues jInput (variableType v) f
  discardFileInput = discardFileInputD jDiscardInput

  openFileR = openFileRD jOpenFileR
  openFileW = openFileWD jOpenFileWorA
  openFileA = openFileAD jOpenFileWorA
  closeFile = closeFileD "close"

  getFileInputLine f v = v &= f $. func "nextLine" string []
  discardFileLine = discardFileLineD "nextLine"
  stringSplit d vnew s = mkSt <$> on2CodeValues jStringSplit vnew 
    (funcApp "Arrays.asList" (listType static_ string) 
    [s $. func "split" (listType static_ string) [litString [d]]])

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = breakD Semi  -- I could have a JumpSym class with functions for "return $ text "break" and then reference those functions here?
  continue = continueD Semi

  returnState = returnD Semi
  multiReturn _ = error $ multiReturnError jName

  valState = valStateD Semi

  comment = G.comment commentStart

  free _ = error $ freeError jName -- could set variable to null? Might be misleading.

  throw = throwD jThrowDoc Semi

  initState = initStateD
  changeState = changeStateD

  initObserverList = initObserverListD
  addObserver = addObserverD

  inOutCall = jInOutCall funcApp
  selfInOutCall c = jInOutCall (selfFuncApp c)
  extInOutCall m = jInOutCall (extFuncApp m)

  multi = lift1List multiStateDocD endStatement

instance ControlStatementSym JavaCode where
  ifCond = G.ifCond ifBodyStart elseIf blockEnd
  ifNoElse = ifNoElseD
  switch  = switchD
  switchAsIf = switchAsIfD

  ifExists = ifExistsD

  for = G.for blockStart blockEnd
  forRange = forRangeD 
  forEach = G.forEach blockStart blockEnd iterForEachLabel iterInLabel
  while = G.while blockStart blockEnd

  tryCatch = tryCatchD jTryCatch
  
  checkState = checkStateD
  notifyObservers = notifyObserversD

  getFileInputAll f v = while (f $. func "hasNextLine" bool [])
    (oneLiner $ valState $ listAppend (valueOf v) (f $. func "nextLine" string []))

instance ScopeSym JavaCode where
  type Scope JavaCode = Doc
  private = toCode privateDocD
  public = toCode publicDocD

instance InternalScope JavaCode where
  scopeDoc = unJC

instance MethodTypeSym JavaCode where
  type MethodType JavaCode = TypeData
  mType t = t
  construct = toCode . G.construct

instance ParameterSym JavaCode where
  type Parameter JavaCode = ParamData
  param = onCodeValue (mkParam paramDocD)
  pointerParam = param

  parameterType = variableType . onCodeValue paramVar

instance MethodSym JavaCode where
  type Method JavaCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor n = G.constructor n n
  destructor _ _ = error $ destructorError jName

  docMain = G.docMain

  function = G.function
  mainFunction = G.mainFunction string "main"

  docFunc = G.docFunc

  inOutMethod n c = jInOut (method n c)

  docInOutMethod n c = jDocInOut (inOutMethod n c)

  inOutFunc n = jInOut (function n)
    
  docInOutFunc n = jDocInOut (inOutFunc n)

instance InternalMethod JavaCode where
  intMethod m n _ s p t ps b = getPutReturn (setParameters (map unJC ps) . 
    if m then over lensMStoGS (setCurrMain m) . setMain else id) $ onCodeValue 
    mthd (liftA5 (jMethod n) s p t (liftList (paramListDocD . checkParams n) ps)
    b)
  intFunc = G.intFunc
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthdDoc) m 
    (onStateValue (onCodeValue commentedItem) cmt)
  
  methodDoc = mthdDoc . unJC
  methodFromData _ = toCode . mthd

instance StateVarSym JavaCode where
  type StateVar JavaCode = Doc
  stateVar = G.stateVar
  stateVarDef _ = G.stateVarDef
  constVar _ = G.constVar (permDoc (static_ :: JavaCode (Permanence JavaCode)))
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance InternalStateVar JavaCode where
  stateVarDoc = unJC
  stateVarFromData = toState . toCode

instance ClassSym JavaCode where
  type Class JavaCode = Doc
  buildClass = G.buildClass classDocD inherit
  enum = G.enum
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass = G.commentedClass

instance InternalClass JavaCode where
  classDoc = unJC
  classFromData = onStateValue toCode

instance ModuleSym JavaCode where
  type Module JavaCode = ModData
  buildModule n _ = G.buildModule' n
  
instance InternalMod JavaCode where
  moduleDoc = modDoc . unJC
  modFromData n = G.modFromData n (\d m -> toCode $ md n m d)
  updateModuleDoc f = onStateValue (onCodeValue (updateModDoc f))

instance BlockCommentSym JavaCode where
  type BlockComment JavaCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unJC

jName :: String
jName = "Java"

jtop :: Doc -> Doc -> Doc -> Doc
jtop end inc lst = vcat [
  inc <+> text "java.util.Arrays" <> end,
  inc <+> text "java.util.BitSet" <> end, --TODO: only include these if they are used in the code?
  inc <+> text "java.util.Scanner" <> end,
  inc <+> text "java.io.PrintWriter" <> end,
  inc <+> text "java.io.FileWriter" <> end,
  inc <+> text "java.io.File" <> end,
  inc <+> text ("java.util." ++ render lst) <> end]

jFloatTypeDocD :: TypeData
jFloatTypeDocD = td Float "double" (text "double")

jStringTypeDoc :: TypeData
jStringTypeDoc = td String "String" (text "String")

jInfileTypeDoc :: TypeData
jInfileTypeDoc = td File "Scanner" (text "Scanner")

jOutfileTypeDoc :: TypeData
jOutfileTypeDoc = td File "PrintWriter" (text "PrintWriter")

jListType :: TypeData -> Doc -> TypeData
jListType (TD Integer _ _) lst = td (List Integer) (render lst ++ "<Integer>") 
  (lst <> angles (text "Integer"))
jListType (TD Float _ _) lst = td (List Float) (render lst ++ "<Double>") 
  (lst <> angles (text "Double"))
jListType t lst = listTypeDocD t lst

jArrayType :: JavaCode (Type JavaCode)
jArrayType = toCode $ td (List $ Object "Object") "Object" (text "Object[]")

jEquality :: JavaCode (Value JavaCode) -> JavaCode (Value JavaCode) -> 
  JavaCode (Value JavaCode)
jEquality v1 v2 = jEquality' (getType $ valueType v2)
  where jEquality' String = objAccess v1 (func "equals" bool [v2])
        jEquality' _ = liftA4 typeBinExpr equalOp bool v1 v2

jCast :: JavaCode (Type JavaCode) -> JavaCode (Value JavaCode) -> 
  JavaCode (Value JavaCode)
jCast t v = jCast' (getType t) (getType $ valueType v)
  where jCast' Float String = funcApp "Double.parseDouble" float [v]
        jCast' Integer (Enum _) = v $. func "ordinal" int []
        jCast' _ _ = on2CodeValues mkVal t $ on2CodeValues castObjDocD 
          (onCodeValue castDocD t) v

jListDecDef :: (RenderSym repr) => repr (Variable repr) -> [repr (Value repr)] 
  -> Doc
jListDecDef v vs = space <> equals <+> new <+> getTypeDoc (variableType v) <+> 
  parens listElements
  where listElements = emptyIfNull vs $ text "Arrays.asList" <> parens 
          (valueList vs)

jConstDecDef :: VarData -> ValData -> Doc
jConstDecDef v def = text "final" <+> typeDoc (varType v) <+> varDoc v <+> 
  equals <+> valDoc def

jThrowDoc :: (RenderSym repr) => repr (Value repr) -> Doc
jThrowDoc errMsg = text "throw new" <+> text "Exception" <> parens (valueDoc 
  errMsg)

jTryCatch :: (RenderSym repr) => repr (Body repr) -> repr (Body repr) -> Doc
jTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent $ bodyDoc tb,
  rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> 
    lbrace,
  indent $ bodyDoc cb,
  rbrace]

jOut :: (RenderSym repr) => Bool -> repr (Value repr) -> repr (Value repr) 
  -> Maybe (repr (Value repr)) -> repr (Statement repr)
jOut newLn printFn v f = jOut' (getType $ valueType v)
  where jOut' (List (Object _)) = outDoc newLn printFn v f
        jOut' (List _) = printSt newLn printFn v f
        jOut' _ = outDoc newLn printFn v f

jDiscardInput :: (RenderSym repr) => repr (Value repr) -> Doc
jDiscardInput inFn = valueDoc inFn <> dot <> text "next()"

jInput :: TypeData -> ValData -> ValData
jInput t inFn = mkVal t $ jInput' (cType t) 
  where jInput' Integer = text "Integer.parseInt" <> parens (valDoc inFn <> 
          dot <> text "nextLine()")
        jInput' Float = text "Double.parseDouble" <> parens (valDoc inFn <> 
          dot <> text "nextLine()")
        jInput' Boolean = valDoc inFn <> dot <> text "nextBoolean()"
        jInput' String = valDoc inFn <> dot <> text "nextLine()"
        jInput' Char = valDoc inFn <> dot <> text "next().charAt(0)"
        jInput' _ = error "Attempt to read value of unreadable type"

jOpenFileR :: (RenderSym repr) => repr (Value repr) -> repr (Type repr) -> 
  repr (Value repr)
jOpenFileR n t = valFromData Nothing t $ new <+> text "Scanner" <> parens 
  (new <+> text "File" <> parens (valueDoc n))

jOpenFileWorA :: (RenderSym repr) => repr (Value repr) -> repr (Type repr) -> 
  repr (Value repr) -> repr (Value repr)
jOpenFileWorA n t wa = valFromData Nothing t $ new <+> text "PrintWriter" <> 
  parens (new <+> text "FileWriter" <> parens (new <+> text "File" <> 
  parens (valueDoc n) <> comma <+> valueDoc wa))

jStringSplit :: VarData -> ValData -> Doc
jStringSplit vnew s = varDoc vnew <+> equals <+> new <+> typeDoc (varType vnew)
  <> parens (valDoc s)

jMethod :: Label -> Doc -> Doc -> TypeData -> Doc -> Doc -> Doc
jMethod n s p t ps b = vcat [
  s <+> p <+> typeDoc t <+> text n <> parens ps <+> text "throws Exception" <+> 
    lbrace,
  indent b,
  rbrace]

jAssignFromArray :: Int -> [JavaCode (Variable JavaCode)] -> 
  [JavaCode (Statement JavaCode)]
jAssignFromArray _ [] = []
jAssignFromArray c (v:vs) = (v &= cast (variableType v)
  (valueOf (var ("outputs[" ++ show c ++ "]") (variableType v))))
  : jAssignFromArray (c+1) vs

jInOutCall :: (Label -> JavaCode (Type JavaCode) -> 
  [JavaCode (Value JavaCode)] -> JavaCode (Value JavaCode)) -> Label -> 
  [JavaCode (Value JavaCode)] -> [JavaCode (Variable JavaCode)] -> 
  [JavaCode (Variable JavaCode)] -> JavaCode (Statement JavaCode)
jInOutCall f n ins [] [] = valState $ f n void ins
jInOutCall f n ins [out] [] = assign out $ f n (variableType out) ins
jInOutCall f n ins [] [out] = if null (filterOutObjs [out])
  then valState $ f n void (valueOf out : ins) 
  else assign out $ f n (variableType out) (valueOf out : ins)
jInOutCall f n ins outs both = fCall rets
  where rets = filterOutObjs both ++ outs
        fCall [x] = assign x $ f n (variableType x) (map valueOf both ++ ins)
        fCall xs = multi $ varDecDef (var "outputs" jArrayType) 
          (f n jArrayType (map valueOf both ++ ins)) : jAssignFromArray 0 xs

jInOut :: (JavaCode (Scope JavaCode) -> JavaCode (Permanence JavaCode) -> 
    JavaCode (Type JavaCode) -> [JavaCode (Parameter JavaCode)] -> 
    JavaCode (Body JavaCode) -> MS (JavaCode (Method JavaCode))) 
  -> JavaCode (Scope JavaCode) -> JavaCode (Permanence JavaCode) -> 
  [JavaCode (Variable JavaCode)] -> [JavaCode (Variable JavaCode)] -> 
  [JavaCode (Variable JavaCode)] -> JavaCode (Body JavaCode) -> 
  MS (JavaCode (Method JavaCode))
jInOut f s p ins [] [] b = f s p void (map param ins) b
jInOut f s p ins [v] [] b = f s p (variableType v) (map param ins) 
  (on3CodeValues surroundBody (varDec v) b (returnState $ valueOf v))
jInOut f s p ins [] [v] b = f s p (if null (filterOutObjs [v]) 
  then void else variableType v) (map param $ v : ins) 
  (if null (filterOutObjs [v]) then b else on2CodeValues appendToBody b 
  (returnState $ valueOf v))
jInOut f s p ins outs both b = f s p (returnTp rets)
  (map param $ both ++ ins) (on3CodeValues surroundBody decls b (returnSt rets))
  where returnTp [x] = variableType x
        returnTp _ = jArrayType
        returnSt [x] = returnState $ valueOf x
        returnSt _ = multi (varDecDef outputs (valueOf (var 
          ("new Object[" ++ show (length rets) ++ "]") jArrayType))
          : assignArray 0 (map valueOf rets)
          ++ [returnState (valueOf outputs)])
        assignArray :: Int -> [JavaCode (Value JavaCode)] -> 
          [JavaCode (Statement JavaCode)]
        assignArray _ [] = []
        assignArray c (v:vs) = (var ("outputs[" ++ show c ++ "]") 
          (valueType v) &= v) : assignArray (c+1) vs
        decls = multi $ map varDec outs
        rets = filterOutObjs both ++ outs
        outputs = var "outputs" jArrayType

jDocInOut :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr) 
    -> [repr (Variable repr)] -> [repr (Variable repr)] -> 
    [repr (Variable repr)] -> repr (Body repr) -> 
    MS (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, repr (Variable repr))] -> [(String, repr (Variable repr))] -> 
  [(String, repr (Variable repr))] -> repr (Body repr) -> 
  MS (repr (Method repr))
jDocInOut f s p desc is [] [] b = docFuncRepr desc (map fst is) [] 
  (f s p (map snd is) [] [] b)
jDocInOut f s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f s p (map snd is) [snd o] [] b)
jDocInOut f s p desc is [] [both] b = docFuncRepr desc (map fst (both : is)) 
  [fst both | not ((isObject . getType . variableType . snd) both)]
  (f s p (map snd is) [] [snd both] b)
jDocInOut f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is) 
  (bRets ++ map fst os) (f s p (map snd is) (map snd os) (map snd bs) b)
  where bRets = bRets' (map fst (filter (not . isObject . getType . 
          variableType . snd) bs))
        bRets' [x] = [x]
        bRets' xs = "array containing the following values:" : xs
