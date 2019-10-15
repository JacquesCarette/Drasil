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
  TypeSym(..), InternalType(..), UnaryOpSym(..), BinaryOpSym(..), 
  VariableSym(..), InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), MethodTypeSym(..), ParameterSym(..), 
  MethodSym(..), InternalMethod(..), StateVarSym(..), InternalStateVar(..), 
  ClassSym(..), InternalClass(..), ModuleSym(..), InternalMod(..), 
  BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (classDocD, multiStateDocD, bodyDocD, 
  oneLinerD, printListDoc, outDoc, prFn, prStrFn, prLnFn, printFileDocD, 
  boolTypeDocD, intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, 
  enumTypeDocD, listTypeDocD, listInnerTypeD, iteratorError, 
  voidDocD, destructorError, paramDocD, paramListDocD, mkParam, methodDocD, 
  runStrategyD, listSliceD, checkStateD, notifyObserversD, listDecDocD, 
  listDecDefDocD, stringListVals', stringListLists', printStD, stateD, 
  loopStateD, emptyStateD, assignD, assignToListIndexD, multiAssignError, 
  decrementD, incrementD, decrement1D, increment1D, constDecDefD, discardInputD,
  openFileRD, openFileWD, openFileAD, closeFileD, discardFileLineD, breakD, 
  continueD, returnD, multiReturnError, valStateD, freeError, throwD, 
  initStateD, changeStateD, initObserverListD, addObserverD, ifNoElseD, switchD,
  switchAsIfD, ifExistsD, forRangeD, tryCatchD, unOpPrec, notOpDocD, 
  negateOpDocD, unExpr, unExpr', powerPrec, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', typeBinExpr, mkVal, 
  mkVar, litTrueD, litFalseD, litCharD, litFloatD, litIntD, litStringD, 
  classVarD, classVarDocD, objVarDocD, inlineIfD, newObjDocD, varD, staticVarD, 
  extVarD, selfD, enumVarD, classVarD, objVarSelfD, listVarD, listOfD,
  valueOfD, argD, enumElementD, argsListD, objAccessD, objMethodCallD, 
  objMethodCallNoParamsD, selfAccessD, listIndexExistsD, indexOfD, funcAppD, 
  extFuncAppD, newObjD,notNullD, funcDocD, castDocD, listSetFuncDocD, 
  castObjDocD, funcD, getD, setD, listSizeD, listAddD, listAppendD, iterBeginD, 
  iterEndD, listAccessD, listSetD, getFuncD, setFuncD, listAddFuncD, 
  listAppendFuncD, iterBeginError, iterEndError, listAccessFuncD, listSetFuncD, 
  staticDocD, dynamicDocD, bindingError, privateDocD, publicDocD, dot, new, 
  blockCmtStart, blockCmtEnd, docCmtStart, doubleSlash, elseIfLabel, inLabel, 
  blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, commentedModD, 
  appendToBody, surroundBody, filterOutObjs)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (block, 
  varDec, varDecDef, listDec, listDecDef, objDecNew, objDecNewNoParams, 
  construct, comment, ifCond, for, forEach, while, method, getMethod, setMethod,
  privMethod, pubMethod, constructor, docMain, function, mainFunction, docFunc, 
  docInOutFunc, intFunc, stateVar, stateVarDef, constVar, privMVar, pubMVar, 
  pubGVar, buildClass, enum, privClass, pubClass, docClass, commentedClass, 
  buildModule', fileDoc, docMod)
import GOOL.Drasil.Data (Boolean, Other, Terminator(..), FileType(..), 
  FileData(..), fileD, TypedFunc(..), funcDoc, ModData(..), md, MethodData(..), 
  mthd, updateMthdDoc, OpData(..), ParamData(..), updateParamDoc, ProgData(..), 
  progD, TypeData(..), td, TypedType(..), cType, typeString, typeDoc, 
  updateTypedType, TypedValue(..), updateValDoc, valDoc, toOtherVal, 
  Binding(..), TypedVar(..), otherVar, varBind, varName, varDoc, toOtherVar, 
  typeToFunc, typeToVal, typeToVar, funcToType, valToType, varToType)
import GOOL.Drasil.Helpers (liftA4, liftA5, liftList, lift1List, getNestDegree, 
  checkParams)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Control.Applicative (Applicative, liftA2, liftA3)
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
  prog n = liftList (progD n)

instance RenderSym CSharpCode where
  type RenderFile CSharpCode = FileData
  fileDoc code = G.fileDoc Combined csExt (top code) bottom code

  docMod = G.docMod

  commentedMod = liftA2 commentedModD

instance InternalFile CSharpCode where
  top _ = liftA2 cstop endStatement (include "")
  bottom = return empty

  getFilePath = filePath . unCSC
  fileFromData ft fp = fmap (fileD ft fp)

instance KeywordSym CSharpCode where
  type Keyword CSharpCode = Doc
  endStatement = return semi
  endStatementLoop = return empty

  include _ = return $ text "using"
  inherit n = return $ colon <+> text n

  list _ = return $ text "List"

  blockStart = return lbrace
  blockEnd = return rbrace

  ifBodyStart = blockStart
  elseIf = return elseIfLabel
  
  iterForEachLabel = return $ text "foreach"
  iterInLabel = return inLabel

  commentStart = return doubleSlash
  blockCommentStart = return blockCmtStart
  blockCommentEnd = return blockCmtEnd
  docCommentStart = return docCmtStart
  docCommentEnd = blockCommentEnd

  keyDoc = unCSC

instance PermanenceSym CSharpCode where
  type Permanence CSharpCode = Doc
  static_ = return staticDocD
  dynamic_ = return dynamicDocD

instance InternalPerm CSharpCode where
  permDoc = unCSC
  binding = error $ bindingError csName

instance BodySym CSharpCode where
  type Body CSharpCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner = oneLinerD

  addComments s = liftA2 (addCommentsDocD s) commentStart

  bodyDoc = unCSC

instance BlockSym CSharpCode where
  type Block CSharpCode = Doc
  block = G.block endStatement

instance InternalBlock CSharpCode where
  blockDoc = unCSC
  docBlock = return

instance TypeSym CSharpCode where
  type Type CSharpCode = TypedType
  bool = return boolTypeDocD
  int = return intTypeDocD
  float = return csFloatTypeDoc
  char = return charTypeDocD
  string = return stringTypeDocD
  infile = return csInfileTypeDoc
  outfile = return csOutfileTypeDoc
  listType p st = liftA2 listTypeDocD st (list p)
  listInnerType = fmap listInnerTypeD
  obj t = return $ typeDocD t
  enumType t = return $ enumTypeDocD t
  iterator _ = error $ iteratorError csName
  void = return voidDocD

  getType = cType . unCSC
  getTypeString = typeString . unCSC
  getTypeDoc = typeDoc . unCSC
  
instance InternalType CSharpCode where
  getTypedType = unCSC
  updateType t tf sf df = liftA4 updateTypedType t (return tf) (return sf) 
    (return df)

instance ControlBlockSym CSharpCode where
  runStrategy = runStrategyD

  listSlice = listSliceD

instance UnaryOpSym CSharpCode where
  type UnaryOp CSharpCode = OpData
  notOp = return notOpDocD
  negateOp = return negateOpDocD
  sqrtOp = return $ unOpPrec "Math.Sqrt"
  absOp = return $ unOpPrec "Math.Abs"
  logOp = return $ unOpPrec "Math.Log10"
  lnOp = return $ unOpPrec "Math.Log"
  expOp = return $ unOpPrec "Math.Exp"
  sinOp = return $ unOpPrec "Math.Sin"
  cosOp = return $ unOpPrec "Math.Cos"
  tanOp = return $ unOpPrec "Math.Tan"
  asinOp = return $ unOpPrec "Math.Asin"
  acosOp = return $ unOpPrec "Math.Acos"
  atanOp = return $ unOpPrec "Math.Atan"
  floorOp = return $ unOpPrec "Math.Floor"
  ceilOp = return $ unOpPrec "Math.Ceiling"

instance BinaryOpSym CSharpCode where
  type BinaryOp CSharpCode = OpData
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
  powerOp = return $ powerPrec "Math.Pow"
  moduloOp = return moduloOpDocD
  andOp = return andOpDocD
  orOp = return orOpDocD

instance VariableSym CSharpCode where
  type Variable CSharpCode = TypedVar
  var = varD
  staticVar = staticVarD
  const = var
  extVar = extVarD
  self = selfD
  enumVar = enumVarD
  classVar = classVarD classVarDocD
  objVar = liftA2 csObjVar
  objVarSelf = objVarSelfD
  listVar  = listVarD
  listOf = listOfD 
  iterVar = var

  ($->) = objVar

  variableBind = varBind . unCSC
  variableName = varName . unCSC
  variableType = fmap varToType
  variableDoc = varDoc . unCSC
  toOtherVariable = fmap toOtherVar

instance InternalVariable CSharpCode where
  varFromData b n t d = liftA2 (typeToVar b n) t (return d)

instance ValueSym CSharpCode where
  type Value CSharpCode = TypedValue
  litTrue = litTrueD
  litFalse = litFalseD
  litChar = litCharD
  litFloat = litFloatD
  litInt = litIntD
  litString = litStringD

  ($:) = enumElement

  valueOf = valueOfD
  arg n = argD (litInt n) argsList
  enumElement = enumElementD
  
  argsList = argsListD "args"

  valueType = fmap valToType
  valueDoc = valDoc . unCSC
  getTypedVal = unCSC
  toOtherValue = fmap toOtherVal

instance NumericExpression CSharpCode where
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

instance BooleanExpression CSharpCode where
  (?!) = liftA2 unExpr notOp
  (?&&) = liftA4 typeBinExpr andOp bool
  (?||) = liftA4 typeBinExpr orOp bool

  (?<) = liftA4 typeBinExpr lessOp bool
  (?<=) = liftA4 typeBinExpr lessEqualOp bool
  (?>) = liftA4 typeBinExpr greaterOp bool
  (?>=) = liftA4 typeBinExpr greaterEqualOp bool
  (?==) = liftA4 typeBinExpr equalOp bool
  (?!=) = liftA4 typeBinExpr notEqualOp bool
  
instance ValueExpression CSharpCode where
  inlineIf = liftA3 inlineIfD
  funcApp = funcAppD
  selfFuncApp = funcApp
  extFuncApp = extFuncAppD
  newObj = newObjD newObjDocD
  extNewObj _ = newObj

  exists = notNull
  notNull = notNullD

instance InternalValue CSharpCode where
  inputFunc = liftA2 mkVal string (return $ text "Console.ReadLine()")
  printFunc = liftA2 mkVal void (return $ text "Console.Write")
  printLnFunc = liftA2 mkVal void (return $ text "Console.WriteLine")
  printFileFunc f = liftA2 mkVal void (fmap (printFileDocD "Write") f)
  printFileLnFunc f = liftA2 mkVal void (fmap (printFileDocD "WriteLine") f)
  
  cast = csCast
  
  valFromData p t d = liftA2 (typeToVal p) t (return d)

instance Selector CSharpCode where
  objAccess = objAccessD
  ($.) = objAccess

  objMethodCall = objMethodCallD
  objMethodCallNoParams = objMethodCallNoParamsD

  selfAccess = selfAccessD

  listIndexExists = listIndexExistsD
  argExists i = listSize argsList ?> (litInt $ fromIntegral i)
  
  indexOf = indexOfD "IndexOf"

instance FunctionSym CSharpCode where
  type Function CSharpCode = TypedFunc
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

  listSizeFunc = funcFromData int (funcDocD (text "Count"))
  listAddFunc _ = listAddFuncD "Insert"
  listAppendFunc = listAppendFuncD "Add"

  iterBeginFunc _ = error $ iterBeginError csName
  iterEndFunc _ = error $ iterEndError csName

  listAccessFunc = listAccessFuncD
  listSetFunc = listSetFuncD listSetFuncDocD 
    
  functionType = fmap funcToType
  functionDoc = funcDoc . unCSC

  funcFromData t d = liftA2 typeToFunc t (return d)

instance InternalStatement CSharpCode where
  printSt _ p v _ = printStD p v

  state = stateD
  loopState = loopStateD

  emptyState = emptyStateD
  statementDoc = fst . unCSC
  statementTerm = snd . unCSC
  
  stateFromData d t = return (d, t)

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

  print v = csOut False printFunc v Nothing
  printLn v = csOut True printLnFunc v Nothing
  printStr s = csOut False printFunc (litString s) Nothing
  printStrLn s = csOut True printLnFunc (litString s) Nothing

  printFile f v = csOut False (printFileFunc f) v (Just f)
  printFileLn f v = csOut True (printFileLnFunc f) v (Just f)
  printFileStr f s = csOut False (printFileFunc f) (litString s) (Just f)
  printFileStrLn f s = csOut True (printFileLnFunc f) (litString s) (Just f)

  getInput v = v &= liftA2 csInput (variableType v) inputFunc
  discardInput = discardInputD csDiscardInput
  getFileInput f v = v &= liftA2 csInput (variableType v) (fmap csFileInput f)
  discardFileInput f = valState $ fmap csFileInput f

  openFileR = openFileRD csOpenFileR
  openFileW = openFileWD csOpenFileWorA
  openFileA = openFileAD csOpenFileWorA
  closeFile = closeFileD "Close"

  getFileInputLine = getFileInput
  discardFileLine = discardFileLineD "ReadLine"
  stringSplit d vnew s = assign vnew $ newObj (listType dynamic_ string) 
    [toOtherValue $ s $. func "Split" (listType static_ string) [litChar d]]

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = breakD Semi
  continue = continueD Semi

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
  extInOutCall m = csInOutCall (extFuncApp m)

  multi = lift1List multiStateDocD endStatement

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

  getFileInputAll f v = while ((f $. liftA2 typeToFunc bool (return $ text 
    ".EndOfStream")) ?!) (oneLiner $ valState $ listAppend (valueOf v) (fmap 
    csFileInput f))

instance ScopeSym CSharpCode where
  type Scope CSharpCode = Doc
  private = return privateDocD
  public = return publicDocD

instance InternalScope CSharpCode where
  scopeDoc = unCSC

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypedType
  mType t = t
  construct = return . G.construct

instance ParameterSym CSharpCode where
  type Parameter CSharpCode = ParamData
  param = fmap (mkParam paramDocD)
  pointerParam = param

  parameterName = variableName . fmap (otherVar . paramVar)

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

  inOutFunc n s p ins [v] [] b = function n s p (variableType v) (map param ins)
   (liftA3 surroundBody (varDec v) b (returnState $ valueOf v))
  inOutFunc n s p ins [] [v] b = function n s p (if null (filterOutObjs [v]) 
    then void else variableType v) (map param $ v : ins) 
    (if null (filterOutObjs [v]) then b else liftA2 appendToBody b 
    (returnState $ valueOf v))
  inOutFunc n s p ins outs both b = function n s p void (map (fmap 
    (updateParamDoc csRef) . param) both ++ map param ins ++ 
    map (fmap (updateParamDoc csOutParam) . param) outs) b

  docInOutFunc = G.docInOutFunc
  
  parameters m = map return $ (mthdParams . unCSC) m

instance InternalMethod CSharpCode where
  intMethod m n _ s p t ps b = liftA2 (mthd m) (checkParams n <$> sequence ps)
    (liftA5 (methodDocD n) s p t (liftList paramListDocD ps) b)
  intFunc = G.intFunc
  commentedFunc cmt = liftA2 updateMthdDoc (fmap commentedItem cmt)
  
  isMainMethod = isMainMthd . unCSC
  methodDoc = mthdDoc . unCSC

instance StateVarSym CSharpCode where
  type StateVar CSharpCode = Doc
  stateVar _ = G.stateVar
  stateVarDef _ _ = G.stateVarDef
  constVar _ _ = G.constVar empty
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance InternalStateVar CSharpCode where
  stateVarDoc = unCSC
  stateVarFromData = return

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
  classFromData = return

instance ModuleSym CSharpCode where
  type Module CSharpCode = ModData
  buildModule n _ = G.buildModule' n
    
  moduleName m = name (unCSC m)
  
instance InternalMod CSharpCode where
  isMainModule = isMainMod . unCSC
  moduleDoc = modDoc . unCSC
  modFromData n m d = return $ md n m d

instance BlockCommentSym CSharpCode where
  type BlockComment CSharpCode = Doc
  blockComment lns = liftA2 (blockCmtDoc lns) blockCommentStart blockCommentEnd
  docComment lns = liftA2 (docCmtDoc lns) docCommentStart docCommentEnd

  blockCommentDoc = unCSC

csName :: String
csName = "C#"

cstop :: Doc -> Doc -> Doc
cstop end inc = vcat [
  inc <+> text "System" <> end,
  inc <+> text "System.IO" <> end,
  inc <+> text "System.Collections" <> end,
  inc <+> text "System.Collections.Generic" <> end]

csFloatTypeDoc :: TypedType Other
csFloatTypeDoc = td Float "double" (text "double") -- Same as Java, maybe make a common function

csInfileTypeDoc :: TypedType Other
csInfileTypeDoc = td File "StreamReader" (text "StreamReader")

csOutfileTypeDoc :: TypedType Other
csOutfileTypeDoc = td File "StreamWriter" (text "StreamWriter")

csCast :: CSharpCode (Type CSharpCode a) -> CSharpCode (Value CSharpCode b)
  -> CSharpCode (Value CSharpCode a)
csCast t v = csCast' (unCSC t) (getType $ valueType v)
  where csCast' (OT (TD Float _ _)) String = funcApp "Double.Parse" float 
          [fmap toOtherVal v]
        csCast' _ _ = liftA2 mkVal t $ liftA2 castObjDocD (fmap castDocD t) v

csThrowDoc :: (RenderSym repr) => repr (Value repr Other) -> Doc
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

csOut :: Bool -> CSharpCode (Value CSharpCode Other) -> 
  CSharpCode (Value CSharpCode a) -> Maybe (CSharpCode (Value CSharpCode Other))
  -> CSharpCode (Statement CSharpCode)
csOut newLn printFn v f = csOut' (getTypedVal v)
  where csOut' lv@(LV _) = printListDoc (getNestDegree 1 (cType $ valToType lv))
          (return lv) (prFn f) (prStrFn f) (prLnFn newLn f)
        csOut' _ = outDoc newLn printFn v f

csDiscardInput :: (RenderSym repr) => repr (Value repr Other) -> Doc
csDiscardInput = valueDoc

csFileInput :: TypedValue Other -> TypedValue Other
csFileInput f = mkVal (valToType f) (valDoc f <> dot <> text "ReadLine()")

csInput :: TypedType a -> TypedValue Other -> TypedValue a
csInput t inFn = mkVal t $ text (csInput' (cType t)) <> 
  parens (valDoc inFn)
  where csInput' Integer = "Int32.Parse"
        csInput' Float = "Double.Parse"
        csInput' Boolean = "Boolean.Parse"
        csInput' String = ""
        csInput' Char = "Char.Parse"
        csInput' _ = error "Attempt to read value of unreadable type"

csOpenFileR :: (RenderSym repr) => repr (Value repr Other) -> 
  repr (Type repr Other) -> repr (Value repr Other)
csOpenFileR n r = valFromData Nothing r $ new <+> getTypeDoc r <> 
  parens (valueDoc n)

csOpenFileWorA :: (RenderSym repr) => repr (Value repr Other) -> 
  repr (Type repr Other) -> repr (Value repr Boolean) -> repr (Value repr Other)
csOpenFileWorA n w a = valFromData Nothing w $ new <+> getTypeDoc w <> 
  parens (valueDoc n <> comma <+> valueDoc a)

csRef :: Doc -> Doc
csRef p = text "ref" <+> p

csOutParam :: Doc -> Doc
csOutParam p = text "out" <+> p

csInOutCall :: (Label -> CSharpCode (Type CSharpCode Other) -> 
  [CSharpCode (Value CSharpCode Other)] -> CSharpCode (Value CSharpCode Other)) -> Label -> 
  [CSharpCode (Value CSharpCode Other)] -> [CSharpCode (Variable CSharpCode Other)] -> 
  [CSharpCode (Variable CSharpCode Other)] -> CSharpCode (Statement CSharpCode)
csInOutCall f n ins [out] [] = assign out $ f n (variableType out) ins
csInOutCall f n ins [] [out] = if null (filterOutObjs [out])
  then valState $ f n void (valueOf out : ins) 
  else assign out $ f n (variableType out) (valueOf out : ins)
csInOutCall f n ins outs both = valState $ f n void (map (fmap (updateValDoc 
  csRef) . valueOf) both ++ ins ++ map (fmap (updateValDoc csOutParam) . 
  valueOf) outs)

csVarDec :: Binding -> CSharpCode (Statement CSharpCode) -> 
  CSharpCode (Statement CSharpCode)
csVarDec Static _ = error "Static variables can't be declared locally to a function in C#. Use stateVar to make a static state variable instead."
csVarDec Dynamic d = d

csObjVar :: TypedVar Other -> TypedVar a -> TypedVar a
csObjVar o v = csObjVar' (varBind v)
  where csObjVar' Static = error 
          "Cannot use objVar to access static variables through an object in C#"
        csObjVar' Dynamic = mkVar (varName o ++ "." ++ varName v) 
          (varToType v) (objVarDocD (varDoc o) (varDoc v))
