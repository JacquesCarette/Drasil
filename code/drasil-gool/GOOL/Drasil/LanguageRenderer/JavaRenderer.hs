{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java code is contained in this module
module GOOL.Drasil.LanguageRenderer.JavaRenderer (
  -- * Java Code Configuration -- defines syntax of all Java code
  JavaCode(..)
) where

import Utils.Drasil (blank, indent, indentList)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym, FileSym(..), 
  InternalFile(..), KeywordSym(..), ImportSym(..), PermanenceSym(..), 
  InternalPerm(..), BodySym(..), InternalBody(..), BlockSym(..), 
  InternalBlock(..), ControlBlockSym(..), TypeSym(..), InternalType(..), 
  UnaryOpSym(..), BinaryOpSym(..), InternalOp(..), VariableSym(..), 
  InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  InternalSelector(..), objMethodCall, objMethodCallNoParams, FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), InternalParam(..), MethodSym(..), 
  initializer, InternalMethod(..), StateVarSym(..), InternalStateVar(..), 
  ClassSym(..), InternalClass(..), ModuleSym(..), InternalMod(..), 
  BlockCommentSym(..), ODEInfo(..), ODEOptions(..), ODEMethod(..))
import GOOL.Drasil.LanguageRenderer (packageDocD, classDocD, multiStateDocD, 
  bodyDocD, outDoc, printFileDocD, destructorError, paramDocD, listDecDocD, 
  mkSt, breakDocD, continueDocD, mkStateVal, mkVal, classVarDocD, newObjDocD, 
  castDocD, castObjDocD, staticDocD, dynamicDocD, bindingError, privateDocD, 
  publicDocD, dot, new, classDec, elseIfLabel, forLabel, blockCmtStart, 
  blockCmtEnd, docCmtStart, doubleSlash, blockCmtDoc, docCmtDoc, commentedItem, 
  addCommentsDocD, commentedModD, docFuncRepr, parameterList, appendToBody, 
  surroundBody, intValue)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  oneLiner, multiBody, block, multiBlock, bool, int, double, char, listType, 
  listInnerType, obj, enumType, void, runStrategy, listSlice, notOp, 
  negateOp, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp,
  plusOp, minusOp, multOp, divideOp, moduloOp, andOp, orOp, var, staticVar, 
  extVar, self, enumVar, classVar, objVar, objVarSelf, listVar, listOf, iterVar,
  litTrue, litFalse, litChar, litFloat, litInt, litString, pi, valueOf, arg, 
  enumElement, argsList, inlineIf, objAccess, objMethodCall, 
  objMethodCallNoParams, selfAccess, listIndexExists, indexOf, funcApp, 
  selfFuncApp, extFuncApp, newObj, notNull, func, get, set, listSize, 
  listAdd, listAppend, iterBegin, iterEnd, listAccess, listSet, getFunc, 
  setFunc, listSizeFunc, listAddFunc, listAppendFunc, iterBeginError, 
  iterEndError, listAccessFunc', printSt, state, loopState, emptyState, assign, 
  assignToListIndex, multiAssignError, decrement, increment, decrement1, 
  increment1, varDec, varDecDef, listDec, objDecNew, objDecNewNoParams, 
  funcDecDef, discardInput, discardFileInput, openFileR, openFileW, openFileA, 
  closeFile, discardFileLine, stringListVals, stringListLists, returnState, 
  multiReturnError, valState, comment, freeError, throw, initState, changeState,
  initObserverList, addObserver, ifCond, ifNoElse, switch, switchAsIf, ifExists,
  for, forRange, forEach, while, tryCatch, checkState, notifyObservers, 
  construct, param, method, getMethod, setMethod, privMethod, pubMethod, 
  constructor, docMain, function, mainFunction, docFunc, intFunc, stateVar, 
  stateVarDef, constVar, privMVar, pubMVar, pubGVar, buildClass, enum, 
  privClass, pubClass, docClass, commentedClass, buildModule', modFromData, 
  fileDoc, docMod, fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (unOpPrec, unExpr, 
  unExpr', typeUnExpr, powerPrec, binExpr, binExpr', typeBinExpr)
import GOOL.Drasil.Data (Terminator(..), ScopeTag(..), FileType(..), 
  Exception(..), FileData(..), fileD, FuncData(..), fd, ModData(..), md, 
  updateModDoc, MethodData(..), mthd, updateMthdDoc, OpData(..), od, 
  ParamData(..), pd, ProgData(..), progD, TypeData(..), td, ValData(..), vd, 
  VarData(..), vard)
import GOOL.Drasil.Helpers (angles, vibcat, emptyIfNull, toCode, toState, 
  onCodeValue, onStateValue, on2CodeValues, on2StateValues, on3CodeValues, 
  on3StateValues, onCodeList, onStateList, on2StateLists, on1CodeValue1List, 
  on1StateValue1List)
import GOOL.Drasil.State (GOOLState, MS, VS, lensGStoFS, lensFStoVS, lensCStoMS, lensMStoFS, lensMStoVS, lensVStoFS, initialState, initialFS, modifyReturn, modifyReturnFunc, 
  addODEFilePaths, addProgNameToPaths, addODEFile, getODEFiles,
  addLangImport, addLangImportVS, addExceptionImports, addLibImport, addLibImports, 
  getModuleName, setClassName, getClassName, setCurrMain, setODEDepVars, 
  getODEDepVars, setODEOthVars, getODEOthVars, setOutputsDeclared, 
  isOutputsDeclared, getExceptions, getMethodExcMap, addExceptions)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (modify, runState)
import qualified Data.Map as Map (lookup)
import Data.List (elemIndex, nub, intercalate, sort)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($$), braces, parens, 
  brackets, empty, equals, semi, vcat, lbrace, rbrace, render, colon, integer)

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
  prog n fs = modifyReturnFunc (\_ -> addProgNameToPaths n)
    (on1CodeValue1List (\end -> progD n . map (packageDocD n end)) endStatement)
    (on2StateValues (++) (mapM (zoom lensGStoFS) fs) (onStateValue (map toCode) 
    getODEFiles))

instance RenderSym JavaCode

instance FileSym JavaCode where
  type RenderFile JavaCode = FileData 
  fileDoc = G.fileDoc Combined jExt top bottom

  docMod = G.docMod

  commentedMod cmt m = on2StateValues (on2CodeValues commentedModD) m cmt

instance InternalFile JavaCode where
  top _ = toCode empty
  bottom = toCode empty
  
  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance KeywordSym JavaCode where
  type Keyword JavaCode = Doc
  endStatement = toCode semi
  endStatementLoop = toCode empty

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

instance ImportSym JavaCode where
  type Import JavaCode = Doc
  langImport n = toCode $ jImport n endStatement
  modImport = langImport

  importDoc = unJC

instance PermanenceSym JavaCode where
  type Permanence JavaCode = Doc
  static_ = toCode staticDocD
  dynamic_ = toCode dynamicDocD

instance InternalPerm JavaCode where
  permDoc = unJC
  binding = error $ bindingError jName

instance BodySym JavaCode where
  type Body JavaCode = Doc
  body = onStateList (onCodeList bodyDocD)
  bodyStatements = block
  oneLiner = G.oneLiner

  addComments s = onStateValue (on2CodeValues (addCommentsDocD s) commentStart)

instance InternalBody JavaCode where
  bodyDoc = unJC
  docBody = onStateValue toCode
  multiBody = G.multiBody 

instance BlockSym JavaCode where
  type Block JavaCode = Doc
  block = G.block endStatement

instance InternalBlock JavaCode where
  blockDoc = unJC
  docBlock = onStateValue toCode
  multiBlock = G.multiBlock

instance TypeSym JavaCode where
  type Type JavaCode = TypeData
  bool = G.bool
  int = G.int
  float = G.double
  char = G.char
  string = jStringType
  infile = jInfileType
  outfile = jOutfileType
  listType = jListType
  listInnerType = G.listInnerType
  obj = G.obj
  enumType = G.enumType
  iterator t = t
  void = G.void

  getType = cType . unJC
  getTypeString = typeString . unJC
  getTypeDoc = typeDoc . unJC
  
instance InternalType JavaCode where
  typeFromData t s d = toCode $ td t s d

instance ControlBlockSym JavaCode where
  runStrategy = G.runStrategy

  listSlice' = G.listSlice

  solveODE info opts = let (fl, s) = jODEFile info 
    in modify (addODEFilePaths s . addODEFile fl . addLibImports 
    (map ((odeImport ++ "sampling.") ++) [stH, stI])) >> (zoom lensMStoVS dv 
    >>= (\dpv -> 
      let odeVarType = obj (odeClassName dpv)
          odeVar = var "ode" odeVarType
          odeDepVar = var (odeVarName dpv) (toState dblArray)
          initval = initVal info
          integVal = valueOf $ jODEIntVar (solveMethod opts)
          hndlr = var "stepHandler" (obj stH)
          interp = var "interpolator" (obj stI)
          odeClassName = ((++ "_ODE") . variableName)
          odeVarName = ((++ "_ode") . variableName)
          odeTempName = ((++ "_curr") . variableName)
      in multiBlock [
      block [
        jODEMethod opts,
        objDecDef odeVar (newObj odeVarType (map valueOf $ otherVars info)),
        zoom lensMStoVS initval >>= (\initv -> varDecDef odeDepVar (toState $ 
          mkVal (variableType dpv) (new <+> getTypeDoc dblArray <+> braces
          (valueDoc initv)))),
        listDecDef dv [initval]],
      block [
        on3StateValues (\odec initf handlef -> mkSt $ statementDoc odec <+> 
          keyDoc (blockStart :: JavaCode Doc) $$ 
          vibcat (map (indent . methodDoc) [initf, handlef]) $$ 
          keyDoc (blockEnd :: JavaCode Doc)) 
          (objDecDef hndlr (newObj (obj stH) [])) 
          (function "init" public dynamic_ void (map param [var "t0" float, 
            var "y0" (toState dblArray), var "t" float]) (body []))
          (function "handleStep" public dynamic_ void (map param [interp, 
            var "isLast" (toState $ typeFromData Boolean "boolean" 
            (text "boolean"))]) (bodyStatements [
              varDecDef (var (odeTempName dpv) (toState dblArray)) 
                (objMethodCallNoParams (toState dblArray) (valueOf interp) 
                "getInterpolatedState"),
              valState $ listAppend (valueOf dv) (mkStateVal float 
                (text $ odeTempName dpv ++ "[0]"))])),
        valState $ objMethodCall void integVal "addStepHandler" [valueOf hndlr],
        valState $ objMethodCall void integVal "integrate"   
          [valueOf odeVar, tInit info, valueOf odeDepVar, tFinal info, 
          valueOf odeDepVar]]]))
    where stH = "StepHandler"
          stI = "StepInterpolator"
          dv = depVar info

instance UnaryOpSym JavaCode where
  type UnaryOp JavaCode = OpData
  notOp = G.notOp
  negateOp = G.negateOp
  sqrtOp = unOpPrec "Math.sqrt"
  absOp = unOpPrec "Math.abs"
  logOp = unOpPrec "Math.log10"
  lnOp = unOpPrec "Math.log"
  expOp = unOpPrec "Math.exp"
  sinOp = unOpPrec "Math.sin"
  cosOp = unOpPrec "Math.cos"
  tanOp = unOpPrec "Math.tan"
  asinOp = unOpPrec "Math.asin"
  acosOp = unOpPrec "Math.acos"
  atanOp = unOpPrec "Math.atan"
  floorOp = unOpPrec "Math.floor"
  ceilOp = unOpPrec "Math.ceil"

instance BinaryOpSym JavaCode where
  type BinaryOp JavaCode = OpData
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
  powerOp = powerPrec "Math.pow"
  moduloOp = G.moduloOp
  andOp = G.andOp
  orOp = G.orOp

instance InternalOp JavaCode where
  uOpDoc = opDoc . unJC
  bOpDoc = opDoc . unJC
  uOpPrec = opPrec . unJC
  bOpPrec = opPrec . unJC
  
  uOpFromData p d = toState $ toCode $ od p d
  bOpFromData p d = toState $ toCode $ od p d

instance VariableSym JavaCode where
  type Variable JavaCode = VarData
  var = G.var
  staticVar = G.staticVar
  const = var
  extVar = G.extVar
  self = G.self
  enumVar = G.enumVar
  classVar = G.classVar classVarDocD
  extClassVar = classVar
  objVar o v = join $ on3StateValues (\ovs ob vr -> if (variableName ob ++ "." 
    ++ variableName vr) `elem` ovs then toState vr else G.objVar (toState ob) 
    (toState vr)) getODEOthVars o v
  objVarSelf = G.objVarSelf
  listVar = G.listVar
  listOf = G.listOf
  iterVar = G.iterVar

  ($->) = objVar

  variableBind = varBind . unJC
  variableName = varName . unJC
  variableType = onCodeValue varType
  variableDoc = varDoc . unJC
  
instance InternalVariable JavaCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym JavaCode where
  type Value JavaCode = ValData
  litTrue = G.litTrue
  litFalse = G.litFalse
  litChar = G.litChar
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString

  pi = G.pi

  ($:) = enumElement

  valueOf v = join $ on2StateValues (\dvs vr -> maybe (G.valueOf v) (mkStateVal 
    (listInnerType $ toState $ variableType vr) . (variableDoc vr <>) . 
    brackets . integer . toInteger) (elemIndex (variableName vr) dvs)) 
    getODEDepVars v
  arg n = G.arg (litInt n) argsList
  enumElement = G.enumElement

  argsList = G.argsList "args"

  valueType = onCodeValue valType
  valueDoc = valDoc . unJC

instance NumericExpression JavaCode where
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

instance BooleanExpression JavaCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = jEquality
  (?!=) = typeBinExpr notEqualOp bool
  
instance ValueExpression JavaCode where
  inlineIf = G.inlineIf
  funcApp n t vs = do
    cm <- zoom lensVStoFS getModuleName
    mem <- getMethodExcMap
    modify (maybe id addExceptions (Map.lookup (cm ++ "." ++ n) mem))
    G.funcApp n t vs
  selfFuncApp n t vs = do
    slf <- self :: VS (JavaCode (Variable JavaCode))
    mem <- getMethodExcMap
    let tp = getTypeString (variableType slf)
    modify (maybe id addExceptions (Map.lookup (tp ++ "." ++ n) mem))
    G.selfFuncApp self n t vs
  extFuncApp l n t vs = do
    mem <- getMethodExcMap
    modify (maybe id addExceptions (Map.lookup (l ++ "." ++ n) mem))
    G.extFuncApp l n t vs
  newObj ot vs = do
    t <- ot
    mem <- getMethodExcMap
    let tp = getTypeString t
    modify (maybe id addExceptions (Map.lookup (tp ++ "." ++ tp) mem))
    G.newObj newObjDocD ot vs
  extNewObj _ = newObj

  exists = notNull
  notNull = G.notNull

instance InternalValue JavaCode where
  inputFunc = modify (addLangImportVS "java.util.Scanner") >> mkStateVal 
    (obj "Scanner") (parens $ text "new Scanner(System.in)")
  printFunc = mkStateVal void (text "System.out.print")
  printLnFunc = mkStateVal void (text "System.out.println")
  printFileFunc = on2StateValues (\v -> mkVal v . printFileDocD "print" . 
    valueDoc) void
  printFileLnFunc = on2StateValues (\v -> mkVal v . printFileDocD "println" . 
    valueDoc) void
  
  cast = jCast
  
  valuePrec = valPrec . unJC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector JavaCode where
  objAccess = G.objAccess
  ($.) = objAccess

  selfAccess = G.selfAccess

  listIndexExists = G.listIndexExists
  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf = G.indexOf "indexOf"

instance InternalSelector JavaCode where
  objMethodCall' f t o ps = do
    ob <- o
    mem <- getMethodExcMap
    let tp = getTypeString (valueType ob)
    modify (maybe id addExceptions (Map.lookup (tp ++ "." ++ f) mem))
    G.objMethodCall f t o ps
  objMethodCallNoParams' = G.objMethodCallNoParams

instance FunctionSym JavaCode where
  type Function JavaCode = FuncData
  func = G.func

  get = G.get
  set = G.set

  listSize = G.listSize
  listAdd = G.listAdd
  listAppend = G.listAppend

  iterBegin = G.iterBegin
  iterEnd = G.iterEnd

instance SelectorFunction JavaCode where
  listAccess = G.listAccess
  listSet = G.listSet
  at = listAccess

instance InternalFunction JavaCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

  listSizeFunc = G.listSizeFunc
  listAddFunc _ = G.listAddFunc "add"
  listAppendFunc = G.listAppendFunc "add"

  iterBeginFunc _ = error $ G.iterBeginError jName
  iterEndFunc _ = error $ G.iterEndError jName
  
  listAccessFunc = G.listAccessFunc' "get"
  listSetFunc v i toVal = func "set" (onStateValue valueType v) [intValue i, 
    toVal]

  functionType = onCodeValue funcType
  functionDoc = funcDoc . unJC

  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance InternalStatement JavaCode where
  printSt _ _ = G.printSt

  state = G.state
  loopState = G.loopState

  emptyState = G.emptyState
  statementDoc = fst . unJC
  statementTerm = snd . unJC
  
  stateFromData d t = toCode (d, t)

instance StatementSym JavaCode where
  -- Terminator determines how statements end
  type Statement JavaCode = (Doc, Terminator)
  assign = G.assign Semi
  assignToListIndex = G.assignToListIndex
  multiAssign _ _ = error $ G.multiAssignError jName
  (&=) = assign
  (&-=) = G.decrement
  (&+=) = G.increment
  (&++) = G.increment1
  (&~-) = G.decrement1

  varDec = G.varDec static_ dynamic_
  varDecDef = G.varDecDef
  listDec n v = zoom lensMStoVS v >>= (\v' -> G.listDec (listDecDocD v') 
    (litInt n) v)
  listDecDef v vs = modify (if null vs then id else addLangImport 
    "java.util.Arrays") >> objDecNew v [funcApp "Arrays.asList" 
    (onStateValue variableType v) vs | not (null vs)]
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew _ = objDecNew
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams _ = objDecNewNoParams
  constDecDef vr' vl' = zoom lensMStoVS $ on2StateValues (\vr vl -> mkSt $ 
    jConstDecDef vr vl) vr' vl'

  print = jOut False Nothing printFunc
  printLn = jOut True Nothing printLnFunc
  printStr = jOut False Nothing printFunc . litString
  printStrLn = jOut True Nothing printLnFunc . litString

  printFile f = jOut False (Just f) (printFileFunc f)
  printFileLn f = jOut True (Just f) (printFileLnFunc f)
  printFileStr f = jOut False (Just f) (printFileFunc f) . litString
  printFileStrLn f = jOut True (Just f) (printFileLnFunc f) . litString

  getInput v = v &= jInput (onStateValue variableType v) inputFunc
  discardInput = G.discardInput jDiscardInput
  getFileInput f v = v &= jInput (onStateValue variableType v) f
  discardFileInput = G.discardFileInput jDiscardInput

  openFileR = G.openFileR jOpenFileR
  openFileW = G.openFileW jOpenFileWorA
  openFileA = G.openFileA jOpenFileWorA
  closeFile = G.closeFile "close"

  getFileInputLine f v = v &= f $. func "nextLine" string []
  discardFileLine = G.discardFileLine "nextLine"
  stringSplit d vnew s = modify (addLangImport "java.util.Arrays") >> 
    onStateValue mkSt (zoom lensMStoVS $ jStringSplit vnew (funcApp 
    "Arrays.asList" (listType static_ string) 
    [s $. func "split" (listType static_ string) [litString [d]]]))

  stringListVals = G.stringListVals
  stringListLists = G.stringListLists

  break = toState $ mkSt breakDocD  -- I could have a JumpSym class with functions for "return $ text "break" and then reference those functions here?
  continue = toState $ mkSt continueDocD

  returnState = G.returnState Semi
  multiReturn _ = error $ G.multiReturnError jName

  valState = G.valState Semi

  comment = G.comment commentStart

  free _ = error $ G.freeError jName -- could set variable to null? Might be misleading.

  throw = G.throw jThrowDoc Semi

  initState = G.initState
  changeState = G.changeState

  initObserverList = G.initObserverList
  addObserver = G.addObserver

  inOutCall = jInOutCall funcApp
  selfInOutCall = jInOutCall selfFuncApp
  extInOutCall m = jInOutCall (extFuncApp m)

  multi = onStateList (on1CodeValue1List multiStateDocD endStatement)

instance ControlStatementSym JavaCode where
  ifCond = G.ifCond ifBodyStart elseIf blockEnd
  ifNoElse = G.ifNoElse
  switch  = G.switch
  switchAsIf = G.switchAsIf

  ifExists = G.ifExists

  for = G.for blockStart blockEnd
  forRange = G.forRange 
  forEach = G.forEach blockStart blockEnd iterForEachLabel iterInLabel
  while = G.while blockStart blockEnd

  tryCatch = G.tryCatch jTryCatch
  
  checkState = G.checkState
  notifyObservers = G.notifyObservers

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
  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym JavaCode where
  type Parameter JavaCode = ParamData
  param = G.param paramDocD
  pointerParam = param

instance InternalParam JavaCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameterDoc = paramDoc . unJC
  paramFromData v d = on2CodeValues pd v (toCode d)

instance MethodSym JavaCode where
  type Method JavaCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor ps is b = getClassName >>= (\n -> G.constructor n ps is b)
  destructor _ = error $ destructorError jName

  docMain = G.docMain

  function = G.function
  mainFunction = G.mainFunction string "main"

  docFunc = G.docFunc

  inOutMethod n = jInOut (method n)

  docInOutMethod n = jDocInOut (inOutMethod n)

  inOutFunc n = jInOut (function n)
    
  docInOutFunc n = jDocInOut (inOutFunc n)

instance InternalMethod JavaCode where
  intMethod m n s p t ps b = do
    tp <- t
    pms <- sequence ps
    bd <- b
    mem <- zoom lensMStoVS getMethodExcMap
    es <- getExceptions
    mn <- zoom lensMStoFS getModuleName
    let excs = maybe es (nub . (++ es)) (Map.lookup (key mn n) mem) 
        key mnm nm = mnm ++ "." ++ nm
    modify ((if m then setCurrMain else id) . addExceptionImports excs) 
    toState $ methodFromData Pub $ jMethod n (map exc excs) s p tp pms bd
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
  stateVarFromData = onStateValue toCode

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
  buildModule n = G.buildModule' n langImport
  
instance InternalMod JavaCode where
  moduleDoc = modDoc . unJC
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateModDoc f)

instance BlockCommentSym JavaCode where
  type BlockComment JavaCode = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unJC

odeImport :: String
odeImport = "org.apache.commons.math3.ode."

dblArray :: JavaCode (Type JavaCode)
dblArray = typeFromData (List Float) "double[]" (text "double[]")

jODEMethod :: ODEOptions JavaCode -> MS (JavaCode (Statement JavaCode))
jODEMethod opts = modify (addLibImport (odeImport ++ "nonstiff." ++ it)) >> 
  varDecDef (jODEIntVar m) (newObj (obj it) (jODEParams m))
  where m = solveMethod opts
        it = jODEInt m
        jODEParams RK45 = [stepSize opts, stepSize opts, absTol opts, 
          relTol opts]
        jODEParams Adams = [litInt 3, stepSize opts, stepSize opts, absTol opts,
          relTol opts]
        jODEParams _ = error "Chosen ODE method unavailable in Java"

jODEIntVar :: ODEMethod -> VS (JavaCode (Variable JavaCode))
jODEIntVar m = var "it" (obj $ jODEInt m)

jODEInt :: ODEMethod -> String
jODEInt RK45 = "DormandPrince54Integrator"
jODEInt Adams = "AdamsBashforthIntegrator"
jODEInt _ = error "Chosen ODE method unavailable in Java"

jODEFile :: ODEInfo JavaCode -> (FileData, GOOLState)
jODEFile info = (unJC fl, fst s)
  where (fl, s) = runState odeFile (initialState, initialFS)
        fode = "FirstOrderDifferentialEquations"
        dv = depVar info
        ovars = otherVars info 
        odeFile = join $ on1StateValue1List (\dpv ovs -> 
          let n = variableName dpv
              cn = n ++ "_ODE"
              dn = "d" ++ n 
              othVars = map (modify (setODEOthVars (map variableName 
                ovs)) >>) ovars
          in fileDoc (buildModule cn [] [zoom lensCStoMS (modify (addLibImport 
            (odeImport ++ fode))) >> modify (setClassName cn) >> classFromData 
            (on2StateLists (\svars mths 
              -> scopeDoc (public :: JavaCode (Scope JavaCode)) <+> classDec 
              <+> text cn <+> text "implements" <+> text fode <+> 
              keyDoc (blockStart :: JavaCode (Keyword JavaCode)) $$ 
              indentList [
                vcat (map stateVarDoc svars), 
                blank, 
                vibcat (map methodDoc mths)] 
              $$ keyDoc (blockEnd :: JavaCode (Keyword JavaCode))) 
              (map privMVar othVars) 
              (map (zoom lensCStoMS) [initializer (map param othVars) 
                (zip othVars (map valueOf othVars)),
              pubMethod "getDimension" int [] (oneLiner $ returnState $ 
                litInt 1),
              pubMethod "computeDerivatives" void (map param [var "t" float, 
                var n (toState dblArray), var dn (toState dblArray)]) (oneLiner 
                $ var (dn ++ "[0]") float &= (modify (setODEDepVars 
                [variableName dpv, dn] . setODEOthVars (map variableName ovs)) 
                >> ode info))]))])) 
            (zoom lensFStoVS dv) (map (zoom lensFStoVS) ovars)

jName :: String
jName = "Java"

jImport :: Label -> JavaCode (Keyword JavaCode) -> Doc
jImport n end = text ("import " ++ n) <> keyDoc end

jStringType :: (RenderSym repr) => VS (repr (Type repr))
jStringType = toState $ typeFromData String "String" (text "String")

jInfileType :: (RenderSym repr) => VS (repr (Type repr))
jInfileType = modifyReturn (addLangImportVS "java.util.Scanner") $ 
  typeFromData File "Scanner" (text "Scanner")

jOutfileType :: (RenderSym repr) => VS (repr (Type repr))
jOutfileType = modifyReturn (addLangImportVS "java.io.PrintWriter") $ 
  typeFromData File "PrintWriter" (text "PrintWriter")

jListType :: (RenderSym repr) => repr (Permanence repr) -> VS (repr (Type repr))
  -> VS (repr (Type repr))
jListType p t = modify (addLangImportVS $ "java.util." ++ render lst) >> 
  (t >>= (jListType' . getType))
  where jListType' Integer = toState $ typeFromData (List Integer) (render lst 
          ++ "<Integer>") (lst <> angles (text "Integer"))
        jListType' Float = toState $ typeFromData (List Float) (render lst ++ "<Double>") 
          (lst <> angles (text "Double"))
        jListType' _ = G.listType p t
        lst = keyDoc $ list p


jArrayType :: VS (JavaCode (Type JavaCode))
jArrayType = toState $ typeFromData (List $ Object "Object") "Object" 
  (text "Object[]")

jFileType :: (RenderSym repr) => VS (repr (Type repr))
jFileType = modifyReturn (addLangImportVS "java.io.File") $ typeFromData File 
  "File" (text "File")

jFileWriterType :: (RenderSym repr) => VS (repr (Type repr))
jFileWriterType = modifyReturn (addLangImportVS "java.io.FileWriter") $ 
  typeFromData File "FileWriter" (text "FileWriter")

jEquality :: VS (JavaCode (Value JavaCode)) -> VS (JavaCode (Value JavaCode)) 
  -> VS (JavaCode (Value JavaCode))
jEquality v1 v2 = v2 >>= jEquality' . getType . valueType
  where jEquality' String = objAccess v1 (func "equals" bool [v2])
        jEquality' _ = typeBinExpr equalOp bool v1 v2

jCast :: VS (JavaCode (Type JavaCode)) -> VS (JavaCode (Value JavaCode)) -> 
  VS (JavaCode (Value JavaCode))
jCast t v = join $ on2StateValues (\tp vl -> jCast' (getType tp) (getType $ 
  valueType vl) tp vl) t v
  where jCast' Float String _ _ = funcApp "Double.parseDouble" float [v]
        jCast' Integer (Enum _) _ _ = v $. func "ordinal" int []
        jCast' _ _ tp vl = mkStateVal t (castObjDocD (castDocD (getTypeDoc 
          tp)) (valueDoc vl))

jConstDecDef :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) 
  -> Doc
jConstDecDef v def = text "final" <+> getTypeDoc (variableType v) <+> 
  variableDoc v <+> equals <+> valueDoc def

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

jOut :: (RenderSym repr) => Bool -> Maybe (VS (repr (Value repr))) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> MS (repr (Statement repr))
jOut newLn f printFn v = zoom lensMStoVS v >>= jOut' . getType . valueType
  where jOut' (List (Object _)) = outDoc newLn f printFn v
        jOut' (List _) = printSt newLn f printFn v
        jOut' _ = outDoc newLn f printFn v

jDiscardInput :: (RenderSym repr) => repr (Value repr) -> Doc
jDiscardInput inFn = valueDoc inFn <> dot <> text "next()"

jInput :: (RenderSym repr) => VS (repr (Type repr)) -> VS (repr (Value repr)) 
  -> VS (repr (Value repr))
jInput = on2StateValues (\t -> mkVal t . jInput' (getType t))
  where jInput' Integer inFn = text "Integer.parseInt" <> parens (valueDoc inFn 
          <> dot <> text "nextLine()")
        jInput' Float inFn = text "Double.parseDouble" <> parens (valueDoc inFn 
          <> dot <> text "nextLine()")
        jInput' Boolean inFn = valueDoc inFn <> dot <> text "nextBoolean()"
        jInput' String inFn = valueDoc inFn <> dot <> text "nextLine()"
        jInput' Char inFn = valueDoc inFn <> dot <> text "next().charAt(0)"
        jInput' _ _ = error "Attempt to read value of unreadable type"

jOpenFileR :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr))
jOpenFileR n t = newObj t [newObj jFileType [n]]

jOpenFileWorA :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))
jOpenFileWorA n t wa = newObj t [newObj jFileWriterType [newObj jFileType [n], 
  wa]]

jStringSplit :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> VS Doc
jStringSplit = on2StateValues (\vnew s -> variableDoc vnew <+> equals <+> new 
  <+> getTypeDoc (variableType vnew) <> parens (valueDoc s))

jMethod :: (RenderSym repr) => Label -> [String] -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> [repr (Parameter repr)] -> 
  repr (Body repr) -> Doc
jMethod n es s p t ps b = vcat [
  scopeDoc s <+> permDoc p <+> getTypeDoc t <+> text n <> 
    parens (parameterList ps) <+> emptyIfNull es (text "throws" <+> 
    text (intercalate ", " (sort es))) <+> lbrace,
  indent $ bodyDoc b,
  rbrace]

jAssignFromArray :: Int -> [VS (JavaCode (Variable JavaCode))] -> 
  [MS (JavaCode (Statement JavaCode))]
jAssignFromArray _ [] = []
jAssignFromArray c (v:vs) = (v &= cast (onStateValue variableType v)
  (valueOf (var ("outputs[" ++ show c ++ "]") (onStateValue variableType v))))
  : jAssignFromArray (c+1) vs

jInOutCall :: (Label -> VS (JavaCode (Type JavaCode)) -> 
  [VS (JavaCode (Value JavaCode))] -> VS (JavaCode (Value JavaCode))) -> Label 
  -> [VS (JavaCode (Value JavaCode))] -> [VS (JavaCode (Variable JavaCode))] -> 
  [VS (JavaCode (Variable JavaCode))] -> MS (JavaCode (Statement JavaCode))
jInOutCall f n ins [] [] = valState $ f n void ins
jInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out) 
  ins
jInOutCall f n ins [] [out] = assign out $ f n (onStateValue variableType out) 
  (valueOf out : ins)
jInOutCall f n ins outs both = fCall rets
  where rets = both ++ outs
        fCall [x] = assign x $ f n (onStateValue variableType x) 
          (map valueOf both ++ ins)
        fCall xs = isOutputsDeclared >>= (\odec -> modify setOutputsDeclared >>
          multi ((if odec then assign else varDecDef) (var "outputs" 
          jArrayType) (f n jArrayType (map valueOf both ++ ins)) : 
          jAssignFromArray 0 xs))

jInOut :: (JavaCode (Scope JavaCode) -> JavaCode (Permanence JavaCode) -> 
    VS (JavaCode (Type JavaCode)) -> [MS (JavaCode (Parameter JavaCode))] -> 
    MS (JavaCode (Body JavaCode)) -> MS (JavaCode (Method JavaCode))) 
  -> JavaCode (Scope JavaCode) -> JavaCode (Permanence JavaCode) -> 
  [VS (JavaCode (Variable JavaCode))] -> [VS (JavaCode (Variable JavaCode))] -> 
  [VS (JavaCode (Variable JavaCode))] -> MS (JavaCode (Body JavaCode)) -> 
  MS (JavaCode (Method JavaCode))
jInOut f s p ins [] [] b = f s p void (map param ins) b
jInOut f s p ins [v] [] b = f s p (onStateValue variableType v) (map param ins) 
  (on3StateValues (on3CodeValues surroundBody) (varDec v) b (returnState $ 
  valueOf v))
jInOut f s p ins [] [v] b = f s p (onStateValue variableType v) 
  (map param $ v : ins) (on2StateValues (on2CodeValues appendToBody) b 
  (returnState $ valueOf v))
jInOut f s p ins outs both b = f s p (returnTp rets)
  (map param $ both ++ ins) (on3StateValues (on3CodeValues surroundBody) decls 
  b (returnSt rets))
  where returnTp [x] = onStateValue variableType x
        returnTp _ = jArrayType
        returnSt [x] = returnState $ valueOf x
        returnSt _ = multi (varDecDef outputs (valueOf (var 
          ("new Object[" ++ show (length rets) ++ "]") jArrayType))
          : assignArray 0 (map valueOf rets)
          ++ [returnState (valueOf outputs)])
        assignArray :: Int -> [VS (JavaCode (Value JavaCode))] -> 
          [MS (JavaCode (Statement JavaCode))]
        assignArray _ [] = []
        assignArray c (v:vs) = (var ("outputs[" ++ show c ++ "]") 
          (onStateValue valueType v) &= v) : assignArray (c+1) vs
        decls = multi $ map varDec outs
        rets = both ++ outs
        outputs = var "outputs" jArrayType

jDocInOut :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr) 
    -> [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
    [VS (repr (Variable repr))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, VS (repr (Variable repr)))] -> [(String, VS (repr (Variable repr)))]
  -> [(String, VS (repr (Variable repr)))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))
jDocInOut f s p desc is [] [] b = docFuncRepr desc (map fst is) [] 
  (f s p (map snd is) [] [] b)
jDocInOut f s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f s p (map snd is) [snd o] [] b)
jDocInOut f s p desc is [] [both] b = docFuncRepr desc (map fst (both : is)) 
  [fst both] (f s p (map snd is) [] [snd both] b)
jDocInOut f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is) 
  rets (f s p (map snd is) (map snd os) (map snd bs) b)
  where rets = "array containing the following values:" : map fst bs ++ 
          map fst os
