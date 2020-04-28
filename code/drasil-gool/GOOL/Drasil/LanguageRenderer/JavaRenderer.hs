{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java code is contained in this module
module GOOL.Drasil.LanguageRenderer.JavaRenderer (
  -- * Java Code Configuration -- defines syntax of all Java code
  JavaCode(..)
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, MSBody, VSType, SVariable, SValue, 
  MSStatement, MSParameter, SMethod, OOProg, ProgramSym(..), FileSym(..), 
  PermanenceSym(..), BodySym(..), oneLiner, BlockSym(..), TypeSym(..),
  TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..), Literal(..), 
  MathConstant(..), VariableValue(..), CommandLineArgs(..), 
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, selfFuncApp, extFuncApp, newObj,
  InternalValueExp(..), FunctionSym(..), ($.), GetSet(..), List(..), 
  InternalList(..), Iterator(..), StatementSym(..), AssignStatement(..), 
  (&=), DeclStatement(..), IOStatement(..), StringStatement(..), 
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..), 
  StatePattern(..), ObserverPattern(..), StrategyPattern(..), ScopeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(..), ImportSym(..), 
  ImportElim, PermElim(binding), RenderBody(..), BodyElim, RenderBlock(..), 
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..), 
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind), 
  RenderValue(..), ValueElim(valuePrec),  InternalGetSet(..), 
  InternalListFunc(..), InternalIterator(..), RenderFunction(..), 
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..), 
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm), 
  RenderScope(..), ScopeElim, MethodTypeSym(..), RenderParam(..), 
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim, 
  StateVarElim, RenderClass(..), ClassElim, RenderMod(..), ModuleElim, 
  BlockCommentSym(..), BlockCommentElim)
import qualified GOOL.Drasil.RendererClasses as RC (import', perm, body, block,
  type', uOp, bOp, variable, value, function, statement, scope, parameter,
  method, stateVar, class', module', blockComment')
import GOOL.Drasil.LanguageRenderer (dot, new, elseIfLabel, forLabel, 
  blockCmtStart, blockCmtEnd, docCmtStart, bodyStart, bodyEnd, endStatement, 
  commentStart, variableList, parameterList, appendToBody, surroundBody, 
  intValue)
import qualified GOOL.Drasil.LanguageRenderer as R (package, class', multiStmt, 
  body, printFile, param, listDec, classVar, cast, castObj, static, dynamic, 
  break, continue, private, public, blockCmt, docCmt, addComments, commentedMod,
  commentedItem)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmt, mkStateVal, mkVal,
  unOpPrec, powerPrec, unExpr, unExpr', unExprNumDbl, typeUnExpr, binExpr, 
  binExprNumDbl', typeBinExpr)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, int, listInnerType, obj, funcType, csc, sec, 
  cot, negateOp, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, 
  lessEqualOp, plusOp, minusOp, multOp, divideOp, moduloOp, var, staticVar, 
  arrayElem, litChar, litDouble, litInt, litString, valueOf, arg, argsList, 
  objAccess, objMethodCall, funcAppMixedArgs, selfFuncAppMixedArgs, 
  newObjMixedArgs, lambda, func, get, set, listAdd, listAppend, iterBegin, 
  iterEnd, listAccess, listSet, getFunc, setFunc, listAppendFunc, stmt, 
  loopStmt, emptyStmt, assign, increment, objDecNew, print, closeFile, 
  returnStmt, valStmt, comment, throw, ifCond, tryCatch, construct, param, 
  method, getMethod, setMethod, constructor, function, docFunc, buildClass, 
  implementingClass, docClass, commentedClass, modFromData, fileDoc, docMod, 
  fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (docFuncRepr)
import qualified GOOL.Drasil.LanguageRenderer.SemiPolymorphic as SP (  
  bindingError, extVar, classVar, objVarSelf, iterVar, extFuncAppMixedArgs, 
  indexOf, listAddFunc, iterBeginError, iterEndError, listDecDef', 
  discardFileLine, checkState, destructorError, stateVarDef, constVar, 
  intClass, objVar, arrayType, pi, notNull, printSt, arrayDec, 
  arrayDecDef, openFileR, openFileW, openFileA, forEach, docMain, mainFunction, 
  stateVar, buildModule', litArray, call', listSizeFunc, listAccessFunc', 
  funcDecDef, discardFileInput, extraClass)
import qualified GOOL.Drasil.LanguageRenderer.CLike as C (float, double, char, 
  listType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat, 
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, increment1, 
  varDec, varDecDef, listDec, extObjDecNew, discardInput, switch, for, forRange,
  while, notifyObservers, intFunc, multiAssignError, multiReturnError)
import qualified GOOL.Drasil.LanguageRenderer.Macros as M (ifExists, decrement, 
  decrement1, runStrategy, listSlice, stringListVals, stringListLists)
import GOOL.Drasil.AST (Terminator(..), FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateMod, MethodData(..), mthd, 
  updateMthd, OpData(..), ParamData(..), pd, ProgData(..), progD, TypeData(..), 
  td, ValData(..), vd, VarData(..), vard)
import GOOL.Drasil.CodeAnalysis (Exception(..), ExceptionType(..), exception, 
  stdExc, HasException(..))
import GOOL.Drasil.Helpers (angles, emptyIfNull, toCode, toState, onCodeValue, 
  onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues, 
  onCodeList, onStateList)
import GOOL.Drasil.State (VS, lensGStoFS, lensMStoFS, lensMStoVS, lensVStoFS, 
  lensVStoMS, modifyReturn, modifyReturnList, revFiles, addProgNameToPaths, 
  addLangImport, addLangImportVS, addExceptionImports, getModuleName, 
  setFileType, getClassName, setCurrMain, setOutputsDeclared, 
  isOutputsDeclared, getExceptions, getMethodExcMap, addExceptions)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (modify)
import qualified Data.Map as Map (lookup)
import Data.List (nub, intercalate, sort)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, 
  equals, vcat, lbrace, rbrace, colon)

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

instance OOProg JavaCode where

instance ProgramSym JavaCode where
  type Program JavaCode = ProgData
  prog n fs = modifyReturnList (map (zoom lensGStoFS) fs) (revFiles . 
    addProgNameToPaths n) (onCodeList (progD n . map (R.package n 
    endStatement)))

instance RenderSym JavaCode

instance FileSym JavaCode where
  type File JavaCode = FileData 
  fileDoc m = modify (setFileType Combined) >> G.fileDoc jExt top bottom m

  docMod = G.docMod jExt

instance RenderFile JavaCode where
  top _ = toCode empty
  bottom = toCode empty
  
  commentedMod cmt m = on2StateValues (on2CodeValues R.commentedMod) m cmt
  
  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym JavaCode where
  type Import JavaCode = Doc
  langImport n = toCode $ jImport n
  modImport = langImport

instance ImportElim JavaCode where
  import' = unJC

instance PermanenceSym JavaCode where
  type Permanence JavaCode = Doc
  static = toCode R.static
  dynamic = toCode R.dynamic

instance PermElim JavaCode where
  perm = unJC
  binding = error $ SP.bindingError jName

instance BodySym JavaCode where
  type Body JavaCode = Doc
  body = onStateList (onCodeList R.body)

  addComments s = onStateValue (onCodeValue (R.addComments s commentStart))

instance RenderBody JavaCode where
  multiBody = G.multiBody 

instance BodyElim JavaCode where
  body = unJC

instance BlockSym JavaCode where
  type Block JavaCode = Doc
  block = G.block

instance RenderBlock JavaCode where
  multiBlock = G.multiBlock

instance BlockElim JavaCode where
  block = unJC

instance TypeSym JavaCode where
  type Type JavaCode = TypeData
  bool = jBool
  int = G.int
  float = C.float
  double = C.double
  char = C.char
  string = jStringType
  infile = jInfileType
  outfile = jOutfileType
  listType = jListType "ArrayList"
  arrayType = SP.arrayType
  listInnerType = G.listInnerType
  obj = G.obj
  funcType = G.funcType
  iterator t = t
  void = C.void

instance TypeElim JavaCode where
  getType = cType . unJC
  getTypeString = typeString . unJC
  
instance RenderType JavaCode where
  typeFromData t s d = toCode $ td t s d

instance InternalTypeElim JavaCode where
  type' = typeDoc . unJC

instance UnaryOpSym JavaCode where
  type UnaryOp JavaCode = OpData
  notOp = C.notOp
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
  andOp = C.andOp
  orOp = C.orOp

instance OpElim JavaCode where
  uOp = opDoc . unJC
  bOp = opDoc . unJC
  uOpPrec = opPrec . unJC
  bOpPrec = opPrec . unJC

instance VariableSym JavaCode where
  type Variable JavaCode = VarData
  var = G.var
  staticVar = G.staticVar
  const = var
  extVar = SP.extVar
  self = C.self
  classVar = SP.classVar R.classVar
  extClassVar = classVar
  objVar = SP.objVar
  objVarSelf = SP.objVarSelf
  arrayElem i = G.arrayElem (litInt i)
  iterVar = SP.iterVar

instance VariableElim JavaCode where
  variableName = varName . unJC
  variableType = onCodeValue varType
  
instance InternalVarElim JavaCode where
  variableBind = varBind . unJC
  variable = varDoc . unJC

instance RenderVariable JavaCode where
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym JavaCode where
  type Value JavaCode = ValData
  valueType = onCodeValue valType

instance Literal JavaCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar
  litDouble = G.litDouble
  litFloat = C.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = SP.litArray
  litList t es = zoom lensVStoMS (modify (if null es then id else addLangImport 
    "java.util.Arrays")) >> newObj (listType t) [funcApp "Arrays.asList" 
    (listType t) es | not (null es)]

instance MathConstant JavaCode where
  pi = SP.pi

instance VariableValue JavaCode where
  valueOf = G.valueOf

instance CommandLineArgs JavaCode where
  arg n = G.arg (litInt n) argsList
  argsList = G.argsList "args"
  argExists i = listSize argsList ?> litInt (fromIntegral i)

instance NumericExpression JavaCode where
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

instance BooleanExpression JavaCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

instance Comparison JavaCode where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = jEquality
  (?!=) = typeBinExpr notEqualOp bool
  
instance ValueExpression JavaCode where
  inlineIf = C.inlineIf

  -- Exceptions from function/method calls should already be in the exception 
  -- map from the CodeInfo pass, but it's possible that one of the higher-level 
  -- functions implicitly calls these functions in the Java renderer, so we 
  -- also check here to add the exceptions from the called function to the map
  funcAppMixedArgs n t vs ns = addCallExcsCurrMod n >> 
    G.funcAppMixedArgs n t vs ns
  selfFuncAppMixedArgs n t ps ns = addCallExcsCurrMod n >> 
    G.selfFuncAppMixedArgs dot self n t ps ns
  extFuncAppMixedArgs l n t vs ns = do
    mem <- getMethodExcMap
    modify (maybe id addExceptions (Map.lookup (l ++ "." ++ n) mem))
    SP.extFuncAppMixedArgs l n t vs ns
  libFuncAppMixedArgs = C.libFuncAppMixedArgs
  newObjMixedArgs ot vs ns = addConstructorCallExcsCurrMod ot (\t -> 
    G.newObjMixedArgs "new " t vs ns)
  extNewObjMixedArgs l ot vs ns = do
    t <- ot
    mem <- getMethodExcMap
    let tp = getTypeString t
    modify (maybe id addExceptions (Map.lookup (l ++ "." ++ tp) mem))
    newObjMixedArgs (toState t) vs ns
  libNewObjMixedArgs = C.libNewObjMixedArgs

  lambda = G.lambda jLambda

  notNull = SP.notNull

instance RenderValue JavaCode where
  inputFunc = modify (addLangImportVS "java.util.Scanner") >> mkStateVal 
    (obj "Scanner") (parens $ text "new Scanner(System.in)")
  printFunc = mkStateVal void (text "System.out.print")
  printLnFunc = mkStateVal void (text "System.out.println")
  printFileFunc = on2StateValues (\v -> mkVal v . R.printFile "print" . 
    RC.value) void
  printFileLnFunc = on2StateValues (\v -> mkVal v . R.printFile "println" . 
    RC.value) void
  
  cast = jCast

  call = SP.call' jName
  
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance ValueElim JavaCode where
  valuePrec = valPrec . unJC
  value = val . unJC

instance InternalValueExp JavaCode where
  objMethodCallMixedArgs' f t o ps ns = do
    ob <- o
    mem <- getMethodExcMap
    let tp = getTypeString (valueType ob)
    modify (maybe id addExceptions (Map.lookup (tp ++ "." ++ f) mem))
    G.objMethodCall f t o ps ns

instance FunctionSym JavaCode where
  type Function JavaCode = FuncData
  func = G.func
  objAccess = G.objAccess

instance GetSet JavaCode where
  get = G.get
  set = G.set

instance List JavaCode where
  listSize = C.listSize
  listAdd = G.listAdd
  listAppend = G.listAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = SP.indexOf "indexOf"

instance InternalList JavaCode where
  listSlice' = M.listSlice

instance Iterator JavaCode where
  iterBegin = G.iterBegin
  iterEnd = G.iterEnd

instance InternalGetSet JavaCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

instance InternalListFunc JavaCode where
  listSizeFunc = SP.listSizeFunc
  listAddFunc _ = SP.listAddFunc "add"
  listAppendFunc = G.listAppendFunc "add"
  listAccessFunc = SP.listAccessFunc' "get"
  listSetFunc v i toVal = func "set" (onStateValue valueType v) [intValue i, 
    toVal]

instance InternalIterator JavaCode where
  iterBeginFunc _ = error $ SP.iterBeginError jName
  iterEndFunc _ = error $ SP.iterEndError jName

instance RenderFunction JavaCode where
  funcFromData d = onStateValue (onCodeValue (`fd` d))
  
instance FunctionElim JavaCode where
  functionType = onCodeValue fType
  function = funcDoc . unJC

instance InternalAssignStmt JavaCode where
  multiAssign _ _ = error $ C.multiAssignError jName

instance InternalIOStmt JavaCode where
  printSt _ _ = SP.printSt

instance InternalControlStmt JavaCode where
  multiReturn _ = error $ C.multiReturnError jName

instance RenderStatement JavaCode where
  stmt = G.stmt
  loopStmt = G.loopStmt

  emptyStmt = G.emptyStmt
  
  stmtFromData d t = toCode (d, t)

instance StatementElim JavaCode where
  statement = fst . unJC
  statementTerm = snd . unJC

instance StatementSym JavaCode where
  -- Terminator determines how statements end
  type Statement JavaCode = (Doc, Terminator)
  valStmt = G.valStmt Semi
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement JavaCode where
  assign = G.assign Semi
  (&-=) = M.decrement
  (&+=) = G.increment
  (&++) = C.increment1
  (&--) = M.decrement1

instance DeclStatement JavaCode where
  varDec = C.varDec static dynamic empty
  varDecDef = C.varDecDef
  listDec n v = zoom lensMStoVS v >>= (\v' -> C.listDec (R.listDec v') 
    (litInt n) v)
  listDecDef = SP.listDecDef'
  arrayDec n = SP.arrayDec (litInt n)
  arrayDecDef = SP.arrayDecDef
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = C.extObjDecNew
  constDecDef vr' vl' = zoom lensMStoVS $ on2StateValues (\vr vl -> mkStmt $ 
    jConstDecDef vr vl) vr' vl'
  funcDecDef = SP.funcDecDef

instance IOStatement JavaCode where
  print = jOut False Nothing printFunc
  printLn = jOut True Nothing printLnFunc
  printStr = jOut False Nothing printFunc . litString
  printStrLn = jOut True Nothing printLnFunc . litString

  printFile f = jOut False (Just f) (printFileFunc f)
  printFileLn f = jOut True (Just f) (printFileLnFunc f)
  printFileStr f = jOut False (Just f) (printFileFunc f) . litString
  printFileStrLn f = jOut True (Just f) (printFileLnFunc f) . litString

  getInput v = v &= jInput (onStateValue variableType v) inputFunc
  discardInput = C.discardInput jDiscardInput
  getFileInput f v = v &= jInput (onStateValue variableType v) f
  discardFileInput = SP.discardFileInput jDiscardInput

  openFileR = SP.openFileR jOpenFileR
  openFileW = SP.openFileW jOpenFileWorA
  openFileA = SP.openFileA jOpenFileWorA
  closeFile = G.closeFile "close"

  getFileInputLine f v = v &= f $. func "nextLine" string []
  discardFileLine = SP.discardFileLine "nextLine"
  getFileInputAll f v = while (f $. func "hasNextLine" bool [])
    (oneLiner $ valStmt $ listAppend (valueOf v) (f $. func "nextLine" string []))

instance StringStatement JavaCode where
  stringSplit d vnew s = modify (addLangImport "java.util.Arrays") >> 
    onStateValue mkStmt (zoom lensMStoVS $ jStringSplit vnew (funcApp 
    "Arrays.asList" (listType string) 
    [s $. func "split" (listType string) [litString [d]]]))

  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FuncAppStatement JavaCode where
  inOutCall = jInOutCall funcApp
  selfInOutCall = jInOutCall selfFuncApp
  extInOutCall m = jInOutCall (extFuncApp m)

instance CommentStatement JavaCode where
  comment = G.comment commentStart

instance ControlStatement JavaCode where
  break = toState $ mkStmt R.break
  continue = toState $ mkStmt R.continue

  returnStmt = G.returnStmt Semi
  
  throw = G.throw jThrowDoc Semi

  ifCond = G.ifCond bodyStart elseIfLabel bodyEnd
  switch  = C.switch

  ifExists = M.ifExists

  for = C.for bodyStart bodyEnd
  forRange = C.forRange 
  forEach = SP.forEach bodyStart bodyEnd forLabel colon
  while = C.while bodyStart bodyEnd

  tryCatch = G.tryCatch jTryCatch
  
instance StatePattern JavaCode where 
  checkState = SP.checkState

instance ObserverPattern JavaCode where
  notifyObservers = C.notifyObservers

instance StrategyPattern JavaCode where
  runStrategy = M.runStrategy

instance ScopeSym JavaCode where
  type Scope JavaCode = Doc
  private = toCode R.private
  public = toCode R.public

instance RenderScope JavaCode where
  scopeFromData _ = toCode
  
instance ScopeElim JavaCode where
  scope = unJC

instance MethodTypeSym JavaCode where
  type MethodType JavaCode = TypeData
  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym JavaCode where
  type Parameter JavaCode = ParamData
  param = G.param R.param
  pointerParam = param

instance RenderParam JavaCode where
  paramFromData v d = on2CodeValues pd v (toCode d)

instance ParamElim JavaCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unJC

instance MethodSym JavaCode where
  type Method JavaCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor ps is b = getClassName >>= (\n -> G.constructor n ps is b)

  docMain = SP.docMain

  function = G.function
  mainFunction = SP.mainFunction string "main"

  docFunc = G.docFunc

  inOutMethod n = jInOut (method n)

  docInOutMethod n = jDocInOut (inOutMethod n)

  inOutFunc n = jInOut (function n)
    
  docInOutFunc n = jDocInOut (inOutFunc n)

instance RenderMethod JavaCode where
  intMethod m n s p t ps b = do
    tp <- t
    pms <- sequence ps
    bd <- b
    mem <- zoom lensMStoVS getMethodExcMap
    es <- getExceptions
    mn <- zoom lensMStoFS getModuleName
    let excs = map (unJC . toConcreteExc) $ maybe es (nub . (++ es)) 
          (Map.lookup (key mn n) mem) 
        key mnm nm = mnm ++ "." ++ nm
    modify ((if m then setCurrMain else id) . addExceptionImports excs) 
    toState $ toCode $ mthd $ jMethod n (map exc excs) s p tp pms bd
  intFunc = C.intFunc
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m 
    (onStateValue (onCodeValue R.commentedItem) cmt)
    
  destructor _ = error $ SP.destructorError jName
  
instance MethodElim JavaCode where
  method = mthdDoc . unJC

instance StateVarSym JavaCode where
  type StateVar JavaCode = Doc
  stateVar = SP.stateVar
  stateVarDef _ = SP.stateVarDef
  constVar _ = SP.constVar (RC.perm (static :: JavaCode (Permanence JavaCode)))
  
instance StateVarElim JavaCode where
  stateVar = unJC

instance ClassSym JavaCode where
  type Class JavaCode = Doc
  buildClass = G.buildClass
  extraClass = SP.extraClass
  implementingClass = G.implementingClass

  docClass = G.docClass

instance RenderClass JavaCode where
  intClass = SP.intClass R.class'
  
  inherit n = toCode $ maybe empty ((text "extends" <+>) . text) n
  implements is = toCode $ text "implements" <+> text (intercalate ", " is)

  commentedClass = G.commentedClass
  
instance ClassElim JavaCode where
  class' = unJC

instance ModuleSym JavaCode where
  type Module JavaCode = ModData
  buildModule n = SP.buildModule' n langImport
  
instance RenderMod JavaCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)
  
instance ModuleElim JavaCode where
  module' = modDoc . unJC

instance BlockCommentSym JavaCode where
  type BlockComment JavaCode = Doc
  blockComment lns = toCode $ R.blockCmt lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns docCmtStart 
    blockCmtEnd)

instance BlockCommentElim JavaCode where
  blockComment' = unJC

instance HasException JavaCode where
  toConcreteExc Standard = toCode $ stdExc "Exception"
  toConcreteExc FileNotFound = toCode $ exception "java.io" 
    "FileNotFoundException"
  toConcreteExc IO = toCode $ exception "java.io" "IOException"

jName :: String
jName = "Java"

jImport :: Label -> Doc
jImport n = text ("import " ++ n) <> endStatement

jBool :: (RenderSym r) => VSType r
jBool = toState $ typeFromData Boolean "boolean" (text "boolean")

jStringType :: (RenderSym r) => VSType r
jStringType = toState $ typeFromData String "String" (text "String")

jInfileType :: (RenderSym r) => VSType r
jInfileType = modifyReturn (addLangImportVS "java.util.Scanner") $ 
  typeFromData File "Scanner" (text "Scanner")

jOutfileType :: (RenderSym r) => VSType r
jOutfileType = modifyReturn (addLangImportVS "java.io.PrintWriter") $ 
  typeFromData File "PrintWriter" (text "PrintWriter")

jListType :: (RenderSym r) => String -> VSType r -> VSType r
jListType l t = modify (addLangImportVS $ "java.util." ++ l) >> 
  (t >>= (jListType' . getType))
  where jListType' Integer = toState $ typeFromData (List Integer) 
          (l ++ "<Integer>") (lst <> angles (text "Integer"))
        jListType' Float = toState $ typeFromData (List Float) 
          (l ++ "<Float>") (lst <> angles (text "Float"))
        jListType' Double = toState $ typeFromData (List Double) 
          (l ++ "<Double>") (lst <> angles (text "Double"))
        jListType' _ = C.listType l t
        lst = text l

jArrayType :: VSType JavaCode
jArrayType = arrayType (obj "Object")

jFileType :: (RenderSym r) => VSType r
jFileType = modifyReturn (addLangImportVS "java.io.File") $ typeFromData File 
  "File" (text "File")

jFileWriterType :: (RenderSym r) => VSType r
jFileWriterType = modifyReturn (addLangImportVS "java.io.FileWriter") $ 
  typeFromData File "FileWriter" (text "FileWriter")

jEquality :: SValue JavaCode -> SValue JavaCode -> SValue JavaCode
jEquality v1 v2 = v2 >>= jEquality' . getType . valueType
  where jEquality' String = objAccess v1 (func "equals" bool [v2])
        jEquality' _ = typeBinExpr equalOp bool v1 v2

jLambda :: (RenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
jLambda ps ex = parens (variableList ps) <+> text "->" <+> RC.value ex

jCast :: VSType JavaCode -> SValue JavaCode -> SValue JavaCode
jCast t v = join $ on2StateValues (\tp vl -> jCast' (getType tp) (getType $ 
  valueType vl) tp vl) t v
  where jCast' Double String _ _ = funcApp "Double.parseDouble" double [v]
        jCast' Float String _ _ = funcApp "Float.parseFloat" float [v]
        jCast' _ _ tp vl = mkStateVal t (R.castObj (R.cast (RC.type' 
          tp)) (RC.value vl))

jConstDecDef :: (RenderSym r) => r (Variable r) -> r (Value r) -> Doc
jConstDecDef v def = text "final" <+> RC.type' (variableType v) <+> 
  RC.variable v <+> equals <+> RC.value def

jThrowDoc :: (RenderSym r) => r (Value r) -> Doc
jThrowDoc errMsg = text "throw new" <+> text "Exception" <> parens (RC.value 
  errMsg)

jTryCatch :: (RenderSym r) => r (Body r) -> r (Body r) -> Doc
jTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent $ RC.body tb,
  rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> 
    lbrace,
  indent $ RC.body cb,
  rbrace]

jOut :: (RenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r -> 
  MSStatement r
jOut newLn f printFn v = zoom lensMStoVS v >>= jOut' . getType . valueType
  where jOut' (List (Object _)) = G.print newLn f printFn v
        jOut' (List _) = printSt newLn f printFn v
        jOut' _ = G.print newLn f printFn v

jDiscardInput :: (RenderSym r) => r (Value r) -> Doc
jDiscardInput inFn = RC.value inFn <> dot <> text "next()"

jInput :: (RenderSym r) => VSType r -> SValue r -> SValue r
jInput = on2StateValues (\t -> mkVal t . jInput' (getType t))
  where jInput' Integer inFn = text "Integer.parseInt" <> parens (RC.value inFn 
          <> dot <> text "nextLine()")
        jInput' Float inFn = text "Float.parseFloat" <> parens (RC.value inFn 
          <> dot <> text "nextLine()")
        jInput' Double inFn = text "Double.parseDouble" <> parens (RC.value 
          inFn <> dot <> text "nextLine()")
        jInput' Boolean inFn = RC.value inFn <> dot <> text "nextBoolean()"
        jInput' String inFn = RC.value inFn <> dot <> text "nextLine()"
        jInput' Char inFn = RC.value inFn <> dot <> text "next().charAt(0)"
        jInput' _ _ = error "Attempt to read value of unreadable type"

jOpenFileR :: (RenderSym r) => SValue r -> VSType r -> SValue r
jOpenFileR n t = newObj t [newObj jFileType [n]]

jOpenFileWorA :: (RenderSym r) => SValue r -> VSType r -> SValue r -> SValue r
jOpenFileWorA n t wa = newObj t [newObj jFileWriterType [newObj jFileType [n], 
  wa]]

jStringSplit :: (RenderSym r) => SVariable r -> SValue r -> VS Doc
jStringSplit = on2StateValues (\vnew s -> RC.variable vnew <+> equals <+> new 
  <+> RC.type' (variableType vnew) <> parens (RC.value s))

jMethod :: (RenderSym r) => Label -> [String] -> r (Scope r) -> r (Permanence r)
  -> r (Type r) -> [r (Parameter r)] -> r (Body r) -> Doc
jMethod n es s p t ps b = vcat [
  RC.scope s <+> RC.perm p <+> RC.type' t <+> text n <> 
    parens (parameterList ps) <+> emptyIfNull es (text "throws" <+> 
    text (intercalate ", " (sort es))) <+> lbrace,
  indent $ RC.body b,
  rbrace]

jAssignFromArray :: Integer -> [SVariable JavaCode] -> [MSStatement JavaCode]
jAssignFromArray _ [] = []
jAssignFromArray c (v:vs) = (v &= cast (onStateValue variableType v)
  (valueOf $ arrayElem c outputs)) : jAssignFromArray (c+1) vs
  where outputs = var "outputs" jArrayType

jInOutCall :: (Label -> VSType JavaCode -> [SValue JavaCode] -> 
  SValue JavaCode) -> Label -> [SValue JavaCode] -> [SVariable JavaCode] -> 
  [SVariable JavaCode] -> MSStatement JavaCode
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
          multi ((if odec then assign else varDecDef) (var "outputs" 
          jArrayType) (f n jArrayType (map valueOf both ++ ins)) : 
          jAssignFromArray 0 xs))

jInOut :: (JavaCode (Scope JavaCode) -> JavaCode (Permanence JavaCode) -> 
    VSType JavaCode -> [MSParameter JavaCode] -> MSBody JavaCode -> 
    SMethod JavaCode) 
  -> JavaCode (Scope JavaCode) -> JavaCode (Permanence JavaCode) -> 
  [SVariable JavaCode] -> [SVariable JavaCode] -> [SVariable JavaCode] -> 
  MSBody JavaCode -> SMethod JavaCode
jInOut f s p ins [] [] b = f s p void (map param ins) b
jInOut f s p ins [v] [] b = f s p (onStateValue variableType v) (map param ins) 
  (on3StateValues (on3CodeValues surroundBody) (varDec v) b (returnStmt $ 
  valueOf v))
jInOut f s p ins [] [v] b = f s p (onStateValue variableType v) 
  (map param $ v : ins) (on2StateValues (on2CodeValues appendToBody) b 
  (returnStmt $ valueOf v))
jInOut f s p ins outs both b = f s p (returnTp rets)
  (map param $ both ++ ins) (on3StateValues (on3CodeValues surroundBody) decls 
  b (returnSt rets))
  where returnTp [x] = onStateValue variableType x
        returnTp _ = jArrayType
        returnSt [x] = returnStmt $ valueOf x
        returnSt _ = multi (arrayDec (toInteger $ length rets) outputs
          : assignArray 0 (map valueOf rets)
          ++ [returnStmt (valueOf outputs)])
        assignArray :: Integer -> [SValue JavaCode] -> [MSStatement JavaCode]
        assignArray _ [] = []
        assignArray c (v:vs) = (arrayElem c outputs &= v) : assignArray (c+1) vs
        decls = multi $ map varDec outs
        rets = both ++ outs
        outputs = var "outputs" jArrayType

jDocInOut :: (RenderSym r) => (r (Scope r) -> r (Permanence r) -> [SVariable r] 
    -> [SVariable r] -> [SVariable r] -> MSBody r -> SMethod r)
  -> r (Scope r) -> r (Permanence r) -> String -> [(String, SVariable r)] -> 
  [(String, SVariable r)] -> [(String, SVariable r)] -> MSBody r -> SMethod r
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

addCallExcsCurrMod :: String -> VS ()
addCallExcsCurrMod n = do
  cm <- zoom lensVStoFS getModuleName
  mem <- getMethodExcMap
  modify (maybe id addExceptions (Map.lookup (cm ++ "." ++ n) mem))

addConstructorCallExcsCurrMod :: (RenderSym r) => VSType r -> 
  (VSType r -> SValue r) -> SValue r
addConstructorCallExcsCurrMod ot f = do
  t <- ot
  cm <- zoom lensVStoFS getModuleName
  mem <- getMethodExcMap
  let tp = getTypeString t
  modify (maybe id addExceptions (Map.lookup (cm ++ "." ++ tp) mem))
  f (toState t)