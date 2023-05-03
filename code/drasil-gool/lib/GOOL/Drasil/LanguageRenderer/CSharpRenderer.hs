{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# code is contained in this module
module GOOL.Drasil.LanguageRenderer.CSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..), csName, csVersion
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, MSBody, VSType, SVariable, SValue, 
  VSFunction, MSStatement, MSParameter, SMethod, OOProg, ProgramSym(..), 
  FileSym(..), PermanenceSym(..), BodySym(..), oneLiner, BlockSym(..), 
  TypeSym(..), TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..), 
  Argument(..), Literal(..), MathConstant(..), VariableValue(..), 
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..), 
  Comparison(..), ValueExpression(..), funcApp, selfFuncApp, extFuncApp, 
  newObj, InternalValueExp(..), objMethodCallNoParams, FunctionSym(..), ($.), 
  GetSet(..), List(..), InternalList(..), StatementSym(..), 
  AssignStatement(..), (&=), DeclStatement(..), IOStatement(..), 
  StringStatement(..), FuncAppStatement(..), CommentStatement(..), 
  ControlStatement(..), StatePattern(..), ObserverPattern(..), 
  StrategyPattern(..), ScopeSym(..), ParameterSym(..), MethodSym(..), 
  StateVarSym(..), ClassSym(..), ModuleSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(..), ImportSym(..), 
  ImportElim, PermElim(binding), RenderBody(..), BodyElim, RenderBlock(..), 
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..), 
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind), 
  RenderValue(..), ValueElim(valuePrec), InternalGetSet(..), InternalListFunc(..), 
  RenderFunction(..), FunctionElim(functionType), InternalAssignStmt(..), 
  InternalIOStmt(..), InternalControlStmt(..), RenderStatement(..), 
  StatementElim(statementTerm), RenderScope(..), ScopeElim, MethodTypeSym(..), 
  RenderParam(..), ParamElim(parameterName, parameterType), RenderMethod(..), 
  MethodElim, StateVarElim, RenderClass(..), ClassElim, RenderMod(..), ModuleElim, 
  BlockCommentSym(..), BlockCommentElim)
import qualified GOOL.Drasil.RendererClasses as RC (import', perm, body, block,
  type', uOp, bOp, variable, value, function, statement, scope, parameter,
  method, stateVar, class', module', blockComment')
import GOOL.Drasil.LanguageRenderer (new, dot, blockCmtStart, blockCmtEnd, 
  docCmtStart, bodyStart, bodyEnd, endStatement, commentStart, elseIfLabel, 
  inLabel, tryLabel, catchLabel, throwLabel, exceptionObj', new', listSep',
  args, nullLabel, listSep, access, containing, mathFunc, valueList, 
  variableList, appendToBody, surroundBody)
import qualified GOOL.Drasil.LanguageRenderer as R (class', multiStmt, body, 
  printFile, param, method, listDec, classVar, func, cast, listSetFunc, 
  castObj, static, dynamic, break, continue, private, public, blockCmt, docCmt, 
  addComments, commentedMod, commentedItem)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmt, 
  mkStateVal, mkVal, VSOp, unOpPrec, powerPrec, unExpr, unExpr', 
  unExprNumDbl, typeUnExpr, binExpr, binExprNumDbl', typeBinExpr)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, listInnerType, obj, csc, sec, cot, 
  negateOp, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, 
  lessEqualOp, plusOp, minusOp, multOp, divideOp, moduloOp, var, staticVar, 
  objVar, arrayElem, litChar, litDouble, litInt, litString, valueOf, arg, 
  argsList, objAccess, objMethodCall, call, funcAppMixedArgs, 
  selfFuncAppMixedArgs, newObjMixedArgs, lambda, func, get, set, listAdd, 
  listAppend, listAccess, listSet, getFunc, setFunc, listAppendFunc, stmt, 
  loopStmt, emptyStmt, assign, subAssign, increment, objDecNew, print, 
  closeFile, returnStmt, valStmt, comment, throw, ifCond, tryCatch, construct, 
  param, method, getMethod, setMethod, function, buildClass, implementingClass, 
  commentedClass, modFromData, fileDoc, fileFromData)
import qualified GOOL.Drasil.LanguageRenderer.CommonPseudoOO as CP (int,
  constructor, doxFunc, doxClass, doxMod, extVar, classVar, objVarSelf,
  extFuncAppMixedArgs, indexOf, listAddFunc, discardFileLine, intClass, 
  arrayType, pi, printSt, arrayDec, arrayDecDef, openFileA, forEach, docMain, 
  mainFunction, buildModule', string, constDecDef, docInOutFunc, bindingError, 
  notNull, listDecDef, destructorError, stateVarDef, constVar, listSetFunc, 
  extraClass, listAccessFunc, doubleRender, openFileR, openFileW, stateVar, 
  inherit, implements)
import qualified GOOL.Drasil.LanguageRenderer.CLike as C (float, double, char, 
  listType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat, 
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, increment1, 
  decrement1, varDec, varDecDef, listDec, extObjDecNew, switch, for, while, 
  intFunc, multiAssignError, multiReturnError, multiTypeError)
import qualified GOOL.Drasil.LanguageRenderer.Macros as M (ifExists, 
  runStrategy, listSlice, stringListVals, stringListLists, forRange, 
  notifyObservers, checkState)
import GOOL.Drasil.AST (Terminator(..), FileType(..), FileData(..), fileD, 
  FuncData(..), fd, ModData(..), md, updateMod, MethodData(..), mthd, 
  updateMthd, OpData(..), ParamData(..), pd, updateParam, ProgData(..), progD, 
  TypeData(..), td, ValData(..), vd, updateValDoc, Binding(..), VarData(..), 
  vard)
import GOOL.Drasil.Helpers (angles, hicat, toCode, toState, onCodeValue, 
  onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues, 
  on2StateWrapped, onCodeList, onStateList)
import GOOL.Drasil.State (VS, lensGStoFS, lensMStoVS, modifyReturn, revFiles,
  addLangImport, addLangImportVS, setFileType, getClassName, setCurrMain)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Control.Lens.Zoom (zoom)
import Control.Monad (join)
import Control.Monad.State (modify)
import Data.Composition ((.:))
import Data.List (intercalate)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($$), parens, empty,
  equals, vcat, lbrace, rbrace, braces, colon, space, quotes)

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

instance OOProg CSharpCode where

instance ProgramSym CSharpCode where
  type Program CSharpCode = ProgData
  prog n files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n) fs

instance RenderSym CSharpCode

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

instance PermanenceSym CSharpCode where
  type Permanence CSharpCode = Doc
  static = toCode R.static
  dynamic = toCode R.dynamic

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
  type Type CSharpCode = TypeData
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
  listInnerType = G.listInnerType
  obj = G.obj
  funcType = csFuncType
  void = C.void

instance TypeElim CSharpCode where
  getType = cType . unCSC
  getTypeString = typeString . unCSC
  
instance RenderType CSharpCode where
  multiType _ = error $ C.multiTypeError csName
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim CSharpCode where
  type' = typeDoc . unCSC

instance UnaryOpSym CSharpCode where
  type UnaryOp CSharpCode = OpData
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
  powerOp = addSystemImport $ powerPrec $ mathFunc "Pow"
  moduloOp = G.moduloOp
  andOp = C.andOp
  orOp = C.orOp

instance OpElim CSharpCode where
  uOp = opDoc . unCSC
  bOp = opDoc . unCSC
  uOpPrec = opPrec . unCSC
  bOpPrec = opPrec . unCSC

instance VariableSym CSharpCode where
  type Variable CSharpCode = VarData
  var = G.var
  staticVar = G.staticVar
  constant = var
  extVar = CP.extVar
  self = C.self
  classVar = CP.classVar R.classVar
  extClassVar = classVar
  objVar = G.objVar
  objVarSelf = CP.objVarSelf
  arrayElem i = G.arrayElem (litInt i)

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
  litList = csLitList listType

instance MathConstant CSharpCode where
  pi = CP.pi

instance VariableValue CSharpCode where
  valueOf = G.valueOf

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
  selfFuncAppMixedArgs = G.selfFuncAppMixedArgs dot self
  extFuncAppMixedArgs = CP.extFuncAppMixedArgs
  libFuncAppMixedArgs = C.libFuncAppMixedArgs
  newObjMixedArgs = G.newObjMixedArgs (new ++ " ")
  extNewObjMixedArgs _ = newObjMixedArgs
  libNewObjMixedArgs = C.libNewObjMixedArgs

  lambda = G.lambda csLambda

  notNull = CP.notNull nullLabel

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
  
  valFromData p t' d = do 
    t <- t' 
    toState $ on2CodeValues (vd p) t (toCode d)
  
instance ValueElim CSharpCode where
  valuePrec = valPrec . unCSC
  value = val . unCSC
  
instance InternalValueExp CSharpCode where
  objMethodCallMixedArgs' = G.objMethodCall

instance FunctionSym CSharpCode where
  type Function CSharpCode = FuncData
  func = G.func
  objAccess = G.objAccess

instance GetSet CSharpCode where
  get = G.get
  set = G.set

instance List CSharpCode where
  listSize = C.listSize
  listAdd = G.listAdd
  listAppend = G.listAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = CP.indexOf csIndex
  
instance InternalList CSharpCode where
  listSlice' = M.listSlice

instance InternalGetSet CSharpCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

instance InternalListFunc CSharpCode where
  listSizeFunc = funcFromData (R.func csListSize) int
  listAddFunc _ = CP.listAddFunc csListAdd
  listAppendFunc = G.listAppendFunc csListAppend
  listAccessFunc = CP.listAccessFunc
  listSetFunc = CP.listSetFunc R.listSetFunc
    
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

  emptyStmt = G.emptyStmt
  
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim CSharpCode where
  statement = fst . unCSC
  statementTerm = snd . unCSC

instance StatementSym CSharpCode where
  type Statement CSharpCode = (Doc, Terminator)
  valStmt = G.valStmt Semi
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement CSharpCode where
  assign = G.assign Semi
  (&-=) = G.subAssign Semi
  (&+=) = G.increment
  (&++) = C.increment1
  (&--) = C.decrement1

instance DeclStatement CSharpCode where
  varDec v = zoom lensMStoVS v >>= (\v' -> csVarDec (variableBind v') $ 
    C.varDec static dynamic empty v)
  varDecDef = C.varDecDef Semi
  listDec n v = zoom lensMStoVS v >>= (\v' -> C.listDec (R.listDec v') 
    (litInt n) v)
  listDecDef = CP.listDecDef
  arrayDec n = CP.arrayDec (litInt n)
  arrayDecDef = CP.arrayDecDef
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = C.extObjDecNew
  constDecDef = CP.constDecDef
  funcDecDef = csFuncDecDef

instance IOStatement CSharpCode where
  print      = G.print False Nothing printFunc
  printLn    = G.print True  Nothing printLnFunc
  printStr   = G.print False Nothing printFunc   . litString
  printStrLn = G.print True  Nothing printLnFunc . litString

  printFile f      = G.print False (Just f) (printFileFunc f)
  printFileLn f    = G.print True  (Just f) (printFileLnFunc f)
  printFileStr f   = G.print False (Just f) (printFileFunc f)   . litString
  printFileStrLn f = G.print True  (Just f) (printFileLnFunc f) . litString

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
  selfInOutCall = csInOutCall selfFuncApp
  extInOutCall m = csInOutCall (extFuncApp m)

instance CommentStatement CSharpCode where
  comment = G.comment commentStart

instance ControlStatement CSharpCode where
  break =  mkStmt R.break
  continue =  mkStmt R.continue

  returnStmt = G.returnStmt Semi
  
  throw msg = do
    modify (addLangImport csSystem)
    G.throw csThrowDoc Semi msg

  ifCond = G.ifCond parens bodyStart elseIfLabel bodyEnd
  switch = C.switch parens break

  ifExists = M.ifExists

  for = C.for bodyStart bodyEnd
  forRange = M.forRange
  forEach = CP.forEach bodyStart bodyEnd csForEach inLabel 
  while = C.while parens bodyStart bodyEnd

  tryCatch = G.tryCatch csTryCatch

instance StatePattern CSharpCode where 
  checkState = M.checkState

instance ObserverPattern CSharpCode where
  notifyObservers = M.notifyObservers

instance StrategyPattern CSharpCode where
  runStrategy = M.runStrategy

instance ScopeSym CSharpCode where
  type Scope CSharpCode = Doc
  private = toCode R.private
  public = toCode R.public

instance RenderScope CSharpCode where
  scopeFromData _ = toCode
  
instance ScopeElim CSharpCode where
  scope = unCSC

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypeData
  mType = zoom lensMStoVS 
  construct = G.construct

instance ParameterSym CSharpCode where
  type Parameter CSharpCode = ParamData
  param = G.param R.param
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
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor ps is b = getClassName >>= (\n -> CP.constructor n ps is b)

  docMain = CP.docMain
 
  function = G.function
  mainFunction = CP.mainFunction string csMain

  docFunc = CP.doxFunc

  inOutMethod n s p = csInOut (method n s p)

  docInOutMethod n s p = CP.docInOutFunc (inOutMethod n s p)

  inOutFunc n s = csInOut (function n s)

  docInOutFunc n s = CP.docInOutFunc (inOutFunc n s)

instance RenderMethod CSharpCode where
  intMethod m n s p t ps b = do
    modify (if m then setCurrMain else id)
    tp <- t
    pms <- sequence ps
    toCode . mthd . R.method n s p tp pms <$> b
  intFunc = C.intFunc
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m 
    (onStateValue (onCodeValue R.commentedItem) cmt)
    
  destructor _ = error $ CP.destructorError csName
  
  mthdFromData _ d = toState $ toCode $ mthd d
  
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
  type BlockComment CSharpCode = Doc
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

csBoolType :: (RenderSym r) => VSType r
csBoolType = typeFromData Boolean csBool (text csBool)

csFuncType :: (RenderSym r) => [VSType r] -> VSType r -> VSType r
csFuncType ps r = do
  pts <- sequence ps
  rt <- r
  typeFromData (Func (map getType pts) (getType rt))
    (csFunc `containing` intercalate listSep (map getTypeString $ pts ++ [rt]))
    (text csFunc <> angles (hicat listSep' $ map RC.type' $ pts ++ [rt]))

csListSize, csForEach, csNamedArgSep, csLambdaSep :: Doc
csListSize = text "Count"
csForEach = text "foreach"
csNamedArgSep = colon <> space
csLambdaSep = text "=>"

csSystem, csConsole, csGeneric, csIO, csList, csInt, csFloat, csBool, 
  csChar, csParse, csReader, csWriter, csReadLine, csWrite, csWriteLine, 
  csIndex, csListAdd, csListAppend, csClose, csEOS, csSplit, csMain,
  csFunc :: String
csSystem = "System"
csConsole = "Console"
csGeneric = csSysAccess $ "Collections" `access` "Generic"
csIO = csSysAccess "IO"
csList = "List"
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
csListAdd = "Insert"
csListAppend = "Add"
csClose = "Close"
csEOS = "EndOfStream"
csSplit = "Split"
csMain = "Main"
csFunc = "Func"

csSysAccess :: String -> String
csSysAccess = access csSystem

csUnaryMath :: (Monad r) => String -> VSOp r
csUnaryMath = addSystemImport . unOpPrec . mathFunc

csInfileType :: (RenderSym r) => VSType r
csInfileType = join $ modifyReturn (addLangImportVS csIO) $ 
  typeFromData InFile csReader (text csReader)

csOutfileType :: (RenderSym r) => VSType r
csOutfileType = join $ modifyReturn (addLangImportVS csIO) $ 
  typeFromData OutFile csWriter (text csWriter)

csLitList :: (RenderSym r) => (VSType r -> VSType r) -> VSType r -> [SValue r] 
  -> SValue r
csLitList f t' es' = do 
  es <- sequence es' 
  lt <- f t'
  mkVal lt (new' <+> RC.type' lt
    <+> braces (valueList es))

csLambda :: (RenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
csLambda ps ex = parens (variableList ps) <+> csLambdaSep <+> RC.value ex

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
csCast = join .: on2StateValues (\t v -> csCast' (getType t) (getType $ 
  valueType v) t v)
  where csCast' Double String _ v = csDblParse (toState v)
        csCast' Float String _ v = csFloatParse (toState v)
        csCast' _ _ t v = mkStateVal (toState t) (R.castObj (R.cast 
          (RC.type' t)) (RC.value v))

-- This implementation generates a statement lambda to define the function. 
-- C# 7 supports local functions, which would be a cleaner way to implement
-- this, but the mcs compiler used in our Travis builds does not yet support 
-- all features of C# 7, so we cannot generate local functions.
-- If support for local functions is added to mcs in the future, this
-- should be re-written to generate a local function.
csFuncDecDef :: (RenderSym r) => SVariable r -> [SVariable r] -> MSBody r -> 
  MSStatement r
csFuncDecDef v ps bod = do
  vr <- zoom lensMStoVS v
  pms <- mapM (zoom lensMStoVS) ps
  t <- zoom lensMStoVS $ funcType (map (pure . variableType) pms) 
    (pure $ variableType vr)
  b <- bod
  modify (addLangImport csSystem)
  mkStmt $ RC.type' t <+> text (variableName vr) <+> equals <+>
    parens (variableList pms) <+> csLambdaSep <+> bodyStart $$ 
    indent (RC.body b) $$ bodyEnd 

csThrowDoc :: (RenderSym r) => r (Value r) -> Doc
csThrowDoc errMsg = throwLabel <+> new' <+> exceptionObj' <> 
  parens (RC.value errMsg)

csTryCatch :: (RenderSym r) => r (Body r) -> r (Body r) -> Doc
csTryCatch tb cb = vcat [
  tryLabel <+> lbrace,
  indent $ RC.body tb,
  rbrace <+> catchLabel <+> 
    lbrace,
  indent $ RC.body cb,
  rbrace]

csDiscardInput :: SValue CSharpCode -> MSStatement CSharpCode
csDiscardInput = valStmt

csFileInput :: (RenderSym r) => SValue r -> SValue r
csFileInput f = objMethodCallNoParams string f csReadLine 

csInput :: VSType CSharpCode -> SValue CSharpCode -> SValue CSharpCode
csInput tp inFn = do
  t <- tp
  csInputImport (getType t) (csInput' (getType t) inFn)
  where csInput' Integer = csIntParse
        csInput' Float = csFloatParse
        csInput' Double = csDblParse
        csInput' Boolean = csBoolParse
        csInput' String = id
        csInput' Char = csCharParse
        csInput' _ = error "Attempt to read value of unreadable type"
        csInputImport t = if t `elem` [Integer, Float, Double, Boolean, Char] 
          then addSystemImport else id

csOpenFileR :: (RenderSym r) => SValue r -> VSType r -> SValue r
csOpenFileR n r = newObj r [n]

csOpenFileWorA :: (RenderSym r) => SValue r -> VSType r -> SValue r -> SValue r
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

csVarDec :: Binding -> MSStatement CSharpCode -> MSStatement CSharpCode
csVarDec Static _ = error "Static variables can't be declared locally to a function in C#. Use stateVar to make a static state variable instead."
csVarDec Dynamic d = d

csInOut :: (VSType CSharpCode -> [MSParameter CSharpCode] -> MSBody CSharpCode -> 
    SMethod CSharpCode) -> 
  [SVariable CSharpCode] -> [SVariable CSharpCode] -> [SVariable CSharpCode] -> 
  MSBody CSharpCode -> SMethod CSharpCode
csInOut f ins [v] [] b = f (onStateValue variableType v) (map param ins)
  (on3StateValues (on3CodeValues surroundBody) (varDec v) b (returnStmt $ 
  valueOf v))
csInOut f ins [] [v] b = f (onStateValue variableType v) 
  (map param $ v : ins) (on2StateValues (on2CodeValues appendToBody) b 
  (returnStmt $ valueOf v))
csInOut f ins outs both b = f void (map (onStateValue (onCodeValue 
  (updateParam csRef)) . param) both ++ map param ins ++ map (onStateValue 
  (onCodeValue (updateParam csOut)) . param) outs) b
