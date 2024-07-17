{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

-- | The logic to render Julia code is contained in this module
module GOOL.Drasil.LanguageRenderer.JuliaRenderer (
  -- * Julia Code Configuration -- defines syntax of all Julia code
  JuliaCode(..), jlName, jlVersion
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.InterfaceCommon (SharedProg, Label, VSType, SValue, litZero,
  SVariable, MSStatement, MSBlock, SMethod, BodySym(..), BlockSym(..),
  TypeSym(..), TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..),
  Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), funcApp, extFuncApp, List(..),
  InternalList(..), ThunkSym(..), VectorType(..), VectorDecl(..),
  VectorThunk(..), VectorExpression(..), ThunkAssign(..), StatementSym(..),
  AssignStatement(..), DeclStatement(..), IOStatement(..), StringStatement(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  ScopeSym(..), ParameterSym(..), MethodSym(..), (&=), switchAsIf)
import GOOL.Drasil.InterfaceGOOL (OOProg, FSModule, ProgramSym(..), FileSym(..),
  ModuleSym(..), FunctionSym(..), PermanenceSym(..), ObserverPattern(..),
  StrategyPattern(..), GetSet(..), InternalValueExp(..), StateVarSym(..),
  ClassSym(..), OOTypeSym(..), OOVariableSym(..), OODeclStatement(..),
  OOFuncAppStatement(..), OOMethodSym(..), OOValueExpression(..),
  OOVariableValue, OOValueSym)
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(..), ImportSym(..),
  ImportElim, PermElim(binding), RenderBody(..), BodyElim, RenderBlock(..),
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..),
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind),
  RenderValue(..), ValueElim(..), InternalGetSet(..), InternalListFunc(..),
  RenderFunction(..), FunctionElim(functionType), InternalAssignStmt(..),
  InternalIOStmt(..), InternalControlStmt(..), RenderStatement(..),
  StatementElim(statementTerm), RenderScope(..), ScopeElim, MethodTypeSym(..),
  RenderParam(..), ParamElim(parameterName, parameterType), RenderMethod(..),
  MethodElim, StateVarElim, RenderClass(..), ClassElim, RenderMod(..),
  ModuleElim, BlockCommentSym(..), BlockCommentElim)
import qualified GOOL.Drasil.RendererClasses as RC (import', perm, body, block,
  type', uOp, bOp, variable, value, function, statement, scope, parameter,
  method, stateVar, class', module', blockComment')
import GOOL.Drasil.LanguageRenderer (printLabel, listSep, listSep',
  variableList, parameterList, forLabel, inLabel, tryLabel, catchLabel)
import qualified GOOL.Drasil.LanguageRenderer as R (sqrt, abs, log10, log, exp,
  sin, cos, tan, asin, acos, atan, floor, ceil, multiStmt, body, addComments,
  blockCmt, docCmt, commentedMod, listSetFunc, commentedItem, break, continue)
import GOOL.Drasil.LanguageRenderer.Constructors (mkVal, mkStateVal, VSOp,
  unOpPrec, powerPrec, unExpr, unExpr', binExpr, multPrec, typeUnExpr,
  typeBinExpr, mkStmt, mkStmtNoEnd)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (OptionalSpace(..))
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  block, multiBlock, listInnerType, litChar, litDouble, litInt, litString,
  valueOf, negateOp, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp,
  lessEqualOp, plusOp, minusOp, multOp, divideOp, moduloOp, var, call,
  funcAppMixedArgs, lambda, listAccess, listSet, arrayElem, modFromData,
  fileDoc, fileFromData, tryCatch, csc, multiBody, sec, cot, stmt, loopStmt,
  emptyStmt, assign, increment, subAssign, print, comment, valStmt, function,
  returnStmt, construct, param, docFunc, throw, arg, argsList, ifCond, smartAdd)
import qualified GOOL.Drasil.LanguageRenderer.CommonPseudoOO as CP (bool,
  boolRender, extVar, funcType, buildModule, docMod', funcDecDef, litArray,
  listDec, listDecDef, listAccessFunc, listSetFunc, bindingError, notNull,
  extFuncAppMixedArgs, functionDoc, listSize, listAdd, listAppend, intToIndex',
  indexToInt', inOutFunc, docInOutFunc', forLoopError, varDec, varDecDef,
  openFileR', openFileW', openFileA', multiReturn, multiAssign, inOutCall,
  mainBody, argExists, forEach')
import qualified GOOL.Drasil.LanguageRenderer.CLike as C (litTrue, litFalse,
  notOp, andOp, orOp, inlineIf, while)
import qualified GOOL.Drasil.LanguageRenderer.Macros as M (increment1,
  decrement1, ifExists, stringListVals, stringListLists)
import GOOL.Drasil.AST (Terminator(..), FileType(..), FileData(..), fileD,
  FuncData(..), ModData(..), md, updateMod, MethodData(..), mthd, OpData(..),
  ParamData(..), ProgData(..), TypeData(..), td, ValData(..), vd, VarData(..),
  vard, CommonThunk, progD, fd, pd, updateMthd, commonThunkDim, commonThunkElim,
  vectorize, vectorize2, commonVecIndex, sumComponents, pureValue)
import GOOL.Drasil.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  on2CodeValues, on2StateValues, onCodeList, onStateList, emptyIfEmpty)
import GOOL.Drasil.State (VS, lensGStoFS, revFiles, setFileType, lensMStoVS,
  getModuleImports, addModuleImportVS, getUsing, getLangImports, getLibImports,
  addLibImportVS, useVarName, getMainDoc, genLoopIndex, genVarName)

import Control.Applicative (liftA2)
import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)
import Data.List (intercalate, sort)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), empty, brackets, vcat,
  quotes, doubleQuotes, parens, equals, colon)
import qualified Text.PrettyPrint.HughesPJ as D (float)

jlExt :: String
jlExt = "jl"

newtype JuliaCode a = JLC {unJLC :: a}

instance Functor JuliaCode where
  fmap f (JLC x) = JLC (f x)

instance Applicative JuliaCode where
  pure = JLC
  (JLC f) <*> (JLC x) = JLC (f x)

instance Monad JuliaCode where
  JLC x >>= f = f x

instance SharedProg JuliaCode
instance OOProg JuliaCode

instance ProgramSym JuliaCode where
  type Program JuliaCode = ProgData
  prog n st files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n st) fs

instance RenderSym JuliaCode

instance FileSym JuliaCode where
  type File JuliaCode = FileData
  fileDoc m = do
    modify (setFileType Combined)
    G.fileDoc jlExt top bottom m
  docMod = CP.docMod' jlExt

instance RenderFile JuliaCode where
  top _ = toCode empty
  bottom = toCode empty

  commentedMod = on2StateValues (on2CodeValues R.commentedMod)

  fileFromData = G.fileFromData (onCodeValue . fileD)

instance ImportSym JuliaCode where
  type Import JuliaCode = Doc
  langImport n = let modName = text n
                     fileName = text $ n ++ '.' : jlExt
    in toCode $ vcat [includeLabel <> parens (doubleQuotes fileName),
      importLabel <+> text "." <> modName] -- TODO: we want a dot only when the import is locally defined.
  modImport = langImport

instance ImportElim JuliaCode where
  import' = unJLC

instance PermElim JuliaCode where
  perm = unJLC
  binding = error $ CP.bindingError jlName

instance BodySym JuliaCode where
  type Body JuliaCode = Doc
  body = onStateList (onCodeList R.body)

  addComments s = onStateValue (onCodeValue (R.addComments s jlCmtStart))

instance RenderBody JuliaCode where
  multiBody = G.multiBody

instance BodyElim JuliaCode where
  body = unJLC

instance BlockSym JuliaCode where
  type Block JuliaCode = Doc
  block = G.block

instance RenderBlock JuliaCode where
  multiBlock = G.multiBlock

instance BlockElim JuliaCode where
  block = unJLC

instance TypeSym JuliaCode where
  type Type JuliaCode = TypeData
  bool = CP.bool
  int = jlIntType
  float = jlFloatType
  double = jlDoubleType
  char = jlCharType
  string = jlStringType
  infile = jlInfileType
  outfile = jlOutfileType
  listType = jlListType
  arrayType = listType -- Treat arrays and lists the same, as in Python
  listInnerType = G.listInnerType
  funcType = CP.funcType
  void = jlVoidType

instance TypeElim JuliaCode where
  getType = cType . unJLC
  getTypeString v = let tp = typeString $ unJLC v in
    case cType $ unJLC v of
      (Object _) -> error jlClassError
      _ -> tp

instance RenderType JuliaCode where
  multiType ts = do
    typs <- sequence ts
    let mt = jlTuple $ map getTypeString typs
    typeFromData Void mt (text mt)
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim JuliaCode where
  type' v = let t = typeDoc $ unJLC v in
    case cType $ unJLC v of
      (Object _) -> t <> error jlClassError
      _ -> t

instance UnaryOpSym JuliaCode where
  type UnaryOp JuliaCode = OpData
  notOp = C.notOp
  negateOp = G.negateOp
  sqrtOp = jlUnaryMath R.sqrt
  absOp = jlUnaryMath R.abs
  logOp = jlUnaryMath R.log10
  lnOp = jlUnaryMath R.log
  expOp = jlUnaryMath R.exp
  sinOp = jlUnaryMath R.sin
  cosOp = jlUnaryMath R.cos
  tanOp = jlUnaryMath R.tan
  asinOp = jlUnaryMath R.asin
  acosOp = jlUnaryMath R.acos
  atanOp = jlUnaryMath R.atan
  floorOp = jlUnaryMath R.floor
  ceilOp = jlUnaryMath R.ceil

instance BinaryOpSym JuliaCode where
  type BinaryOp JuliaCode = OpData
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
  powerOp = powerPrec jlPower
  moduloOp = G.moduloOp
  andOp = C.andOp
  orOp = C.orOp

instance OpElim JuliaCode where
  uOp = opDoc . unJLC
  bOp = opDoc . unJLC
  uOpPrec = opPrec . unJLC
  bOpPrec = opPrec . unJLC

instance VariableSym JuliaCode where
  type Variable JuliaCode = VarData
  var = G.var
  constant = var
  extVar l n t = modify (addModuleImportVS l) >> CP.extVar l n t
  arrayElem i = G.arrayElem (litInt i)

instance VariableElim JuliaCode where
  variableName = varName . unJLC
  variableType = onCodeValue varType

instance InternalVarElim JuliaCode where
  variableBind = varBind . unJLC
  variable = varDoc . unJLC

instance RenderVariable JuliaCode where
  varFromData b n t' d = do
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym JuliaCode where
  type Value JuliaCode = ValData
  valueType = onCodeValue valType

instance Argument JuliaCode where
  pointerArg = id

instance Literal JuliaCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = jlLitFloat
  litInt = G.litInt
  litString = G.litString
  litArray = CP.litArray brackets
  litList = litArray

instance MathConstant JuliaCode where
  pi :: SValue JuliaCode
  pi = mkStateVal double jlPi

instance VariableValue JuliaCode where
  valueOf = G.valueOf

instance CommandLineArgs JuliaCode where
  arg n = G.arg (litInt $ n+1) argsList
  argsList = G.argsList jlArgs
  argExists = CP.argExists

instance NumericExpression JuliaCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) v1' v2' = do -- Julia has integer division via `÷` or `div()`
    v1 <- v1'
    v2 <- v2'
    let jlDivision Integer Integer = binExpr (multPrec jlIntDiv)
        jlDivision _ _ = binExpr divideOp
    jlDivision (getType $ valueType v1) (getType $ valueType v2)
        (pure v1) (pure v2)
  (#%) = binExpr moduloOp
  (#^) = binExpr powerOp

  log = unExpr logOp
  ln = unExpr lnOp
  exp = unExpr expOp
  sin = unExpr sinOp
  cos = unExpr cosOp
  tan = unExpr tanOp
  csc = G.csc
  sec = G.sec
  cot = G.cot
  arcsin = unExpr asinOp
  arccos = unExpr acosOp
  arctan = unExpr atanOp
  floor = unExpr floorOp
  ceil = unExpr ceilOp

instance BooleanExpression JuliaCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

instance Comparison JuliaCode where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression JuliaCode where
  inlineIf = C.inlineIf

  funcAppMixedArgs = G.funcAppMixedArgs
  extFuncAppMixedArgs l n t ps ns = do
    modify (addModuleImportVS l)
    CP.extFuncAppMixedArgs l n t ps ns
  libFuncAppMixedArgs l n t ps ns = do
    modify (addLibImportVS l)
    CP.extFuncAppMixedArgs l n t ps ns

  lambda = G.lambda jlLambda

  notNull = CP.notNull jlNull

instance RenderValue JuliaCode where
  inputFunc = mkStateVal string (jlReadLine <> parens empty)
  printFunc = mkStateVal void jlPrintFunc
  printLnFunc = mkStateVal void jlPrintLnFunc
  printFileFunc _ = mkStateVal void empty
  printFileLnFunc _ = mkStateVal void empty

  cast = jlCast

  call = G.call jlNamedArgSep

  valFromData p i t' d = do
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim JuliaCode where
  valuePrec = valPrec . unJLC
  valueInt = valInt . unJLC
  value = val . unJLC

instance List JuliaCode where
  intToIndex = CP.intToIndex'
  indexToInt = CP.indexToInt'
  listSize = CP.listSize
  listAdd = CP.listAdd
  listAppend = CP.listAppend
  listAccess = G.listAccess
  listSet = G.listSet
  indexOf = jlIndexOf

instance InternalList JuliaCode where
  listSlice' b e s vn vo = jlListSlice vn vo b e (fromMaybe (litInt 1) s)

instance InternalListFunc JuliaCode where
  listSizeFunc l = do
    f <- funcApp jlListSize int [l]
    funcFromData (RC.value f) int
  listAddFunc l i v = do
    f <- funcApp jlListAdd void [l, i, v]
    funcFromData (RC.value f) void
  listAppendFunc l v = do
    f <- funcApp jlListAppend void [l, v]
    funcFromData (RC.value f) void
  listAccessFunc = CP.listAccessFunc
  listSetFunc = CP.listSetFunc R.listSetFunc

instance ThunkSym JuliaCode where
  type Thunk JuliaCode = CommonThunk VS

instance ThunkAssign JuliaCode where
  thunkAssign v t = do
    iName <- genLoopIndex
    let
      i = var iName int
      dim = fmap pure $ t >>= commonThunkDim (fmap unJLC . (\l -> listSize l #- litInt 1) . fmap pure) . unJLC
      loopInit = zoom lensMStoVS (fmap unJLC t) >>= commonThunkElim
        (const emptyStmt) (const $ assign v $ litZero $ fmap variableType v)
      loopBody = zoom lensMStoVS (fmap unJLC t) >>= commonThunkElim
        (valStmt . listSet (valueOf v) (valueOf i) . vecIndex (valueOf i) . pure . pure)
        ((v &+=) . vecIndex (valueOf i) . pure . pure)
    multi [loopInit,
      forRange i (litInt 0) dim (litInt 1) $ body [block [loopBody]]]

instance VectorType JuliaCode where
  vecType = listType

instance VectorDecl JuliaCode where
  vecDec = listDec
  vecDecDef = listDecDef

instance VectorThunk JuliaCode where
  vecThunk = pure . pure . pureValue . fmap unJLC . valueOf

instance VectorExpression JuliaCode where
  vecScale k = fmap $ fmap $ vectorize (fmap unJLC . (k #*) . fmap pure)
  vecAdd = liftA2 $ liftA2 $ vectorize2 (\v1 v2 -> fmap unJLC $ fmap pure v1 #+ fmap pure v2)
  vecIndex i = (>>= fmap pure . commonVecIndex (fmap unJLC . flip listAccess i . fmap pure) . unJLC)
  vecDot = liftA2 $ liftA2 $ fmap sumComponents <$> vectorize2 (\v1 v2 -> fmap unJLC $ fmap pure v1 #* fmap pure v2)

instance RenderFunction JuliaCode where
  funcFromData d = onStateValue $ onCodeValue (`fd` d)

instance FunctionElim JuliaCode where
  functionType = onCodeValue fType
  function = funcDoc . unJLC

instance InternalAssignStmt JuliaCode where
  multiAssign = CP.multiAssign id

instance InternalIOStmt JuliaCode where
  printSt = jlPrint

instance InternalControlStmt JuliaCode where
  multiReturn = CP.multiReturn id

instance RenderStatement JuliaCode where
  stmt = G.stmt
  loopStmt = G.loopStmt
  emptyStmt = G.emptyStmt
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim JuliaCode where
  statement = fst . unJLC
  statementTerm = snd . unJLC

instance StatementSym JuliaCode where
  type Statement JuliaCode = (Doc, Terminator)
  valStmt = G.valStmt Empty
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement JuliaCode where
  assign = G.assign Empty
  (&-=) = G.subAssign Empty
  (&+=) = G.increment
  (&++) = M.increment1
  (&--) = M.decrement1

instance DeclStatement JuliaCode where
  varDec = CP.varDec
  varDecDef = CP.varDecDef
  listDec _ = CP.listDec
  listDecDef = CP.listDecDef
  arrayDec = listDec
  arrayDecDef = listDecDef
  constDecDef = jlConstDecDef
  funcDecDef = CP.funcDecDef

instance IOStatement JuliaCode where
  print      = jlOut False Nothing printFunc
  printLn    = jlOut True  Nothing printLnFunc
  printStr   = jlOut False Nothing printFunc   . litString
  printStrLn = jlOut True  Nothing printLnFunc . litString

  printFile f      = jlOut False (Just f) printFunc
  printFileLn f    = jlOut True (Just f) printLnFunc
  printFileStr f   = printFile   f . litString
  printFileStrLn f = printFileLn f . litString

  getInput = jlInput inputFunc
  discardInput = valStmt inputFunc
  getFileInput f = jlInput (readLine f)
  discardFileInput f = valStmt (readLine f)
  openFileR f n = f &= CP.openFileR' n
  openFileW f n = f &= CP.openFileW' n
  openFileA f n = f &= CP.openFileA' n
  closeFile f = valStmt $ funcApp jlCloseFunc void [f]
  getFileInputLine = getFileInput
  discardFileLine = discardFileInput
  getFileInputAll f v = v &= readLines f

instance StringStatement JuliaCode where
  stringSplit d vnew s = vnew &= funcApp jlSplit (listType string) [s, litString [d]]
  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FuncAppStatement JuliaCode where
  inOutCall = CP.inOutCall funcApp
  extInOutCall m = CP.inOutCall (extFuncApp m)

instance CommentStatement JuliaCode where
  comment = G.comment jlCmtStart

instance ControlStatement JuliaCode where
  break = mkStmtNoEnd R.break
  continue = mkStmtNoEnd R.continue
  returnStmt = G.returnStmt Empty
  throw = G.throw jlThrow Empty
  ifCond = G.ifCond id empty jlSpace elseIfLabel empty jlEnd
  switch = switchAsIf
  ifExists = M.ifExists
  for _ _ _ _ = error $ CP.forLoopError jlName
  forRange i initv finalv stepv = forEach i (jlRange initv finalv stepv)
  forEach = CP.forEach' jlForEach
  while = C.while id empty jlEnd
  tryCatch = G.tryCatch jlTryCatch

instance ScopeSym JuliaCode where
  type Scope JuliaCode = Doc

  private = toCode empty -- Julia doesn't have private/public members
  public = toCode empty

instance RenderScope JuliaCode where
  scopeFromData _ = toCode

instance ScopeElim JuliaCode where
  scope = unJLC

instance MethodTypeSym JuliaCode where
  type MethodType JuliaCode = TypeData

  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym JuliaCode where
  type Parameter JuliaCode = ParamData

  param = G.param jlParam
  pointerParam = param

instance RenderParam JuliaCode where
  paramFromData v' d = do
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)

instance ParamElim JuliaCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unJLC

instance MethodSym JuliaCode where
  type Method JuliaCode = MethodData
  docMain = mainFunction
  function = G.function
  mainFunction = CP.mainBody
  docFunc = G.docFunc CP.functionDoc

  inOutFunc n s = CP.inOutFunc (function n s)
  docInOutFunc n s = CP.docInOutFunc' CP.functionDoc (inOutFunc n s)

instance RenderMethod JuliaCode where
  intFunc _ n _ _ _ ps b = do
    pms <- sequence ps
    toCode . mthd . jlIntFunc n pms <$> b

  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)
  mthdFromData _ d = toState $ toCode $ mthd d
  -- OO-Only (remove when ready)
  intMethod = undefined--
  destructor _ = undefined--

instance MethodElim JuliaCode where
  method = mthdDoc . unJLC

instance ModuleSym JuliaCode where
  type Module JuliaCode = ModData
  buildModule n is fs _ = jlModContents n is fs <&>
    updateModuleDoc (\m -> emptyIfEmpty m (vibcat [jlModStart n, m, jlEnd]))

instance RenderMod JuliaCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim JuliaCode where
  module' = modDoc . unJLC

instance BlockCommentSym JuliaCode where
  type BlockComment JuliaCode = Doc

  blockComment lns = toCode $ R.blockCmt lns jlBlockCmtStart jlBlockCmtEnd
  docComment = onStateValue (\lns -> toCode $ R.docCmt lns jlDocCmtStart
    jlDocCmtEnd)

instance BlockCommentElim JuliaCode where
  blockComment' = unJLC

-- convenience
jlName, jlVersion :: String
jlName = "Julia"
jlVersion = "1.10.3"

-- Concrete versions of each Julia datatype
jlIntConc, jlFloatConc, jlDoubleConc, jlCharConc, jlStringConc, jlListConc,
  jlFile, jlVoid :: String
jlIntConc = "Int64"
jlFloatConc = "Float32"
jlDoubleConc = "Float64"
jlCharConc = "Char"
jlStringConc = "String"
jlListConc = "Array"
jlFile = "IOStream"
jlVoid = "Nothing"

-- Abstract versions of each Julia datatype
-- Currently only concrete ones are used, but because of Julia's workflow
-- It might be nice to use abstract types when possible
-- jlIntAbs, jlFloatAbs, jlDoubleAbs, jlCharAbs, jlStringAbs, jlListAbs, jlFile,
--   jlVoid :: String
-- jlIntAbs = "Integer"
-- jlFloatAbs = "AbstractFloat"
-- jlDoubleAbs = "AbstractFloat"
-- jlCharAbs = "AbstractChar"
-- jlStringAbs = "AbstractString"
-- jlListAbs = "AbstractArray"

jlClassError :: String
jlClassError = "Classes are not supported in Julia"

-- The only consistent way of creating floats is by casting
jlLitFloat :: (RenderSym r) => Float -> SValue r
jlLitFloat f = mkStateVal float (text jlFloatConc <> parens (D.float f))

jlCast :: (RenderSym r) => VSType r -> SValue r -> SValue r
jlCast t' v' = do
  t <- t'
  v <- v'
  let vTp = getType $ valueType v
      tTp = getType t
      vDoc = RC.value v
      tDoc = RC.type' t
      jlCast' :: CodeType -> CodeType -> Doc -> Doc -> Doc
      -- Converting string to char
      jlCast' String Char vDoc' _ = text "only" <> parens vDoc'
      -- Converting string to something else
      jlCast' String _    vDoc' tDoc' = text "parse" <> parens (tDoc' <> listSep' <+> vDoc')
      -- Converting non-string to char
      jlCast' _      Char vDoc' _ = text "only" <> parens (text "string" <> parens vDoc')
      -- Converting something to string
      jlCast' _      String vDoc' _ = text "string" <> parens vDoc'
      -- Converting non-string to non-string
      jlCast' _      _    vDoc' tDoc' = tDoc' <> parens vDoc'
  mkVal t (jlCast' vTp tTp vDoc tDoc)

jlConstDecDef :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
jlConstDecDef v' def' = do
  v <- zoom lensMStoVS v'
  def <- zoom lensMStoVS def'
  modify $ useVarName $ variableName v
  mkStmt $ RC.variable v <+> equals <+> RC.value def --TODO: prepend `constDec' ` when in global scope

-- List API
jlListSize, jlListAdd, jlListAppend, jlListAbsdex :: Label
jlListSize = "length"
jlListAdd = "insert!"
jlListAppend = "append!"
jlListAbsdex    = "findfirst"

jlIndexOf :: (SharedProg r) => SValue r -> SValue r -> SValue r
jlIndexOf l v = do
  v' <- v
  let t = toCode $ valueType v'
  indexToInt $ funcApp
    jlListAbsdex t [lambda [var "x" t] (valueOf (var "x" t) ?== v), l]

jlListSlice :: (RenderSym r) => SVariable r -> SValue r ->
  Maybe (SValue r) -> Maybe (SValue r) -> SValue r -> MSBlock r
jlListSlice vn vo beg end step = do
  stepV <- zoom lensMStoVS step

  let mbStepV = valueInt stepV
  bName <- case (beg, mbStepV) of
            (Nothing, Nothing) -> genVarName [] "begIdx"
            _                  -> return ""
  eName <- case mbStepV of
            Nothing -> genVarName [] "endIdx"
            _       -> return ""

  let begVar = var bName int
      endVar = var eName int

      (setBeg, begVal) = case (beg, mbStepV) of
        -- If we have a value for beg, just use it
        (Just b, _)        -> (emptyStmt, intToIndex b)
        -- If we don't have a value for `beg` but we do for `step`, use `begin` or `end`
        (Nothing, Just s)  -> (emptyStmt,
          if s > 0 then mkStateVal int jlBegin else mkStateVal int jlEnd)
        -- Otherwise, generate an if-statement to calculate `beg` at runtime
        (Nothing, Nothing) -> (varDecDef begVar $
          inlineIf (step ?> litInt 0) (litInt 1) (listSize vo),
          valueOf begVar)
      
      -- Similar to `begVal`, but if we're given a value, we have to either
      -- do nothing or add 2 based on the sign of `step`, because `end` needs to be inclusive, 
      (setEnd, endVal) = case (end, mbStepV) of
        (Just e, Just s)  -> (emptyStmt,
          if s > 0 then e else e `G.smartAdd` litInt 2)
        (Just e, Nothing) -> (varDecDef endVar $
          inlineIf (step ?> litInt 0) e (e `G.smartAdd` litInt 2),
          valueOf endVar)
        (Nothing, Just s) -> (emptyStmt,
          if s > 0 then mkStateVal int jlEnd else mkStateVal int jlBegin)
        (Nothing, Nothing) -> (varDecDef endVar $
          inlineIf (step ?> litInt 0) (listSize vo) (litInt 1), valueOf endVar)

      setToSlice = jlListSlice' vn vo begVal endVal step mbStepV

  block [
      setBeg,
      setEnd,
      setToSlice
    ]

jlListSlice' :: (RenderSym r) => SVariable r -> SValue r -> SValue r ->
  SValue r -> SValue r -> Maybe Integer -> MSStatement r
jlListSlice' vn vo beg end step mStep = do
  vold  <- zoom lensMStoVS vo
  beg'  <- zoom lensMStoVS beg
  end'  <- zoom lensMStoVS end
  step' <- zoom lensMStoVS step
  let stepDoc = case mStep of
        (Just 1) -> empty
        _        -> colon <> RC.value step'
      theSlice = mkStateVal void (RC.value vold <> brackets (RC.value beg' <> stepDoc <> colon <> RC.value end'))
  vn &= theSlice
  

-- Other functionality
jlRange :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
jlRange initv finalv stepv = do
  t <- listType int
  iv <- initv
  sv <- stepv
  fv <- finalv
  mkVal t (RC.value iv <> colon <> RC.value sv <> colon <> RC.value fv)

jlSplit :: String
jlSplit = "split"

jlPrintFunc, jlPrintLnFunc :: Doc
jlPrintFunc = text printLabel
jlPrintLnFunc = text "println"

jlParseFunc :: Label
jlParseFunc = "parse"

jlType, arrow, jlNamedArgSep :: Doc
jlType = colon <> colon
arrow = text "->"
jlNamedArgSep = equals

jlTuple :: [String] -> String
jlTuple ts = "Tuple{" ++ intercalate listSep ts ++ "}"

-- Operators
jlUnaryMath :: (Monad r) => String -> VSOp r
jlUnaryMath = unOpPrec

jlPower, jlIntDiv :: String
jlPower = "^"
jlIntDiv = "÷"

-- Constants
jlPi :: Doc
jlPi = text "pi"

-- Comments
jlCmtStart, jlBlockCmtStart, jlBlockCmtEnd, jlDocCmtStart, jlDocCmtEnd :: Doc
jlCmtStart      = text "#"
jlBlockCmtStart = text "#="
jlBlockCmtEnd   = text "=#"
jlDocCmtStart   = text "\"\"\""
jlDocCmtEnd     = text "\"\"\""

-- Control structures

jlSpace :: OptionalSpace
jlSpace = OSpace {oSpace = empty}

-- | Creates a for-each loop in Julia
jlForEach :: (RenderSym r) => r (Variable r) -> r (Value r) -> r (Body r) -> Doc
jlForEach i lstVar b = vcat [
  forLabel <+> RC.variable i <+> inLabel <+> RC.value lstVar,
  indent $ RC.body b,
  jlEnd]

-- | Creates the contents of a module in Julia
jlModContents :: Label -> [Label] -> [SMethod JuliaCode] ->
  FSModule JuliaCode
jlModContents n is fs = CP.buildModule n (do
  lis <- getLangImports
  libis <- getLibImports
  mis <- getModuleImports
  us <- getUsing
  pure $ vibcat [
    vcat (map (RC.import' . li) lis),
    vcat (map (RC.import' . li) (sort $ is ++ libis)),
    vcat (map (RC.import' . mi) mis),
    vcat (map usingModule us)])
  (pure empty) (do getMainDoc) fs []
  where mi, li :: Label -> JuliaCode (Import JuliaCode)
        mi = modImport
        li = langImport

-- Functions
-- | Creates a function.  n is function name, pms is list of parameters, and 
--   bod is body.
jlIntFunc :: (RenderSym r) => Label -> [r (Parameter r)] ->
  r (Body r) -> Doc
jlIntFunc n pms bod = do
  vcat [jlFunc <+> text n <> parens (parameterList pms),
    indent $ RC.body bod, jlEnd]

jlLambda :: (RenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
jlLambda ps ex = variableList ps <+> arrow <+> RC.value ex

-- Exceptions
jlThrow :: (RenderSym r) => r (Value r) -> Doc
jlThrow errMsg = jlThrowLabel <> parens (RC.value errMsg)

jlTryCatch :: (RenderSym r) => r (Body r) -> r (Body r) -> Doc
jlTryCatch tryB catchB = vcat [
  tryLabel,
  indent $ RC.body tryB,
  catchLabel <+> jlException,
  indent $ RC.body catchB,
  jlEnd]

jlException :: Doc
jlException = text "ErrorException"

includeLabel, importLabel :: Doc
includeLabel = text "include"
importLabel = text "import"

jlMod, elseIfLabel, jlFunc, jlBegin, jlEnd, jlThrowLabel :: Doc
jlMod        = text "module"
elseIfLabel  = text "elseif"
jlFunc       = text "function"
jlBegin      = text "begin"
jlEnd        = text "end"
jlThrowLabel = text "error" -- TODO: this hints at an underdeveloped exception system

jlParam :: (RenderSym r) => r (Variable r) -> Doc
jlParam v = RC.variable v <> jlType <> RC.type' (variableType v)

-- Type names specific to Julia (there's a lot of them)
jlIntType :: (RenderSym r) => VSType r
jlIntType = typeFromData Integer jlIntConc (text jlIntConc)

jlFloatType :: (RenderSym r) => VSType r
jlFloatType = typeFromData Float jlFloatConc (text jlFloatConc)

jlDoubleType :: (RenderSym r) => VSType r
jlDoubleType = typeFromData Double jlDoubleConc (text jlDoubleConc)

jlCharType :: (RenderSym r) => VSType r
jlCharType = typeFromData Char jlCharConc (text jlCharConc)

jlStringType :: (RenderSym r) => VSType r
jlStringType = typeFromData String jlStringConc (text jlStringConc)

jlInfileType :: (RenderSym r) => VSType r
jlInfileType = typeFromData InFile jlFile (text jlFile)

jlOutfileType :: (RenderSym r) => VSType r
jlOutfileType = typeFromData OutFile jlFile (text jlFile)

jlListType :: (RenderSym r) => VSType r -> VSType r
jlListType t' = do
  t <- t'
  let typeName = jlListConc ++ "{" ++ getTypeString t ++ "}"
  typeFromData (List $ getType t) typeName (text typeName)

jlVoidType :: (RenderSym r) => VSType r
jlVoidType = typeFromData Void jlVoid (text jlVoid)

jlNull :: Label
jlNull = "nothing"

-- Modules
-- | Creates the text for the start of a module.
--   n is the name of the module.
jlModStart :: Label -> Doc
jlModStart n = jlMod <+> text n

using :: Doc
using = text "using"

usingModule :: Label -> Doc -- TODO: see if you need to add context for package vs file
usingModule n = using <+> text n

-- IO
jlPrint :: Bool -> Maybe (SValue JuliaCode) -> SValue JuliaCode ->
  SValue JuliaCode -> MSStatement JuliaCode
-- Printing to console
jlPrint _ f' p' v' = do
  f <- zoom lensMStoVS $ fromMaybe (mkStateVal void empty) f' -- The file to print to
  prf <- zoom lensMStoVS p' -- The print function to use
  v <- zoom lensMStoVS v' -- The value to print
  let fl = emptyIfEmpty (RC.value f) $ RC.value f <> listSep'
  mkStmtNoEnd $ RC.value prf <> parens (fl <> RC.value v)

-- jlPrint can handle lists, so don't use G.print for lists
jlOut :: (RenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r ->
  MSStatement r
jlOut newLn f printFn v = zoom lensMStoVS v >>= jlOut' . getType . valueType
  where jlOut' (List _) = printSt newLn f printFn v
        jlOut' _ = G.print newLn f printFn v

jlInput :: SValue JuliaCode -> SVariable JuliaCode -> MSStatement JuliaCode
jlInput inSrc v = v &= (v >>= jlInput' . getType . variableType)
  where jlInput' Integer = jlParse jlIntConc int inSrc
        jlInput' Float = jlParse jlFloatConc float inSrc
        jlInput' Double = jlParse jlDoubleConc double inSrc
        jlInput' Boolean = jlParse CP.boolRender bool inSrc
        jlInput' String = inSrc
        jlInput' Char = jlParse jlCharConc char inSrc
        jlInput' _ = error "Attempt to read a value of unreadable type"

readLine, readLines :: (RenderSym r) => SValue r -> SValue r
readLine f = funcApp jlReadLineFunc string [f]
readLines f = funcApp jlReadLinesFunc (listType string) [f]

jlReadLine :: Doc
jlReadLine = text jlReadLineFunc

jlReadLineFunc, jlReadLinesFunc, jlCloseFunc :: Label
jlReadLineFunc = "readline"
jlReadLinesFunc = "readlines"
jlCloseFunc = "close"

jlArgs :: Label
jlArgs = "ARGS"

jlParse :: (RenderSym r) => Label -> VSType r -> SValue r -> SValue r
jlParse tl tp v = let
  typeLabel = mkStateVal void (text tl)
  in funcApp jlParseFunc tp [typeLabel, v]

-- OO-Only (remove when ready)

instance FunctionSym JuliaCode where
  type Function JuliaCode = FuncData
  func = undefined--
  objAccess = undefined--

instance InternalValueExp JuliaCode where
  objMethodCallMixedArgs' = undefined--

instance GetSet JuliaCode where
  get = undefined--
  set = undefined--

instance ObserverPattern JuliaCode where
  notifyObservers = undefined--

instance StrategyPattern JuliaCode where
  runStrategy = undefined--

instance StateVarSym JuliaCode where
  type StateVar JuliaCode = Doc

  stateVar = undefined--
  stateVarDef = undefined--
  constVar = undefined--

instance StateVarElim JuliaCode where
  stateVar = undefined--

instance ClassSym JuliaCode where
  type Class JuliaCode = Doc

  buildClass = undefined--
  extraClass = undefined--
  implementingClass = undefined--
  docClass = undefined--

instance RenderClass JuliaCode where
  intClass = undefined--
  inherit = undefined--
  implements = undefined--
  commentedClass = undefined--

instance ClassElim JuliaCode where
  class' = undefined--

instance PermanenceSym JuliaCode where
  type Permanence JuliaCode = Doc
  static = undefined--
  dynamic = undefined--

instance OOTypeSym JuliaCode where
  obj = undefined--

instance OOVariableSym JuliaCode where
  staticVar = undefined--
  self = undefined--
  classVar = undefined--
  extClassVar = undefined--
  objVar = undefined--
  objVarSelf = undefined--

instance OOValueExpression JuliaCode where
  selfFuncAppMixedArgs = undefined--
  newObjMixedArgs = undefined--
  extNewObjMixedArgs = undefined--
  libNewObjMixedArgs = undefined--

instance OODeclStatement JuliaCode where
  objDecDef = undefined--
  objDecNew = undefined--
  extObjDecNew = undefined--

instance OOFuncAppStatement JuliaCode where
  selfInOutCall = undefined--

instance OOMethodSym JuliaCode where
  method = undefined--
  getMethod = undefined--
  setMethod = undefined--
  constructor = undefined--
  inOutMethod = undefined--
  docInOutMethod _ = undefined--

instance OOVariableValue JuliaCode

instance OOValueSym JuliaCode

instance InternalGetSet JuliaCode where
  getFunc = undefined--
  setFunc = undefined--