{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

-- | The logic to render Julia code is contained in this module
module GOOL.Drasil.LanguageRenderer.JuliaRenderer (
  -- * Julia Code Configuration -- defines syntax of all Julia code
  JuliaCode(..), jlName, jlVersion
) where

import Utils.Drasil (blank, indent, indentList)

-- TODO: Sort the dependencies to match the other modules
import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, VSType, SValue, SVariable, VSFunction,
  MSStatement, OOProg, ProgramSym(..), SMethod, MSBody, MSParameter, Initializers,
  FileSym(..), PermanenceSym(..), BodySym(..), BlockSym(..), TypeSym(..),
  TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..), Argument(..),
  Literal(..), MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..), CSStateVar,
  ValueExpression(..), funcApp, funcAppNamedArgs, bodyStatements, InternalValueExp(..), FunctionSym(..), GetSet(..),
  List(..), InternalList(..), ThunkSym(..), VectorType(..), VectorDecl(..),
  VectorThunk(..), VectorExpression(..), ThunkAssign(..), StatementSym(..),
  AssignStatement(..), DeclStatement(..), IOStatement(..), StringStatement(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  StatePattern(..), ObserverPattern(..), StrategyPattern(..), ScopeSym(..),
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..),
  (&=))
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(..), ImportSym(..),
  ImportElim, PermElim(binding), RenderBody(..), BodyElim, RenderBlock(..),
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..),
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind),
  RenderValue(..), ValueElim(valuePrec), InternalGetSet(..), ParentSpec,
  InternalListFunc(..), RenderFunction(..),
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..),
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm),
  RenderScope(..), ScopeElim, MethodTypeSym(..), RenderParam(..),
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim,
  StateVarElim, RenderClass(..), ClassElim, RenderMod(..), ModuleElim,
  BlockCommentSym(..), BlockCommentElim)
import qualified GOOL.Drasil.RendererClasses as RC (RenderBody(multiBody),
  import', perm, body, block, type', uOp, bOp, variable, value, function,
  statement, scope, parameter, method, stateVar, class', module', blockComment')
import GOOL.Drasil.LanguageRenderer (printLabel, listSep, listSep',
  ClassDocRenderer, variableList, parameterList)
import qualified GOOL.Drasil.LanguageRenderer as R (sqrt, abs, log10, log, exp, 
  sin, cos, tan, asin, acos, atan, floor, ceil, multiStmt, body, addComments,
  blockCmt, docCmt, commentedMod, listSetFunc, dynamic, stateVar, stateVarList)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStateVal, mkStateVar, VSOp,
  unOpPrec, powerPrec, unExpr, unExpr', binExpr, multPrec, typeUnExpr,
  typeBinExpr, mkStmtNoEnd, mkVal)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  block, listInnerType, obj, litChar, litDouble, litInt, litString, valueOf, negateOp, equalOp,
  notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp, minusOp,
  multOp, divideOp, moduloOp, var, call, funcAppMixedArgs,
  lambda, modFromData, fileDoc, fileFromData, csc, multiBody,
  sec, cot, stmt, loopStmt, emptyStmt, assign, increment, subAssign, print,
  comment, valStmt, listAccess, func, objAccess, listSet, docClass,
  commentedClass, buildClass, method, getMethod, setMethod, returnStmt, objVar,
  construct, param, defaultOptSpace, ifCond, initStmts)
import qualified GOOL.Drasil.LanguageRenderer.CommonPseudoOO as CP (string',
  bool, funcType, buildModule, docMod', litArray, listDec, listDecDef, mainBody,
  listAccessFunc, listSetFunc, intClass, bindingError)
import qualified GOOL.Drasil.LanguageRenderer.CLike as C (litTrue, litFalse,
  litFloat, notOp, andOp, orOp, inlineIf)
import qualified GOOL.Drasil.LanguageRenderer.Macros as M (increment1, decrement1)
import GOOL.Drasil.AST (Terminator(..), FileType(..), FileData(..), fileD, FuncData(..), ModData(..),
  md, updateMod, MethodData(..), mthd, OpData(..), ParamData(..), ProgData(..), TypeData(..), td,
  ValData(..), vd, VarData(..), vard, CommonThunk, progD, fd,
  ScopeTag(..), pd)
import GOOL.Drasil.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue, on2CodeValues,
  on2StateValues, onCodeList, onStateList, emptyIfEmpty, on2StateWrapped)
import GOOL.Drasil.State (MS, VS, CS, lensGStoFS, revFiles, setFileType, lensCStoMS,
  lensMStoVS, lensVStoMS, getModuleImports, getUsing, getLangImports,
  getLibImports, getMainDoc, useVarName, getClassName, setClassName)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe)
import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)
import Data.List (intercalate, sort, partition)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), empty, brackets, vcat,
  quotes, parens, equals, colon)

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

instance OOProg JuliaCode

-- I have no clue what this does, but most of the other targets do it...
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
  langImport _ = undefined
  modImport = undefined

instance ImportElim JuliaCode where
  import' = undefined

instance PermanenceSym JuliaCode where
  type Permanence JuliaCode = Doc
  static = toCode empty -- TODO: merge with Python
  dynamic = toCode R.dynamic

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
  multiBlock = undefined

instance BlockElim JuliaCode where
  block = unJLC

instance TypeSym JuliaCode where
  type Type JuliaCode = TypeData
  bool = CP.bool
  int = jlIntType
  float = jlFloatType
  double = jlDoubleType
  char = jlCharType
  string = CP.string'
  infile = jlInfileType
  outfile = jlOutfileType
  listType = jlListType
  arrayType = listType -- Treat arrays and lists the same, as in Python
  listInnerType = G.listInnerType
  obj = G.obj -- Julia's not OO, but I *think* this is fine
  funcType = CP.funcType -- Julia's functions support multiple-dispatch, so we might need to revisit this
  void = jlVoidType

instance TypeElim JuliaCode where
  getType = cType . unJLC
  getTypeString = typeString . unJLC

instance RenderType JuliaCode where
  multiType ts = do
    typs <- sequence ts
    let mt = jlTuple $ map getTypeString typs
    typeFromData Void mt (text mt)
  typeFromData t s d = toState $ toCode $ td t s d

instance InternalTypeElim JuliaCode where
  type' = typeDoc . unJLC

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
  staticVar = undefined
  constant = undefined
  extVar _ = undefined
  self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar jlSelf (obj l) (text jlSelf))
  classVar = undefined
  extClassVar = undefined
  objVar = G.objVar
  objVarSelf = objVar self
  arrayElem _ = undefined

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
  pointerArg = undefined

instance Literal JuliaCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = G.litChar quotes
  litDouble = G.litDouble
  litFloat = C.litFloat
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
  arg _ = undefined
  argsList = undefined
  argExists _ = undefined

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
  selfFuncAppMixedArgs = undefined
  extFuncAppMixedArgs = undefined
  libFuncAppMixedArgs = undefined
  newObjMixedArgs = undefined
  extNewObjMixedArgs _ _ _ _ = undefined
  libNewObjMixedArgs = undefined

  lambda = G.lambda jlLambda

  notNull = undefined

instance RenderValue JuliaCode where
  inputFunc = undefined
  printFunc = mkStateVal void jlPrintFunc
  printLnFunc = mkStateVal void jlPrintLnFunc
  printFileFunc _ = undefined
  printFileLnFunc _ = undefined

  cast = undefined

  call = G.call jlNamedArgSep

  valFromData p t' d = do
    t <- t'
    toState $ on2CodeValues (vd p) t (toCode d)

instance ValueElim JuliaCode where
  valuePrec = valPrec . unJLC
  value = val . unJLC

instance InternalValueExp JuliaCode where
  objMethodCallMixedArgs' = undefined

instance FunctionSym JuliaCode where
  type Function JuliaCode = FuncData
  func = G.func
  objAccess = G.objAccess -- TODO: this is probably wrong :(

instance GetSet JuliaCode where
  get = undefined
  set = undefined

instance List JuliaCode where
  listSize = on2StateWrapped (\f v -> mkVal (functionType f) -- TODO: merge with Python
    (jlListSize (RC.value v) (RC.function f))) listSizeFunc
  listAdd l i v = funcApp jlListInsert void [l, i #+ litInt 1, v]
  listAppend l v = funcApp jlListAppend void [l, v]
  listAccess l i = G.listAccess l (i #+ litInt 1)
  listSet l i = G.listSet l (i #+ litInt 1)
  indexOf l v = do
    v' <- v
    let t = return $ valueType v'
    funcApp jlListIndex t [lambda [var "x" t] (valueOf (var "x" t) ?== v), l] #- litInt 1

instance InternalList JuliaCode where
  -- TODO: This has the same behaviour as all except Python, which does it right.
  listSlice' b e s vn vo = jlListSlice vn vo (bIndex b) (eIndex e) (getVal s)
    where bIndex Nothing = mkStateVal void jlStart
          bIndex (Just x) = x #+ litInt 1
          eIndex Nothing = mkStateVal void jlEnd
          eIndex (Just x) = x #+ litInt 1
          getVal = fromMaybe $ mkStateVal void empty

instance InternalGetSet JuliaCode where
  getFunc = undefined
  setFunc = undefined

instance InternalListFunc JuliaCode where
  listSizeFunc = funcFromData jlListSizeFunc int
  listAddFunc = undefined
  listAppendFunc = undefined
  listAccessFunc = CP.listAccessFunc
  listSetFunc = CP.listSetFunc R.listSetFunc

instance ThunkSym JuliaCode where
  type Thunk JuliaCode = CommonThunk VS

instance ThunkAssign JuliaCode where
  thunkAssign _ _ = undefined

instance VectorType JuliaCode where
  vecType = listType

instance VectorDecl JuliaCode where
  vecDec = undefined
  vecDecDef = undefined

instance VectorThunk JuliaCode where
  vecThunk = undefined

instance VectorExpression JuliaCode where
  vecScale _ = undefined
  vecAdd = undefined
  vecIndex _ = undefined
  vecDot = undefined

instance RenderFunction JuliaCode where
  funcFromData d = onStateValue $ onCodeValue (`fd` d)

instance FunctionElim JuliaCode where
  functionType = onCodeValue fType
  function = funcDoc . unJLC

instance InternalAssignStmt JuliaCode where
  multiAssign = undefined

instance InternalIOStmt JuliaCode where
  printSt = jlPrint

instance InternalControlStmt JuliaCode where
  multiReturn = undefined

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
  varDec v = do -- TODO: Merge with Python
    v' <- zoom lensMStoVS v
    modify $ useVarName (variableName v')
    mkStmtNoEnd empty
  varDecDef v e = do -- TODO: Merge with Python
    v' <- zoom lensMStoVS v
    modify $ useVarName (variableName v')
    assign v e
  listDec _ = CP.listDec
  listDecDef = CP.listDecDef
  arrayDec = listDec
  arrayDecDef = listDecDef
  objDecDef = undefined
  objDecNew = undefined
  extObjDecNew = undefined
  constDecDef _ _ = undefined
  funcDecDef = undefined

instance IOStatement JuliaCode where
  print      = jlOut False Nothing printFunc
  printLn    = jlOut True  Nothing printLnFunc
  printStr   = jlOut False Nothing printFunc   . litString
  printStrLn = jlOut True  Nothing printLnFunc . litString

  printFile f      = jlOut False (Just f) printFunc
  printFileLn f    = jlOut True (Just f) printLnFunc
  printFileStr f   = printFile   f . litString
  printFileStrLn f = printFileLn f . litString

  getInput _ = undefined
  discardInput = undefined
  getFileInput _ _ = undefined
  discardFileInput _ = undefined
  openFileR _ _ = undefined
  openFileW = undefined
  openFileA = undefined
  closeFile = undefined
  getFileInputLine _ _ = undefined
  discardFileLine _ = undefined
  getFileInputAll _ _ = undefined

instance StringStatement JuliaCode where
  stringSplit d vnew s = vnew &= funcApp jlSplit (listType string) [s, litString [d]]
  stringListVals = undefined
  stringListLists = undefined

instance FuncAppStatement JuliaCode where
  inOutCall = undefined
  selfInOutCall = undefined
  extInOutCall _ = undefined

instance CommentStatement JuliaCode where
  comment = G.comment jlCmtStart

instance ControlStatement JuliaCode where
  break = undefined
  continue = undefined
  returnStmt = G.returnStmt Empty
  throw _ = undefined
  ifCond = G.ifCond id empty G.defaultOptSpace elseIfLabel jlEnd -- TODO: make Python use id instead of parens
  switch = undefined
  ifExists = undefined
  for _ _ _ _ = undefined
  forRange _ _ _ _ = undefined
  forEach = undefined
  while = undefined
  tryCatch = undefined

instance StatePattern JuliaCode where
  checkState = undefined

instance ObserverPattern JuliaCode where
  notifyObservers = undefined

instance StrategyPattern JuliaCode where
  runStrategy = undefined

instance ScopeSym JuliaCode where
  type Scope JuliaCode = Doc

  private = toCode empty -- Julia doesn't have private/public members
  public = toCode empty

instance RenderScope JuliaCode where
  scopeFromData _ = undefined

instance ScopeElim JuliaCode where
  scope = unJLC

instance MethodTypeSym JuliaCode where
  type MethodType JuliaCode = TypeData

  mType = zoom lensMStoVS
  construct = G.construct

instance ParameterSym JuliaCode where
  type Parameter JuliaCode = ParamData

  param = G.param jlParam
  pointerParam = undefined

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
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor ps is b = getClassName >>= (\n -> jlConstructor n ps is b)
  docMain = undefined
  function = undefined
  mainFunction = CP.mainBody
  docFunc = undefined
  inOutMethod _ _ _ = undefined
  docInOutMethod _ _ _ = undefined
  inOutFunc _ _ = undefined
  docInOutFunc _ _ = undefined

instance RenderMethod JuliaCode where
  intMethod _ n _ _ t ps b = do
    tp  <- t
    pms <- sequence ps
    bod <- b
    name <- getClassName
    let pmlst = parameterList pms
        oneParam = emptyIfEmpty pmlst listSep'
        self' :: SVariable JuliaCode
        self' = self
    sl <- zoom lensMStoVS self'
    mthdFromData Pub (vcat [
      jlFunc <+> text n <> parens (RC.variable sl <> jlType <> text name <>
        oneParam <> pmlst) <> jlType <> RC.type' tp,
      indent $ RC.body bod,
      jlEnd])
  intFunc _ _ _ _ = undefined
  commentedFunc _ _ = undefined
  destructor _ = undefined
  mthdFromData _ d = toState $ toCode $ mthd d

instance MethodElim JuliaCode where
  method = mthdDoc . unJLC

instance StateVarSym JuliaCode where
  type StateVar JuliaCode = Doc

  stateVar s p v = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
    (RC.scope s) (RC.perm p) . RC.statement) (stmt $ varDec' v)
  stateVarDef = undefined
  constVar = undefined

instance StateVarElim JuliaCode where
  stateVar = unJLC

instance ClassSym JuliaCode where
  type Class JuliaCode = Doc

  buildClass = G.buildClass
  extraClass = undefined
  implementingClass = undefined
  docClass = G.docClass jlClassDoc

instance RenderClass JuliaCode where
  intClass = jlIntClass
  inherit sup = case sup of
    Nothing  -> toCode empty
    (Just _) -> error "Julia doesn't support inheritance."
  implements = undefined
  commentedClass = G.commentedClass

instance ClassElim JuliaCode where
  class' = unJLC

instance ModuleSym JuliaCode where
  type Module JuliaCode = ModData

  buildModule n is = CP.buildModule n (do
    lis <- getLangImports
    libis <- getLibImports
    mis <- getModuleImports
    us <- getUsing
    pure $ vibcat [
      vcat (map (RC.import' . li) lis),
      vcat (map (RC.import' . li) (sort $ is ++ libis)),
      vcat (map (RC.import' . mi) mis),
      vcat (map usingModule us)])
    (pure empty) getMainDoc
    where mi, li :: Label -> JuliaCode (Import JuliaCode)
          mi = modImport
          li = langImport

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

jlInt, jlFloat, jlDouble, jlChar, jlFile, jlList, jlVoid :: String
jlInt = "Integer" -- Q: Do we use concrete or abstract types?
jlFloat = "AbstractFloat"
jlDouble = "AbstractFloat"
jlChar = "AbstractChar"
jlFile = "IOStream"
jlList = "AbstractArray"
jlVoid = "Nothing"

jlListInsert, jlListAppend, jlListIndex :: String
jlListInsert = "insert!"
jlListAppend = "append!"
jlListIndex    = "findfirst"

jlListSizeFunc :: Doc
jlListSizeFunc = text "length"

jlListSize :: Doc -> Doc -> Doc
jlListSize v f = f <> parens v

jlListSlice :: (RenderSym r, Monad r) => SVariable r -> SValue r -> SValue r ->
  SValue r -> SValue r -> MS (r Doc)
jlListSlice vn vo beg end step = zoom lensMStoVS $ do
  vnew <- vn
  vold <- vo
  b <- beg
  e <- end
  s <- step
  let step' = emptyIfEmpty (RC.value s) (colon <> RC.value s)
  pure $ toCode $ RC.variable vnew <+> equals <+> RC.value vold <>
    brackets (RC.value b <> step' <> colon <> RC.value e)

jlSplit :: String
jlSplit = "split"

jlPrintFunc, jlPrintLnFunc :: Doc
jlPrintFunc = text printLabel
jlPrintLnFunc = text "println"

jlType, arrow, jlNamedArgSep :: Doc
jlType = colon <> colon
arrow = text "->"
jlNamedArgSep = equals -- TODO: Merge with Python

jlTuple :: [String] -> String
jlTuple ts = "Tuple{" ++ intercalate listSep ts ++ "}"

-- Operators
jlUnaryMath :: (Monad r) => String -> VSOp r
jlUnaryMath = unOpPrec

jlPower, jlIntDiv :: String
jlPower = "^"
jlIntDiv = "÷" -- `div()` is also allowed - should we give a choice or anything?

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


jlIntClass :: (RenderSym r, Monad r) => Label -> r (Scope r) -> r ParentSpec -> 
  [CSStateVar r] -> [SMethod r] -> CS (r Doc)
jlIntClass n _ i svrs mths = do
  modify (setClassName n) 
  svs <- onStateList (R.stateVarList . map RC.stateVar) svrs
  let (outerMths, cnstrs) = partition isMthd mths
  ms <- onStateList (vibcat . map RC.method) (map (zoom lensCStoMS) outerMths)
  cs <- onStateList (vibcat . map RC.method) (map (zoom lensCStoMS) cnstrs)
  return $ onCodeValue (\_ -> jlClass n svs ms cs) i 
  where isMthd _ = False

-- | Creates the doc for a class.
-- | n is the name of the class.
-- | vs is the variables of the class.
-- | ms is the methods of the class (other than the constructor).
-- | cstrs is the constructors.
jlClass :: Label -> Doc -> Doc -> Doc -> Doc
jlClass n vs ms cstrs = vcat [
  mutDec <+> structDec <+> text n,
  indentList [vs, blank, cstrs],
  jlEnd,
  blank,
  ms]

jlClassDoc :: ClassDocRenderer
jlClassDoc desc = [desc | not (null desc)]

-- | Constructor method.  Exists because Julia's constructors need to be 
--   completely different from other 'methods' in Julia.
jlConstructor :: (RenderSym r) => Label -> [MSParameter r] -> Initializers r ->
  MSBody r -> SMethod r
jlConstructor fName ps initStmts b = jlConstructorMethod fName
  ps (RC.multiBody [jlCallConstructor, b])
  where args = map (\(_, vl) -> vl) initStmts
        constructorFunc = funcApp fName (obj fName)
        jlCallConstructor = bodyStatements [returnStmt $ constructorFunc args]

jlConstructorMethod :: (RenderSym r) =>
  Label -> [MSParameter r] -> MSBody r -> SMethod r
jlConstructorMethod n ps b = do
  pms <- sequence ps
  bod <- b
  name <- getClassName
  mthdFromData Pub (vcat [
    jlFunc <+> text n <> parens (parameterList pms),
    indent $ RC.body bod,
    jlEnd])

jlLambda :: (RenderSym r) => [r (Variable r)] -> r (Value r) -> Doc
jlLambda ps ex = variableList ps <+> arrow <+> RC.value ex

elseIfLabel, jlFunc, jlStart, jlEnd :: Doc
elseIfLabel = text "elseif"
jlFunc      = text "function"
jlStart     = text "start"
jlEnd       = text "end"

jlSelf :: String
jlSelf = "self" -- TODO: this is a placeholder (or maybe it's fine)

structDec, mutDec :: Doc
structDec = text "struct"
mutDec = text "mutable"

jlParam :: (RenderSym r) => r (Variable r) -> Doc
jlParam v = RC.variable v <> jlType <> RC.type' (variableType v)

varDec' :: SVariable JuliaCode -> MSStatement JuliaCode
varDec' v' = do
  v <- zoom lensMStoVS v'
  modify $ useVarName (variableName v)
  mkStmtNoEnd (RC.variable v <> jlType <> RC.type' (variableType v))

-- Type names specific to Julia (there's a lot of them)
jlIntType :: (RenderSym r) => VSType r
jlIntType = typeFromData Integer jlInt (text jlInt)

jlFloatType :: (RenderSym r) => VSType r
jlFloatType = typeFromData Float jlFloat (text jlFloat)

jlDoubleType :: (RenderSym r) => VSType r
jlDoubleType = typeFromData Double jlDouble (text jlDouble)

jlCharType :: (RenderSym r) => VSType r
jlCharType = typeFromData Char jlChar (text jlChar)

jlInfileType :: (RenderSym r) => VSType r
jlInfileType = typeFromData InFile jlFile (text jlFile)

jlOutfileType :: (RenderSym r) => VSType r
jlOutfileType = typeFromData OutFile jlFile (text jlFile)

jlListType :: (RenderSym r) => VSType r -> VSType r
jlListType t' = do
  t <- t'
  let typeName = jlList ++ "{" ++ getTypeString t ++ "}"
  typeFromData (List $ getType t) typeName (text typeName)

jlVoidType :: (RenderSym r) => VSType r
jlVoidType = typeFromData Void jlVoid (text jlVoid)

-- Modules
using :: Doc
using = text "using" -- TODO: merge with C++

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
        -- Do we need an exception for objects?
