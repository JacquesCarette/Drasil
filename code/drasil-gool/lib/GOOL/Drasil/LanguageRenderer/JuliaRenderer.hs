{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

-- | The logic to render Julia code is contained in this module
module GOOL.Drasil.LanguageRenderer.JuliaRenderer (
  -- * Julia Code Configuration -- defines syntax of all Julia code
  JuliaCode(..), jlName, jlVersion
) where

import Utils.Drasil (stringList)

-- TODO: Sort the dependencies to match the other modules
import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (VSType, SValue, MSStatement, OOProg, ProgramSym(..),
  FileSym(..), PermanenceSym(..), BodySym(..), BlockSym(..), TypeSym(..),
  TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..), Argument(..),
  Literal(..), MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), InternalValueExp(..), FunctionSym(..), GetSet(..),
  List(..), InternalList(..), ThunkSym(..), VectorType(..), VectorDecl(..),
  VectorThunk(..), VectorExpression(..), ThunkAssign(..), StatementSym(..),
  AssignStatement(..), DeclStatement(..), IOStatement(..), StringStatement(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  StatePattern(..), ObserverPattern(..), StrategyPattern(..), ScopeSym(..),
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(..), ImportSym(..), 
  ImportElim, PermElim(binding), RenderBody(..), BodyElim, RenderBlock(..), 
  BlockElim, RenderType(..), InternalTypeElim, UnaryOpSym(..), BinaryOpSym(..), 
  OpElim(uOpPrec, bOpPrec), RenderVariable(..), InternalVarElim(variableBind), 
  RenderValue(..), ValueElim(valuePrec), InternalGetSet(..), 
  InternalListFunc(..), RenderFunction(..), 
  FunctionElim(functionType), InternalAssignStmt(..), InternalIOStmt(..), 
  InternalControlStmt(..), RenderStatement(..), StatementElim(statementTerm), 
  RenderScope(..), ScopeElim, MethodTypeSym(..), RenderParam(..), 
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim, 
  StateVarElim, RenderClass(..), ClassElim, RenderMod(..), ModuleElim, 
  BlockCommentSym(..), BlockCommentElim)
import qualified GOOL.Drasil.RendererClasses as RC (import', perm, body, block, 
  type', uOp, bOp, variable, value, function, statement, scope, parameter,
  method, stateVar, class', module', blockComment')
import GOOL.Drasil.LanguageRenderer (printLabel, listSep, listSep', ModuleDocRenderer)
import qualified GOOL.Drasil.LanguageRenderer as R (sqrt, abs, log10, log, exp, sin, cos, tan, asin, acos, atan, 
  floor, ceil, commentedMod)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStateVal, VSOp, unOpPrec, powerPrec, unExpr, unExpr', binExpr, multPrec, typeUnExpr, typeBinExpr, mkStmtNoEnd)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  listInnerType, obj, litChar, litDouble, litInt, litString, negateOp, equalOp,
  notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp, minusOp,
  multOp, divideOp, modFromData, fileDoc, docMod, moduloOp, fileFromData, csc, sec, cot, assign, increment, subAssign, print)
import qualified GOOL.Drasil.LanguageRenderer.CommonPseudoOO as CP (string', funcType, litArray)
import qualified GOOL.Drasil.LanguageRenderer.CLike as C (litTrue, litFalse,
  litFloat, notOp, andOp, orOp, inlineIf)
import qualified GOOL.Drasil.LanguageRenderer.Macros as M (increment1, decrement1)
import GOOL.Drasil.AST (Terminator(..), FileType(..), FileData(..), fileD, FuncData(..), ModData(..),
  md, updateMod, MethodData(..), OpData(..), ParamData(..), ProgData(..), TypeData(..), td,
  ValData(..), VarData(..), CommonThunk, progD)
import GOOL.Drasil.Helpers (toCode, toState, onCodeValue, on2CodeValues, on2StateValues, onCodeList, emptyIfEmpty)
import GOOL.Drasil.State (VS, lensGStoFS, revFiles, setFileType, lensMStoVS)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Data.Maybe (fromMaybe)
import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)
import Data.List (intercalate)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), empty, brackets, quotes, parens)
import Metadata.Drasil.DrasilMetaCall (watermark)

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
  docMod = G.docMod jlModDoc jlExt

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
  static = undefined
  dynamic = undefined

instance PermElim JuliaCode where
  perm = undefined
  binding = undefined

instance BodySym JuliaCode where
  type Body JuliaCode = Doc
  body = undefined

  addComments _ = undefined

instance RenderBody JuliaCode where
  multiBody = undefined

instance BodyElim JuliaCode where
  body = undefined

instance BlockSym JuliaCode where
  type Block JuliaCode = Doc
  block = undefined

instance RenderBlock JuliaCode where
  multiBlock = undefined

instance BlockElim JuliaCode where
  block = undefined

instance TypeSym JuliaCode where
  type Type JuliaCode = TypeData
  bool = typeFromData Boolean "Bool" (text "Bool") -- TODO: merge with Swift
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
  uOp = undefined
  bOp = undefined
  uOpPrec = undefined
  bOpPrec = undefined

instance VariableSym JuliaCode where
  type Variable JuliaCode = VarData
  var = undefined
  staticVar = undefined
  constant = undefined
  extVar _ = undefined
  self = undefined
  classVar = undefined
  extClassVar = undefined
  objVar = undefined
  objVarSelf = undefined
  arrayElem _ = undefined

instance VariableElim JuliaCode where
  variableName = undefined
  variableType = undefined

instance InternalVarElim JuliaCode where
  variableBind = undefined
  variable = undefined

instance RenderVariable JuliaCode where
  varFromData _ _ _ _ = undefined

instance ValueSym JuliaCode where
  type Value JuliaCode = ValData
  valueType = undefined

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
  valueOf = undefined

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

  funcAppMixedArgs = undefined
  selfFuncAppMixedArgs = undefined
  extFuncAppMixedArgs = undefined
  libFuncAppMixedArgs = undefined
  newObjMixedArgs = undefined
  extNewObjMixedArgs _ _ _ _ = undefined
  libNewObjMixedArgs = undefined

  lambda = undefined

  notNull = undefined

instance RenderValue JuliaCode where
  inputFunc = undefined
  printFunc = undefined
  printLnFunc = undefined
  printFileFunc _ = undefined
  printFileLnFunc _ = undefined

  cast = undefined

  call _ _ _ _ _ _ = undefined

  valFromData _ _ _ = undefined

instance ValueElim JuliaCode where
  valuePrec = undefined
  value = undefined

instance InternalValueExp JuliaCode where
  objMethodCallMixedArgs' = undefined

instance FunctionSym JuliaCode where
  type Function JuliaCode = FuncData
  func = undefined
  objAccess = undefined

instance GetSet JuliaCode where
  get = undefined
  set = undefined

instance List JuliaCode where
  listSize = undefined
  listAdd = undefined
  listAppend = undefined
  listAccess = undefined
  listSet = undefined
  indexOf = undefined

instance InternalList JuliaCode where
  listSlice' _ _ _ _ _ = undefined

instance InternalGetSet JuliaCode where
  getFunc = undefined
  setFunc = undefined

instance InternalListFunc JuliaCode where
  listSizeFunc = undefined
  listAddFunc = undefined
  listAppendFunc = undefined
  listAccessFunc = undefined
  listSetFunc = undefined

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
  funcFromData _ = undefined

instance FunctionElim JuliaCode where
  functionType = undefined
  function :: JuliaCode (Function JuliaCode) -> Doc
  function = undefined

instance InternalAssignStmt JuliaCode where
  multiAssign = undefined

instance InternalIOStmt JuliaCode where
  printSt = jlPrint

instance InternalControlStmt JuliaCode where
  multiReturn = undefined

instance RenderStatement JuliaCode where
  stmt = undefined
  loopStmt = undefined
  emptyStmt = undefined
  stmtFromData _ _ = undefined

instance StatementElim JuliaCode where
  statement = undefined
  statementTerm = undefined

instance StatementSym JuliaCode where
  type Statement JuliaCode = (Doc, Terminator)
  valStmt = undefined
  multi = undefined

instance AssignStatement JuliaCode where
  assign = G.assign Empty
  (&-=) = G.subAssign Empty
  (&+=) = G.increment
  (&++) = M.increment1
  (&--) = M.decrement1

instance DeclStatement JuliaCode where
  varDec = undefined
  varDecDef = undefined
  listDec _ = undefined
  listDecDef = undefined
  arrayDec = undefined
  arrayDecDef = undefined
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
  stringSplit _ _ _ = undefined
  stringListVals = undefined
  stringListLists = undefined

instance FuncAppStatement JuliaCode where
  inOutCall = undefined
  selfInOutCall = undefined
  extInOutCall _ = undefined

instance CommentStatement JuliaCode where
  comment = undefined

instance ControlStatement JuliaCode where
  break = undefined
  continue = undefined
  returnStmt = undefined
  throw _ = undefined
  ifCond = undefined
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

  private = undefined
  public = undefined

instance RenderScope JuliaCode where
  scopeFromData _ = undefined

instance ScopeElim JuliaCode where
  scope = undefined

instance MethodTypeSym JuliaCode where
  type MethodType JuliaCode = TypeData

  mType = undefined
  construct = undefined

instance ParameterSym JuliaCode where
  type Parameter JuliaCode = ParamData

  param = undefined
  pointerParam = undefined

instance RenderParam JuliaCode where
  paramFromData _ _ = undefined

instance ParamElim JuliaCode where
  parameterName = undefined
  parameterType = undefined
  parameter = undefined

instance MethodSym JuliaCode where
  type Method JuliaCode = MethodData

  method = undefined
  getMethod = undefined
  setMethod = undefined
  constructor = undefined
  docMain = undefined
  function = undefined
  mainFunction = undefined
  docFunc = undefined
  inOutMethod _ _ _ = undefined
  docInOutMethod _ _ _ = undefined
  inOutFunc _ _ = undefined
  docInOutFunc _ _ = undefined

instance RenderMethod JuliaCode where
  intMethod _ = undefined
  intFunc _ _ _ _ = undefined
  commentedFunc _ _ = undefined
  destructor _ = undefined
  mthdFromData _ _ = undefined

instance MethodElim JuliaCode where
  method = undefined

instance StateVarSym JuliaCode where
  type StateVar JuliaCode = Doc

  stateVar _ _ _ = undefined
  stateVarDef = undefined
  constVar = undefined

instance StateVarElim JuliaCode where
  stateVar = undefined

instance ClassSym JuliaCode where
  type Class JuliaCode = Doc

  buildClass = undefined
  extraClass = undefined
  implementingClass = undefined
  docClass = undefined

instance RenderClass JuliaCode where
  intClass = undefined
  inherit = undefined
  implements = undefined
  commentedClass = undefined

instance ClassElim JuliaCode where
  class' = undefined

instance ModuleSym JuliaCode where
  type Module JuliaCode = ModData

  buildModule n is fs cs = undefined

instance RenderMod JuliaCode where
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim JuliaCode where
  module' = undefined

instance BlockCommentSym JuliaCode where
  type BlockComment JuliaCode = Doc

  blockComment _ = undefined
  docComment = undefined

instance BlockCommentElim JuliaCode where
  blockComment' = undefined

-- convenience
jlName, jlVersion :: String
jlName = "Julia"
jlVersion = "1.10.3"

jlInt, jlFloat, jlDouble, jlChar, jlFile, jlList, jlVoid :: String
jlInt = "Int32" -- TODO: Do we use concrete or abstract types?
jlFloat = "Float32"
jlDouble = "Float64"
jlChar = "Char"
jlFile = "IOStream"
jlList = "Vector"
jlVoid = "Nothing"

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

-- Documentation
jlModDoc :: ModuleDocRenderer
jlModDoc desc as date m = m : [desc | not (null desc)] ++
      [jlDocField jlAuthorDoc (stringList as) | not (null as)] ++
      [jlDocField jlDateDoc date | not (null date)] ++
      [jlDocField jlNoteDoc watermark]

jlDocCommandInit, jlDocCommandSep, jlAuthorDoc, jlDateDoc, jlNoteDoc :: String
jlDocCommandInit = "- "
jlDocCommandSep = ": "
jlAuthorDoc = "Authors"
jlDateDoc = "Date"
jlNoteDoc = "Note"

-- | Creates an arbitrary Julia-DocC Markup field for documentation.
-- Takes two strings, one for the field type ('ty'), and another for the
-- field information ('info')
jlDocField :: String -> String -> String
jlDocField ty info = jlDocCommandInit ++ ty ++ jlDocCommandSep ++ info