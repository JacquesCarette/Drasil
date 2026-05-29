{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- | MVP renderer for logging statements.

module Drasil.Shared.LanguageRenderer.Logger (LoggerCode(..)) where

import Drasil.Shared.InterfaceCommon (SharedProg, VSType, TypeSym(..),
  VariableSym(..), ValueSym(..), Literal(..), BlockSym(..), BodySym(..),
  StatementSym(..), Argument(..), VectorType(..), VectorDecl(..), ThunkSym(..),
  VectorThunk(..), VectorExpression(..), ThunkAssign(..), AssignStatement(..),
  DeclStatement(..), IOStatement(..), StringStatement(..), FunctionSym(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  InternalList(..), MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), IndexTranslator(..), Array(..), List(..), Set(..),
  TypeElim(..), VariableElim(..), ParameterSym(..), VisibilitySym(..),
  MethodSym(..), ScopeSym(..), BinderSym(..))
import Drasil.GOOL.InterfaceGOOL (OOProg, OOTypeSym(..), OOVariableSym(..),
  OOVariableValue, OODeclStatement(..), OOFuncAppStatement(..),
  OOValueExpression(..), InternalValueExp(..), GetSet(..), ObserverPattern(..),
  StrategyPattern(..), ClassSym(..), OOMethodSym(..), StateVarSym(..),
  AttachmentSym(..), OOValueSym, OOFunctionSym(..), convTypeOO)
import qualified Drasil.GOOL.InterfaceGOOL as GOOL (ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.GProc.InterfaceProc (ProcProg)
import qualified Drasil.GProc.InterfaceProc as GProc (ProgramSym(..),
  FileSym(..), ModuleSym(..))
import Drasil.Shared.AST (TypeData(..), td)
import Drasil.Shared.CodeType (CodeType(..))

import Text.PrettyPrint.HughesPJ (Doc, text, comma, space, brackets, braces,
  punctuate, hcat)
import qualified Text.PrettyPrint.HughesPJ as P (char, integer, float, double)
import Data.List (intercalate)

newtype (SharedProg lang) => LoggerCode lang a = LC {unLC :: a} deriving Functor

instance Applicative (LoggerCode lang) where
  pure = LC
  (LC f) <*> (LC x) = LC (f x)

instance Monad (LoggerCode lang) where
  LC x >>= f = f x

instance SharedProg (LoggerCode lang)
instance ProcProg (LoggerCode lang)
instance OOProg (LoggerCode lang)

instance VariableSym (LoggerCode lang) where
  type Variable (LoggerCode lang) = Doc
  var n _ = return $ return $ text n
  constant n _ = return $ return $ text n
  extVar l n _ = return $ return $ text l <> text "." <> text n

instance OOVariableSym (LoggerCode lang) where
  classVar = var
  classConst = constant
  self = return $ return $ text "self"
  classVarAccess cls vr = do
    cls' <- cls
    vr' <- vr
    let clsDoc = (typeDoc . unLC) cls'
        vrDoc = unLC vr'
    return $ return $ clsDoc <> text "." <> vrDoc
  extClassVarAccess = classVarAccess
  instanceVarAccess ob vr = do
    ob' <- ob
    vr' <- vr
    return $ return $ unLC ob' <> text "." <> unLC vr'
  instanceVarSelf vr = do
    vr' <- vr
    return $ return $ text "self." <> unLC vr'

instance ValueSym (LoggerCode lang) where
  type Value (LoggerCode lang) = Doc
  valueType = error "Not implemented"

instance TypeSym (LoggerCode lang) where
  bool = typeFromData Boolean "Boolean"
  int = typeFromData Integer "Integer"
  float = typeFromData Float "Float"
  double = typeFromData Double "Double"
  char = typeFromData Char "Char"
  string = typeFromData String "String"
  infile = typeFromData InFile "InFile"
  outfile = typeFromData OutFile "OutFile"
  listType tp = do
    tp' <- tp
    let tpData = unLC tp'
    typeFromData (List (cType tpData)) ("List<" ++ typeString tpData ++ ">")
  setType tp = do
    tp' <- tp
    let tpData = unLC tp'
    typeFromData (Set (cType tpData)) ("Set<" ++ typeString tpData ++ ">")
  arrayType tp = do
    tp' <- tp
    let tpData = unLC tp'
    typeFromData (Array (cType tpData)) ("Array<" ++ typeString tpData ++ ">")
  listInnerType tp = tp >>= (convTypeOO . cType . unLC)
  funcType inTps outTp = do
    inTps' <- sequence inTps
    outTp' <- outTp
    let inTpsData = map unLC inTps'
        outTpData = unLC outTp'
    typeFromData (Func (map cType inTpsData) (cType outTpData))
      ("(" ++ intercalate " × " (map typeString inTpsData) ++ ") → " ++ typeString outTpData)
  void = typeFromData Void "Void"

instance OOTypeSym (LoggerCode lang) where
  obj nm = typeFromData (Object nm) ("Object<" ++ nm ++ ">")

typeFromData :: CodeType -> String -> VSType (LoggerCode lang)
typeFromData tp str = return $ return $ td tp str (text str)

instance Literal (LoggerCode lang) where
  litTrue = litString "True"
  litFalse = litString "False"
  litChar = return . return . P.char
  litDouble = return . return . P.double
  litFloat = return . return . P.float
  litInt = return . return . P.integer
  litString = return . return . text
  litArray _ vs = do
    vs' <- sequence vs
    let docs = map unLC vs'
    return $ return $ brackets $ hcat $ punctuate (comma <> space) docs
  litList = litArray
  litSet _ vs = do
    vs' <- sequence vs
    let docs = map unLC vs'
    return $ return $ braces $ hcat $ punctuate (comma <> space) docs

instance IndexTranslator (LoggerCode lang) where
  intToIndex = id
  indexToInt = id

instance Array (LoggerCode lang) where
  arrayElem idx' vr' = do
    idx <- idx'
    vr <- vr'
    return $ return $ unLC idx <> brackets (unLC vr)

-- Not Implemented
instance BlockSym (LoggerCode lang) where
  type Block (LoggerCode lang) = ()
  block = undefined

instance BodySym (LoggerCode lang) where
  type Body (LoggerCode lang) = ()
  body = undefined
  addComments = undefined

instance StatementSym (LoggerCode lang) where
  type Statement (LoggerCode lang) = ()
  valStmt = undefined
  emptyStmt = undefined
  multi = undefined

instance Argument (LoggerCode lang) where
  pointerArg = undefined

instance VectorType (LoggerCode lang) where
  vecType = undefined

instance VectorDecl (LoggerCode lang) where
  vecDec = undefined
  vecDecDef = undefined

instance ThunkSym (LoggerCode lang) where
  type Thunk (LoggerCode lang) = ()

instance VectorThunk (LoggerCode lang) where
  vecThunk = undefined

instance VectorExpression (LoggerCode lang) where
  vecScale = undefined
  vecAdd = undefined
  vecIndex = undefined
  vecDot = undefined

instance ThunkAssign (LoggerCode lang) where
  thunkAssign = undefined

instance AssignStatement (LoggerCode lang) where
  (&-=) = undefined
  (&+=) = undefined
  (&++) = undefined
  (&--) = undefined
  assign = undefined

instance DeclStatement (LoggerCode lang) where
  varDec = undefined
  varDecDef = undefined
  listDec = undefined
  listDecDef = undefined
  setDec = undefined
  setDecDef = undefined
  arrayDec = undefined
  arrayDecDef = undefined
  constDecDef = undefined
  funcDecDef = undefined

instance IOStatement (LoggerCode lang) where
  print = undefined
  printLn = undefined
  printStr = undefined
  printStrLn = undefined

  printFile = undefined
  printFileLn = undefined
  printFileStr = undefined
  printFileStrLn = undefined

  getInput = undefined
  discardInput = undefined
  getFileInput = undefined
  discardFileInput = undefined

  openFileR = undefined
  openFileW = undefined
  openFileA = undefined
  closeFile = undefined

  getFileInputLine = undefined
  discardFileLine = undefined
  getFileInputAll = undefined

instance StringStatement (LoggerCode lang) where
  stringSplit = undefined
  stringListVals = undefined
  stringListLists = undefined

instance FunctionSym (LoggerCode lang) where
  type Function (LoggerCode lang) = ()

instance FuncAppStatement (LoggerCode lang) where
  inOutCall = undefined
  extInOutCall = undefined

instance CommentStatement (LoggerCode lang) where
  comment = undefined

instance ControlStatement (LoggerCode lang) where
  break = undefined
  continue = undefined
  returnStmt = undefined
  throw = undefined
  ifCond = undefined
  switch = undefined
  ifExists = undefined
  for = undefined
  forRange = undefined
  forEach = undefined
  while = undefined
  tryCatch = undefined
  assert = undefined

instance InternalList (LoggerCode lang) where
  listSlice' = undefined

instance MathConstant (LoggerCode lang) where
  pi = undefined

instance VariableValue (LoggerCode lang) where
  valueOf = undefined

instance CommandLineArgs (LoggerCode lang) where
  arg = undefined
  argsList = undefined
  argExists = undefined

instance NumericExpression (LoggerCode lang) where
  (#~) = undefined
  (#/^) = undefined
  (#|) = undefined
  (#+) = undefined
  (#-) = undefined
  (#*) = undefined
  (#/) = undefined
  (#%) = undefined
  (#^) = undefined
  log = undefined
  ln = undefined
  exp = undefined
  sin = undefined
  cos = undefined
  tan = undefined
  csc = undefined
  sec = undefined
  cot = undefined
  arcsin = undefined
  arccos = undefined
  arctan = undefined
  floor = undefined
  ceil = undefined

instance BooleanExpression (LoggerCode lang) where
  (?!) = undefined
  (?&&) = undefined
  (?||) = undefined

instance Comparison (LoggerCode lang) where
  (?<) = undefined
  (?<=) = undefined
  (?>) = undefined
  (?>=) = undefined
  (?==) = undefined
  (?!=) = undefined

instance ValueExpression (LoggerCode lang) where
  inlineIf = undefined
  funcAppMixedArgs = undefined
  extFuncAppMixedArgs = undefined
  libFuncAppMixedArgs = undefined
  lambda = undefined
  notNull = undefined

instance List (LoggerCode lang) where
  listSize = undefined
  listAdd = undefined
  listAppend = undefined
  listAccess = undefined
  listSet = undefined
  indexOf = undefined

instance Set (LoggerCode lang) where
  contains = undefined
  setAdd = undefined
  setRemove = undefined
  setUnion = undefined

instance TypeElim (LoggerCode lang) where
  getType = undefined
  getTypeString = undefined

instance VariableElim (LoggerCode lang) where
  variableName = undefined
  variableType = undefined

instance ParameterSym (LoggerCode lang) where
  type Parameter (LoggerCode lang) = ()
  param = undefined
  pointerParam = undefined

instance VisibilitySym (LoggerCode lang) where
  type Visibility (LoggerCode lang) = ()
  private = undefined
  public = undefined

instance MethodSym (LoggerCode lang) where
  type Method (LoggerCode lang) = ()
  docMain = undefined
  function = undefined
  mainFunction = undefined
  docFunc = undefined
  inOutFunc = undefined
  docInOutFunc = undefined

instance ScopeSym (LoggerCode lang) where
  global = undefined
  local = undefined
  mainFn = undefined

instance BinderSym (LoggerCode lang) where
  binder = undefined

-- GOOL-specific
instance GOOL.ProgramSym (LoggerCode lang) where
  type Program (LoggerCode lang) = ()
  prog = undefined

instance GOOL.FileSym (LoggerCode lang) where
  type File (LoggerCode lang) = ()
  fileDoc = undefined
  docMod = undefined

instance GOOL.ModuleSym (LoggerCode lang) where
  type Module (LoggerCode lang) = ()
  buildModule = undefined

instance ClassSym (LoggerCode lang) where
  type Class (LoggerCode lang) = ()
  buildClass = undefined
  extraClass = undefined
  implementingClass = undefined
  docClass = undefined

instance OOMethodSym (LoggerCode lang) where
  method = undefined
  getMethod = undefined
  setMethod = undefined
  constructor = undefined
  inOutMethod = undefined
  docInOutMethod = undefined

instance StateVarSym (LoggerCode lang) where
  type StateVar (LoggerCode lang) = ()
  stateVar = undefined
  stateVarDef = undefined
  constVar = undefined

instance AttachmentSym (LoggerCode lang) where
  type Attachment (LoggerCode lang) = ()
  classLevel = undefined
  instanceLevel = undefined

instance OOVariableValue (LoggerCode lang) where

instance OODeclStatement (LoggerCode lang) where
  objDecDef = undefined
  objDecNew = undefined
  extObjDecNew = undefined

instance OOFuncAppStatement (LoggerCode lang) where
  selfInOutCall = undefined

instance OOValueExpression (LoggerCode lang) where
  selfFuncAppMixedArgs = undefined
  newObjMixedArgs = undefined
  extNewObjMixedArgs = undefined
  libNewObjMixedArgs = undefined

instance OOValueSym (LoggerCode lang)

instance InternalValueExp (LoggerCode lang) where
  objMethodCallMixedArgs' = undefined

instance GetSet (LoggerCode lang) where
  get = undefined
  set = undefined

instance ObserverPattern (LoggerCode lang) where
  notifyObservers = undefined

instance OOFunctionSym (LoggerCode lang) where
  func = undefined
  objAccess = undefined

instance StrategyPattern (LoggerCode lang) where
  runStrategy = undefined

-- GProc-specific
instance GProc.ProgramSym (LoggerCode lang) where
  type Program (LoggerCode lang) = ()
  prog = undefined

instance GProc.FileSym (LoggerCode lang) where
  type File (LoggerCode lang) = ()
  fileDoc = undefined
  docMod = undefined

instance GProc.ModuleSym (LoggerCode lang) where
  type Module (LoggerCode lang) = ()
  buildModule = undefined
