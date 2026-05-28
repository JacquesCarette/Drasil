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
import Drasil.GOOL.InterfaceGOOL (OOTypeSym(..), OOVariableSym(..), convTypeOO)
import Drasil.GProc.InterfaceProc (ProcProg)
import qualified Drasil.GProc.InterfaceProc as GProc (ProgramSym(..),
  FileSym(..), ModuleSym(..))
import Drasil.Shared.AST (TypeData(..), td)
import Drasil.Shared.CodeType (CodeType(..))

import Text.PrettyPrint.HughesPJ (Doc, text, comma, space, brackets, braces,
  punctuate, hcat)
import qualified Text.PrettyPrint.HughesPJ as P (char, integer, float, double)
import Data.List (intercalate)

newtype LoggerCode a = LC {unLC :: a} deriving Functor

instance Applicative LoggerCode where
  pure = LC
  (LC f) <*> (LC x) = LC (f x)

instance Monad LoggerCode where
  LC x >>= f = f x

instance SharedProg LoggerCode
instance ProcProg LoggerCode

instance VariableSym LoggerCode where
  type Variable LoggerCode = Doc
  var n _ = return $ return $ text n
  constant n _ = return $ return $ text n
  extVar l n _ = return $ return $ text l <> text "." <> text n

instance OOVariableSym LoggerCode where
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

instance ValueSym LoggerCode where
  type Value LoggerCode = Doc
  valueType = error "Not implemented"

instance TypeSym LoggerCode where
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

instance OOTypeSym LoggerCode where
  obj nm = typeFromData (Object nm) ("Object<" ++ nm ++ ">")

typeFromData :: CodeType -> String -> VSType LoggerCode
typeFromData tp str = return $ return $ td tp str (text str)

instance Literal LoggerCode where
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

instance IndexTranslator LoggerCode where
  intToIndex = id
  indexToInt = id

instance Array LoggerCode where
  arrayElem idx' vr' = do
    idx <- idx'
    vr <- vr'
    return $ return $ unLC idx <> brackets (unLC vr)

-- Not Implemented
instance BlockSym LoggerCode where
  type Block LoggerCode = ()
  block = undefined

instance BodySym LoggerCode where
  type Body LoggerCode = ()
  body = undefined
  addComments = undefined

instance StatementSym LoggerCode where
  type Statement LoggerCode = ()
  valStmt = undefined
  emptyStmt = undefined
  multi = undefined

instance Argument LoggerCode where
  pointerArg = undefined

instance VectorType LoggerCode where
  vecType = undefined

instance VectorDecl LoggerCode where
  vecDec = undefined
  vecDecDef = undefined

instance ThunkSym LoggerCode where
  type Thunk LoggerCode = ()

instance VectorThunk LoggerCode where
  vecThunk = undefined

instance VectorExpression LoggerCode where
  vecScale = undefined
  vecAdd = undefined
  vecIndex = undefined
  vecDot = undefined

instance ThunkAssign LoggerCode where
  thunkAssign = undefined

instance AssignStatement LoggerCode where
  (&-=) = undefined
  (&+=) = undefined
  (&++) = undefined
  (&--) = undefined
  assign = undefined

instance DeclStatement LoggerCode where
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

instance IOStatement LoggerCode where
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

instance StringStatement LoggerCode where
  stringSplit = undefined
  stringListVals = undefined
  stringListLists = undefined


instance FunctionSym LoggerCode where
  type Function LoggerCode = ()

instance FuncAppStatement LoggerCode where
  inOutCall = undefined
  extInOutCall = undefined

instance CommentStatement LoggerCode where
  comment = undefined

instance ControlStatement LoggerCode where
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

instance InternalList LoggerCode where
  listSlice' = undefined

instance MathConstant LoggerCode where
  pi = undefined

instance VariableValue LoggerCode where
  valueOf = undefined

instance CommandLineArgs LoggerCode where
  arg = undefined
  argsList = undefined
  argExists = undefined

instance NumericExpression LoggerCode where
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

instance BooleanExpression LoggerCode where
  (?!) = undefined
  (?&&) = undefined
  (?||) = undefined

instance Comparison LoggerCode where
  (?<) = undefined
  (?<=) = undefined
  (?>) = undefined
  (?>=) = undefined
  (?==) = undefined
  (?!=) = undefined

instance ValueExpression LoggerCode where
  inlineIf = undefined
  funcAppMixedArgs = undefined
  extFuncAppMixedArgs = undefined
  libFuncAppMixedArgs = undefined
  lambda = undefined
  notNull = undefined

instance List LoggerCode where
  listSize = undefined
  listAdd = undefined
  listAppend = undefined
  listAccess = undefined
  listSet = undefined
  indexOf = undefined

instance Set LoggerCode where
  contains = undefined
  setAdd = undefined
  setRemove = undefined
  setUnion = undefined

instance TypeElim LoggerCode where
  getType = undefined
  getTypeString = undefined

instance VariableElim LoggerCode where
  variableName = undefined
  variableType = undefined

instance ParameterSym LoggerCode where
  type Parameter LoggerCode = ()
  param = undefined
  pointerParam = undefined

instance VisibilitySym LoggerCode where
  type Visibility LoggerCode = ()
  private = undefined
  public = undefined

instance MethodSym LoggerCode where
  type Method LoggerCode = ()
  docMain = undefined
  function = undefined
  mainFunction = undefined
  docFunc = undefined
  inOutFunc = undefined
  docInOutFunc = undefined

instance ScopeSym LoggerCode where
  global = undefined
  local = undefined
  mainFn = undefined

instance BinderSym LoggerCode where
  binder = undefined

instance GProc.ProgramSym LoggerCode where
  type Program LoggerCode = ()
  prog = undefined

instance GProc.FileSym LoggerCode where
  type File LoggerCode = ()
  fileDoc = undefined
  docMod = undefined

instance GProc.ModuleSym LoggerCode where
  type Module LoggerCode = ()
  buildModule = undefined
