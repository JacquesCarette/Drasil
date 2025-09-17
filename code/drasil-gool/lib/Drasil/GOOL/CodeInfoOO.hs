{-# LANGUAGE TypeFamilies, Rank2Types #-}

-- Performs code analysis on the GOOL code
module Drasil.GOOL.CodeInfoOO (CodeInfoOO(..)) where

import Drasil.Shared.InterfaceCommon (SharedProg, BodySym(..), BlockSym(..),
  TypeSym(..), TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..),
  Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), List(..), Set(..), InternalList(..),
  ThunkSym(..), VectorType(..), VectorDecl(..), VectorThunk(..),
  VectorExpression(..), ThunkAssign(..), StatementSym(..), AssignStatement(..),
  DeclStatement(..), IOStatement(..), StringStatement(..), FunctionSym(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  ScopeSym(..), ParameterSym(..), MethodSym(..), VisibilitySym(..))
import Drasil.GOOL.InterfaceGOOL (OOProg, ProgramSym(..), FileSym(..),
  ModuleSym(..), ClassSym(..), OOMethodSym(..), OOTypeSym(..),
  OOVariableSym(..), PermanenceSym(..), StateVarSym(..), OOValueSym,
  OOVariableValue, OOValueExpression(..), InternalValueExp(..),
  OOFunctionSym(..), GetSet(..), OODeclStatement(..), OOFuncAppStatement(..),
  ObserverPattern(..), StrategyPattern(..))
import Drasil.Shared.CodeType (CodeType(Void))
import Drasil.Shared.AST (VisibilityTag(..), qualName)
import Drasil.Shared.CodeAnalysis (ExceptionType(..))
import Drasil.Shared.Helpers (toCode, sequence1_, sequence2_, sequence3_,
  sequence4_, unzipA2_)
import Drasil.Shared.State (GOOLState, FileState, MethodState, ValueState, ClassState,
  VS, lensGStoFS, lensFStoCS, lensFStoMS,
  lensCStoMS, lensMStoVS, lensVStoFS, lensCStoFS,
  setClassName, getClassName, setModuleName, getModuleName, addClass,
  updateClassMap, addException, updateMethodExcMap, updateCallMap, addCall)

import Control.Monad.State
import qualified Control.Monad.State as S (get)
import Control.Lens.Zoom (zoom)
import Data.Maybe (fromMaybe)

data CodeInfoOO = CodeInfoOO
  deriving Eq

instance SharedProg CodeInfoOO
instance OOProg CodeInfoOO

instance ProgramSym CodeInfoOO where
  type Program CodeInfoOO = State GOOLState GOOLState
  prog _ _ fs = do
    mapM_ (zoom lensGStoFS) fs
    S.get

instance FileSym CodeInfoOO where
  type File CodeInfoOO = State FileState ()
  fileDoc = sequence1_

  docMod _ _ _ = sequence1_

instance PermanenceSym CodeInfoOO where
  type Permanence CodeInfoOO = State GOOLState()
  static  = toCode ()
  dynamic = toCode ()

instance BodySym CodeInfoOO where
  type Body CodeInfoOO = State MethodState ()
  body = sequence_

  addComments _ _ = noInfo

instance BlockSym CodeInfoOO where
  type Block CodeInfoOO = State MethodState ()
  block = sequence_

instance TypeSym CodeInfoOO where
  type Type CodeInfoOO = State ValueState String
  bool              = noInfoType
  int               = noInfoType
  float             = noInfoType
  double            = noInfoType
  char              = noInfoType
  string            = noInfoType
  infile            = noInfoType
  outfile           = noInfoType
  setType       _   = noInfoType
  listType      _   = noInfoType
  arrayType     _   = noInfoType
  listInnerType _   = noInfoType
  funcType      _ _ = noInfoType
  void              = noInfoType

instance OOTypeSym CodeInfoOO where
  obj               = pure

instance TypeElim CodeInfoOO where
  type TypeName CodeInfoOO = State ValueState String
  getType _     = Void
  getTypeString tp = tp

instance ScopeSym CodeInfoOO where
  type Scope CodeInfoOO = State GOOLState ()
  global = toCode ()
  mainFn = toCode ()
  local = toCode ()

instance VariableSym CodeInfoOO where
  type Variable CodeInfoOO = State ValueState ()
  var       _ _ = noInfo
  constant  _ _ = noInfo
  extVar  _ _ _ = noInfo
  arrayElem _ _ = noInfo

instance OOVariableSym CodeInfoOO where
  staticVar'  _ _ _ = noInfo
  self              = noInfo
  classVar    _ _   = noInfo
  extClassVar _ _   = noInfo
  objVar      _ _   = noInfo
  objVarSelf  _     = noInfo

instance VariableElim CodeInfoOO where
  variableName _ = ""
  variableType _ = toCode ""

instance ValueSym CodeInfoOO where
  type Value CodeInfoOO = State ValueState ()
  valueType _ = toCode ""

instance OOValueSym CodeInfoOO

instance Argument CodeInfoOO where
  pointerArg = id

instance Literal CodeInfoOO where
  litTrue     = noInfo
  litFalse    = noInfo
  litChar   _ = noInfo
  litDouble _ = noInfo
  litFloat  _ = noInfo
  litInt    _ = noInfo
  litString _ = noInfo
  litArray  _ = sequence_
  litList   _ = sequence_
  litSet   _ = sequence_

instance MathConstant CodeInfoOO where
  pi = noInfo

instance VariableValue CodeInfoOO where
  valueOf _ = noInfo

instance OOVariableValue CodeInfoOO

instance CommandLineArgs CodeInfoOO where
  arg       _ = noInfo
  argsList    = noInfo
  argExists _ = noInfo

instance NumericExpression CodeInfoOO where
  (#~)  = sequence1_
  (#/^) = sequence1_
  (#|)  = sequence1_
  (#+)  = sequence2_
  (#-)  = sequence2_
  (#*)  = sequence2_
  (#/)  = sequence2_
  (#%)  = sequence2_
  (#^)  = sequence2_

  log    = sequence1_
  ln     = sequence1_
  exp    = sequence1_
  sin    = sequence1_
  cos    = sequence1_
  tan    = sequence1_
  csc    = sequence1_
  sec    = sequence1_
  cot    = sequence1_
  arcsin = sequence1_
  arccos = sequence1_
  arctan = sequence1_
  floor  = sequence1_
  ceil   = sequence1_

instance BooleanExpression CodeInfoOO where
  (?!)  = sequence1_
  (?&&) = sequence2_
  (?||) = sequence2_

instance Comparison CodeInfoOO where
  (?<)  = sequence2_
  (?<=) = sequence2_
  (?>)  = sequence2_
  (?>=) = sequence2_
  (?==) = sequence2_
  (?!=) = sequence2_

instance ValueExpression CodeInfoOO where
  inlineIf = sequence3_
  funcAppMixedArgs n _ = currModCall n
  extFuncAppMixedArgs l n _ vs ns = do
    sequence_ vs
    unzipA2_ ns
    addExternalCall l n
  libFuncAppMixedArgs = extFuncAppMixedArgs

  lambda _ = sequence1_

  notNull = sequence1_

instance OOValueExpression CodeInfoOO where
  selfFuncAppMixedArgs = funcAppMixedArgs
  newObjMixedArgs ot vs ns = do
    sequence_ vs
    unzipA2_ ns
    addCurrModConstructorCall ot
  extNewObjMixedArgs l ot vs ns = do
    sequence_ vs
    unzipA2_ ns
    addExternalConstructorCall l ot
  libNewObjMixedArgs = extNewObjMixedArgs

instance InternalValueExp CodeInfoOO where
  objMethodCallMixedArgs' n _ v vs ns = v >> currModCall n vs ns

instance FunctionSym CodeInfoOO where
  type Function CodeInfoOO = State ValueState ()

instance OOFunctionSym CodeInfoOO where
  func  _ _ = sequence_
  objAccess = sequence2_

instance GetSet CodeInfoOO where
  get v _ = sequence1_ v
  set v _ = sequence2_ v

instance List CodeInfoOO where
  intToIndex = sequence1_
  indexToInt = sequence1_
  listSize   = sequence1_
  listAdd    = sequence3_
  listAppend = sequence2_
  listAccess = sequence2_
  listSet    = sequence3_
  indexOf    = sequence2_

instance Set CodeInfoOO where
  contains = sequence2_
  setAdd = sequence2_
  setRemove = sequence2_
  setUnion = sequence2_

instance InternalList CodeInfoOO where
  listSlice' b e s _ vl = zoom lensMStoVS $ do
    mapM_ (fromMaybe noInfo) [b,e,s]
    _ <- vl
    noInfo

instance ThunkSym CodeInfoOO where
  type Thunk CodeInfoOO = State ValueState ()

instance ThunkAssign CodeInfoOO where
  thunkAssign _ = zoom lensMStoVS . sequence1_

instance VectorType CodeInfoOO where
  vecType _ = noInfoType

instance VectorDecl CodeInfoOO where
  vecDec  _ _ _ = noInfo
  vecDecDef _ _ = zoom lensMStoVS . sequence_

instance VectorThunk CodeInfoOO where
  vecThunk _ = noInfo

instance VectorExpression CodeInfoOO where
  vecScale = sequence2_
  vecAdd = sequence2_
  vecIndex = sequence2_
  vecDot = sequence2_

instance StatementSym CodeInfoOO where
  type Statement CodeInfoOO = State MethodState ()
  valStmt = zoom lensMStoVS . sequence1_
  emptyStmt = noInfo
  multi    = sequence_

instance AssignStatement CodeInfoOO where
  assign _ = zoom lensMStoVS . sequence1_
  (&-=)  _ = zoom lensMStoVS . sequence1_
  (&+=)  _ = zoom lensMStoVS . sequence1_
  (&++)  _ = noInfo
  (&--)  _ = noInfo

instance DeclStatement CodeInfoOO where
  varDec               _ _ = noInfo
  varDecDef            _ _ = zoom lensMStoVS . sequence1_
  setDec               _ _ = noInfo
  setDecDef            _ _ = zoom lensMStoVS . sequence1_
  listDec            _ _ _ = noInfo
  listDecDef           _ _ = zoom lensMStoVS . sequence_
  arrayDec           _ _ _ = noInfo
  arrayDecDef          _ _ = zoom lensMStoVS . sequence_
  constDecDef          _ _ = zoom lensMStoVS . sequence1_
  funcDecDef         _ _ _ = sequence1_

instance OODeclStatement CodeInfoOO where
  objDecDef            _ _ = zoom lensMStoVS . sequence1_
  objDecNew            _ _ = zoom lensMStoVS . sequence_
  extObjDecNew       _ _ _ = zoom lensMStoVS . sequence_

instance IOStatement CodeInfoOO where
  print        = zoom lensMStoVS . sequence1_
  printLn      = zoom lensMStoVS . sequence1_
  printStr   _ = noInfo
  printStrLn _ = noInfo

  printFile      v   = zoom lensMStoVS . sequence2_ v
  printFileLn    v   = zoom lensMStoVS . sequence2_ v
  printFileStr   v _ = zoom lensMStoVS $ sequence1_ v
  printFileStrLn v _ = zoom lensMStoVS $ sequence1_ v

  getInput       _ = noInfo
  discardInput     = noInfo
  getFileInput v _ = zoom lensMStoVS $ sequence1_ v
  discardFileInput = zoom lensMStoVS . sequence1_

  openFileR _ v = modify (addException FileNotFound) >>
    sequence1_ (zoom lensMStoVS v)
  openFileW _ v = modify (addException IO) >> sequence1_ (zoom lensMStoVS v)
  openFileA _ v = modify (addException IO) >> sequence1_ (zoom lensMStoVS v)
  closeFile     = zoom lensMStoVS . sequence1_

  getFileInputLine v _ = zoom lensMStoVS $ sequence1_ v
  discardFileLine      = zoom lensMStoVS . sequence1_
  getFileInputAll  v _ = sequence1_ (zoom lensMStoVS v)

instance StringStatement CodeInfoOO where
  stringSplit _ _ = zoom lensMStoVS . sequence1_

  stringListVals  _ = zoom lensMStoVS . sequence1_
  stringListLists _ = zoom lensMStoVS . sequence1_

instance FuncAppStatement CodeInfoOO where
  inOutCall n vs _ _ = zoom lensMStoVS $ do
    sequence_ vs
    addCurrModCall n
  extInOutCall l n vs _ _ = zoom lensMStoVS $ do
    sequence_ vs
    addExternalCall l n

instance OOFuncAppStatement CodeInfoOO where
  selfInOutCall n vs _ _ = zoom lensMStoVS $ do
    sequence_ vs
    addCurrModCall n

instance CommentStatement CodeInfoOO where
  comment _ = noInfo

instance ControlStatement CodeInfoOO where
  break    = noInfo
  continue = noInfo

  returnStmt = zoom lensMStoVS . sequence1_

  throw _ = modify (addException Standard)

  ifCond = evalConds
  switch v cs b = do
    _ <- zoom lensMStoVS v
    evalConds cs b

  ifExists v = sequence3_ (zoom lensMStoVS v)

  for dec v = sequence4_ dec (zoom lensMStoVS v)
  forRange _ b e s = sequence4_ (zoom lensMStoVS b) (zoom lensMStoVS e)
    (zoom lensMStoVS s)
  forEach _ v = sequence2_ (zoom lensMStoVS v)
  while v = sequence2_ (zoom lensMStoVS v)

  tryCatch _ cb = do
    _ <- cb
    noInfo

  assert cond msg = do
    _ <- zoom lensMStoVS cond
    _ <- zoom lensMStoVS msg
    noInfo

instance ObserverPattern CodeInfoOO where
  notifyObservers f _ = sequence1_ (zoom lensMStoVS f)

instance StrategyPattern CodeInfoOO where
  runStrategy _ ss vl _ = do
    mapM_ snd ss
    _ <- zoom lensMStoVS $ fromMaybe noInfo vl
    noInfo

instance VisibilitySym CodeInfoOO where
  type Visibility CodeInfoOO = State GOOLState VisibilityTag
  private = toCode Priv
  public  = toCode Pub

instance ParameterSym CodeInfoOO where
  type Parameter CodeInfoOO = State GOOLState()
  param        _ = noInfo
  pointerParam _ = noInfo

instance MethodSym CodeInfoOO where
  type Method CodeInfoOO = State MethodState ()
  docMain = updateMEMandCM "main"
  function n _ _ _ = updateMEMandCM n
  mainFunction = updateMEMandCM "main"
  docFunc _ _ _ f = do
    _ <- f
    noInfo

  inOutFunc      n _ _ _ _     = updateMEMandCM n
  docInOutFunc   n _ _ _ _ _   = updateMEMandCM n

instance OOMethodSym CodeInfoOO where
  method n _ _ _ _ = updateMEMandCM n
  getMethod _ = noInfo
  setMethod _ = noInfo
  constructor _ il b = do
    mapM_ (zoom lensMStoVS . snd) il
    _ <- b
    cn <- getClassName
    modify (updateCallMap cn . updateMethodExcMap cn)
    noInfo

  inOutMethod    n _ _ _ _ _   = updateMEMandCM n
  docInOutMethod n _ _ _ _ _ _ = updateMEMandCM n

instance StateVarSym CodeInfoOO where
  type StateVar CodeInfoOO = State GOOLState()
  stateVar    _ _ _   = noInfo
  stateVarDef _ _ _ _ = noInfo
  constVar    _ _ _   = noInfo

instance ClassSym CodeInfoOO where
  type Class CodeInfoOO = State ClassState ()
  buildClass _ _ cs ms = do
    n <- zoom lensCStoFS getModuleName
    implementingClass n [] [] cs ms
  extraClass n _ _ cs ms = do
    modify (setClassName n)
    mapM_ (zoom lensCStoMS) cs
    mapM_ (zoom lensCStoMS) ms
    noInfo
  implementingClass n _ _ cs ms = do
    modify (addClass n . setClassName n)
    mapM_ (zoom lensCStoMS) cs
    mapM_ (zoom lensCStoMS) ms
    noInfo

  docClass _ c = do
    _ <- c
    noInfo

instance ModuleSym CodeInfoOO where
  type Module CodeInfoOO = State FileState ()
  buildModule n _ funcs classes = do
    modify (setModuleName n)
    mapM_ (zoom lensFStoCS) classes
    mapM_ (zoom lensFStoMS) funcs
    modify (updateClassMap n)

-- Helpers

noInfo :: State s ()
noInfo = pure ()

noInfoType :: State s String
noInfoType = pure ""

updateMEMandCM :: String -> Body CodeInfoOO -> Method CodeInfoOO
updateMEMandCM n b = do
  _ <- b
  modify (updateCallMap n . updateMethodExcMap n)
  noInfo

evalConds :: [(Value CodeInfoOO, Body CodeInfoOO)] -> Body CodeInfoOO ->
  Statement CodeInfoOO
evalConds cs def = do
  mapM_ (zoom lensMStoVS . fst) cs
  mapM_ snd cs
  _ <- def
  noInfo

addCurrModCall :: String -> Value CodeInfoOO
addCurrModCall n = do
  mn <- zoom lensVStoFS getModuleName
  modify (addCall (qualName mn n))
  noInfo

addCurrModConstructorCall :: Type CodeInfoOO -> Value CodeInfoOO
addCurrModConstructorCall ot = do
  tp <- ot
  addCurrModCall tp

addExternalCall :: String -> String -> Value CodeInfoOO
addExternalCall l n = modify (addCall (qualName l n)) >> noInfo

addExternalConstructorCall :: String -> Type CodeInfoOO -> Value CodeInfoOO
addExternalConstructorCall l ot = do
  tp <- ot
  addExternalCall l tp

currModCall :: String -> [VS ()] ->
  [(VS (), VS ())] -> VS ()
currModCall n ps ns = do
  sequence_ ps
  unzipA2_ ns
  addCurrModCall n
