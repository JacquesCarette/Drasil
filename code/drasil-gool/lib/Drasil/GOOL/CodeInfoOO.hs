{-# LANGUAGE TypeFamilies, Rank2Types #-}

-- Performs code analysis on the GOOL code
module Drasil.GOOL.CodeInfoOO (CodeInfoOO(..)) where

import Drasil.GOOL.InterfaceCommon (MSBody, VSType, SValue, MSStatement, 
  SMethod, SharedProg, BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..),
  VariableSym(..), VariableElim(..), ValueSym(..), Argument(..), Literal(..),
  MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), List(..), Set(..), InternalList(..), ThunkSym(..), VectorType(..),
  VectorDecl(..), VectorThunk(..), VectorExpression(..), ThunkAssign(..),
  StatementSym(..), AssignStatement(..), DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), ScopeSym(..), ParameterSym(..),
  MethodSym(..), VisibilitySym(..))
import Drasil.GOOL.InterfaceGOOL (OOProg, ProgramSym(..), FileSym(..),
  ModuleSym(..), ClassSym(..), OOMethodSym(..), OOTypeSym(..),
  OOVariableSym(..), PermanenceSym(..), StateVarSym(..), OOValueSym,
  OOVariableValue, OOValueExpression(..), InternalValueExp(..),
  OOFunctionSym(..), GetSet(..), OODeclStatement(..), OOFuncAppStatement(..),
  ObserverPattern(..), StrategyPattern(..))
import Drasil.GOOL.CodeType (CodeType(Void))
import Drasil.GOOL.AST (VisibilityTag(..), qualName)
import Drasil.GOOL.CodeAnalysis (ExceptionType(..))
import Drasil.GOOL.Helpers (toCode, toState)
import Drasil.GOOL.State (GOOLState, VS, lensGStoFS, lensFStoCS, lensFStoMS,
  lensCStoMS, lensMStoVS, lensVStoFS, lensCStoFS, modifyReturn, 
  setClassName, getClassName, setModuleName, getModuleName, addClass, 
  updateClassMap, addException, updateMethodExcMap, updateCallMap, addCall, 
  callMapTransClosure, updateMEMWithCalls)

import Control.Monad.State (State, modify)
import qualified Control.Monad.State as S (get)
import Control.Lens.Zoom (zoom)
import Data.Maybe (fromMaybe)

newtype CodeInfoOO a = CI {unCI :: a} deriving Eq

-- FIXME: Use DerivingVia language extension (and maybe DeriveFunctor) to 
-- derive the Functor, Applicative, Monad instances for this 
-- (and for JavaCode, PythonCode, etc.)
instance Functor CodeInfoOO where
  fmap f (CI x) = CI (f x)

instance Applicative CodeInfoOO where
  pure = CI
  (CI f) <*> (CI x) = CI (f x)

instance Monad CodeInfoOO where
  CI x >>= f = f x

instance SharedProg CodeInfoOO
instance OOProg CodeInfoOO

instance ProgramSym CodeInfoOO where
  type Program CodeInfoOO = GOOLState
  prog _ _ fs = do
    mapM_ (zoom lensGStoFS) fs
    modify (updateMEMWithCalls . callMapTransClosure)
    s <- S.get
    toState $ toCode s

instance FileSym CodeInfoOO where
  type File CodeInfoOO = ()
  fileDoc = execute1
  
  docMod _ _ _ = execute1

instance PermanenceSym CodeInfoOO where
  type Permanence CodeInfoOO = ()
  static  = toCode ()
  dynamic = toCode ()

instance BodySym CodeInfoOO where
  type Body CodeInfoOO = ()
  body = executeList

  addComments _ _ = noInfo

instance BlockSym CodeInfoOO where
  type Block CodeInfoOO = ()
  block = executeList

instance TypeSym CodeInfoOO where
  type Type CodeInfoOO = String
  bool              = noInfoType
  int               = noInfoType
  float             = noInfoType
  double            = noInfoType
  char              = noInfoType
  string            = noInfoType
  infile            = noInfoType
  outfile           = noInfoType
  listType      _   = noInfoType
  arrayType     _   = noInfoType
  listInnerType _   = noInfoType
  funcType      _ _ = noInfoType
  void              = noInfoType

instance OOTypeSym CodeInfoOO where
  obj               = toState . toCode

instance TypeElim CodeInfoOO where
  getType _     = Void
  getTypeString = unCI

instance ScopeSym CodeInfoOO where
  type Scope CodeInfoOO = ()
  global = toCode ()
  mainFn = toCode ()
  local = toCode ()

instance VariableSym CodeInfoOO where
  type Variable CodeInfoOO = ()
  var'      _ _ _ = noInfo
  constant' _ _ _ = noInfo
  extVar   _ _ _  = noInfo
  arrayElem _ _   = noInfo

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
  type Value CodeInfoOO = ()
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
  litArray  _ = executeList
  litList   _ = executeList

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
  (#~)  = execute1
  (#/^) = execute1
  (#|)  = execute1
  (#+)  = execute2
  (#-)  = execute2
  (#*)  = execute2
  (#/)  = execute2
  (#%)  = execute2
  (#^)  = execute2

  log    = execute1
  ln     = execute1
  exp    = execute1
  sin    = execute1
  cos    = execute1
  tan    = execute1
  csc    = execute1
  sec    = execute1
  cot    = execute1
  arcsin = execute1
  arccos = execute1
  arctan = execute1
  floor  = execute1
  ceil   = execute1

instance BooleanExpression CodeInfoOO where
  (?!)  = execute1
  (?&&) = execute2
  (?||) = execute2
  isin = execute2

instance Comparison CodeInfoOO where
  (?<)  = execute2
  (?<=) = execute2
  (?>)  = execute2
  (?>=) = execute2
  (?==) = execute2
  (?!=) = execute2
    
instance ValueExpression CodeInfoOO where
  inlineIf = execute3
  funcAppMixedArgs n _ = currModCall n
  extFuncAppMixedArgs l n _ vs ns = do
    sequence_ vs
    executePairList ns
    addExternalCall l n  
  libFuncAppMixedArgs = extFuncAppMixedArgs

  lambda _ = execute1

  notNull = execute1

instance OOValueExpression CodeInfoOO where
  selfFuncAppMixedArgs = funcAppMixedArgs
  newObjMixedArgs ot vs ns = do
    sequence_ vs
    executePairList ns
    addCurrModConstructorCall ot
  extNewObjMixedArgs l ot vs ns = do
    sequence_ vs
    executePairList ns
    addExternalConstructorCall l ot
  libNewObjMixedArgs = extNewObjMixedArgs
  
instance InternalValueExp CodeInfoOO where
  objMethodCallMixedArgs' n _ v vs ns = v >> currModCall n vs ns

instance FunctionSym CodeInfoOO where
  type Function CodeInfoOO = ()

instance OOFunctionSym CodeInfoOO where
  func  _ _ = executeList
  objAccess = execute2
  
instance GetSet CodeInfoOO where
  get v _ = execute1 v
  set v _ = execute2 v

instance List CodeInfoOO where
  intToIndex = execute1
  indexToInt = execute1
  listSize   = execute1
  listAdd    = execute3
  listAppend = execute2
  listAccess = execute2
  listSet    = execute3
  indexOf    = execute2

instance Set CodeInfo where
  contains = execute2

instance InternalList CodeInfoOO where
  listSlice' b e s _ vl = zoom lensMStoVS $ do
    mapM_ (fromMaybe noInfo) [b,e,s]
    _ <- vl
    noInfo

instance ThunkSym CodeInfoOO where
  type Thunk CodeInfoOO = ()

instance ThunkAssign CodeInfoOO where
  thunkAssign _ = zoom lensMStoVS . execute1

instance VectorType CodeInfoOO where
  vecType _ = noInfoType

instance VectorDecl CodeInfoOO where
  vecDec _ _ = noInfo
  vecDecDef _ = zoom lensMStoVS . executeList

instance VectorThunk CodeInfoOO where
  vecThunk _ = noInfo

instance VectorExpression CodeInfoOO where
  vecScale = execute2
  vecAdd = execute2
  vecIndex = execute2
  vecDot = execute2

instance StatementSym CodeInfoOO where
  type Statement CodeInfoOO = ()
  valStmt = zoom lensMStoVS . execute1
  multi    = executeList
  
instance AssignStatement CodeInfoOO where
  assign _ = zoom lensMStoVS . execute1
  (&-=)  _ = zoom lensMStoVS . execute1
  (&+=)  _ = zoom lensMStoVS . execute1
  (&++)  _ = noInfo
  (&--)  _ = noInfo

instance DeclStatement CodeInfoOO where
  varDec                 _ = noInfo
  varDecDef              _ = zoom lensMStoVS . execute1
  listDec              _ _ = noInfo
  listDecDef             _ = zoom lensMStoVS . executeList
  arrayDec             _ _ = noInfo
  arrayDecDef            _ = zoom lensMStoVS . executeList
  constDecDef            _ = zoom lensMStoVS . execute1
  funcDecDef           _ _ = execute1

instance OODeclStatement CodeInfoOO where
  objDecDef              _ = zoom lensMStoVS . execute1
  objDecNew              _ = zoom lensMStoVS . executeList
  extObjDecNew         _ _ = zoom lensMStoVS . executeList

instance IOStatement CodeInfoOO where
  print        = zoom lensMStoVS . execute1
  printLn      = zoom lensMStoVS . execute1
  printStr   _ = noInfo
  printStrLn _ = noInfo

  printFile      v   = zoom lensMStoVS . execute2 v
  printFileLn    v   = zoom lensMStoVS . execute2 v
  printFileStr   v _ = zoom lensMStoVS $ execute1 v
  printFileStrLn v _ = zoom lensMStoVS $ execute1 v

  getInput       _ = noInfo
  discardInput     = noInfo
  getFileInput v _ = zoom lensMStoVS $ execute1 v
  discardFileInput = zoom lensMStoVS . execute1

  openFileR _ v = modify (addException FileNotFound) >> 
    execute1 (zoom lensMStoVS v)
  openFileW _ v = modify (addException IO) >> execute1 (zoom lensMStoVS v)
  openFileA _ v = modify (addException IO) >> execute1 (zoom lensMStoVS v)
  closeFile     = zoom lensMStoVS . execute1

  getFileInputLine v _ = zoom lensMStoVS $ execute1 v
  discardFileLine      = zoom lensMStoVS . execute1
  getFileInputAll  v _ = execute1 (zoom lensMStoVS v)

instance StringStatement CodeInfoOO where
  stringSplit _ _ = zoom lensMStoVS . execute1

  stringListVals  _ = zoom lensMStoVS . execute1
  stringListLists _ = zoom lensMStoVS . execute1

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

  returnStmt = zoom lensMStoVS . execute1

  throw _ = modifyReturn (addException Standard) (toCode ())

  ifCond = evalConds
  switch v cs b = do
    _ <- zoom lensMStoVS v
    evalConds cs b

  ifExists v = execute3 (zoom lensMStoVS v)

  for dec v = execute4 dec (zoom lensMStoVS v)
  forRange _ b e s = execute4 (zoom lensMStoVS b) (zoom lensMStoVS e) 
    (zoom lensMStoVS s)
  forEach _ v = execute2 (zoom lensMStoVS v)
  while v = execute2 (zoom lensMStoVS v)

  tryCatch _ cb = do
    _ <- cb
    noInfo

instance ObserverPattern CodeInfoOO where
  notifyObservers f _ _ = execute1 (zoom lensMStoVS f)
  
instance StrategyPattern CodeInfoOO where
  runStrategy _ ss vl _ = do
    mapM_ snd ss
    _ <- zoom lensMStoVS $ fromMaybe noInfo vl
    noInfo

instance VisibilitySym CodeInfoOO where
  type Visibility CodeInfoOO = VisibilityTag
  private = toCode Priv
  public  = toCode Pub

instance ParameterSym CodeInfoOO where
  type Parameter CodeInfoOO = ()
  param        _ = noInfo
  pointerParam _ = noInfo

instance MethodSym CodeInfoOO where
  type Method CodeInfoOO = ()
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
  type StateVar CodeInfoOO = ()
  stateVar    _ _ _   = noInfo
  stateVarDef _ _ _ _ = noInfo
  constVar    _ _ _   = noInfo

instance ClassSym CodeInfoOO where
  type Class CodeInfoOO = ()
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
  type Module CodeInfoOO = ()
  buildModule n _ funcs classes = do
    modify (setModuleName n)
    mapM_ (zoom lensFStoCS) classes 
    mapM_ (zoom lensFStoMS) funcs
    modifyReturn (updateClassMap n) (toCode ())

-- Helpers

noInfo :: State s (CodeInfoOO ())
noInfo = toState $ toCode ()

noInfoType :: State s (CodeInfoOO String)
noInfoType = toState $ toCode ""

updateMEMandCM :: String -> MSBody CodeInfoOO -> SMethod CodeInfoOO
updateMEMandCM n b = do
  _ <- b
  modify (updateCallMap n . updateMethodExcMap n)
  noInfo

evalConds :: [(SValue CodeInfoOO, MSBody CodeInfoOO)] -> MSBody CodeInfoOO -> 
  MSStatement CodeInfoOO
evalConds cs def = do
  mapM_ (zoom lensMStoVS . fst) cs
  mapM_ snd cs
  _ <- def
  noInfo

addCurrModCall :: String -> SValue CodeInfoOO
addCurrModCall n = do
  mn <- zoom lensVStoFS getModuleName 
  modify (addCall (qualName mn n)) 
  noInfo

addCurrModConstructorCall :: VSType CodeInfoOO -> SValue CodeInfoOO
addCurrModConstructorCall ot = do
  t <- ot
  let tp = getTypeString t
  addCurrModCall tp

addExternalCall :: String -> String -> SValue CodeInfoOO
addExternalCall l n = modify (addCall (qualName l n)) >> noInfo

addExternalConstructorCall :: String -> VSType CodeInfoOO -> SValue CodeInfoOO
addExternalConstructorCall l ot = do
  t <- ot
  let tp = getTypeString t
  addExternalCall l tp

execute1 :: State a (CodeInfoOO ()) -> State a (CodeInfoOO ())
execute1 s = do
  _ <- s
  noInfo

executeList :: [State a (CodeInfoOO ())] -> State a (CodeInfoOO ())
executeList l = do
  sequence_ l
  noInfo

executePairList :: [(State a (CodeInfoOO ()), State a (CodeInfoOO ()))] -> 
  State a (CodeInfoOO ())
executePairList ps = do
  mapM_ fst ps
  mapM_ snd ps
  noInfo

execute2 :: State a (CodeInfoOO ()) -> State a (CodeInfoOO ()) -> 
  State a (CodeInfoOO ())
execute2 s1 s2 = do
  _ <- s1
  execute1 s2

execute3 :: State a (CodeInfoOO ()) -> State a (CodeInfoOO ()) -> 
  State a (CodeInfoOO ()) -> State a (CodeInfoOO ())
execute3 s1 s2 s3 = do
  _ <- s1
  execute2 s2 s3

execute4 :: State a (CodeInfoOO ()) -> State a (CodeInfoOO ()) -> 
  State a (CodeInfoOO ()) -> State a (CodeInfoOO ()) -> State a (CodeInfoOO ())
execute4 s1 s2 s3 s4 = do
  _ <- s1
  execute3 s2 s3 s4

currModCall :: String -> [VS (CodeInfoOO ())] -> 
  [(VS (CodeInfoOO ()), VS (CodeInfoOO ()))] -> VS (CodeInfoOO ())
currModCall n ps ns = do
  sequence_ ps
  executePairList ns
  addCurrModCall n
