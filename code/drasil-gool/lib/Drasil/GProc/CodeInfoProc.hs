{-# LANGUAGE TypeFamilies, Rank2Types, FlexibleInstances, MultiParamTypeClasses #-}

-- Performs code analysis on the GOOL code
module Drasil.GProc.CodeInfoProc (CodeInfoProc(..)) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), MSBody, SValue, VSType,
  VSBinder, MSStatement, SMethod, SharedProg, BodySym(..), BlockSym(..),
  TypeSym(..), ScopeSym(..), VariableSym(..), VariableElim(..), ValueSym(..),
  Argument(..), Literal(..), MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), IndexTranslator(..), Reference(..),
  Array(..), List(..), Set(..), InternalList(..), StatementSym(..),
  AssignStatement(..), DeclStatement(..), IOStatement(..), StringStatement(..),
  FunctionSym(..), FuncAppStatement(..), CommentStatement(..),
  ControlStatement(..), VisibilitySym(..), ParameterSym(..), MethodSym(..),
  BinderSym(..))
import Drasil.GProc.InterfaceProc (ProcProg, ProgramSym(..),
  FileSym(..), ModuleSym(..))
import Drasil.Shared.CodeType (CodeType(Void))
import Drasil.Shared.AST (VisibilityTag(..), qualName, TypeData(..), td,
  ScopeData(..), ScopeTag (..), sd, bindFormD)
import Drasil.Shared.CodeAnalysis (ExceptionType(..))
import Drasil.Shared.Helpers (toCode, toState)
import Drasil.Shared.State (GOOLState, VS, lensGStoFS, lensFStoMS, lensMStoVS,
  lensVStoFS, modifyReturn, setModuleName, getModuleName, updateClassMap,
  addException, updateMethodExcMap, updateCallMap, addCall, callMapTransClosure,
  updateMEMWithCalls)

import Control.Monad.State (State, modify)
import qualified Control.Monad.State as S (get)
import Control.Lens.Zoom (zoom)
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.HughesPJ (empty)

newtype CodeInfoProc a = CI {unCI :: a} deriving Eq

-- FIXME: Use DerivingVia language extension (and maybe DeriveFunctor) to
-- derive the Functor, Applicative, Monad instances for this
-- (and for JavaCode, PythonCode, etc.)
instance Functor CodeInfoProc where
  fmap f (CI x) = CI (f x)

instance Applicative CodeInfoProc where
  pure = CI
  (CI f) <*> (CI x) = CI (f x)

instance Monad CodeInfoProc where
  CI x >>= f = f x

instance SharedProg CodeInfoProc

instance ProcProg CodeInfoProc

instance UnRepr CodeInfoProc contents where
  unRepr = unCI

instance ProgramSym CodeInfoProc where
  type Program CodeInfoProc = GOOLState
  prog _ _ fs = do
    mapM_ (zoom lensGStoFS) fs
    modify (updateMEMWithCalls . callMapTransClosure)
    s <- S.get
    toState $ toCode s

instance FileSym CodeInfoProc where
  type File CodeInfoProc = ()
  fileDoc = execute1

  docMod _ _ _ _ = execute1

instance BodySym CodeInfoProc where
  type Body CodeInfoProc = ()
  body = executeList

  addComments _ _ = noInfo

instance BlockSym CodeInfoProc where
  type Block CodeInfoProc = ()
  block = executeList

instance TypeSym CodeInfoProc where
  bool            = noInfoVSType
  int             = noInfoVSType
  float           = noInfoVSType
  double          = noInfoVSType
  char            = noInfoVSType
  string          = noInfoVSType
  infile          = noInfoVSType
  outfile         = noInfoVSType
  referenceType _ = noInfoVSType
  listType      _ = noInfoVSType
  setType       _ = noInfoVSType
  arrayType     _ = noInfoVSType
  innerType     _ = noInfoVSType
  funcType    _ _ = noInfoVSType
  void            = noInfoVSType

instance ScopeSym CodeInfoProc where
  global = noInfoScope
  mainFn = noInfoScope
  local = noInfoScope

instance VariableSym CodeInfoProc where
  type Variable CodeInfoProc = ()
  var       _ _ = noInfo
  constant  _ _ = noInfo
  extVar  _ _ _ = noInfo

instance VariableElim CodeInfoProc where
  variableName _ = ""
  variableType _ = noInfoType

instance ValueSym CodeInfoProc where
  type Value CodeInfoProc = ()
  valueType _ = noInfoType

instance Argument CodeInfoProc where
  pointerArg = id

instance Literal CodeInfoProc where
  litTrue     = noInfo
  litFalse    = noInfo
  litChar   _ = noInfo
  litDouble _ = noInfo
  litFloat  _ = noInfo
  litInt    _ = noInfo
  litString _ = noInfo
  litArray  _ = executeList
  litList   _ = executeList
  litSet   _ = executeList

instance MathConstant CodeInfoProc where
  pi = noInfo

instance VariableValue CodeInfoProc where
  valueOf _ = noInfo

instance CommandLineArgs CodeInfoProc where
  arg       _ = noInfo
  argsList    = noInfo
  argExists _ = noInfo

instance NumericExpression CodeInfoProc where
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

instance BooleanExpression CodeInfoProc where
  (?!)  = execute1
  (?&&) = execute2
  (?||) = execute2

instance Comparison CodeInfoProc where
  (?<)  = execute2
  (?<=) = execute2
  (?>)  = execute2
  (?>=) = execute2
  (?==) = execute2
  (?!=) = execute2

instance ValueExpression CodeInfoProc where
  inlineIf = execute3
  funcAppMixedArgs n _ = currModCall n
  extFuncAppMixedArgs l n _ vs ns = do
    sequence_ vs
    executePairList ns
    addExternalCall l n
  libFuncAppMixedArgs = extFuncAppMixedArgs

  lambda _ = execute1

  notNull = execute1

instance FunctionSym CodeInfoProc where
  type Function CodeInfoProc = ()

instance IndexTranslator CodeInfoProc where
  intToIndex = execute1
  indexToInt = execute1

instance Reference CodeInfoProc where
  makeRef = execute1
  maybeDeref = execute1

instance Array CodeInfoProc where
  arrayElem _ _ = noInfo
  arrayLength _ = noInfo
  arrayCopy _ = noInfo

instance List CodeInfoProc where
  listSize       = execute1
  listAdd l i v  = execute3 (zoom lensMStoVS l) (zoom lensMStoVS i) (zoom lensMStoVS v)
  listAppend l v = execute2 (zoom lensMStoVS l) (zoom lensMStoVS v)
  listAccess     = execute2
  listSet l i v  = execute3 (zoom lensMStoVS l) (zoom lensMStoVS i) (zoom lensMStoVS v)
  indexOf        = execute2

instance Set CodeInfoProc where
 contains = execute2
 setAdd = execute2
 setRemove = execute2
 setUnion = execute2

instance InternalList CodeInfoProc where
  listSlice' b e s _ vl = zoom lensMStoVS $ do
    mapM_ (fromMaybe noInfo) [b,e,s]
    _ <- vl
    noInfo

instance BinderSym CodeInfoProc where
  binder _ _ = noInfoBinder

instance StatementSym CodeInfoProc where
  type Statement CodeInfoProc = ()
  valStmt = zoom lensMStoVS . execute1
  emptyStmt = noInfo
  multi    = executeList

instance AssignStatement CodeInfoProc where
  assign _ = zoom lensMStoVS . execute1
  (&-=)  _ = zoom lensMStoVS . execute1
  (&+=)  _ = zoom lensMStoVS . execute1
  (&++)  _ = noInfo
  (&--)  _ = noInfo

instance DeclStatement CodeInfoProc where
  varDec               _ _ = noInfo
  varDecDef            _ _ = zoom lensMStoVS . execute1
  setDec               _ _ = noInfo
  setDecDef            _ _ = zoom lensMStoVS . execute1
  listDec            _ _ _ = noInfo
  listDecDef           _ _ = zoom lensMStoVS . executeList
  arrayDec           _ _ _ = noInfo
  arrayDecDef          _ _ = zoom lensMStoVS . executeList
  constDecDef          _ _ = zoom lensMStoVS . execute1
  funcDecDef         _ _ _ = execute1

instance IOStatement CodeInfoProc where
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

instance StringStatement CodeInfoProc where
  stringSplit _ _ = zoom lensMStoVS . execute1

  stringListVals  _ = zoom lensMStoVS . execute1
  stringListLists _ = zoom lensMStoVS . execute1

instance FuncAppStatement CodeInfoProc where
  inOutCall n vs _ _ = zoom lensMStoVS $ do
    sequence_ vs
    addCurrModCall n
  extInOutCall l n vs _ _ = zoom lensMStoVS $ do
    sequence_ vs
    addExternalCall l n

instance CommentStatement CodeInfoProc where
  comment _ = noInfo

instance ControlStatement CodeInfoProc where
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

  assert cond msg = do
    _ <- zoom lensMStoVS cond
    _ <- zoom lensMStoVS msg
    noInfo

instance VisibilitySym CodeInfoProc where
  type Visibility CodeInfoProc = VisibilityTag
  private = toCode Priv
  public  = toCode Pub

instance ParameterSym CodeInfoProc where
  type Parameter CodeInfoProc = ()
  param        _ = noInfo
  pointerParam _ = noInfo

instance MethodSym CodeInfoProc where
  type Method CodeInfoProc = ()
  docMain = updateMEMandCM "main"
  function n _ _ _ = updateMEMandCM n
  mainFunction = updateMEMandCM "main"
  docFunc _ _ _ f = do
    _ <- f
    noInfo

  inOutFunc      n _ _ _ _     = updateMEMandCM n
  docInOutFunc   n _ _ _ _ _   = updateMEMandCM n

instance ModuleSym CodeInfoProc where
  type Module CodeInfoProc = ()
  buildModule n _ funcs = do
    modify (setModuleName n)
    mapM_ (zoom lensFStoMS) funcs
    modifyReturn (updateClassMap n) (toCode ())

-- Helpers

noInfo :: State s (CodeInfoProc ())
noInfo = toState $ toCode ()

emptyType :: TypeData
emptyType = td Void "" empty -- Hack

noInfoType :: CodeInfoProc TypeData
noInfoType = return emptyType

noInfoVSType :: VSType CodeInfoProc
noInfoVSType = return noInfoType

noInfoScope :: CodeInfoProc ScopeData
noInfoScope = return $ sd Global -- Hack

noInfoBinder :: VSBinder CodeInfoProc
noInfoBinder = return $ return $ bindFormD "" emptyType

updateMEMandCM :: String -> MSBody CodeInfoProc -> SMethod CodeInfoProc
updateMEMandCM n b = do
  _ <- b
  modify (updateCallMap n . updateMethodExcMap n)
  noInfo

evalConds :: [(SValue CodeInfoProc, MSBody CodeInfoProc)] -> MSBody CodeInfoProc ->
  MSStatement CodeInfoProc
evalConds cs def = do
  mapM_ (zoom lensMStoVS . fst) cs
  mapM_ snd cs
  _ <- def
  noInfo

addCurrModCall :: String -> SValue CodeInfoProc
addCurrModCall n = do
  mn <- zoom lensVStoFS getModuleName
  modify (addCall (qualName mn n))
  noInfo

addExternalCall :: String -> String -> SValue CodeInfoProc
addExternalCall l n = modify (addCall (qualName l n)) >> noInfo

execute1 :: State a (CodeInfoProc ()) -> State a (CodeInfoProc ())
execute1 s = do
  _ <- s
  noInfo

executeList :: [State a (CodeInfoProc ())] -> State a (CodeInfoProc ())
executeList l = do
  sequence_ l
  noInfo

executePairList :: [(State a (CodeInfoProc ()), State a (CodeInfoProc ()))] ->
  State a (CodeInfoProc ())
executePairList ps = do
  mapM_ fst ps
  mapM_ snd ps
  noInfo

execute2 :: State a (CodeInfoProc ()) -> State a (CodeInfoProc ()) ->
  State a (CodeInfoProc ())
execute2 s1 s2 = do
  _ <- s1
  execute1 s2

execute3 :: State a (CodeInfoProc ()) -> State a (CodeInfoProc ()) ->
  State a (CodeInfoProc ()) -> State a (CodeInfoProc ())
execute3 s1 s2 s3 = do
  _ <- s1
  execute2 s2 s3

execute4 :: State a (CodeInfoProc ()) -> State a (CodeInfoProc ()) ->
  State a (CodeInfoProc ()) -> State a (CodeInfoProc ()) -> State a (CodeInfoProc ())
execute4 s1 s2 s3 s4 = do
  _ <- s1
  execute3 s2 s3 s4

currModCall :: String -> [VS (CodeInfoProc ())] ->
  [(VS (CodeInfoProc ()), VS (CodeInfoProc ()))] -> VS (CodeInfoProc ())
currModCall n ps ns = do
  sequence_ ps
  executePairList ns
  addCurrModCall n
