{-# LANGUAGE TypeFamilies #-}
-- Performs code analysis on the GOOL code
module Drasil.GOOL.CodeInfoOO (CodeInfoOO(..)) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), VSBinder, Variable, Value,
  SValue, BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..), VariableSym(..),
  VariableElim(..), ValueSym(..), Argument(..), Literal(..), MathConstant(..),
  VariableValue(..), CommandLineArgs(..), NumericExpression(..),
  BooleanExpression(..), Comparison(..), ValueExpression(..),
  IndexTranslator(..), Reference(..), Array(..), List(..), ListStatement(..),
  Set(..), InternalList(..), EmptyStatement(..), MultiStatement(..),
  ValueStatement(..), AssignStatement(..), DeclStatement(..), PrintConsole(..),
  ReadConsole(..), FileHandling(..), PrintFile(..), ReadFile(..),
  StringStatement(..), FuncAppStatement(..), CommentStatement(..),
  ControlStatement(..), ScopeSym(..), ParameterSym(..), MethodSym(..),
  VisibilitySym(..), BinderSym(..))
import Drasil.GOOL.InterfaceGOOL (OOProg, ProgramSym(..), FileSym(..),
  ModuleSym(..), ClassSym(..), OOMethodSym(..), OOTypeSym(..), OOVariableSym(..),
  SelfSym(..), AttachmentSym(..), StateVarSym(..), OOValueExpression(..),
  InternalValueExp(..), OOFunctionSym(..), GetSet(..), OODeclStatement(..),
  OOFuncAppStatement(..), ObserverPattern(..), StrategyPattern(..))
import Drasil.Shared.CodeType (CodeType(Void))
import Drasil.Shared.AST (qualName, td, ScopeData, ScopeTag(..), sd, bindFormD)
import Drasil.Shared.CodeAnalysis (ExceptionType(..))
import Drasil.Shared.Helpers (toCode, toState)
import Drasil.Shared.State (GOOLState, MS, VS, lensGStoFS, lensFStoCS,
  lensFStoMS, lensCStoMS, lensMStoVS, lensVStoFS, lensCStoFS, modifyReturn,
  setClassName, getClassName, setModuleName, getModuleName, addClass,
  updateClassMap, addException, updateMethodExcMap, updateCallMap, addCall,
  callMapTransClosure, updateMEMWithCalls)

import Control.Monad.State (State, modify)
import qualified Control.Monad.State as S (get)
import Control.Lens.Zoom (zoom)
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.HughesPJ (empty)

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

instance OOProg CodeInfoOO () () () () () () GOOLState () () () ()

instance UnRepr CodeInfoOO contents where
  unRepr = unCI

instance ProgramSym CodeInfoOO GOOLState () where
  prog _ _ fs = do
    mapM_ (zoom lensGStoFS) fs
    modify (updateMEMWithCalls . callMapTransClosure)
    s <- S.get
    toState $ toCode s

instance FileSym CodeInfoOO () () where
  fileDoc m = do
    _ <- m
    pure $ pure $ error "[fileDoc] The return value of this isn't used, and the thunk shouldn't fire."

  docMod _ _ _ _ fl = do
    _ <- fl
    pure $ pure $ error "[docMod] The return value of this isn't used, and the thunk shouldn't fire."

instance AttachmentSym CodeInfoOO () where
  classLevel  = toCode ()
  instanceLevel = toCode ()

instance BodySym CodeInfoOO () () where
  body b = do
    sequence_ b
    pure $ pure $ error "[body] The return value of this isn't used, and the thunk shouldn't fire."

  addComments _ _ = pure $ pure $ error "[addComments] The return value of this isn't used, and the thunk shouldn't fire."

instance BlockSym CodeInfoOO () () where
  block b = do
    sequence_ b
    pure $ pure $ error "[block] The return value of this isn't used, and the thunk shouldn't fire."

instance TypeSym CodeInfoOO () where
  bool            = pure $ pure $ error "[bool] The return value of this isn't used, and the thunk shouldn't fire."
  int             = pure $ pure $ error "[int] The return value of this isn't used, and the thunk shouldn't fire."
  float           = pure $ pure $ error "[float] The return value of this isn't used, and the thunk shouldn't fire."
  double          = pure $ pure $ error "[double] The return value of this isn't used, and the thunk shouldn't fire."
  char            = pure $ pure $ error "[char] The return value of this isn't used, and the thunk shouldn't fire."
  string          = pure $ pure $ error "[string] The return value of this isn't used, and the thunk shouldn't fire."
  infile          = pure $ pure $ error "[infile] The return value of this isn't used, and the thunk shouldn't fire."
  outfile         = pure $ pure $ error "[outfile] The return value of this isn't used, and the thunk shouldn't fire."
  referenceType _ = pure $ pure $ error "[referenceType] The return value of this isn't used, and the thunk shouldn't fire."
  setType       _ = pure $ pure $ error "[setType] The return value of this isn't used, and the thunk shouldn't fire."
  listType      _ = pure $ pure $ error "[listType] The return value of this isn't used, and the thunk shouldn't fire."
  arrayType     _ = pure $ pure $ error "[arrayType] The return value of this isn't used, and the thunk shouldn't fire."
  innerType     _ = pure $ pure $ error "[innerType] The return value of this isn't used, and the thunk shouldn't fire."
  funcType    _ _ = pure $ pure $ error "[funcType] The return value of this isn't used, and the thunk shouldn't fire."
  void            = pure $ pure $ error "[void] The return value of this isn't used, and the thunk shouldn't fire."

instance OOTypeSym CodeInfoOO () where
  obj             _ = pure $ pure $ error "[obj] The return value of this isn't used, and the thunk shouldn't fire."

instance TypeElim CodeInfoOO () where
  getCodeType _ = Void

instance ScopeSym CodeInfoOO where
  global = noInfoScope
  mainFn = noInfoScope
  local = noInfoScope

instance VariableSym CodeInfoOO () where
  var       _ _ = pure $ pure $ error "[var] The return value of this isn't used, and the thunk shouldn't fire."
  constant  _ _ = pure $ pure $ error "[constant] The return value of this isn't used, and the thunk shouldn't fire."
  extVar  _ _ _ = pure $ pure $ error "[extVar] The return value of this isn't used, and the thunk shouldn't fire."

instance OOVariableSym CodeInfoOO () where
  classVar _ _ = pure $ pure $ error "[classVar] The return value of this isn't used, and the thunk shouldn't fire."
  classConst _ _ = pure $ pure $ error "[classConst] The return value of this isn't used, and the thunk shouldn't fire."
  classVarAccess    _ _   = pure $ pure $ error "[classVarAccess] The return value of this isn't used, and the thunk shouldn't fire."
  extClassVarAccess _ _   = pure $ pure $ error "[extClassVarAccess] The return value of this isn't used, and the thunk shouldn't fire."
  instanceVarAccess      _ _   = pure $ pure $ error "[instanceVarAccess] The return value of this isn't used, and the thunk shouldn't fire."

instance SelfSym CodeInfoOO where
  self              = pure $ pure $ error "[self] The return value of this isn't used, and the thunk shouldn't fire."

instance VariableElim CodeInfoOO () where
  variableName _ = ""
  variableType _ = pure $ error "[variableType] The return value of this isn't used, and the thunk shouldn't fire."

instance ValueSym CodeInfoOO () where
  valueType _ = pure $ error "[valueType] The return value of this isn't used, and the thunk shouldn't fire."

instance Argument CodeInfoOO where
  pointerArg = id

instance Literal CodeInfoOO () where
  litTrue     = pure $ error "[litTrue] The return value of this isn't used, and the thunk shouldn't fire."
  litFalse    = pure $ error "[litFalse] The return value of this isn't used, and the thunk shouldn't fire."
  litChar   _ = pure $ error "[litChar] The return value of this isn't used, and the thunk shouldn't fire."
  litDouble _ = pure $ error "[litDouble] The return value of this isn't used, and the thunk shouldn't fire."
  litFloat  _ = pure $ error "[litFloat] The return value of this isn't used, and the thunk shouldn't fire."
  litInt    _ = pure $ error "[litInt] The return value of this isn't used, and the thunk shouldn't fire."
  litString _ = pure $ error "[litString] The return value of this isn't used, and the thunk shouldn't fire."
  litArray  _ = executeListErr
  litList   _ = executeListErr
  litSet   _ = executeListErr

instance MathConstant CodeInfoOO where
  pi = pure $ error "[pi] The return value of this isn't used, and the thunk shouldn't fire."

instance VariableValue CodeInfoOO where
  valueOf _ = pure $ error "[valueOf] The return value of this isn't used, and the thunk shouldn't fire."

instance CommandLineArgs CodeInfoOO where
  arg       _ = pure $ error "[arg] The return value of this isn't used, and the thunk shouldn't fire."
  argsList    = pure $ error "[argsList] The return value of this isn't used, and the thunk shouldn't fire."
  argExists _ = pure $ error "[argExists] The return value of this isn't used, and the thunk shouldn't fire."

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

instance Comparison CodeInfoOO where
  (?<)  = execute2
  (?<=) = execute2
  (?>)  = execute2
  (?>=) = execute2
  (?==) = execute2
  (?!=) = execute2

instance ValueExpression CodeInfoOO () where
  inlineIf = execute3
  funcAppMixedArgs n _ = do
    _ <- currModCall n
    pure $ pure $ pure $ error "[funcAppMixedArgs] The return value of this isn't used, and the thunk shouldn't fire."
  extFuncAppMixedArgs l n _ vs ns = do
    sequence_ vs
    mapM_ fst ns
    mapM_ snd ns
    addExternalCallVal l n
  libFuncAppMixedArgs = extFuncAppMixedArgs

  lambda _ = execute1

  notNull = execute1

instance OOValueExpression CodeInfoOO () where
  newObjMixedArgs _ vs ns = do
    sequence_ vs
    mapM_ fst ns
    mapM_ snd ns
    pure $ error "[newObjMixedArgs] The return value of this isn't used, and the thunk shouldn't fire."
  extNewObjMixedArgs _ _ vs ns = do
    sequence_ vs
    mapM_ fst ns
    mapM_ snd ns
    pure $ error "[extNewObjMixedArgs] The return value of this isn't used, and the thunk shouldn't fire."
  libNewObjMixedArgs = extNewObjMixedArgs

instance InternalValueExp CodeInfoOO () where
  objMethodCallMixedArgs' n _ v vs ns = do
    _ <- v
    _ <- currModCall n vs ns
    pure $ pure $ error "[objMethodCallMixedArgs'] The return value of this isn't used, and the thunk shouldn't fire."
  classMethodCallMixedArgs' n _ cls vs ns = cls >> currModCall n vs ns

instance OOFunctionSym CodeInfoOO () where
  func  _ _ l = do
    sequence_ l
    pure $ pure $ error "The return value of this isn't used, and the thunk shouldn't fire."
  objAccess s1 s2 = do
    _ <- s1
    _ <- s2
    pure $ pure $ error "The return value of this isn't used, and the thunk shouldn't fire."

instance GetSet CodeInfoOO where
  get v _ = execute1 v
  set v _ = execute2 v

instance IndexTranslator CodeInfoOO where
  intToIndex = execute1
  indexToInt = execute1

instance Reference CodeInfoOO where
  makeRef = execute1
  maybeDeref = execute1

instance Array CodeInfoOO where
  arrayElem _ _ = pure $ pure $ error "[arrayElem] The return value of this isn't used, and the thunk shouldn't fire."
  arrayLength _ = pure $ error "[arrayLength] The return value of this isn't used, and the thunk shouldn't fire."
  arrayCopy _ = pure $ error "[arrayCopy] The return value of this isn't used, and the thunk shouldn't fire."

instance List CodeInfoOO where
  listSize       = execute1
  listAccess     = execute2
  indexOf        = execute2

instance ListStatement CodeInfoOO () where
  listAdd l i v  = execute3 (zoom lensMStoVS l) (zoom lensMStoVS i) (zoom lensMStoVS v)
  listAppend l v = execute2 (zoom lensMStoVS l) (zoom lensMStoVS v)
  listSet l i v  = execute3 (zoom lensMStoVS l) (zoom lensMStoVS i) (zoom lensMStoVS v)

instance Set CodeInfoOO where
  contains = execute2
  setAdd = execute2
  setRemove = execute2
  setUnion = execute2

instance InternalList CodeInfoOO () where
  listSlice' b e s _ vl = zoom lensMStoVS $ do
    mapM_ (fromMaybe (pure $ error "[listSlice'] The return value of this isn't used, and the thunk shouldn't fire.")) [b,e,s]
    _ <- vl
    pure $ pure $ error "[bool] The return value of this isn't used, and the thunk shouldn't fire."

instance BinderSym CodeInfoOO () where
  binder _ _ = noInfoBinder

instance EmptyStatement CodeInfoOO () where
  emptyStmt = noInfo

instance MultiStatement CodeInfoOO () where
  multi    = executeList

instance ValueStatement CodeInfoOO () where
  valStmt = zoom lensMStoVS . execute1

instance AssignStatement CodeInfoOO () where
  assign _ = zoom lensMStoVS . execute1
  (&-=)  _ = zoom lensMStoVS . execute1
  (&+=)  _ = zoom lensMStoVS . execute1
  (&++)  _ = noInfo
  (&--)  _ = noInfo

instance DeclStatement CodeInfoOO () () where
  varDec               _ _ = noInfo
  varDecDef            _ _ = zoom lensMStoVS . execute1
  setDec               _ _ = noInfo
  setDecDef            _ _ = zoom lensMStoVS . execute1
  listDec            _ _ _ = noInfo
  listDecDef           _ _ = zoom lensMStoVS . executeListErr
  arrayDec           _ _ _ _ = noInfo
  arrayDecDef          _ _ = zoom lensMStoVS . executeListErr
  constDecDef          _ _ = zoom lensMStoVS . execute1
  funcDecDef         _ _ _ bod = do
    _ <- bod
    pure $ pure $ error "[funcDecDef] The return value of this isn't used, and the thunk shouldn't fire."

instance OODeclStatement CodeInfoOO () where
  objDecDef            _ _ = zoom lensMStoVS . execute1
  objDecNew            _ _ = zoom lensMStoVS . executeListErr
  extObjDecNew       _ _ _ = zoom lensMStoVS . executeListErr

instance PrintConsole CodeInfoOO () where
  print        = zoom lensMStoVS . execute1
  printLn      = zoom lensMStoVS . execute1
  printStr   _ = noInfo
  printStrLn _ = noInfo

instance ReadConsole CodeInfoOO () where
  getInput       _ = noInfo
  discardInput     = noInfo

instance FileHandling CodeInfoOO () where
  openFileR _ v = modify (addException FileNotFound) >>
    execute1 (zoom lensMStoVS v)
  openFileW _ v = modify (addException IO) >> execute1 (zoom lensMStoVS v)
  openFileA _ v = modify (addException IO) >> execute1 (zoom lensMStoVS v)
  closeFile     = zoom lensMStoVS . execute1

instance PrintFile CodeInfoOO () where
  printFile      v   = zoom lensMStoVS . execute2 v
  printFileLn    v   = zoom lensMStoVS . execute2 v
  printFileStr   v _ = zoom lensMStoVS $ execute1 v
  printFileStrLn v _ = zoom lensMStoVS $ execute1 v

instance ReadFile CodeInfoOO () where
  getFileInput v _ = zoom lensMStoVS $ execute1 v
  discardFileInput = zoom lensMStoVS . execute1
  getFileInputLine v _ = zoom lensMStoVS $ execute1 v
  discardFileLine      = zoom lensMStoVS . execute1
  getFileInputAll  v _ = execute1 (zoom lensMStoVS v)

instance StringStatement CodeInfoOO () where
  stringSplit _ _ = zoom lensMStoVS . execute1

  stringListVals  _ = zoom lensMStoVS . execute1
  stringListLists _ = zoom lensMStoVS . execute1

instance FuncAppStatement CodeInfoOO () where
  inOutCall n vs _ _ = zoom lensMStoVS $ do
    sequence_ vs
    addCurrModCallSmt n
  extInOutCall l n vs _ _ = zoom lensMStoVS $ do
    sequence_ vs
    addExternalCallSmt l n

instance OOFuncAppStatement CodeInfoOO () where
  selfInOutCall n vs _ _ = zoom lensMStoVS $ do
    sequence_ vs
    addCurrModCallSmt n

instance CommentStatement CodeInfoOO () where
  comment _ = noInfo

instance ControlStatement CodeInfoOO () () where
  break    = noInfo
  continue = noInfo

  returnStmt = zoom lensMStoVS . execute1

  throw _ = modifyReturn (addException Standard) (toCode ())

  ifCond = evalConds
  switch v cs b = do
    _ <- zoom lensMStoVS v
    evalConds cs b

  ifExists v t f = do
    _ <- zoom lensMStoVS v
    _ <- t
    _ <- f
    pure $ pure $ error "[bool] The return value of this isn't used, and the thunk shouldn't fire."

  for dec v stmt bod = do
    _ <- dec
    _ <- zoom lensMStoVS v
    _ <- stmt
    _ <- bod
    pure $ pure $ error "[bool] The return value of this isn't used, and the thunk shouldn't fire."
  forRange _ b e s bod = do
    _ <- zoom lensMStoVS b
    _ <- zoom lensMStoVS e
    _ <- zoom lensMStoVS s
    _ <- bod
    pure $ pure $ error "[bool] The return value of this isn't used, and the thunk shouldn't fire."
  forEach _ v bod = do
    _ <- zoom lensMStoVS v
    _ <- bod
    pure $ pure $ error "[bool] The return value of this isn't used, and the thunk shouldn't fire."
  while v bod = do
    _ <- zoom lensMStoVS v
    _ <- bod
    pure $ pure $ error "[bool] The return value of this isn't used, and the thunk shouldn't fire."

  tryCatch _ cb = do
    _ <- cb
    noInfo

  assert cond msg = do
    _ <- zoom lensMStoVS cond
    _ <- zoom lensMStoVS msg
    noInfo

instance ObserverPattern CodeInfoOO () () where
  notifyObservers f _ = do
    _ <- zoom lensMStoVS f
    pure $ pure $ error "The return value of this isn't used, and the thunk shouldn't fire."

instance StrategyPattern CodeInfoOO () () where
  runStrategy _ ss vl _ = do
    mapM_ snd ss
    _ <- zoom lensMStoVS $ fromMaybe (pure $ pure $ error "[runStrategy] The return value of this isn't used, and the thunk shouldn't fire.") vl
    pure $ pure $ error "[runStrategy] The return value of this isn't used, and the thunk shouldn't fire."

instance VisibilitySym CodeInfoOO () where
  private = pure ()
  public  = pure ()

instance ParameterSym CodeInfoOO where
  param        _ = pure $ pure $ error "The return value of this isn't used, and the thunk shouldn't fire."
  pointerParam _ = pure $ pure $ error "The return value of this isn't used, and the thunk shouldn't fire."

instance MethodSym CodeInfoOO () () () () where
  docMain = updateMEMandCM "main"
  function n _ _ _ = updateMEMandCM n
  mainFunction = updateMEMandCM "main"
  docFunc _ _ _ f = do
    _ <- f
    noInfo

  inOutFunc      n _ _ _ _     = updateMEMandCM n
  docInOutFunc   n _ _ _ _ _   = updateMEMandCM n

instance OOMethodSym CodeInfoOO () () () () () where
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

instance StateVarSym CodeInfoOO () () () where
  stateVar    _ _ _   = noInfo
  stateVarDef _ _ _ _ = noInfo
  constVar    _ _ _   = noInfo

instance ClassSym CodeInfoOO () () where
  buildClass _ _ cs ms = do
    n <- zoom lensCStoFS getModuleName
    implementingClass n [] [] cs ms
  extraClass n _ _ cs ms = do
    modify (setClassName n)
    mapM_ (zoom lensCStoMS) cs
    mapM_ (zoom lensCStoMS) ms
    pure $ error "[extraClass] The return value of this isn't used, and the thunk shouldn't fire."

  implementingClass n _ _ cs ms = do
    modify (addClass n . setClassName n)
    mapM_ (zoom lensCStoMS) cs
    mapM_ (zoom lensCStoMS) ms
    pure $ error "[implementingClass] The return value of this isn't used, and the thunk shouldn't fire."

  docClass _ c = do
    _ <- c
    pure $ error "[docClass] The return value of this isn't used, and the thunk shouldn't fire."

instance ModuleSym CodeInfoOO () () where
  buildModule n _ funcs classes = do
    modify (setModuleName n)
    mapM_ (zoom lensFStoCS) classes
    mapM_ (zoom lensFStoMS) funcs
    modifyReturn (updateClassMap n) (pure $ error "[buildModule] The return value of this isn't used, and the thunk shouldn't fire.")

-- Helpers

noInfo :: State s (CodeInfoOO ())
noInfo = toState $ toCode ()

noInfoScope :: CodeInfoOO ScopeData
noInfoScope = pure $ sd Global -- Hack

noInfoBinder :: VSBinder CodeInfoOO
noInfoBinder = pure $ pure $ bindFormD "" (td Void "" empty) -- Hack

updateMEMandCM :: String -> MS (CodeInfoOO ()) -> MS (CodeInfoOO ())
updateMEMandCM n b = do
  _ <- b
  modify (updateCallMap n . updateMethodExcMap n)
  noInfo

evalConds :: [(SValue CodeInfoOO, MS (CodeInfoOO ()))] -> MS (CodeInfoOO ()) ->
  MS (CodeInfoOO ())
evalConds cs def = do
  mapM_ (zoom lensMStoVS . fst) cs
  mapM_ snd cs
  _ <- def
  noInfo

addCurrModCallVal :: String -> SValue CodeInfoOO
addCurrModCallVal n = do
  mn <- zoom lensVStoFS getModuleName
  modify (addCall (qualName mn n))
  pure $ error "[addCurrModCallSmt] The return value of this isn't used, and the thunk shouldn't fire."

addCurrModCallSmt :: String -> VS (CodeInfoOO ())
addCurrModCallSmt n = do
  mn <- zoom lensVStoFS getModuleName
  modify (addCall (qualName mn n))
  pure $ error "[addCurrModCallSmt] The return value of this isn't used, and the thunk shouldn't fire."

addExternalCallSmt :: String -> String -> VS (CodeInfoOO ())
addExternalCallSmt l n = do
  modify (addCall (qualName l n))
  pure $ error "[addExternalCall] The return value of this isn't used, and the thunk shouldn't fire."

addExternalCallVal :: String -> String -> SValue CodeInfoOO
addExternalCallVal l n = do
  modify (addCall (qualName l n))
  pure $ error "[addExternalCall] The return value of this isn't used, and the thunk shouldn't fire."

executeList :: [State a (CodeInfoOO ())] -> State a (CodeInfoOO ())
executeList l = do
  sequence_ l
  noInfo

executeListErr :: [State a (CodeInfoOO b)] -> State a (CodeInfoOO c)
executeListErr l = do
  sequence_ l
  pure $ pure $ error "[executeListErr] The return value of this isn't used, and the thunk shouldn't fire."

execute1 :: State a (CodeInfoOO b) -> State a (CodeInfoOO c)
execute1 s = do
  _ <- s
  pure $ pure $ error "[execute1] The return value of this isn't used, and the thunk shouldn't fire."

execute2 :: State a (CodeInfoOO b) -> State a (CodeInfoOO c) ->
  State a (CodeInfoOO d)
execute2 s1 s2 = do
  _ <- s1
  execute1 s2

execute3 :: State a (CodeInfoOO b) -> State a (CodeInfoOO c) ->
  State a (CodeInfoOO d) -> State a (CodeInfoOO e)
execute3 s1 s2 s3 = do
  _ <- s1
  execute2 s2 s3

currModCall :: String -> [VS (CodeInfoOO Value)] ->
  [(VS (CodeInfoOO Variable), VS (CodeInfoOO Value))] -> VS (CodeInfoOO Value)
currModCall n ps ns = do
  sequence_ ps
  mapM_ fst ns
  mapM_ snd ns
  addCurrModCallVal n
