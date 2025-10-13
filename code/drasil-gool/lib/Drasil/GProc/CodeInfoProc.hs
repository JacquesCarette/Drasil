{-# LANGUAGE TypeFamilies, Rank2Types #-}

-- Performs code analysis on the GOOL code
module Drasil.GProc.CodeInfoProc (CodeInfoProc(..)) where

import Drasil.Shared.InterfaceCommon (Body, Value, Statement, Method,
  SharedProg, BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..),
  ScopeSym(..), VariableSym(..), VariableElim(..), ValueSym(..), Argument(..),
  Literal(..), MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), List(..), Set(..), InternalList(..), ThunkSym(..),
  VectorType(..), VectorDecl(..), VectorThunk(..), VectorExpression(..),
  ThunkAssign(..), StatementSym(..), AssignStatement(..), DeclStatement(..),
  IOStatement(..), StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), VisibilitySym(..),
  ParameterSym(..), MethodSym(..))
import Drasil.GProc.InterfaceProc (ProcProg, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.Shared.CodeType (CodeType(Void))
import Drasil.Shared.AST (VisibilityTag(..), qualName)
import Drasil.Shared.CodeAnalysis (ExceptionType(..))
import Drasil.Shared.Helpers (toCode, unzipA2_, sequence1_, sequence2_,
  sequence3_, sequence4_)
import Drasil.Shared.State (GOOLState, VS, lensGStoFS, lensFStoMS, lensMStoVS,
  lensVStoFS, setModuleName, getModuleName, updateClassMap, addException,
  updateMethodExcMap, updateCallMap, addCall, ValueState, FileState,
  MethodState)

import Control.Monad.State (State, modify)
import qualified Control.Monad.State as S (get)
import Control.Lens.Zoom (zoom)
import Data.Maybe (fromMaybe)


data CodeInfoProc = CodeInfoProc
  deriving Eq

instance SharedProg CodeInfoProc
instance ProcProg CodeInfoProc

instance ProgramSym CodeInfoProc where
  type Program CodeInfoProc = State GOOLState GOOLState
  prog _ _ fs = do
    mapM_ (zoom lensGStoFS) fs
    S.get

instance FileSym CodeInfoProc where
  type File CodeInfoProc = State FileState ()
  fileDoc = sequence1_
  
  docMod _ _ _ = sequence1_

instance BodySym CodeInfoProc where
  type Body CodeInfoProc = State MethodState ()
  body = sequence_

  addComments _ _ = noInfo

instance BlockSym CodeInfoProc where
  type Block CodeInfoProc = State MethodState ()
  block = sequence_

instance TypeSym CodeInfoProc where
  type Type CodeInfoProc = State ValueState String
  bool              = noInfoType
  int               = noInfoType
  float             = noInfoType
  double            = noInfoType
  char              = noInfoType
  string            = noInfoType
  infile            = noInfoType
  outfile           = noInfoType
  listType      _   = noInfoType
  setType      _   = noInfoType
  arrayType     _   = noInfoType
  listInnerType _   = noInfoType
  funcType      _ _ = noInfoType
  void              = noInfoType

instance TypeElim CodeInfoProc where
  type TypeName CodeInfoProc = State ValueState String
  getType _     = Void
  getTypeString tp = tp

instance ScopeSym CodeInfoProc where
  type Scope CodeInfoProc = State GOOLState ()
  global = toCode ()
  mainFn = toCode ()
  local = toCode ()

instance VariableSym CodeInfoProc where
  type Variable CodeInfoProc = State ValueState ()
  var       _ _ = noInfo
  constant  _ _ = noInfo
  extVar  _ _ _ = noInfo
  arrayElem _ _ = noInfo

instance VariableElim CodeInfoProc where
  variableType _ = toCode ""

instance ValueSym CodeInfoProc where
  type Value CodeInfoProc = State ValueState ()
  valueType _ = toCode ""

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
  litArray  _ = sequence_
  litList   _ = sequence_
  litSet   _ = sequence_

instance MathConstant CodeInfoProc where
  pi = noInfo

instance VariableValue CodeInfoProc where
  valueOf _ = noInfo

instance CommandLineArgs CodeInfoProc where
  arg       _ = noInfo
  argsList    = noInfo
  argExists _ = noInfo

instance NumericExpression CodeInfoProc where
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

instance BooleanExpression CodeInfoProc where
  (?!)  = sequence1_
  (?&&) = sequence2_
  (?||) = sequence2_

instance Comparison CodeInfoProc where
  (?<)  = sequence2_
  (?<=) = sequence2_
  (?>)  = sequence2_
  (?>=) = sequence2_
  (?==) = sequence2_
  (?!=) = sequence2_
    
instance ValueExpression CodeInfoProc where
  inlineIf = sequence3_
  funcAppMixedArgs n _ = currModCall n
  extFuncAppMixedArgs l n _ vs ns = do
    sequence_ vs
    unzipA2_ ns
    addExternalCall l n  
  libFuncAppMixedArgs = extFuncAppMixedArgs

  lambda _ = sequence1_

  notNull = sequence1_

instance FunctionSym CodeInfoProc where
  type Function CodeInfoProc = State ValueState ()

instance List CodeInfoProc where
  intToIndex = sequence1_
  indexToInt = sequence1_
  listSize   = sequence1_
  listAdd    = sequence3_
  listAppend = sequence2_
  listAccess = sequence2_
  listSet    = sequence3_
  indexOf    = sequence2_

instance Set CodeInfoProc where
 contains = sequence2_
 setAdd = sequence2_
 setRemove = sequence2_
 setUnion = sequence2_

instance InternalList CodeInfoProc where
  listSlice' b e s _ vl = zoom lensMStoVS $ do
    mapM_ (fromMaybe noInfo) [b,e,s]
    _ <- vl
    noInfo

instance ThunkSym CodeInfoProc where
  type Thunk CodeInfoProc = State ValueState ()

instance ThunkAssign CodeInfoProc where
  thunkAssign _ = zoom lensMStoVS . sequence1_

instance VectorType CodeInfoProc where
  vecType _ = noInfoType

instance VectorDecl CodeInfoProc where
  vecDec  _ _ _ = noInfo
  vecDecDef _ _ = zoom lensMStoVS . sequence_

instance VectorThunk CodeInfoProc where
  vecThunk _ = noInfo

instance VectorExpression CodeInfoProc where
  vecScale = sequence2_
  vecAdd = sequence2_
  vecIndex = sequence2_
  vecDot = sequence2_

instance StatementSym CodeInfoProc where
  type Statement CodeInfoProc = State MethodState ()
  valStmt = zoom lensMStoVS . sequence1_
  emptyStmt = noInfo
  multi    = sequence_
  
instance AssignStatement CodeInfoProc where
  assign _ = zoom lensMStoVS . sequence1_
  (&-=)  _ = zoom lensMStoVS . sequence1_
  (&+=)  _ = zoom lensMStoVS . sequence1_
  (&++)  _ = noInfo
  (&--)  _ = noInfo

instance DeclStatement CodeInfoProc where
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

instance IOStatement CodeInfoProc where
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

instance StringStatement CodeInfoProc where
  stringSplit _ _ = zoom lensMStoVS . sequence1_

  stringListVals  _ = zoom lensMStoVS . sequence1_
  stringListLists _ = zoom lensMStoVS . sequence1_

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

instance VisibilitySym CodeInfoProc where
  type Visibility CodeInfoProc = State GOOLState VisibilityTag
  private = toCode Priv
  public  = toCode Pub

instance ParameterSym CodeInfoProc where
  type Parameter CodeInfoProc = State GOOLState ()
  param        _ = noInfo
  pointerParam _ = noInfo

instance MethodSym CodeInfoProc where
  type Method CodeInfoProc = State MethodState ()
  docMain = updateMEMandCM "main"
  function n _ _ _ = updateMEMandCM n
  mainFunction = updateMEMandCM "main"
  docFunc _ _ _ f = do
    _ <- f
    noInfo

  inOutFunc      n _ _ _ _     = updateMEMandCM n
  docInOutFunc   n _ _ _ _ _   = updateMEMandCM n

instance ModuleSym CodeInfoProc where
  type Module CodeInfoProc = State FileState ()
  buildModule n _ funcs = do
    modify (setModuleName n)
    mapM_ (zoom lensFStoMS) funcs
    modify (updateClassMap n)

-- Helpers

noInfo :: State s ()
noInfo = pure ()

noInfoType :: State s String
noInfoType = pure ""

updateMEMandCM :: String -> Body CodeInfoProc -> Method CodeInfoProc
updateMEMandCM n b = do
  _ <- b
  modify (updateCallMap n . updateMethodExcMap n)
  noInfo

evalConds :: [(Value CodeInfoProc, Body CodeInfoProc)] -> Body CodeInfoProc -> 
  Statement CodeInfoProc
evalConds cs def = do
  mapM_ (zoom lensMStoVS . fst) cs
  mapM_ snd cs
  _ <- def
  noInfo

addCurrModCall :: String -> Value CodeInfoProc
addCurrModCall n = do
  mn <- zoom lensVStoFS getModuleName 
  modify (addCall (qualName mn n)) 
  noInfo

addExternalCall :: String -> String -> Value CodeInfoProc
addExternalCall l n = modify (addCall (qualName l n)) >> noInfo

currModCall :: String -> [VS ()] -> [(VS (), VS ())] -> VS ()
currModCall n ps ns = do
  sequence_ ps
  unzipA2_ ns
  addCurrModCall n
