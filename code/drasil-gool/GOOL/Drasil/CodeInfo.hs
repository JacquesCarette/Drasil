{-# LANGUAGE TypeFamilies, Rank2Types #-}

module GOOL.Drasil.CodeInfo (CodeInfo(..)) where

import GOOL.Drasil.Symantics (ProgramSym(..), FileSym(..), PermanenceSym(..), 
  BodySym(..), BlockSym(..), ControlBlockSym(..), TypeSym(..), VariableSym(..), 
  ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), Selector(..), InternalSelector(..), FunctionSym(..), 
  SelectorFunction(..), StatementSym(..), ControlStatementSym(..), ScopeSym(..),
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
  ClassSym(..), ModuleSym(..), BlockCommentSym(..))
import GOOL.Drasil.CodeType (CodeType(Void))
import GOOL.Drasil.Data (Binding(Dynamic), ScopeTag(..), Exception(..),
  exception, stdExc)
import GOOL.Drasil.Helpers (toCode, toState)
import GOOL.Drasil.State (GOOLState, MS, lensGStoFS, lensFStoCS, lensFStoMS, 
  lensCStoMS, lensMStoFS, modifyReturn, setClassName, setModuleName, 
  getModuleName, addClass, updateClassMap, addException, updateMethodExcMap)

import Control.Monad.State (State, modify)
import qualified Control.Monad.State as S (get)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (empty)

newtype CodeInfo a = CI {unCI :: a} deriving Eq

instance Functor CodeInfo where
  fmap f (CI x) = CI (f x)

instance Applicative CodeInfo where
  pure = CI
  (CI f) <*> (CI x) = CI (f x)

instance Monad CodeInfo where
  return = CI
  CI x >>= f = f x

instance ProgramSym CodeInfo where
  type Program CodeInfo = GOOLState
  prog _ fs = do
    mapM_ (zoom lensGStoFS) fs
    s <- S.get
    toState $ toCode s
  
noInfo :: State s (CodeInfo ())
noInfo = toState $ toCode ()

instance FileSym CodeInfo where
  type RenderFile CodeInfo = ()
  fileDoc m = do
    _ <- m
    noInfo
  
  docMod _ _ _ m = do
    _ <- m
    noInfo

  commentedMod _ m = do
    _ <- m
    noInfo

instance PermanenceSym CodeInfo where
  type Permanence CodeInfo = ()
  static_ = toCode ()
  dynamic_ = toCode ()

instance BodySym CodeInfo where
  type Body CodeInfo = ()
  body bs = do
    sequence_ bs
    noInfo
  bodyStatements ss = do
    sequence_ ss
    noInfo
  oneLiner s = do
    _ <- s
    noInfo

  addComments _ _ = noInfo

instance BlockSym CodeInfo where
  type Block CodeInfo = ()
  block ss = do
    sequence_ ss
    noInfo

instance TypeSym CodeInfo where
  type Type CodeInfo = ()
  bool = noInfo
  int = noInfo
  float = noInfo
  char = noInfo
  string = noInfo
  infile = noInfo
  outfile = noInfo
  listType _ _ = noInfo
  listInnerType _ = noInfo
  obj _ = noInfo
  enumType _ = noInfo
  iterator _ = noInfo
  void = noInfo

  getType _ = Void
  getTypeString _ = ""
  getTypeDoc _ = empty

instance ControlBlockSym CodeInfo where
  runStrategy _ _ _ _ = noInfo

  listSlice' _ _ _ _ _ = noInfo

  solveODE _ _ = noInfo

instance VariableSym CodeInfo where
  type Variable CodeInfo = ()
  var _ _ = noInfo
  staticVar _ _ = noInfo
  const _ _ = noInfo
  extVar _ _ _ = noInfo
  self = noInfo
  enumVar _ _ = noInfo
  classVar _ _ = noInfo
  extClassVar _ _ = noInfo
  objVar _ _ = noInfo
  objVarSelf _ = noInfo
  listVar _ _ _ = noInfo
  listOf _ _ = noInfo
  iterVar _ _ = noInfo

  ($->) _ _ = noInfo
  
  variableBind _ = Dynamic
  variableName _ = ""
  variableType _ = toCode ()
  variableDoc _ = empty

instance ValueSym CodeInfo where
  type Value CodeInfo = ()
  litTrue = noInfo
  litFalse = noInfo
  litChar _ = noInfo
  litFloat _ = noInfo
  litInt _ = noInfo
  litString _ = noInfo

  pi = noInfo

  ($:) _ _ = noInfo

  valueOf _ = noInfo
  arg _ = noInfo
  enumElement _ _ = noInfo
  
  argsList = noInfo

  valueType _ = toCode ()
  valueDoc _ = empty

instance NumericExpression CodeInfo where
  (#~) _ = noInfo
  (#/^) _ = noInfo
  (#|) _ = noInfo
  (#+) _ _ = noInfo
  (#-) _ _ = noInfo
  (#*) _ _ = noInfo
  (#/) _ _ = noInfo
  (#%) _ _ = noInfo
  (#^) _ _ = noInfo

  log _ = noInfo
  ln _ = noInfo
  exp _ = noInfo
  sin _ = noInfo
  cos _ = noInfo
  tan _ = noInfo
  csc _ = noInfo
  sec _ = noInfo
  cot _ = noInfo
  arcsin _ = noInfo
  arccos _ = noInfo
  arctan _ = noInfo
  floor _ = noInfo
  ceil _ = noInfo

instance BooleanExpression CodeInfo where
  (?!) _ = noInfo
  (?&&) _ _ = noInfo
  (?||) _ _ = noInfo

  (?<) _ _ = noInfo
  (?<=) _ _ = noInfo
  (?>) _ _ = noInfo
  (?>=) _ _ = noInfo
  (?==) _ _ = noInfo
  (?!=) _ _ = noInfo
    
instance ValueExpression CodeInfo where
  inlineIf _ _ _ = noInfo
  funcApp _ _ _ = noInfo
  selfFuncApp _ _ _ = noInfo
  extFuncApp _ _ _ _ = noInfo
  newObj _ _ = noInfo
  extNewObj _ _ _ = noInfo

  exists _ = noInfo
  notNull _ = noInfo

instance Selector CodeInfo where
  objAccess _ _ = noInfo
  ($.) _ _ = noInfo

  selfAccess _ = noInfo

  listIndexExists _ _ = noInfo
  argExists _ = noInfo
  
  indexOf _ _ = noInfo
  
instance InternalSelector CodeInfo where
  objMethodCall' _ _ _ _ = noInfo
  objMethodCallNoParams' _ _ _ = noInfo

instance FunctionSym CodeInfo where
  type Function CodeInfo = ()
  func _ _ _ = noInfo
  
  get _ _ = noInfo
  set _ _ _ = noInfo

  listSize _ = noInfo
  listAdd _ _ _ = noInfo
  listAppend _ _ = noInfo

  iterBegin _ = noInfo
  iterEnd _ = noInfo

instance SelectorFunction CodeInfo where
  listAccess _ _ = noInfo
  listSet _ _ _ = noInfo
  at _ _ = noInfo

instance StatementSym CodeInfo where
  type Statement CodeInfo = ()
  assign _ _ = noInfo
  assignToListIndex _ _ _ = noInfo
  multiAssign _ _ = noInfo
  (&=) _ _ = noInfo
  (&-=) _ _ = noInfo
  (&+=) _ _ = noInfo
  (&++) _ = noInfo
  (&~-) _ = noInfo

  varDec _ = noInfo
  varDecDef _ _ = noInfo
  listDec _ _ = noInfo
  listDecDef _ _ = noInfo
  objDecDef _ _ = noInfo
  objDecNew _ _ = noInfo
  extObjDecNew _ _ _ = noInfo
  objDecNewNoParams _ = noInfo
  extObjDecNewNoParams _ _ = noInfo
  constDecDef _ _ = noInfo

  print _ = noInfo
  printLn _ = noInfo
  printStr _ = noInfo
  printStrLn _ = noInfo

  printFile _ _ = noInfo
  printFileLn _ _ = noInfo
  printFileStr _ _ = noInfo
  printFileStrLn _ _ = noInfo

  getInput _ = noInfo
  discardInput = noInfo
  getFileInput _ _ = noInfo
  discardFileInput _ = noInfo

  openFileR _ _ = modifyReturn (addException fnfExc) 
    (toCode ())
  openFileW _ _ = modifyReturn (addException ioExc) (toCode ())
  openFileA _ _ = modifyReturn (addException ioExc) (toCode ())
  closeFile _ = noInfo

  getFileInputLine _ _ = noInfo
  discardFileLine _ = noInfo
  stringSplit _ _ _ = noInfo

  stringListVals _ _ = noInfo
  stringListLists _ _ = noInfo

  break = noInfo
  continue = noInfo

  returnState _ = noInfo
  multiReturn _ = noInfo

  valState _ = noInfo

  comment _ = noInfo

  free _ = noInfo

  throw _ = modifyReturn (addException genericExc) (toCode ())

  initState _ _ = noInfo
  changeState _ _ = noInfo

  initObserverList _ _ = noInfo
  addObserver _ = noInfo

  inOutCall _ _ _ _ = noInfo
  selfInOutCall _ _ _ _ = noInfo
  extInOutCall _ _ _ _ _ = noInfo

  multi ss = do
    sequence_ ss
    noInfo

instance ControlStatementSym CodeInfo where
  ifCond = evalConds
  ifNoElse cs = do
    mapM_ snd cs
    noInfo
  switch _ = evalConds
  switchAsIf _ = evalConds

  ifExists _ ib eb = do
    _ <- ib
    _ <- eb
    noInfo

  for _ _ _ b = do
    _ <- b
    noInfo
  forRange _ _ _ _ b = do
    _ <- b
    noInfo
  forEach _ _ b = do
    _ <- b
    noInfo
  while _ b = do
    _ <- b
    noInfo

  tryCatch _ cb = do
    _ <- cb
    noInfo

  checkState _ = evalConds

  notifyObservers _ _ = noInfo

  getFileInputAll _ _ = noInfo

instance ScopeSym CodeInfo where
  type Scope CodeInfo = ScopeTag
  private = toCode Priv
  public = toCode Pub

instance MethodTypeSym CodeInfo where
  type MethodType CodeInfo = ()
  mType _ = noInfo
  construct _ = noInfo

instance ParameterSym CodeInfo where
  type Parameter CodeInfo = ()
  param _ = noInfo
  pointerParam _ = noInfo

instance MethodSym CodeInfo where
  type Method CodeInfo = ()
  method n _ _ _ _ = updateMEM n
  getMethod _ = noInfo
  setMethod _ = noInfo
  privMethod n _ _ = updateMEM n
  pubMethod n _ _ = updateMEM n
  constructor _ b = do
    _ <- b
    mn <- zoom lensMStoFS getModuleName
    modify (updateMethodExcMap mn)
    noInfo
  destructor _ = noInfo

  docMain = updateMEM "main"

  function n _ _ _ _ = updateMEM n
  mainFunction = updateMEM "main"

  docFunc _ _ _ f = do
    _ <- f
    noInfo

  inOutMethod n _ _ _ _ _ = updateMEM n

  docInOutMethod n _ _ _ _ _ _ = updateMEM n

  inOutFunc n _ _ _ _ _ = updateMEM n

  docInOutFunc n _ _ _ _ _ _ = updateMEM n

instance StateVarSym CodeInfo where
  type StateVar CodeInfo = ()
  stateVar _ _ _ = noInfo
  stateVarDef _ _ _ _ _ = noInfo
  constVar _ _ _ _ = noInfo
  privMVar _ = noInfo
  pubMVar _ = noInfo
  pubGVar _ = noInfo

instance ClassSym CodeInfo where
  type Class CodeInfo = ()
  buildClass n _ s _ ms = do
    modify ((if unCI s == Pub then addClass n else id) . setClassName n)
    mapM_ (zoom lensCStoMS) ms
    noInfo
  enum n _ s = if unCI s == Pub then modifyReturn (addClass n) (toCode ()) else 
    noInfo 
  privClass n _ _ ms = do
    modify (setClassName n)
    mapM_ (zoom lensCStoMS) ms
    noInfo
  pubClass n _ _ ms = do
    modify (addClass n . setClassName n)
    mapM_ (zoom lensCStoMS) ms
    noInfo

  docClass _ c = do
    _ <- c
    noInfo

  commentedClass _ c = do
    _ <- c
    noInfo

instance ModuleSym CodeInfo where
  type Module CodeInfo = ()
  buildModule n fs cs = do
    modify (setModuleName n)
    mapM_ (zoom lensFStoCS) cs 
    mapM_ (zoom lensFStoMS) fs
    modifyReturn (updateClassMap n) (toCode ())

instance BlockCommentSym CodeInfo where
  type BlockComment CodeInfo = ()
  blockComment _ = toCode ()
  docComment _ = noInfo

  blockCommentDoc _ = empty


-- Helpers

fnfExc, ioExc, genericExc :: Exception
fnfExc = exception "java.io" "FileNotFoundException"
ioExc = exception "java.io" "IOException"
genericExc = stdExc "Exception"

updateMEM :: String -> MS (CodeInfo (Body CodeInfo)) -> 
  MS (CodeInfo (Method CodeInfo))
updateMEM n b = do
  _ <- b
  modify (updateMethodExcMap n)
  noInfo

evalConds :: [(a, MS (CodeInfo (Body CodeInfo)))] -> 
  MS (CodeInfo (Body CodeInfo)) -> MS (CodeInfo (Statement CodeInfo))
evalConds cs def = do
  mapM_ snd cs
  _ <- def
  noInfo