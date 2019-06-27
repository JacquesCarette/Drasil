{-# LANGUAGE TypeFamilies #-}

module Language.Drasil.Code.Imperative.Symantics (
  -- Types
  Label, Library,
  -- Typeclasses
  PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
  BodySym(..), ControlBlockSym(..), BlockSym(..), StateTypeSym(..), 
  UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), 
  StateVarSym(..), ClassSym(..), ModuleSym(..)
) where

type Label = String
type Library = String

class (RenderSym repr) => PackageSym repr where
  type Package repr 
  packMods :: Label -> [repr (RenderFile repr)] -> repr (Package repr)

class (ModuleSym repr, ControlBlockSym repr) => RenderSym repr where
  type RenderFile repr
  fileDoc :: repr (Module repr) -> repr (RenderFile repr)
  top :: repr (Module repr) -> repr (Block repr)
  bottom :: repr (Block repr)

class (ValueSym repr, PermanenceSym repr) => KeywordSym repr where
  type Keyword repr
  endStatement     :: repr (Keyword repr)
  endStatementLoop :: repr (Keyword repr)

  include :: Label -> repr (Keyword repr)
  inherit :: repr (Keyword repr)

  list     :: repr (Permanence repr) -> repr (Keyword repr)
  listObj  :: repr (Keyword repr)

  blockStart :: repr (Keyword repr)
  blockEnd   :: repr (Keyword repr)

  ifBodyStart :: repr (Keyword repr)
  elseIf      :: repr (Keyword repr)

  iterForEachLabel :: repr (Keyword repr)
  iterInLabel      :: repr (Keyword repr)

  commentStart :: repr (Keyword repr)

  printFunc       :: repr (Keyword repr)
  printLnFunc     :: repr (Keyword repr)
  printFileFunc   :: repr (Value repr) -> repr (Keyword repr)
  printFileLnFunc :: repr (Value repr) -> repr (Keyword repr)

class PermanenceSym repr where
  type Permanence repr
  static_  :: repr (Permanence repr)
  dynamic_ :: repr (Permanence repr)

class (BlockSym repr) => BodySym repr where
  type Body repr
  body           :: [repr (Block repr)] -> repr (Body repr)
  bodyStatements :: [repr (Statement repr)] -> repr (Body repr)
  oneLiner       :: repr (Statement repr) -> repr (Body repr)

  addComments :: Label -> repr (Body repr) -> repr (Body repr)

class (StatementSym repr) => BlockSym repr where
  type Block repr
  block   :: [repr (Statement repr)] -> repr (Block repr)

class (PermanenceSym repr) => StateTypeSym repr where
  type StateType repr
  bool          :: repr (StateType repr)
  int           :: repr (StateType repr)
  float         :: repr (StateType repr)
  char          :: repr (StateType repr)
  string        :: repr (StateType repr)
  infile        :: repr (StateType repr)
  outfile       :: repr (StateType repr)
  listType      :: repr (Permanence repr) -> repr (StateType repr) -> repr (StateType repr)
  listInnerType :: repr (StateType repr) -> repr (StateType repr)
  obj           :: Label -> repr (StateType repr)
  enumType      :: Label -> repr (StateType repr)
  iterator      :: repr (StateType repr) -> repr (StateType repr)
  void          :: repr (StateType repr)

class (BodySym repr, ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, repr (Body repr))] -> 
    Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) -> repr (Block repr)

  listSlice        :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Value repr) -> Maybe (repr (Value repr)) -> 
    Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) -> repr (Block repr)

class UnaryOpSym repr where
  type UnaryOp repr
  notOp    :: repr (UnaryOp repr)
  negateOp :: repr (UnaryOp repr)
  sqrtOp   :: repr (UnaryOp repr)
  absOp    :: repr (UnaryOp repr)
  logOp    :: repr (UnaryOp repr)
  lnOp     :: repr (UnaryOp repr)
  expOp    :: repr (UnaryOp repr)
  sinOp    :: repr (UnaryOp repr)
  cosOp    :: repr (UnaryOp repr)
  tanOp    :: repr (UnaryOp repr)
  asinOp   :: repr (UnaryOp repr)
  acosOp   :: repr (UnaryOp repr)
  atanOp   :: repr (UnaryOp repr)
  floorOp  :: repr (UnaryOp repr)
  ceilOp   :: repr (UnaryOp repr)

class BinaryOpSym repr where
  type BinaryOp repr
  equalOp        :: repr (BinaryOp repr)
  notEqualOp     :: repr (BinaryOp repr)
  greaterOp      :: repr (BinaryOp repr)
  greaterEqualOp :: repr (BinaryOp repr)
  lessOp         :: repr (BinaryOp repr)
  lessEqualOp    :: repr (BinaryOp repr)
  plusOp         :: repr (BinaryOp repr)
  minusOp        :: repr (BinaryOp repr)
  multOp         :: repr (BinaryOp repr)
  divideOp       :: repr (BinaryOp repr)
  powerOp        :: repr (BinaryOp repr)
  moduloOp       :: repr (BinaryOp repr)
  andOp          :: repr (BinaryOp repr)
  orOp           :: repr (BinaryOp repr)

class (StateTypeSym repr, StateVarSym repr) => ValueSym repr where
  type Value repr
  litTrue   :: repr (Value repr)
  litFalse  :: repr (Value repr)
  litChar   :: Char -> repr (Value repr)
  litFloat  :: Double -> repr (Value repr)
  litInt    :: Integer -> repr (Value repr)
  litString :: String -> repr (Value repr)

  --other operators ($)
  ($->) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 9 $->
  ($:)  :: Label -> Label -> repr (Value repr)
  infixl 9 $:


  const        :: Label -> repr (StateType repr) -> repr (Value repr)
  var          :: Label -> repr (StateType repr) -> repr (Value repr)
  extVar       :: Library -> Label -> repr (StateType repr) -> repr (Value repr)
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  self         :: Label -> repr (Value repr)
  arg          :: Integer -> repr (Value repr)
  enumElement  :: Label -> Label -> repr (Value repr)
  enumVar      :: Label -> Label -> repr (Value repr)
  objVar       :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  objVarSelf   :: Label -> Label -> repr (StateType repr) -> repr (Value repr)
  listVar      :: Label -> repr (Permanence repr) -> repr (StateType repr) -> 
    repr (Value repr)
  listOf       :: Label -> repr (StateType repr) -> repr (Value repr)
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> repr (StateType repr) -> repr (Value repr)

  inputFunc :: repr (Value repr)
  argsList  :: repr (Value repr)

  valueName :: repr (Value repr) -> String -- Function for converting a value to a string of the value's name
  valueType :: repr (Value repr) -> repr (StateType repr)

class (ValueSym repr, UnaryOpSym repr, BinaryOpSym repr) => 
  NumericExpression repr where
  (#~)  :: repr (Value repr) -> repr (Value repr)
  infixl 8 #~
  (#/^) :: repr (Value repr) -> repr (Value repr)
  infixl 7 #/^
  (#|)  :: repr (Value repr) -> repr (Value repr)
  infixl 7 #|
  (#+)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 5 #+
  (#-)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 5 #-
  (#*)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 6 #*
  (#/)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 6 #/
  (#%)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 6 #%
  (#^)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 7 #^

  log    :: repr (Value repr) -> repr (Value repr)
  ln     :: repr (Value repr) -> repr (Value repr)
  exp    :: repr (Value repr) -> repr (Value repr)
  sin    :: repr (Value repr) -> repr (Value repr)
  cos    :: repr (Value repr) -> repr (Value repr)
  tan    :: repr (Value repr) -> repr (Value repr)
  csc    :: repr (Value repr) -> repr (Value repr)
  sec    :: repr (Value repr) -> repr (Value repr)
  cot    :: repr (Value repr) -> repr (Value repr)
  arcsin :: repr (Value repr) -> repr (Value repr)
  arccos :: repr (Value repr) -> repr (Value repr)
  arctan :: repr (Value repr) -> repr (Value repr)
  floor  :: repr (Value repr) -> repr (Value repr)
  ceil   :: repr (Value repr) -> repr (Value repr)

-- I considered having two separate classes, BooleanExpressions and BooleanComparisons,
-- but this would require cyclic constraints, since it is feasible to have
-- BooleanComparisons of BooleanExpressions and also BooleanExpressions of BooleanComparisons.
-- This has the drawback of requiring a NumericExpression constraint for the first
-- 3 functions here, even though they don't really need it.
class (ValueSym repr, NumericExpression repr) => BooleanExpression repr where
  (?!)  :: repr (Value repr) -> repr (Value repr)
  infixr 6 ?!
  (?&&) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 2 ?&&
  (?||) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 1 ?||

  (?<)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 4 ?<
  (?<=) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 4 ?<=
  (?>)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 4 ?>
  (?>=) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 4 ?>=
  (?==) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 3 ?==
  (?!=) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  infixl 3 ?!=

class (ValueSym repr, NumericExpression repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: repr (Value repr) -> repr (Value repr) -> repr (Value repr) ->
    repr (Value repr)
  funcApp      :: Label -> repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Value repr)
  selfFuncApp  :: Label -> repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Value repr)
  extFuncApp   :: Library -> Label -> repr (StateType repr) -> 
    [repr (Value repr)] -> repr (Value repr)
  stateObj     :: repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Value repr)
  extStateObj  :: Library -> repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Value repr)
  listStateObj :: repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Value repr)

  exists  :: repr (Value repr) -> repr (Value repr)
  notNull :: repr (Value repr) -> repr (Value repr)

-- The cyclic constraints issue arises here too. I've constrained this by ValueExpression,
-- but really one might want one of these values as part of an expression, so the
-- constraint would have to go both ways. I'm not sure what the solution is for
-- these sorts of problems, other than removing the constraints altogether, but 
-- then what is the purpose of splitting the typeclasses into smaller typeclasses?
-- I'm leaving it as is for now, even though I suspect this will change in the future.
class (FunctionSym repr, ValueSym repr, ValueExpression repr) => 
  Selector repr where
  objAccess :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)
  ($.)      :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)
  infixl 9 $.

  objMethodCall     :: repr (StateType repr) -> repr (Value repr) -> Label -> 
    [repr (Value repr)] -> repr (Value repr)
  objMethodCallNoParams :: repr (StateType repr) -> repr (Value repr) -> Label
    -> repr (Value repr)

  selfAccess :: Label -> repr (Function repr) -> repr (Value repr)

  listSizeAccess :: repr (Value repr) -> repr (Value repr)

  listIndexExists :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  argExists       :: Integer -> repr (Value repr)

  indexOf :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

  stringEqual :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

  castObj        :: repr (Function repr) -> repr (Value repr) -> 
    repr (Value repr)
  castStrToFloat :: repr (Value repr) -> repr (Value repr)

class (ValueSym repr, ValueExpression repr) => FunctionSym repr where
  type Function repr
  func           :: Label -> repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Function repr)
  cast           :: repr (StateType repr) -> repr (StateType repr) -> 
    repr (Function repr)
  castListToInt  :: repr (Function repr)
  get            :: Label -> repr (StateType repr) -> repr (Function repr)
  set            :: Label -> repr (Value repr) -> repr (Function repr)

  listSize           :: repr (Function repr)
  listAdd            :: repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Function repr)
  listAppend         :: repr (Value repr) -> repr (Function repr)

  iterBegin :: repr (StateType repr) -> repr (Function repr)
  iterEnd   :: repr (StateType repr) -> repr (Function repr)

class (ValueSym repr, FunctionSym repr, Selector repr) => 
  SelectorFunction repr where
  listAccess :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Function repr)
  listSet    :: repr (Value repr) -> repr (Value repr) -> repr (Function repr)

  listAccessEnum   :: repr (StateType repr) -> repr (StateType repr) ->
    repr (Value repr) -> repr (Function repr)
  listSetEnum      :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Function repr)

  at :: repr (StateType repr) -> Label -> repr (Function repr)

class (ValueSym repr, Selector repr, SelectorFunction repr, FunctionSym repr) 
  => StatementSym repr where
  type Statement repr
  (&=)   :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
  infixr 1 &=
  (&-=)  :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
  infixl 1 &-=
  (&+=)  :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
  infixl 1 &+=
  (&++)  :: repr (Value repr) -> repr (Statement repr)
  infixl 8 &++
  (&~-)  :: repr (Value repr) -> repr (Statement repr)
  infixl 8 &~-

  assign            :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  assignToListIndex :: repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Statement repr)
  multiAssign       :: [repr (Value repr)] -> [repr (Value repr)] ->
    repr (Statement repr) 

  varDec           :: repr (Value repr) -> repr (Statement repr)
  varDecDef        :: Label -> repr (StateType repr) -> repr (Value repr) -> 
    repr (Statement repr)
  listDec          :: Label -> Integer -> repr (StateType repr) -> 
    repr (Statement repr)
  listDecDef       :: Label -> repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Statement repr)
  objDecDef        :: Label -> repr (StateType repr) -> repr (Value repr) -> 
    repr (Statement repr)
  objDecNew        :: Label -> repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Statement repr)
  extObjDecNew     :: Label -> Library -> repr (StateType repr) -> 
    [repr (Value repr)] -> repr (Statement repr)
  objDecNewVoid    :: Label -> repr (StateType repr) -> repr (Statement repr)
  extObjDecNewVoid :: Label -> Library -> repr (StateType repr) -> 
    repr (Statement repr)
  constDecDef      :: Label -> repr (StateType repr) -> repr (Value repr) -> 
    repr (Statement repr)

  print      :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Statement repr)
  printLn    :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Statement repr)
  printStr   :: String -> repr (Statement repr)
  printStrLn :: String -> repr (Statement repr)

  printFile      :: repr (Value repr) -> repr (StateType repr) -> 
    repr (Value repr) -> repr (Statement repr)
  printFileLn    :: repr (Value repr) -> repr (StateType repr) -> 
    repr (Value repr) -> repr (Statement repr)
  printFileStr   :: repr (Value repr) -> String -> repr (Statement repr)
  printFileStrLn :: repr (Value repr) -> String -> repr (Statement repr)

  printList       :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Statement repr)
  printLnList     :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Statement repr)
  printFileList   :: repr (Value repr) -> repr (StateType repr) -> 
    repr (Value repr) -> repr (Statement repr)
  printFileLnList :: repr (Value repr) -> repr (StateType repr) -> 
    repr (Value repr) -> repr (Statement repr)

  getIntInput        :: repr (Value repr) -> repr (Statement repr)
  getFloatInput      :: repr (Value repr) -> repr (Statement repr)
  getBoolInput       :: repr (Value repr) -> repr (Statement repr)
  getStringInput     :: repr (Value repr) -> repr (Statement repr)
  getCharInput       :: repr (Value repr) -> repr (Statement repr)
  discardInput       :: repr (Statement repr)
  getIntFileInput    :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  getFloatFileInput  :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  getBoolFileInput   :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  getStringFileInput :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  getCharFileInput   :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  discardFileInput   :: repr (Value repr) -> repr (Statement repr)

  openFileR :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
  openFileW :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
  openFileA :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
  closeFile :: repr (Value repr) -> repr (Statement repr)

  getFileInputLine :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  discardFileLine  :: repr (Value repr) -> repr (Statement repr)
  stringSplit      :: Char -> repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)

  break :: repr (Statement repr)
  continue :: repr (Statement repr)

  returnState :: repr (Value repr) -> repr (Statement repr)
  returnVar :: Label -> repr (StateType repr) -> repr (Statement repr)
  multiReturn :: [repr (Value repr)] -> repr (Statement repr)

  valState :: repr (Value repr) -> repr (Statement repr)

  comment :: Label -> repr (Statement repr)

  free :: repr (Value repr) -> repr (Statement repr)

  throw :: Label -> repr (Statement repr)

  initState   :: Label -> Label -> repr (Statement repr)
  changeState :: Label -> Label -> repr (Statement repr)

  initObserverList :: repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Statement repr)
  addObserver      :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Statement repr)

  -- The two lists are inputs and outputs, respectively
  inOutCall :: Label -> [repr (Value repr)] -> [repr (Value repr)] -> 
    repr (Statement repr)
  extInOutCall :: Library -> Label -> [repr (Value repr)] ->
    [repr (Value repr)] -> repr (Statement repr)

  state     :: repr (Statement repr) -> repr (Statement repr)
  loopState :: repr (Statement repr) -> repr (Statement repr)
  multi     :: [repr (Statement repr)] -> repr (Statement repr)

class (StatementSym repr, BodySym repr) => ControlStatementSym repr where
  ifCond     :: [(repr (Value repr), repr (Body repr))] -> repr (Body repr) ->
    repr (Statement repr)
  ifNoElse   :: [(repr (Value repr), repr (Body repr))] -> repr (Statement repr)
  switch     :: repr (Value repr) -> [(repr (Value repr), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr) -- is there value in separating Literals into their own type?
  switchAsIf :: repr (Value repr) -> [(repr (Value repr), repr (Body repr))] ->
    repr (Body repr) -> repr (Statement repr)

  ifExists :: repr (Value repr) -> repr (Body repr) -> repr (Body repr) ->
    repr (Statement repr)

  for      :: repr (Statement repr) -> repr (Value repr) -> 
    repr (Statement repr) -> repr (Body repr) -> repr (Statement repr)
  forRange :: Label -> repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Body repr) -> repr (Statement repr)
  -- Had to add StateType to forEach because I can't extract the StateType from the value.
  forEach  :: Label -> repr (StateType repr) -> repr (Value repr) -> 
    repr (Body repr) -> repr (Statement repr)
  while    :: repr (Value repr) -> repr (Body repr) -> repr (Statement repr) 

  tryCatch :: repr (Body repr) -> repr (Body repr) -> repr (Statement repr)

  checkState      :: Label -> [(repr (Value repr), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  notifyObservers :: repr (StateType repr) -> Label -> repr (StateType repr) -> 
    [repr (Value repr)] -> repr (Statement repr)

  getFileInputAll  :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

  includeScope :: repr (Scope repr) -> repr (Scope repr)

class MethodTypeSym repr where
  type MethodType repr
  mState    :: repr (StateType repr) -> repr (MethodType repr)
  construct :: Label -> repr (MethodType repr)

class ParameterSym repr where
  type Parameter repr
  stateParam :: repr (Value repr) -> repr (Parameter repr)
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: repr (Value repr) -> repr (Parameter repr)

class (ScopeSym repr, MethodTypeSym repr, ParameterSym repr, StateVarSym repr,
  BodySym repr) => MethodSym repr where
  type Method repr
  -- Second label is class name
  method      :: Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> repr (MethodType repr) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  getMethod   :: Label -> Label -> repr (MethodType repr) -> repr (Method repr)
  setMethod   :: Label -> Label -> Label -> repr (StateType repr) -> 
    repr (Method repr) 
  mainMethod  :: Label -> repr (Body repr) -> repr (Method repr)
  privMethod  :: Label -> Label -> repr (MethodType repr) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  pubMethod   :: Label -> Label -> repr (MethodType repr) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  constructor :: Label -> [repr (Parameter repr)] -> repr (Body repr) -> 
    repr (Method repr)
  destructor :: Label -> [repr (StateVar repr)] -> repr (Method repr)

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Body repr) -> 
    repr (Method repr) 

  -- The two lists are inputs and outputs, respectively
  inOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [repr (Value repr)] -> [repr (Value repr)] -> 
    repr (Body repr) -> repr (Method repr)

class (ScopeSym repr, PermanenceSym repr, StateTypeSym repr) => 
  StateVarSym repr where
  type StateVar repr
  stateVar :: Int -> Label -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (StateType repr) -> repr (StateVar repr)
  privMVar :: Int -> Label -> repr (StateType repr) -> repr (StateVar repr)
  pubMVar  :: Int -> Label -> repr (StateType repr) -> repr (StateVar repr)
  pubGVar  :: Int -> Label -> repr (StateType repr) -> repr (StateVar repr)
  listStateVar :: Int -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> repr (StateType repr) -> repr (StateVar repr)

class (StateVarSym repr, MethodSym repr) => ClassSym repr where
  type Class repr
  buildClass :: Label -> Maybe Label -> repr (Scope repr) -> 
    [repr (StateVar repr)] -> [repr (Method repr)] -> repr (Class repr)
  enum :: Label -> [Label] -> repr (Scope repr) -> repr (Class repr)
  mainClass :: Label -> [repr (StateVar repr)] -> [repr (Method repr)] -> 
    repr (Class repr)
  privClass :: Label -> Maybe Label -> [repr (StateVar repr)] -> 
    [repr (Method repr)] -> repr (Class repr)
  pubClass :: Label -> Maybe Label -> [repr (StateVar repr)] -> 
    [repr (Method repr)] -> repr (Class repr)

class (ClassSym repr) => ModuleSym repr where
  type Module repr
  buildModule :: Label -> [Library] -> [repr (Statement repr)] -> 
    [repr (Method repr)] -> [repr (Class repr)] -> repr (Module repr)
