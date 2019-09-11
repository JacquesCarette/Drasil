{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.Symantics (
  -- Types
  Label, Library,
  -- Typeclasses
  ProgramSym(..), RenderSym(..), InternalFile(..),  KeywordSym(..), 
  PermanenceSym(..), BodySym(..), ControlBlockSym(..), BlockSym(..), 
  StateTypeSym(..), UnaryOpSym(..), BinaryOpSym(..), VariableSym(..), 
  ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), InternalValue(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
  ClassSym(..), ModuleSym(..), BlockCommentSym(..)
) where

import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.Data (Boolean, Val, Binding)
import Text.PrettyPrint.HughesPJ (Doc)

type Label = String
type Library = String

class (RenderSym repr) => ProgramSym repr where
  type Program repr
  prog :: Label -> [repr (RenderFile repr)] -> repr (Program repr)

class (ModuleSym repr, ControlBlockSym repr, InternalFile repr) => 
  RenderSym repr where 
  type RenderFile repr
  fileDoc :: repr (Module repr) -> repr (RenderFile repr)

  -- Module description, list of author names, date as a String, file to comment
  docMod :: String -> [String] -> String -> repr (RenderFile repr) -> 
    repr (RenderFile repr)

  commentedMod :: repr (BlockComment repr) -> repr (RenderFile repr) ->
    repr (RenderFile repr)

class (ModuleSym repr) => InternalFile repr where
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

  commentStart      :: repr (Keyword repr)
  blockCommentStart :: repr (Keyword repr)
  blockCommentEnd   :: repr (Keyword repr)
  docCommentStart   :: repr (Keyword repr)
  docCommentEnd     :: repr (Keyword repr)

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

  getType :: repr (StateType repr) -> CodeType
  getTypeString :: repr (StateType repr) -> String
  getTypeDoc :: repr (StateType repr) -> Doc

class (BodySym repr, ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, repr (Body repr))] -> 
    Maybe (repr (Value repr Val)) -> Maybe (repr (Variable repr)) -> 
    repr (Block repr)

  listSlice        :: repr (Variable repr) -> repr (Value repr Val) -> 
    Maybe (repr (Value repr Val)) -> Maybe (repr (Value repr Val)) ->
    Maybe (repr (Value repr Val)) -> repr (Block repr)

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

class (StateTypeSym repr) => VariableSym repr where
  type Variable repr
  var          :: Label -> repr (StateType repr) -> repr (Variable repr)
  staticVar    :: Label -> repr (StateType repr) -> repr (Variable repr)
  const        :: Label -> repr (StateType repr) -> repr (Variable repr)
  extVar       :: Library -> Label -> repr (StateType repr) -> 
    repr (Variable repr)
  self         :: Label -> repr (Variable repr)
  classVar     :: repr (StateType repr) -> repr (Variable repr) -> repr (Variable repr)
  objVar       :: repr (Variable repr) -> repr (Variable repr) -> repr (Variable repr)
  objVarSelf   :: Label -> Label -> repr (StateType repr) -> 
    repr (Variable repr)
  enumVar      :: Label -> Label -> repr (Variable repr)
  listVar      :: Label -> repr (Permanence repr) -> repr (StateType repr) -> 
    repr (Variable repr)
  listOf       :: Label -> repr (StateType repr) -> repr (Variable repr)
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> repr (StateType repr) -> repr (Variable repr)

  ($->) :: repr (Variable repr) -> repr (Variable repr) -> repr (Variable repr)
  infixl 9 $->

  variableBind :: repr (Variable repr) -> Binding
  variableName :: repr (Variable repr) -> String
  variableType :: repr (Variable repr) -> repr (StateType repr)
  variableDoc  :: repr (Variable repr) -> Doc

  varFromData :: Binding -> String -> repr (StateType repr) -> Doc -> 
    repr (Variable repr)

class (VariableSym repr) => ValueSym repr where
  type Value repr :: * -> *
  litTrue   :: repr (Value repr Boolean)
  litFalse  :: repr (Value repr Boolean)
  litChar   :: Char -> repr (Value repr Val)
  litFloat  :: Double -> repr (Value repr Val)
  litInt    :: Integer -> repr (Value repr Val)
  litString :: String -> repr (Value repr Val)

  --other operators ($)
  ($:)  :: Label -> Label -> repr (Value repr Val)
  infixl 9 $:

  valueOf       :: repr (Variable repr) -> repr (Value repr Val)
--  global       :: Label -> repr (Value repr Val)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> repr (Value repr Val)
  enumElement  :: Label -> Label -> repr (Value repr Val)

  argsList  :: repr (Value repr Val)

  valueType :: repr (Value repr Val) -> repr (StateType repr)
  valueDoc :: repr (Value repr Val) -> Doc

class (ValueSym repr, UnaryOpSym repr, BinaryOpSym repr) => 
  NumericExpression repr where
  (#~)  :: repr (Value repr Val) -> repr (Value repr Val)
  infixl 8 #~
  (#/^) :: repr (Value repr Val) -> repr (Value repr Val)
  infixl 7 #/^
  (#|)  :: repr (Value repr Val) -> repr (Value repr Val)
  infixl 7 #|
  (#+)  :: repr (Value repr Val) -> repr (Value repr Val) -> repr (Value repr Val)
  infixl 5 #+
  (#-)  :: repr (Value repr Val) -> repr (Value repr Val) -> repr (Value repr Val)
  infixl 5 #-
  (#*)  :: repr (Value repr Val) -> repr (Value repr Val) -> repr (Value repr Val)
  infixl 6 #*
  (#/)  :: repr (Value repr Val) -> repr (Value repr Val) -> repr (Value repr Val)
  infixl 6 #/
  (#%)  :: repr (Value repr Val) -> repr (Value repr Val) -> repr (Value repr Val)
  infixl 6 #%
  (#^)  :: repr (Value repr Val) -> repr (Value repr Val) -> repr (Value repr Val)
  infixl 7 #^

  log    :: repr (Value repr Val) -> repr (Value repr Val)
  ln     :: repr (Value repr Val) -> repr (Value repr Val)
  exp    :: repr (Value repr Val) -> repr (Value repr Val)
  sin    :: repr (Value repr Val) -> repr (Value repr Val)
  cos    :: repr (Value repr Val) -> repr (Value repr Val)
  tan    :: repr (Value repr Val) -> repr (Value repr Val)
  csc    :: repr (Value repr Val) -> repr (Value repr Val)
  sec    :: repr (Value repr Val) -> repr (Value repr Val)
  cot    :: repr (Value repr Val) -> repr (Value repr Val)
  arcsin :: repr (Value repr Val) -> repr (Value repr Val)
  arccos :: repr (Value repr Val) -> repr (Value repr Val)
  arctan :: repr (Value repr Val) -> repr (Value repr Val)
  floor  :: repr (Value repr Val) -> repr (Value repr Val)
  ceil   :: repr (Value repr Val) -> repr (Value repr Val)

-- I considered having two separate classes, BooleanExpressions and BooleanComparisons,
-- but this would require cyclic constraints, since it is feasible to have
-- BooleanComparisons of BooleanExpressions and also BooleanExpressions of BooleanComparisons.
-- This has the drawback of requiring a NumericExpression constraint for the first
-- 3 functions here, even though they don't really need it.
class (ValueSym repr, NumericExpression repr) => 
  BooleanExpression repr where
  (?!)  :: repr (Value repr Boolean) -> repr (Value repr Boolean)
  infixr 6 ?!
  (?&&) :: repr (Value repr Boolean) -> repr (Value repr Boolean) -> 
    repr (Value repr Boolean)
  infixl 2 ?&&
  (?||) :: repr (Value repr Boolean) -> repr (Value repr Boolean) -> 
    repr (Value repr Boolean)
  infixl 1 ?||

  (?<)  :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Boolean)
  infixl 4 ?<
  (?<=) :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Boolean)
  infixl 4 ?<=
  (?>)  :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Boolean)
  infixl 4 ?>
  (?>=) :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Boolean)
  infixl 4 ?>=
  (?==) :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Boolean)
  infixl 3 ?==
  (?!=) :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Boolean)
  infixl 3 ?!=

class (ValueSym repr, NumericExpression repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: repr (Value repr Boolean) -> repr (Value repr Val) -> 
    repr (Value repr Val) -> repr (Value repr Val)
  funcApp      :: Label -> repr (StateType repr) -> [repr (Value repr Val)] -> 
    repr (Value repr Val)
  selfFuncApp  :: Label -> repr (StateType repr) -> [repr (Value repr Val)] -> 
    repr (Value repr Val)
  extFuncApp   :: Library -> Label -> repr (StateType repr) -> 
    [repr (Value repr Val)] -> repr (Value repr Val)
  stateObj     :: repr (StateType repr) -> [repr (Value repr Val)] -> 
    repr (Value repr Val)
  extStateObj  :: Library -> repr (StateType repr) -> [repr (Value repr Val)] 
    -> repr (Value repr Val)
  listStateObj :: repr (StateType repr) -> [repr (Value repr Val)] -> 
    repr (Value repr Val)

  exists  :: repr (Value repr Val) -> repr (Value repr Boolean)
  notNull :: repr (Value repr Val) -> repr (Value repr Boolean)

class (ValueExpression repr) => InternalValue repr where
  inputFunc       :: repr (Value repr Val)
  printFunc       :: repr (Value repr Val)
  printLnFunc     :: repr (Value repr Val)
  printFileFunc   :: repr (Value repr Val) -> repr (Value repr Val)
  printFileLnFunc :: repr (Value repr Val) -> repr (Value repr Val)

  cast :: repr (StateType repr) -> repr (Value repr Val) -> 
    repr (Value repr Val)

-- The cyclic constraints issue arises here too. I've constrained this by ValueExpression,
-- but really one might want one of these values as part of an expression, so the
-- constraint would have to go both ways. I'm not sure what the solution is for
-- these sorts of problems, other than removing the constraints altogether, but 
-- then what is the purpose of splitting the typeclasses into smaller typeclasses?
-- I'm leaving it as is for now, even though I suspect this will change in the future.
class (FunctionSym repr, ValueSym repr, ValueExpression repr) => 
  Selector repr where
  objAccess :: repr (Value repr Val) -> repr (Function repr) -> 
    repr (Value repr Val)
  ($.)      :: repr (Value repr Val) -> repr (Function repr) -> 
    repr (Value repr Val)
  infixl 9 $.

  objMethodCall     :: repr (StateType repr) -> repr (Value repr Val) -> Label 
    -> [repr (Value repr Val)] -> repr (Value repr Val)
  objMethodCallNoParams :: repr (StateType repr) -> repr (Value repr Val) -> 
    Label -> repr (Value repr Val)

  selfAccess :: Label -> repr (Function repr) -> repr (Value repr Val)

  listIndexExists :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val)
  argExists       :: Integer -> repr (Value repr Val)

  indexOf :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val)

class (ValueSym repr, ValueExpression repr) => FunctionSym repr where
  type Function repr
  func           :: Label -> repr (StateType repr) -> [repr (Value repr Val)] 
    -> repr (Function repr)

  get :: repr (Value repr Val) -> repr (Variable repr) -> repr (Value repr Val)
  set :: repr (Value repr Val) -> repr (Variable repr) -> repr (Value repr Val) 
    -> repr (Value repr Val)

  listSize   :: repr (Value repr Val) -> repr (Value repr Val)
  listAdd    :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val) -> repr (Value repr Val)
  listAppend :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val)

  iterBegin :: repr (Value repr Val) -> repr (Value repr Val)
  iterEnd   :: repr (Value repr Val) -> repr (Value repr Val)

class (ValueSym repr, InternalValue repr, FunctionSym repr, Selector repr) => 
  SelectorFunction repr where
  listAccess :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val)
  listSet    :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val) -> repr (Value repr Val)
  at         :: repr (Value repr Val) -> Label -> repr (Value repr Val)

class (ValueSym repr, InternalValue repr) => InternalFunction repr where
  getFunc        :: repr (Variable repr) -> repr (Function repr)
  setFunc        :: repr (StateType repr) -> repr (Variable repr) -> 
    repr (Value repr Val) -> repr (Function repr)

  listSizeFunc       :: repr (Function repr)
  listAddFunc        :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val) -> repr (Function repr)
  listAppendFunc         :: repr (Value repr Val) -> repr (Function repr)

  iterBeginFunc :: repr (StateType repr) -> repr (Function repr)
  iterEndFunc   :: repr (StateType repr) -> repr (Function repr)

  listAccessFunc :: repr (StateType repr) -> repr (Value repr Val) -> 
    repr (Function repr)
  listSetFunc    :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val) -> repr (Function repr)

  atFunc :: repr (StateType repr) -> Label -> repr (Function repr)

class (Selector repr) => InternalStatement repr where
  -- newLn, printFunc, value to print, maybe a file to print to 
  printSt :: Bool -> repr (Value repr Val) -> repr (Value repr Val) -> 
    Maybe (repr (Value repr Val)) -> repr (Statement repr)

  state     :: repr (Statement repr) -> repr (Statement repr)
  loopState :: repr (Statement repr) -> repr (Statement repr)

class (ValueSym repr, Selector repr, SelectorFunction repr, FunctionSym repr,
  InternalFunction repr, InternalStatement repr) => StatementSym repr where
  type Statement repr
  (&=)   :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  infixr 1 &=
  (&-=)  :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  infixl 1 &-=
  (&+=)  :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  infixl 1 &+=
  (&++)  :: repr (Variable repr) -> repr (Statement repr)
  infixl 8 &++
  (&~-)  :: repr (Variable repr) -> repr (Statement repr)
  infixl 8 &~-

  assign            :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  assignToListIndex :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Value repr Val) -> repr (Statement repr)
  multiAssign       :: [repr (Variable repr)] -> [repr (Value repr Val)] ->
    repr (Statement repr) 

  varDec           :: repr (Variable repr) -> repr (Statement repr)
  varDecDef        :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  listDec          :: Integer -> repr (Variable repr) -> repr (Statement repr)
  listDecDef       :: repr (Variable repr) -> [repr (Value repr Val)] -> 
    repr (Statement repr)
  objDecDef        :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  objDecNew        :: repr (Variable repr) -> [repr (Value repr Val)] -> 
    repr (Statement repr)
  extObjDecNew     :: Library -> repr (Variable repr) -> 
    [repr (Value repr Val)] -> repr (Statement repr)
  objDecNewVoid    :: repr (Variable repr) -> repr (Statement repr)
  extObjDecNewVoid :: Library -> repr (Variable repr) -> repr (Statement repr)
  constDecDef      :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)

  print      :: repr (Value repr Val) -> repr (Statement repr)
  printLn    :: repr (Value repr Val) -> repr (Statement repr)
  printStr   :: String -> repr (Statement repr)
  printStrLn :: String -> repr (Statement repr)

  printFile      :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Statement repr)
  printFileLn    :: repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Statement repr)
  printFileStr   :: repr (Value repr Val) -> String -> repr (Statement repr)
  printFileStrLn :: repr (Value repr Val) -> String -> repr (Statement repr)

  getInput         :: repr (Variable repr) -> repr (Statement repr)
  discardInput     :: repr (Statement repr)
  getFileInput     :: repr (Value repr Val) -> repr (Variable repr) -> 
    repr (Statement repr)
  discardFileInput :: repr (Value repr Val) -> repr (Statement repr)

  openFileR :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  openFileW :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  openFileA :: repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)
  closeFile :: repr (Value repr Val) -> repr (Statement repr)

  getFileInputLine :: repr (Value repr Val) -> repr (Variable repr) -> 
    repr (Statement repr)
  discardFileLine  :: repr (Value repr Val) -> repr (Statement repr)
  stringSplit      :: Char -> repr (Variable repr) -> repr (Value repr Val) -> 
    repr (Statement repr)

  stringListVals :: [repr (Variable repr)] -> repr (Value repr Val) -> 
    repr (Statement repr)
  stringListLists :: [repr (Variable repr)] -> repr (Value repr Val) ->
    repr (Statement repr)

  break :: repr (Statement repr)
  continue :: repr (Statement repr)

  returnState :: repr (Value repr Val) -> repr (Statement repr)
  multiReturn :: [repr (Value repr Val)] -> repr (Statement repr)

  valState :: repr (Value repr Val) -> repr (Statement repr)

  comment :: Label -> repr (Statement repr)

  free :: repr (Variable repr) -> repr (Statement repr)

  throw :: Label -> repr (Statement repr)

  initState   :: Label -> Label -> repr (Statement repr)
  changeState :: Label -> Label -> repr (Statement repr)

  initObserverList :: repr (StateType repr) -> [repr (Value repr Val)] -> 
    repr (Statement repr)
  addObserver      :: repr (Value repr Val) -> repr (Statement repr)

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [repr (Value repr Val)] -> [repr (Variable repr)] -> 
    [repr (Variable repr)] -> repr (Statement repr)
  extInOutCall :: Library -> Label -> [repr (Value repr Val)] ->
    [repr (Variable repr)] -> [repr (Variable repr)] -> repr (Statement repr)

  multi     :: [repr (Statement repr)] -> repr (Statement repr)

class (StatementSym repr, BodySym repr) => ControlStatementSym repr where
  ifCond     :: [(repr (Value repr Boolean), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  ifNoElse   :: [(repr (Value repr Boolean), repr (Body repr))] -> 
    repr (Statement repr)
  switch     :: repr (Value repr Val) -> [(repr (Value repr Val), 
    repr (Body repr))] -> repr (Body repr) -> repr (Statement repr) -- is there value in separating Literals into their own type?
  switchAsIf :: repr (Value repr Val) -> [(repr (Value repr Val), 
    repr (Body repr))] -> repr (Body repr) -> repr (Statement repr)

  ifExists :: repr (Value repr Val) -> repr (Body repr) -> repr (Body repr) ->
    repr (Statement repr)

  for      :: repr (Statement repr) -> repr (Value repr Boolean) -> 
    repr (Statement repr) -> repr (Body repr) -> repr (Statement repr)
  forRange :: Label -> repr (Value repr Val) -> repr (Value repr Val) -> 
    repr (Value repr Val) -> repr (Body repr) -> repr (Statement repr)
  forEach  :: Label -> repr (Value repr Val) -> repr (Body repr) -> 
    repr (Statement repr)
  while    :: repr (Value repr Boolean) -> repr (Body repr) -> 
    repr (Statement repr) 

  tryCatch :: repr (Body repr) -> repr (Body repr) -> repr (Statement repr)

  checkState      :: Label -> [(repr (Value repr Val), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  notifyObservers :: repr (Function repr) -> repr (StateType repr) -> 
    repr (Statement repr)

  getFileInputAll  :: repr (Value repr Val) -> repr (Variable repr) -> 
    repr (Statement repr)

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class (ScopeSym repr) => InternalScope repr where
  includeScope :: repr (Scope repr) -> repr (Scope repr)

class MethodTypeSym repr where
  type MethodType repr
  mState    :: repr (StateType repr) -> repr (MethodType repr)
  construct :: Label -> repr (MethodType repr)

class ParameterSym repr where
  type Parameter repr
  stateParam :: repr (Variable repr) -> repr (Parameter repr)
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: repr (Variable repr) -> repr (Parameter repr)

  parameterName :: repr (Parameter repr) -> String
  parameterType :: repr (Parameter repr) -> repr (StateType repr)

class (ScopeSym repr, MethodTypeSym repr, ParameterSym repr, StateVarSym repr,
  BodySym repr, BlockCommentSym repr) => MethodSym repr where
  type Method repr
  -- Second label is class name
  method      :: Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> repr (MethodType repr) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  getMethod   :: Label -> repr (Variable repr) -> repr (Method repr)
  setMethod   :: Label -> repr (Variable repr) -> repr (Method repr) 
  mainMethod  :: Label -> repr (Body repr) -> repr (Method repr)
  privMethod  :: Label -> Label -> repr (MethodType repr) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  pubMethod   :: Label -> Label -> repr (MethodType repr) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  constructor :: Label -> [repr (Parameter repr)] -> repr (Body repr) -> 
    repr (Method repr)
  destructor :: Label -> [repr (StateVar repr)] -> repr (Method repr)

  docMain :: Label -> repr (Body repr) -> repr (Method repr)

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Body repr) -> 
    repr (Method repr) 
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> repr (Method repr) -> repr (Method repr) 

  -- The three lists are inputs, outputs, and both, respectively
  inOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [repr (Variable repr)] -> [repr (Variable repr)] -> [repr (Variable repr)] 
    -> repr (Body repr) -> repr (Method repr)
  -- Parameters are: function name, scope, permanence, brief description, input descriptions and variables, output descriptions and variables, descriptions and variables for parameters that are both input and output, function body
  docInOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, repr (Variable repr))] -> [(String, repr 
    (Variable repr))] -> [(String, repr (Variable repr))] -> repr (Body repr)
    -> repr (Method repr)

  commentedFunc :: repr (BlockComment repr) -> repr (Method repr) -> 
    repr (Method repr)

  parameters :: repr (Method repr) -> [repr (Parameter repr)]

class (ScopeSym repr, InternalScope repr, PermanenceSym repr, StateTypeSym repr,
  StatementSym repr) => StateVarSym repr where
  type StateVar repr
  stateVar :: Int -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr) -> repr (StateVar repr)
  stateVarDef :: Int -> Label -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr) -> repr (Value repr Val) -> repr (StateVar repr)
  constVar :: Int -> Label -> repr (Scope repr) ->  repr (Variable repr) -> 
    repr (Value repr Val) -> repr (StateVar repr)
  privMVar :: Int -> repr (Variable repr) -> repr (StateVar repr)
  pubMVar  :: Int -> repr (Variable repr) -> repr (StateVar repr)
  pubGVar  :: Int -> repr (Variable repr) -> repr (StateVar repr)

class (StateVarSym repr, MethodSym repr) => ClassSym repr where
  type Class repr
  buildClass :: Label -> Maybe Label -> repr (Scope repr) -> 
    [repr (StateVar repr)] -> [repr (Method repr)] -> repr (Class repr)
  enum :: Label -> [Label] -> repr (Scope repr) -> repr (Class repr)
  privClass :: Label -> Maybe Label -> [repr (StateVar repr)] -> 
    [repr (Method repr)] -> repr (Class repr)
  pubClass :: Label -> Maybe Label -> [repr (StateVar repr)] -> 
    [repr (Method repr)] -> repr (Class repr)

  docClass :: String -> repr (Class repr) -> repr (Class repr)

  commentedClass :: repr (BlockComment repr) -> repr (Class repr) -> 
    repr (Class repr)

class (ClassSym repr) => ModuleSym repr where
  type Module repr
  buildModule :: Label -> [Library] -> [repr (Method repr)] -> 
    [repr (Class repr)] -> repr (Module repr)
    
  moduleName :: repr (Module repr) -> String
    
class BlockCommentSym repr where
  type BlockComment repr
  blockComment :: [String] -> repr (BlockComment repr)
  docComment :: [String] -> repr (BlockComment repr)
