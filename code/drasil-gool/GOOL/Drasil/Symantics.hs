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
import GOOL.Drasil.Data (Boolean, Other, Binding)
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
  type StateType repr :: * -> *
  bool          :: repr (StateType repr Boolean)
  int           :: repr (StateType repr Other)
  float         :: repr (StateType repr Other)
  char          :: repr (StateType repr Other)
  string        :: repr (StateType repr Other)
  infile        :: repr (StateType repr Other)
  outfile       :: repr (StateType repr Other)
  listType      :: repr (Permanence repr) -> repr (StateType repr Other) -> repr (StateType repr Other)
  listInnerType :: repr (StateType repr Other) -> repr (StateType repr Other)
  obj           :: Label -> repr (StateType repr Other)
  enumType      :: Label -> repr (StateType repr Other)
  iterator      :: repr (StateType repr Other) -> repr (StateType repr Other)
  void          :: repr (StateType repr Other)

  getType :: repr (StateType repr Other) -> CodeType
  getTypeString :: repr (StateType repr Other) -> String
  getTypeDoc :: repr (StateType repr Other) -> Doc

class (BodySym repr, ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, repr (Body repr))] -> 
    Maybe (repr (Value repr Other)) -> Maybe (repr (Variable repr Other)) -> 
    repr (Block repr)

  listSlice        :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    Maybe (repr (Value repr Other)) -> Maybe (repr (Value repr Other)) ->
    Maybe (repr (Value repr Other)) -> repr (Block repr)

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
  type Variable repr :: * -> *
  var          :: Label -> repr (StateType repr Other) -> repr (Variable repr Other)
  staticVar    :: Label -> repr (StateType repr Other) -> repr (Variable repr Other)
  const        :: Label -> repr (StateType repr Other) -> repr (Variable repr Other)
  extVar       :: Library -> Label -> repr (StateType repr Other) -> 
    repr (Variable repr Other)
  self         :: Label -> repr (Variable repr Other)
  classVar     :: repr (StateType repr Other) -> repr (Variable repr Other) -> repr (Variable repr Other)
  objVar       :: repr (Variable repr Other) -> repr (Variable repr Other) -> repr (Variable repr Other)
  objVarSelf   :: Label -> Label -> repr (StateType repr Other) -> 
    repr (Variable repr Other)
  enumVar      :: Label -> Label -> repr (Variable repr Other)
  listVar      :: Label -> repr (Permanence repr) -> repr (StateType repr Other) -> 
    repr (Variable repr Other)
  listOf       :: Label -> repr (StateType repr Other) -> repr (Variable repr Other)
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> repr (StateType repr Other) -> repr (Variable repr Other)

  ($->) :: repr (Variable repr Other) -> repr (Variable repr Other) -> repr (Variable repr Other)
  infixl 9 $->

  variableBind :: repr (Variable repr Other) -> Binding
  variableName :: repr (Variable repr Other) -> String
  variableType :: repr (Variable repr Other) -> repr (StateType repr Other)
  variableDoc  :: repr (Variable repr Other) -> Doc

  varFromData :: Binding -> String -> repr (StateType repr Other) -> Doc -> 
    repr (Variable repr Other)

class (VariableSym repr) => ValueSym repr where
  type Value repr :: * -> *
  litTrue   :: repr (Value repr Boolean)
  litFalse  :: repr (Value repr Boolean)
  litChar   :: Char -> repr (Value repr Other)
  litFloat  :: Double -> repr (Value repr Other)
  litInt    :: Integer -> repr (Value repr Other)
  litString :: String -> repr (Value repr Other)

  --other operators ($)
  ($:)  :: Label -> Label -> repr (Value repr Other)
  infixl 9 $:

  valueOf       :: repr (Variable repr Other) -> repr (Value repr Other)
--  global       :: Label -> repr (Value repr Other)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> repr (Value repr Other)
  enumElement  :: Label -> Label -> repr (Value repr Other)

  argsList  :: repr (Value repr Other)

  valueType :: repr (Value repr Other) -> repr (StateType repr Other)
  valueDoc :: repr (Value repr Other) -> Doc

class (ValueSym repr, UnaryOpSym repr, BinaryOpSym repr) => 
  NumericExpression repr where
  (#~)  :: repr (Value repr Other) -> repr (Value repr Other)
  infixl 8 #~
  (#/^) :: repr (Value repr Other) -> repr (Value repr Other)
  infixl 7 #/^
  (#|)  :: repr (Value repr Other) -> repr (Value repr Other)
  infixl 7 #|
  (#+)  :: repr (Value repr Other) -> repr (Value repr Other) -> repr (Value repr Other)
  infixl 5 #+
  (#-)  :: repr (Value repr Other) -> repr (Value repr Other) -> repr (Value repr Other)
  infixl 5 #-
  (#*)  :: repr (Value repr Other) -> repr (Value repr Other) -> repr (Value repr Other)
  infixl 6 #*
  (#/)  :: repr (Value repr Other) -> repr (Value repr Other) -> repr (Value repr Other)
  infixl 6 #/
  (#%)  :: repr (Value repr Other) -> repr (Value repr Other) -> repr (Value repr Other)
  infixl 6 #%
  (#^)  :: repr (Value repr Other) -> repr (Value repr Other) -> repr (Value repr Other)
  infixl 7 #^

  log    :: repr (Value repr Other) -> repr (Value repr Other)
  ln     :: repr (Value repr Other) -> repr (Value repr Other)
  exp    :: repr (Value repr Other) -> repr (Value repr Other)
  sin    :: repr (Value repr Other) -> repr (Value repr Other)
  cos    :: repr (Value repr Other) -> repr (Value repr Other)
  tan    :: repr (Value repr Other) -> repr (Value repr Other)
  csc    :: repr (Value repr Other) -> repr (Value repr Other)
  sec    :: repr (Value repr Other) -> repr (Value repr Other)
  cot    :: repr (Value repr Other) -> repr (Value repr Other)
  arcsin :: repr (Value repr Other) -> repr (Value repr Other)
  arccos :: repr (Value repr Other) -> repr (Value repr Other)
  arctan :: repr (Value repr Other) -> repr (Value repr Other)
  floor  :: repr (Value repr Other) -> repr (Value repr Other)
  ceil   :: repr (Value repr Other) -> repr (Value repr Other)

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

  (?<)  :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Boolean)
  infixl 4 ?<
  (?<=) :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Boolean)
  infixl 4 ?<=
  (?>)  :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Boolean)
  infixl 4 ?>
  (?>=) :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Boolean)
  infixl 4 ?>=
  (?==) :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Boolean)
  infixl 3 ?==
  (?!=) :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Boolean)
  infixl 3 ?!=

class (ValueSym repr, NumericExpression repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: repr (Value repr Boolean) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Value repr Other)
  funcApp      :: Label -> repr (StateType repr a) -> [repr (Value repr Other)] 
    -> repr (Value repr a)
  selfFuncApp  :: Label -> repr (StateType repr Other) -> [repr (Value repr Other)] -> 
    repr (Value repr Other)
  extFuncApp   :: Library -> Label -> repr (StateType repr Other) -> 
    [repr (Value repr Other)] -> repr (Value repr Other)
  stateObj     :: repr (StateType repr Other) -> [repr (Value repr Other)] -> 
    repr (Value repr Other)
  extStateObj  :: Library -> repr (StateType repr Other) -> [repr (Value repr Other)] 
    -> repr (Value repr Other)
  listStateObj :: repr (StateType repr Other) -> [repr (Value repr Other)] -> 
    repr (Value repr Other)

  exists  :: repr (Value repr Other) -> repr (Value repr Boolean)
  notNull :: repr (Value repr Other) -> repr (Value repr Boolean)

class (ValueExpression repr) => InternalValue repr where
  inputFunc       :: repr (Value repr Other)
  printFunc       :: repr (Value repr Other)
  printLnFunc     :: repr (Value repr Other)
  printFileFunc   :: repr (Value repr Other) -> repr (Value repr Other)
  printFileLnFunc :: repr (Value repr Other) -> repr (Value repr Other)

  cast :: repr (StateType repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other)

-- The cyclic constraints issue arises here too. I've constrained this by ValueExpression,
-- but really one might want one of these values as part of an expression, so the
-- constraint would have to go both ways. I'm not sure what the solution is for
-- these sorts of problems, other than removing the constraints altogether, but 
-- then what is the purpose of splitting the typeclasses into smaller typeclasses?
-- I'm leaving it as is for now, even though I suspect this will change in the future.
class (FunctionSym repr, ValueSym repr, ValueExpression repr) => 
  Selector repr where
  objAccess :: repr (Value repr b) -> repr (Function repr a) -> 
    repr (Value repr a)
  ($.)      :: repr (Value repr b) -> repr (Function repr a) -> 
    repr (Value repr a)
  infixl 9 $.

  objMethodCall     :: repr (StateType repr Other) -> repr (Value repr Other) -> Label 
    -> [repr (Value repr Other)] -> repr (Value repr Other)
  objMethodCallNoParams :: repr (StateType repr Other) -> repr (Value repr Other) -> 
    Label -> repr (Value repr Other)

  selfAccess :: Label -> repr (Function repr Other) -> repr (Value repr Other)

  listIndexExists :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Boolean)
  argExists       :: Integer -> repr (Value repr Other)

  indexOf :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other)

class (ValueSym repr, ValueExpression repr) => FunctionSym repr where
  type Function repr :: * -> *
  func           :: Label -> repr (StateType repr a) -> 
    [repr (Value repr Other)] -> repr (Function repr a)

  get :: repr (Value repr Other) -> repr (Variable repr Other) -> repr (Value repr Other)
  set :: repr (Value repr Other) -> repr (Variable repr Other) -> 
    repr (Value repr Other) -> repr (Value repr Other)

  listSize   :: repr (Value repr Other) -> repr (Value repr Other)
  listAdd    :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Value repr Other)
  listAppend :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other)

  iterBegin :: repr (Value repr Other) -> repr (Value repr Other)
  iterEnd   :: repr (Value repr Other) -> repr (Value repr Other)

class (ValueSym repr, InternalValue repr, FunctionSym repr, Selector repr) => 
  SelectorFunction repr where
  listAccess :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other)
  listSet    :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Value repr Other)
  at         :: repr (Value repr Other) -> Label -> repr (Value repr Other)

class (ValueSym repr, InternalValue repr) => InternalFunction repr where
  getFunc        :: repr (Variable repr Other) -> repr (Function repr Other)
  setFunc        :: repr (StateType repr Other) -> repr (Variable repr Other) -> 
    repr (Value repr Other) -> repr (Function repr Other)

  listSizeFunc       :: repr (Function repr Other)
  listAddFunc        :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Function repr Other)
  listAppendFunc         :: repr (Value repr Other) -> repr (Function repr Other)

  iterBeginFunc :: repr (StateType repr Other) -> repr (Function repr Other)
  iterEndFunc   :: repr (StateType repr Other) -> repr (Function repr Other)

  listAccessFunc :: repr (StateType repr Other) -> repr (Value repr Other) -> 
    repr (Function repr Other)
  listSetFunc    :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Function repr Other)

  atFunc :: repr (StateType repr Other) -> Label -> repr (Function repr Other)

class (Selector repr) => InternalStatement repr where
  -- newLn, printFunc, value to print, maybe a file to print to 
  printSt :: Bool -> repr (Value repr Other) -> repr (Value repr Other) -> 
    Maybe (repr (Value repr Other)) -> repr (Statement repr)

  state     :: repr (Statement repr) -> repr (Statement repr)
  loopState :: repr (Statement repr) -> repr (Statement repr)

class (ValueSym repr, Selector repr, SelectorFunction repr, FunctionSym repr,
  InternalFunction repr, InternalStatement repr) => StatementSym repr where
  type Statement repr
  (&=)   :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  infixr 1 &=
  (&-=)  :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  infixl 1 &-=
  (&+=)  :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  infixl 1 &+=
  (&++)  :: repr (Variable repr Other) -> repr (Statement repr)
  infixl 8 &++
  (&~-)  :: repr (Variable repr Other) -> repr (Statement repr)
  infixl 8 &~-

  assign            :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  assignToListIndex :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Statement repr)
  multiAssign       :: [repr (Variable repr Other)] -> [repr (Value repr Other)] ->
    repr (Statement repr) 

  varDec           :: repr (Variable repr Other) -> repr (Statement repr)
  varDecDef        :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  listDec          :: Integer -> repr (Variable repr Other) -> repr (Statement repr)
  listDecDef       :: repr (Variable repr Other) -> [repr (Value repr Other)] -> 
    repr (Statement repr)
  objDecDef        :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  objDecNew        :: repr (Variable repr Other) -> [repr (Value repr Other)] -> 
    repr (Statement repr)
  extObjDecNew     :: Library -> repr (Variable repr Other) -> 
    [repr (Value repr Other)] -> repr (Statement repr)
  objDecNewVoid    :: repr (Variable repr Other) -> repr (Statement repr)
  extObjDecNewVoid :: Library -> repr (Variable repr Other) -> repr (Statement repr)
  constDecDef      :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)

  print      :: repr (Value repr Other) -> repr (Statement repr)
  printLn    :: repr (Value repr Other) -> repr (Statement repr)
  printStr   :: String -> repr (Statement repr)
  printStrLn :: String -> repr (Statement repr)

  printFile      :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  printFileLn    :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  printFileStr   :: repr (Value repr Other) -> String -> repr (Statement repr)
  printFileStrLn :: repr (Value repr Other) -> String -> repr (Statement repr)

  getInput         :: repr (Variable repr Other) -> repr (Statement repr)
  discardInput     :: repr (Statement repr)
  getFileInput     :: repr (Value repr Other) -> repr (Variable repr Other) -> 
    repr (Statement repr)
  discardFileInput :: repr (Value repr Other) -> repr (Statement repr)

  openFileR :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  openFileW :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  openFileA :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  closeFile :: repr (Value repr Other) -> repr (Statement repr)

  getFileInputLine :: repr (Value repr Other) -> repr (Variable repr Other) -> 
    repr (Statement repr)
  discardFileLine  :: repr (Value repr Other) -> repr (Statement repr)
  stringSplit      :: Char -> repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)

  stringListVals :: [repr (Variable repr Other)] -> repr (Value repr Other) -> 
    repr (Statement repr)
  stringListLists :: [repr (Variable repr Other)] -> repr (Value repr Other) ->
    repr (Statement repr)

  break :: repr (Statement repr)
  continue :: repr (Statement repr)

  returnState :: repr (Value repr Other) -> repr (Statement repr)
  multiReturn :: [repr (Value repr Other)] -> repr (Statement repr)

  valState :: repr (Value repr Other) -> repr (Statement repr)

  comment :: Label -> repr (Statement repr)

  free :: repr (Variable repr Other) -> repr (Statement repr)

  throw :: Label -> repr (Statement repr)

  initState   :: Label -> Label -> repr (Statement repr)
  changeState :: Label -> Label -> repr (Statement repr)

  initObserverList :: repr (StateType repr Other) -> [repr (Value repr Other)] -> 
    repr (Statement repr)
  addObserver      :: repr (Value repr Other) -> repr (Statement repr)

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [repr (Value repr Other)] -> [repr (Variable repr Other)] -> 
    [repr (Variable repr Other)] -> repr (Statement repr)
  extInOutCall :: Library -> Label -> [repr (Value repr Other)] ->
    [repr (Variable repr Other)] -> [repr (Variable repr Other)] -> repr (Statement repr)

  multi     :: [repr (Statement repr)] -> repr (Statement repr)

class (StatementSym repr, BodySym repr) => ControlStatementSym repr where
  ifCond     :: [(repr (Value repr Boolean), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  ifNoElse   :: [(repr (Value repr Boolean), repr (Body repr))] -> 
    repr (Statement repr)
  switch     :: repr (Value repr Other) -> [(repr (Value repr Other), 
    repr (Body repr))] -> repr (Body repr) -> repr (Statement repr) -- is there value in separating Literals into their own type?
  switchAsIf :: repr (Value repr Other) -> [(repr (Value repr Other), 
    repr (Body repr))] -> repr (Body repr) -> repr (Statement repr)

  ifExists :: repr (Value repr Other) -> repr (Body repr) -> repr (Body repr) ->
    repr (Statement repr)

  for      :: repr (Statement repr) -> repr (Value repr Boolean) -> 
    repr (Statement repr) -> repr (Body repr) -> repr (Statement repr)
  forRange :: Label -> repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Body repr) -> repr (Statement repr)
  forEach  :: Label -> repr (Value repr Other) -> repr (Body repr) -> 
    repr (Statement repr)
  while    :: repr (Value repr Boolean) -> repr (Body repr) -> 
    repr (Statement repr) 

  tryCatch :: repr (Body repr) -> repr (Body repr) -> repr (Statement repr)

  checkState      :: Label -> [(repr (Value repr Other), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  notifyObservers :: repr (Function repr Other) -> repr (StateType repr Other) -> 
    repr (Statement repr)

  getFileInputAll  :: repr (Value repr Other) -> repr (Variable repr Other) -> 
    repr (Statement repr)

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class (ScopeSym repr) => InternalScope repr where
  includeScope :: repr (Scope repr) -> repr (Scope repr)

class MethodTypeSym repr where
  type MethodType repr
  mState    :: repr (StateType repr Other) -> repr (MethodType repr)
  construct :: Label -> repr (MethodType repr)

class ParameterSym repr where
  type Parameter repr
  stateParam :: repr (Variable repr Other) -> repr (Parameter repr)
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: repr (Variable repr Other) -> repr (Parameter repr)

  parameterName :: repr (Parameter repr) -> String
  parameterType :: repr (Parameter repr) -> repr (StateType repr Other)

class (ScopeSym repr, MethodTypeSym repr, ParameterSym repr, StateVarSym repr,
  BodySym repr, BlockCommentSym repr) => MethodSym repr where
  type Method repr
  -- Second label is class name
  method      :: Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> repr (MethodType repr) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  getMethod   :: Label -> repr (Variable repr Other) -> repr (Method repr)
  setMethod   :: Label -> repr (Variable repr Other) -> repr (Method repr) 
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
    [repr (Variable repr Other)] -> [repr (Variable repr Other)] -> [repr (Variable repr Other)] 
    -> repr (Body repr) -> repr (Method repr)
  -- Parameters are: function name, scope, permanence, brief description, input descriptions and variables, output descriptions and variables, descriptions and variables for parameters that are both input and output, function body
  docInOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, repr (Variable repr Other))] -> [(String, repr 
    (Variable repr Other))] -> [(String, repr (Variable repr Other))] -> repr (Body repr)
    -> repr (Method repr)

  commentedFunc :: repr (BlockComment repr) -> repr (Method repr) -> 
    repr (Method repr)

  parameters :: repr (Method repr) -> [repr (Parameter repr)]

class (ScopeSym repr, InternalScope repr, PermanenceSym repr, StateTypeSym repr,
  StatementSym repr) => StateVarSym repr where
  type StateVar repr
  stateVar :: Int -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr Other) -> repr (StateVar repr)
  stateVarDef :: Int -> Label -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr Other) -> repr (Value repr Other) -> repr (StateVar repr)
  constVar :: Int -> Label -> repr (Scope repr) ->  repr (Variable repr Other) -> 
    repr (Value repr Other) -> repr (StateVar repr)
  privMVar :: Int -> repr (Variable repr Other) -> repr (StateVar repr)
  pubMVar  :: Int -> repr (Variable repr Other) -> repr (StateVar repr)
  pubGVar  :: Int -> repr (Variable repr Other) -> repr (StateVar repr)

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
