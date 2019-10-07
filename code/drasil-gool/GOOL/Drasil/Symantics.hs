{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.Symantics (
  -- Types
  Label, Library,
  -- Typeclasses
  ProgramSym(..), RenderSym(..), InternalFile(..),  KeywordSym(..), 
  PermanenceSym(..), BodySym(..), ControlBlockSym(..), BlockSym(..), 
  TypeSym(..), UnaryOpSym(..), BinaryOpSym(..), VariableSym(..), 
  InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), 
  InternalMethod(..), StateVarSym(..), ClassSym(..), ModuleSym(..), 
  BlockCommentSym(..)
) where

import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.Data (Boolean, Other, Binding, Terminator, TypedType)
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

  bodyDoc :: repr (Body repr) -> Doc

class (StatementSym repr) => BlockSym repr where
  type Block repr
  block   :: [repr (Statement repr)] -> repr (Block repr)

  docBlock :: Doc -> repr (Block repr)

class (PermanenceSym repr) => TypeSym repr where
  type Type repr :: * -> *
  bool          :: repr (Type repr Boolean)
  int           :: repr (Type repr Other)
  float         :: repr (Type repr Other)
  char          :: repr (Type repr Other)
  string        :: repr (Type repr Other)
  infile        :: repr (Type repr Other)
  outfile       :: repr (Type repr Other)
  listType      :: repr (Permanence repr) -> repr (Type repr a) -> 
    repr (Type repr Other)
  listInnerType :: repr (Type repr Other) -> repr (Type repr Other)
  obj           :: Label -> repr (Type repr Other)
  enumType      :: Label -> repr (Type repr Other)
  iterator      :: repr (Type repr Other) -> repr (Type repr Other)
  void          :: repr (Type repr Other)

  getTypedType :: repr (Type repr a) -> TypedType a
  getType :: repr (Type repr a) -> CodeType
  getTypeString :: repr (Type repr a) -> String
  getTypeDoc :: repr (Type repr a) -> Doc

class (BodySym repr, ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, repr (Body repr))] -> 
    Maybe (repr (Value repr a)) -> Maybe (repr (Variable repr a)) -> 
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

class (TypeSym repr, InternalVariable repr) => VariableSym repr where
  type Variable repr :: * -> *
  var          :: Label -> repr (Type repr a) -> repr (Variable repr a)
  staticVar    :: Label -> repr (Type repr a) -> repr (Variable repr a)
  const        :: Label -> repr (Type repr a) -> repr (Variable repr a)
  extVar       :: Library -> Label -> repr (Type repr a) -> 
    repr (Variable repr a)
  self         :: Label -> repr (Variable repr Other)
  classVar     :: repr (Type repr Other) -> repr (Variable repr a) -> 
    repr (Variable repr a)
  objVar       :: repr (Variable repr Other) -> repr (Variable repr a) -> 
    repr (Variable repr a)
  objVarSelf   :: Label -> Label -> repr (Type repr a) -> 
    repr (Variable repr a)
  enumVar      :: Label -> Label -> repr (Variable repr Other)
  listVar      :: Label -> repr (Permanence repr) -> repr (Type repr a)
    ->  repr (Variable repr Other)
  listOf       :: Label -> repr (Type repr a) -> repr (Variable repr Other)
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> repr (Type repr Other) -> repr (Variable repr Other)

  ($->) :: repr (Variable repr Other) -> repr (Variable repr a) -> 
    repr (Variable repr a)
  infixl 9 $->

  variableBind :: repr (Variable repr a) -> Binding
  variableName :: repr (Variable repr a) -> String
  variableType :: repr (Variable repr a) -> repr (Type repr a)
  variableDoc  :: repr (Variable repr a) -> Doc

class InternalVariable repr where
  varFromData :: Binding -> String -> repr (Type repr a) -> Doc -> 
    repr (Variable repr a)

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

  valueOf       :: repr (Variable repr a) -> repr (Value repr a)
--  global       :: Label -> repr (Value repr Other)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> repr (Value repr Other)
  enumElement  :: Label -> Label -> repr (Value repr Other)

  argsList  :: repr (Value repr Other)

  valueType :: repr (Value repr a) -> repr (Type repr a)
  valueDoc :: repr (Value repr a) -> Doc

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

  (?<)  :: repr (Value repr a) -> repr (Value repr a) -> 
    repr (Value repr Boolean)
  infixl 4 ?<
  (?<=) :: repr (Value repr a) -> repr (Value repr a) -> 
    repr (Value repr Boolean)
  infixl 4 ?<=
  (?>)  :: repr (Value repr a) -> repr (Value repr a) -> 
    repr (Value repr Boolean)
  infixl 4 ?>
  (?>=) :: repr (Value repr a) -> repr (Value repr a) -> 
    repr (Value repr Boolean)
  infixl 4 ?>=
  (?==) :: repr (Value repr a) -> repr (Value repr a) -> 
    repr (Value repr Boolean)
  infixl 3 ?==
  (?!=) :: repr (Value repr a) -> repr (Value repr a) -> 
    repr (Value repr Boolean)
  infixl 3 ?!=

class (ValueSym repr, NumericExpression repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: repr (Value repr Boolean) -> repr (Value repr a) -> 
    repr (Value repr a) -> repr (Value repr a)
  funcApp      :: Label -> repr (Type repr a) -> [repr (Value repr Other)] 
    -> repr (Value repr a)
  selfFuncApp  :: Label -> repr (Type repr a) -> [repr (Value repr Other)] 
    -> repr (Value repr a)
  extFuncApp   :: Library -> Label -> repr (Type repr a) -> 
    [repr (Value repr Other)] -> repr (Value repr a)
  newObj       :: repr (Type repr Other) -> [repr (Value repr Other)] -> 
    repr (Value repr Other)
  extNewObj    :: Library -> repr (Type repr Other) -> 
    [repr (Value repr Other)] -> repr (Value repr Other)

  exists  :: repr (Value repr Other) -> repr (Value repr Boolean)
  notNull :: repr (Value repr Other) -> repr (Value repr Boolean)

class (ValueExpression repr) => InternalValue repr where
  inputFunc       :: repr (Value repr Other)
  printFunc       :: repr (Value repr Other)
  printLnFunc     :: repr (Value repr Other)
  printFileFunc   :: repr (Value repr Other) -> repr (Value repr Other)
  printFileLnFunc :: repr (Value repr Other) -> repr (Value repr Other)

  cast :: repr (Type repr a) -> repr (Value repr b) -> 
    repr (Value repr a)

  valFromData :: Maybe Int -> repr (Type repr a) -> Doc -> repr (Value repr a)
  toOtherValue :: repr (Value repr a) -> repr (Value repr Other)

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

  objMethodCall     :: repr (Type repr a) -> repr (Value repr Other) -> 
    Label -> [repr (Value repr Other)] -> repr (Value repr a)
  objMethodCallNoParams :: repr (Type repr a) -> repr (Value repr Other) 
    -> Label -> repr (Value repr a)

  selfAccess :: Label -> repr (Function repr a) -> repr (Value repr a)

  listIndexExists :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Boolean)
  argExists       :: Integer -> repr (Value repr Boolean)

  indexOf :: repr (Value repr Other) -> repr (Value repr a) -> 
    repr (Value repr Other)

class (ValueSym repr, ValueExpression repr) => FunctionSym repr where
  type Function repr :: * -> *
  func :: Label -> repr (Type repr a) -> [repr (Value repr Other)] -> 
    repr (Function repr a)

  get :: repr (Value repr Other) -> repr (Variable repr a) -> 
    repr (Value repr a)
  set :: repr (Value repr Other) -> repr (Variable repr a) -> 
    repr (Value repr a) -> repr (Value repr Other)

  listSize   :: repr (Value repr Other) -> repr (Value repr Other)
  listAdd    :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr a) -> repr (Value repr Other)
  listAppend :: repr (Value repr Other) -> repr (Value repr a) -> 
    repr (Value repr Other)

  iterBegin :: repr (Value repr Other) -> repr (Value repr Other)
  iterEnd   :: repr (Value repr Other) -> repr (Value repr Other)

class (ValueSym repr, InternalValue repr, FunctionSym repr, Selector repr) => 
  SelectorFunction repr where
  listAccess :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other)
  listSet    :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr a) -> repr (Value repr Other)
  at         :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other)

class (ValueSym repr, InternalValue repr) => InternalFunction repr where
  getFunc        :: repr (Variable repr a) -> repr (Function repr a)
  setFunc        :: repr (Type repr Other) -> repr (Variable repr a) -> 
    repr (Value repr a) -> repr (Function repr Other)

  listSizeFunc       :: repr (Function repr Other)
  listAddFunc        :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr a) -> repr (Function repr Other)
  listAppendFunc         :: repr (Value repr a) -> repr (Function repr Other)

  iterBeginFunc :: repr (Type repr Other) -> repr (Function repr Other)
  iterEndFunc   :: repr (Type repr Other) -> repr (Function repr Other)

  listAccessFunc :: repr (Type repr Other) -> repr (Value repr Other) -> 
    repr (Function repr Other)
  listSetFunc    :: repr (Value repr Other) -> repr (Value repr Other) -> 
    repr (Value repr a) -> repr (Function repr Other)

  functionType :: repr (Function repr a) -> repr (Type repr a)
  functionDoc :: repr (Function repr a) -> Doc

  funcFromData :: repr (Type repr a) -> Doc -> repr (Function repr a)

class (Selector repr) => InternalStatement repr where
  -- newLn, printFunc, value to print, maybe a file to print to 
  printSt :: Bool -> repr (Value repr Other) -> repr (Value repr a) -> 
    Maybe (repr (Value repr Other)) -> repr (Statement repr)

  state     :: repr (Statement repr) -> repr (Statement repr)
  loopState :: repr (Statement repr) -> repr (Statement repr)

  emptyState   :: repr (Statement repr)
  statementDoc :: repr (Statement repr) -> Doc
  statementTerm :: repr (Statement repr) -> Terminator

  stateFromData :: Doc -> Terminator -> repr (Statement repr)

class (ValueSym repr, Selector repr, SelectorFunction repr, FunctionSym repr,
  InternalFunction repr, InternalStatement repr) => StatementSym repr where
  type Statement repr
  (&=)   :: repr (Variable repr a) -> repr (Value repr a) -> 
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

  assign            :: repr (Variable repr a) -> repr (Value repr a) -> 
    repr (Statement repr)
  assignToListIndex :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Statement repr)
  multiAssign       :: [repr (Variable repr Other)] -> [repr (Value repr Other)] ->
    repr (Statement repr) 

  varDec           :: repr (Variable repr a) -> repr (Statement repr)
  varDecDef        :: repr (Variable repr a) -> repr (Value repr a) -> 
    repr (Statement repr)
  listDec          :: Integer -> repr (Variable repr Other) -> repr (Statement repr)
  listDecDef       :: repr (Variable repr Other) -> [repr (Value repr Other)] 
    -> repr (Statement repr)
  objDecDef        :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Statement repr)
  objDecNew        :: repr (Variable repr Other) -> [repr (Value repr Other)] 
    -> repr (Statement repr)
  extObjDecNew     :: Library -> repr (Variable repr Other) -> 
    [repr (Value repr Other)] -> repr (Statement repr)
  objDecNewNoParams    :: repr (Variable repr Other) -> repr (Statement repr)
  extObjDecNewNoParams :: Library -> repr (Variable repr Other) -> 
    repr (Statement repr)
  constDecDef      :: repr (Variable repr a) -> repr (Value repr a) -> 
    repr (Statement repr)

  print      :: repr (Value repr a) -> repr (Statement repr)
  printLn    :: repr (Value repr a) -> repr (Statement repr)
  printStr   :: String -> repr (Statement repr)
  printStrLn :: String -> repr (Statement repr)

  printFile      :: repr (Value repr Other) -> repr (Value repr a) -> 
    repr (Statement repr)
  printFileLn    :: repr (Value repr Other) -> repr (Value repr a) -> 
    repr (Statement repr)
  printFileStr   :: repr (Value repr Other) -> String -> repr (Statement repr)
  printFileStrLn :: repr (Value repr Other) -> String -> repr (Statement repr)

  getInput         :: repr (Variable repr a) -> repr (Statement repr)
  discardInput     :: repr (Statement repr)
  getFileInput     :: repr (Value repr Other) -> repr (Variable repr a) -> 
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
  stringSplit      :: Char -> repr (Variable repr Other) -> 
    repr (Value repr Other) -> repr (Statement repr)

  -- For assigning each element from a list of strings (the value) to a list of variables
  stringListVals :: [repr (Variable repr Other)] -> repr (Value repr Other) -> 
    repr (Statement repr)
  -- For assigning each element from a list of strings (the value) to elements of list-type variables
  stringListLists :: [repr (Variable repr Other)] -> repr (Value repr Other) ->
    repr (Statement repr)

  break :: repr (Statement repr)
  continue :: repr (Statement repr)

  returnState :: repr (Value repr a) -> repr (Statement repr)
  multiReturn :: [repr (Value repr Other)] -> repr (Statement repr)

  valState :: repr (Value repr a) -> repr (Statement repr)

  comment :: Label -> repr (Statement repr)

  free :: repr (Variable repr a) -> repr (Statement repr)

  throw :: Label -> repr (Statement repr)

  initState   :: Label -> Label -> repr (Statement repr)
  changeState :: Label -> Label -> repr (Statement repr)

  initObserverList :: repr (Type repr Other) -> [repr (Value repr Other)] 
    -> repr (Statement repr)
  addObserver      :: repr (Value repr a) -> repr (Statement repr)

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [repr (Value repr Other)] -> 
    [repr (Variable repr Other)] -> [repr (Variable repr Other)] -> 
    repr (Statement repr)
  extInOutCall :: Library -> Label -> [repr (Value repr Other)] ->
    [repr (Variable repr Other)] -> [repr (Variable repr Other)] -> 
    repr (Statement repr)

  multi     :: [repr (Statement repr)] -> repr (Statement repr)

class (StatementSym repr, BodySym repr) => ControlStatementSym repr where
  ifCond     :: [(repr (Value repr Boolean), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  ifNoElse   :: [(repr (Value repr Boolean), repr (Body repr))] -> 
    repr (Statement repr)
  switch     :: repr (Value repr a) -> [(repr (Value repr a), 
    repr (Body repr))] -> repr (Body repr) -> repr (Statement repr) -- is there value in separating Literals into their own type?
  switchAsIf :: repr (Value repr a) -> [(repr (Value repr a), 
    repr (Body repr))] -> repr (Body repr) -> repr (Statement repr)

  ifExists :: repr (Value repr Other) -> repr (Body repr) -> repr (Body repr) ->
    repr (Statement repr)

  for      :: repr (Statement repr) -> repr (Value repr Boolean) -> 
    repr (Statement repr) -> repr (Body repr) -> repr (Statement repr)
  forRange :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Value repr Other) -> repr (Value repr Other) -> repr (Body repr) -> 
    repr (Statement repr)
  forEach  :: repr (Variable repr Other) -> repr (Value repr Other) -> 
    repr (Body repr) -> repr (Statement repr)
  while    :: repr (Value repr Boolean) -> repr (Body repr) -> 
    repr (Statement repr) 

  tryCatch :: repr (Body repr) -> repr (Body repr) -> repr (Statement repr)

  checkState      :: Label -> [(repr (Value repr Other), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  notifyObservers :: repr (Function repr a) -> repr (Type repr b) -> 
    repr (Statement repr)

  getFileInputAll  :: repr (Value repr Other) -> repr (Variable repr Other) -> 
    repr (Statement repr)

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class MethodTypeSym repr where
  type MethodType repr :: * -> *
  mType    :: repr (Type repr a) -> repr (MethodType repr a)
  construct :: Label -> repr (MethodType repr Other)

class ParameterSym repr where
  type Parameter repr
  param :: repr (Variable repr a) -> repr (Parameter repr)
  -- funcParam  :: Label -> repr (Type repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: repr (Variable repr a) -> repr (Parameter repr)

  parameterName :: repr (Parameter repr) -> String

class (ScopeSym repr, MethodTypeSym repr, ParameterSym repr, StateVarSym repr,
  BodySym repr, InternalMethod repr) => MethodSym repr where
  type Method repr
  -- Second label is class name
  method      :: Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> repr (Type repr a) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  getMethod   :: Label -> repr (Variable repr a) -> repr (Method repr)
  setMethod   :: Label -> repr (Variable repr a) -> repr (Method repr)
  privMethod  :: Label -> Label -> repr (Type repr a) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  pubMethod   :: Label -> Label -> repr (Type repr a) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  constructor :: Label -> [repr (Parameter repr)] -> repr (Body repr) -> 
    repr (Method repr)
  destructor :: Label -> [repr (StateVar repr)] -> repr (Method repr)

  docMain :: repr (Body repr) -> repr (Method repr)

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    repr (Type repr a) -> [repr (Parameter repr)] -> repr (Body repr) -> 
    repr (Method repr) 
  mainFunction  :: repr (Body repr) -> repr (Method repr)
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

  parameters :: repr (Method repr) -> [repr (Parameter repr)]

class (ScopeSym repr, MethodTypeSym repr, ParameterSym repr, StateVarSym repr,
  BodySym repr, BlockCommentSym repr) => InternalMethod repr where
  intMethod      :: Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> repr (MethodType repr a) -> 
    [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
  intFunc      :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    repr (MethodType repr a) -> [repr (Parameter repr)] -> repr (Body repr) -> 
    repr (Method repr)
  commentedFunc :: repr (BlockComment repr) -> repr (Method repr) -> 
    repr (Method repr)
  

class (ScopeSym repr, PermanenceSym repr, TypeSym repr, StatementSym repr) => 
  StateVarSym repr where
  type StateVar repr
  stateVar :: Int -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr a) -> repr (StateVar repr)
  stateVarDef :: Int -> Label -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr a) -> repr (Value repr a) -> repr (StateVar repr)
  constVar :: Int -> Label -> repr (Scope repr) ->  repr (Variable repr a) -> 
    repr (Value repr a) -> repr (StateVar repr)
  privMVar :: Int -> repr (Variable repr a) -> repr (StateVar repr)
  pubMVar  :: Int -> repr (Variable repr a) -> repr (StateVar repr)
  pubGVar  :: Int -> repr (Variable repr a) -> repr (StateVar repr)

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
