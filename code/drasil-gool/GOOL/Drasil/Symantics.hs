{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.Symantics (
  -- Types
  Label, Library,
  -- Typeclasses
  ProgramSym(..), RenderSym(..), InternalFile(..),  KeywordSym(..), 
  PermanenceSym(..), InternalPerm(..), BodySym(..), ControlBlockSym(..), 
  BlockSym(..), InternalBlock(..), TypeSym(..), InternalType(..), 
  UnaryOpSym(..), BinaryOpSym(..), VariableSym(..), InternalVariable(..), 
  ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), InternalValue(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), InternalMethod(..), 
  StateVarSym(..), InternalStateVar(..), ClassSym(..), InternalClass(..), 
  ModuleSym(..), InternalMod(..), BlockCommentSym(..)
) where

import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.Data (Binding, Terminator, FileType)
import GOOL.Drasil.State (GOOLState)
import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

type Label = String
type Library = String

class (RenderSym repr) => ProgramSym repr where
  type Program repr
  prog :: Label -> [repr (RenderFile repr)] -> repr (Program repr)

class (ModuleSym repr, InternalFile repr) => 
  RenderSym repr where 
  type RenderFile repr
  fileDoc :: repr (Module repr) -> repr (RenderFile repr)

  -- Module description, list of author names, date as a String, file to comment
  docMod :: String -> [String] -> String -> repr (RenderFile repr) -> 
    repr (RenderFile repr)

  commentedMod :: repr (RenderFile repr) -> 
    State GOOLState (repr (BlockComment repr)) -> repr (RenderFile repr)

class InternalFile repr where
  top :: repr (Module repr) -> repr (Block repr)
  bottom :: repr (Block repr)

  fileFromData :: FileType -> FilePath -> repr (Module repr) -> 
    repr (RenderFile repr)

class (PermanenceSym repr) => KeywordSym repr where
  type Keyword repr
  endStatement     :: repr (Keyword repr)
  endStatementLoop :: repr (Keyword repr)

  include :: Label -> repr (Keyword repr)
  inherit :: Label -> repr (Keyword repr)

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

  keyDoc :: repr (Keyword repr) -> Doc

class (InternalPerm repr) => PermanenceSym repr where
  type Permanence repr
  static_  :: repr (Permanence repr)
  dynamic_ :: repr (Permanence repr)

class InternalPerm repr where
  permDoc :: repr (Permanence repr) -> Doc
  binding :: repr (Permanence repr) -> Binding

class (BlockSym repr) => BodySym repr where
  type Body repr
  body           :: [repr (Block repr)] -> repr (Body repr)
  bodyStatements :: [repr (Statement repr)] -> repr (Body repr)
  oneLiner       :: repr (Statement repr) -> repr (Body repr)

  addComments :: Label -> repr (Body repr) -> repr (Body repr)

  bodyDoc :: repr (Body repr) -> Doc

class (StatementSym repr, InternalBlock repr) => BlockSym repr where
  type Block repr
  block   :: [repr (Statement repr)] -> repr (Block repr)

class InternalBlock repr where
  blockDoc :: repr (Block repr) -> Doc
  docBlock :: Doc -> repr (Block repr)

class (PermanenceSym repr, InternalType repr) => TypeSym repr where
  type Type repr
  bool          :: repr (Type repr)
  int           :: repr (Type repr)
  float         :: repr (Type repr)
  char          :: repr (Type repr)
  string        :: repr (Type repr)
  infile        :: repr (Type repr)
  outfile       :: repr (Type repr)
  listType      :: repr (Permanence repr) -> repr (Type repr) -> repr (Type repr)
  listInnerType :: repr (Type repr) -> repr (Type repr)
  obj           :: Label -> repr (Type repr)
  enumType      :: Label -> repr (Type repr)
  iterator      :: repr (Type repr) -> repr (Type repr)
  void          :: repr (Type repr)

  getType :: repr (Type repr) -> CodeType
  getTypeString :: repr (Type repr) -> String
  getTypeDoc :: repr (Type repr) -> Doc

class InternalType repr where
  typeFromData :: CodeType -> String -> Doc -> repr (Type repr)

class (ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, repr (Body repr))] -> 
    Maybe (repr (Value repr)) -> Maybe (repr (Variable repr)) -> 
    repr (Block repr)

  listSlice        :: repr (Variable repr) -> repr (Value repr) -> 
    Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) ->
    Maybe (repr (Value repr)) -> repr (Block repr)

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
  type Variable repr
  var          :: Label -> repr (Type repr) -> repr (Variable repr)
  staticVar    :: Label -> repr (Type repr) -> repr (Variable repr)
  const        :: Label -> repr (Type repr) -> repr (Variable repr)
  extVar       :: Library -> Label -> repr (Type repr) -> 
    repr (Variable repr)
  self         :: Label -> repr (Variable repr)
  classVar     :: repr (Type repr) -> repr (Variable repr) -> 
    repr (Variable repr)
  extClassVar  :: repr (Type repr) -> repr (Variable repr) -> 
    repr (Variable repr)
  objVar       :: repr (Variable repr) -> repr (Variable repr) -> 
    repr (Variable repr)
  objVarSelf   :: Label -> repr (Variable repr) -> repr (Variable repr)
  enumVar      :: Label -> Label -> repr (Variable repr)
  listVar      :: Label -> repr (Permanence repr) -> repr (Type repr) -> 
    repr (Variable repr)
  listOf       :: Label -> repr (Type repr) -> repr (Variable repr)
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> repr (Type repr) -> repr (Variable repr)

  ($->) :: repr (Variable repr) -> repr (Variable repr) -> repr (Variable repr)
  infixl 9 $->

  variableBind :: repr (Variable repr) -> Binding
  variableName :: repr (Variable repr) -> String
  variableType :: repr (Variable repr) -> repr (Type repr)
  variableDoc  :: repr (Variable repr) -> Doc

class InternalVariable repr where
  varFromData :: Binding -> String -> repr (Type repr) -> Doc -> 
    repr (Variable repr)

class (VariableSym repr, InternalValue repr) => ValueSym repr where
  type Value repr
  litTrue   :: repr (Value repr)
  litFalse  :: repr (Value repr)
  litChar   :: Char -> repr (Value repr)
  litFloat  :: Double -> repr (Value repr)
  litInt    :: Integer -> repr (Value repr)
  litString :: String -> repr (Value repr)

  pi :: repr (Value repr)

  --other operators ($)
  ($:)  :: Label -> Label -> repr (Value repr)
  infixl 9 $:

  valueOf       :: repr (Variable repr) -> repr (Value repr)
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> repr (Value repr)
  enumElement  :: Label -> Label -> repr (Value repr)

  argsList  :: repr (Value repr)

  valueType :: repr (Value repr) -> repr (Type repr)
  valueDoc :: repr (Value repr) -> Doc

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
class (ValueSym repr, NumericExpression repr) => 
  BooleanExpression repr where
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

class (ValueSym repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: repr (Value repr) -> repr (Value repr) -> repr (Value repr) ->
    repr (Value repr)
  funcApp      :: Label -> repr (Type repr) -> [repr (Value repr)] -> 
    repr (Value repr)
  selfFuncApp  :: Label -> Label -> repr (Type repr) -> [repr (Value repr)] -> 
    repr (Value repr)
  extFuncApp   :: Library -> Label -> repr (Type repr) -> 
    [repr (Value repr)] -> repr (Value repr)
  newObj     :: repr (Type repr) -> [repr (Value repr)] -> repr (Value repr)
  extNewObj  :: Library -> repr (Type repr) -> [repr (Value repr)] -> 
    repr (Value repr)

  exists  :: repr (Value repr) -> repr (Value repr)
  notNull :: repr (Value repr) -> repr (Value repr)

class InternalValue repr where
  inputFunc       :: repr (Value repr)
  printFunc       :: repr (Value repr)
  printLnFunc     :: repr (Value repr)
  printFileFunc   :: repr (Value repr) -> repr (Value repr)
  printFileLnFunc :: repr (Value repr) -> repr (Value repr)

  cast :: repr (Type repr) -> repr (Value repr) -> repr (Value repr)

  valFromData :: Maybe Int -> repr (Type repr) -> Doc -> repr (Value repr)

-- The cyclic constraints issue arises here too. I've constrained this by ValueExpression,
-- but really one might want one of these values as part of an expression, so the
-- constraint would have to go both ways. I'm not sure what the solution is for
-- these sorts of problems, other than removing the constraints altogether, but 
-- then what is the purpose of splitting the typeclasses into smaller typeclasses?
-- I'm leaving it as is for now, even though I suspect this will change in the future.
class (FunctionSym repr) => Selector repr where
  objAccess :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)
  ($.)      :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)
  infixl 9 $.

  objMethodCall     :: repr (Type repr) -> repr (Value repr) -> Label -> 
    [repr (Value repr)] -> repr (Value repr)
  objMethodCallNoParams :: repr (Type repr) -> repr (Value repr) -> Label
    -> repr (Value repr)

  selfAccess :: Label -> repr (Function repr) -> repr (Value repr)

  listIndexExists :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  argExists       :: Integer -> repr (Value repr)

  indexOf :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

class (ValueExpression repr, InternalFunction repr) => FunctionSym repr where
  type Function repr
  func           :: Label -> repr (Type repr) -> [repr (Value repr)] -> 
    repr (Function repr)

  get :: repr (Value repr) -> repr (Variable repr) -> repr (Value repr)
  set :: repr (Value repr) -> repr (Variable repr) -> repr (Value repr) -> 
    repr (Value repr)

  listSize   :: repr (Value repr) -> repr (Value repr)
  listAdd    :: repr (Value repr) -> repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr)
  listAppend :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

  iterBegin :: repr (Value repr) -> repr (Value repr)
  iterEnd   :: repr (Value repr) -> repr (Value repr)

class (Selector repr) => SelectorFunction repr where
  listAccess :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  listSet    :: repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Value repr)
  at         :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

class InternalFunction repr where
  getFunc        :: repr (Variable repr) -> repr (Function repr)
  setFunc        :: repr (Type repr) -> repr (Variable repr) -> 
    repr (Value repr) -> repr (Function repr)

  listSizeFunc       :: repr (Function repr)
  listAddFunc        :: repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Function repr)
  listAppendFunc         :: repr (Value repr) -> repr (Function repr)

  iterBeginFunc :: repr (Type repr) -> repr (Function repr)
  iterEndFunc   :: repr (Type repr) -> repr (Function repr)

  listAccessFunc :: repr (Type repr) -> repr (Value repr) -> 
    repr (Function repr)
  listSetFunc    :: repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Function repr)

  functionType :: repr (Function repr) -> repr (Type repr)
  functionDoc :: repr (Function repr) -> Doc

  funcFromData :: repr (Type repr) -> Doc -> repr (Function repr)

class InternalStatement repr where
  -- newLn, printFunc, value to print, maybe a file to print to 
  printSt :: Bool -> repr (Value repr) -> repr (Value repr) -> 
    Maybe (repr (Value repr)) -> repr (Statement repr)

  state     :: repr (Statement repr) -> repr (Statement repr)
  loopState :: repr (Statement repr) -> repr (Statement repr)

  emptyState   :: repr (Statement repr)
  statementDoc :: repr (Statement repr) -> Doc
  statementTerm :: repr (Statement repr) -> Terminator

  stateFromData :: Doc -> Terminator -> repr (Statement repr)

class (KeywordSym repr, SelectorFunction repr, InternalStatement repr) => 
  StatementSym repr where
  type Statement repr
  (&=)   :: repr (Variable repr) -> repr (Value repr) -> repr (Statement repr)
  infixr 1 &=
  (&-=)  :: repr (Variable repr) -> repr (Value repr) -> repr (Statement repr)
  infixl 1 &-=
  (&+=)  :: repr (Variable repr) -> repr (Value repr) -> repr (Statement repr)
  infixl 1 &+=
  (&++)  :: repr (Variable repr) -> repr (Statement repr)
  infixl 8 &++
  (&~-)  :: repr (Variable repr) -> repr (Statement repr)
  infixl 8 &~-

  assign            :: repr (Variable repr) -> repr (Value repr) -> 
    repr (Statement repr)
  assignToListIndex :: repr (Variable repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Statement repr)
  multiAssign       :: [repr (Variable repr)] -> [repr (Value repr)] ->
    repr (Statement repr) 

  varDec           :: repr (Variable repr) -> repr (Statement repr)
  varDecDef        :: repr (Variable repr) -> repr (Value repr) -> 
    repr (Statement repr)
  listDec          :: Integer -> repr (Variable repr) -> repr (Statement repr)
  listDecDef       :: repr (Variable repr) -> [repr (Value repr)] -> 
    repr (Statement repr)
  objDecDef        :: repr (Variable repr) -> repr (Value repr) -> 
    repr (Statement repr)
  objDecNew        :: repr (Variable repr) -> [repr (Value repr)] -> 
    repr (Statement repr)
  extObjDecNew     :: Library -> repr (Variable repr) -> 
    [repr (Value repr)] -> repr (Statement repr)
  objDecNewNoParams    :: repr (Variable repr) -> repr (Statement repr)
  extObjDecNewNoParams :: Library -> repr (Variable repr) -> repr (Statement repr)
  constDecDef      :: repr (Variable repr) -> repr (Value repr) -> 
    repr (Statement repr)

  print      :: repr (Value repr) -> repr (Statement repr)
  printLn    :: repr (Value repr) -> repr (Statement repr)
  printStr   :: String -> repr (Statement repr)
  printStrLn :: String -> repr (Statement repr)

  printFile      :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  printFileLn    :: repr (Value repr) -> repr (Value repr) -> 
    repr (Statement repr)
  printFileStr   :: repr (Value repr) -> String -> repr (Statement repr)
  printFileStrLn :: repr (Value repr) -> String -> repr (Statement repr)

  getInput         :: repr (Variable repr) -> repr (Statement repr)
  discardInput     :: repr (Statement repr)
  getFileInput     :: repr (Value repr) -> repr (Variable repr) -> 
    repr (Statement repr)
  discardFileInput :: repr (Value repr) -> repr (Statement repr)

  openFileR :: repr (Variable repr) -> repr (Value repr) -> 
    repr (Statement repr)
  openFileW :: repr (Variable repr) -> repr (Value repr) -> 
    repr (Statement repr)
  openFileA :: repr (Variable repr) -> repr (Value repr) -> 
    repr (Statement repr)
  closeFile :: repr (Value repr) -> repr (Statement repr)

  getFileInputLine :: repr (Value repr) -> repr (Variable repr) -> 
    repr (Statement repr)
  discardFileLine  :: repr (Value repr) -> repr (Statement repr)
  stringSplit      :: Char -> repr (Variable repr) -> repr (Value repr) -> 
    repr (Statement repr)

  stringListVals :: [repr (Variable repr)] -> repr (Value repr) -> 
    repr (Statement repr)
  stringListLists :: [repr (Variable repr)] -> repr (Value repr) ->
    repr (Statement repr)

  break :: repr (Statement repr)
  continue :: repr (Statement repr)

  returnState :: repr (Value repr) -> repr (Statement repr)
  multiReturn :: [repr (Value repr)] -> repr (Statement repr)

  valState :: repr (Value repr) -> repr (Statement repr)

  comment :: Label -> repr (Statement repr)

  free :: repr (Variable repr) -> repr (Statement repr)

  throw :: Label -> repr (Statement repr)

  initState   :: Label -> Label -> repr (Statement repr)
  changeState :: Label -> Label -> repr (Statement repr)

  initObserverList :: repr (Type repr) -> [repr (Value repr)] -> 
    repr (Statement repr)
  addObserver      :: repr (Value repr) -> repr (Statement repr)

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [repr (Value repr)] -> [repr (Variable repr)] -> 
    [repr (Variable repr)] -> repr (Statement repr)
  selfInOutCall :: Label -> Label -> [repr (Value repr)] -> 
    [repr (Variable repr)] -> [repr (Variable repr)] -> repr (Statement repr)
  extInOutCall :: Library -> Label -> [repr (Value repr)] ->
    [repr (Variable repr)] -> [repr (Variable repr)] -> repr (Statement repr)

  multi     :: [repr (Statement repr)] -> repr (Statement repr)

class (BodySym repr) => ControlStatementSym repr where
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
  forRange :: repr (Variable repr) -> repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Body repr) -> repr (Statement repr)
  forEach  :: repr (Variable repr) -> repr (Value repr) -> repr (Body repr) -> 
    repr (Statement repr)
  while    :: repr (Value repr) -> repr (Body repr) -> repr (Statement repr) 

  tryCatch :: repr (Body repr) -> repr (Body repr) -> repr (Statement repr)

  checkState      :: Label -> [(repr (Value repr), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  notifyObservers :: repr (Function repr) -> repr (Type repr) -> 
    repr (Statement repr)

  getFileInputAll  :: repr (Value repr) -> repr (Variable repr) -> 
    repr (Statement repr)

class (InternalScope repr) => ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class InternalScope repr where
  scopeDoc :: repr (Scope repr) -> Doc

class (TypeSym repr) => MethodTypeSym repr where
  type MethodType repr
  mType    :: repr (Type repr) -> repr (MethodType repr)
  construct :: Label -> repr (MethodType repr)

class ParameterSym repr where
  type Parameter repr
  param :: repr (Variable repr) -> repr (Parameter repr)
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: repr (Variable repr) -> repr (Parameter repr)

  parameterName :: repr (Parameter repr) -> String
  parameterType :: repr (Parameter repr) -> repr (Type repr)

class (StateVarSym repr, ParameterSym repr, ControlBlockSym repr, 
  InternalMethod repr) => MethodSym repr where
  type Method repr
  -- Second label is class name
  method      :: Label -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> repr (Type repr) -> [repr (Parameter repr)] -> repr (Body repr) -> 
    State GOOLState (repr (Method repr))
  getMethod   :: Label -> repr (Variable repr) -> 
    State GOOLState (repr (Method repr))
  setMethod   :: Label -> repr (Variable repr) -> 
    State GOOLState (repr (Method repr)) 
  privMethod  :: Label -> Label -> repr (Type repr) -> [repr (Parameter repr)] 
    -> repr (Body repr) -> State GOOLState (repr (Method repr))
  pubMethod   :: Label -> Label -> repr (Type repr) -> [repr (Parameter repr)] 
    -> repr (Body repr) -> State GOOLState (repr (Method repr))
  constructor :: Label -> [repr (Parameter repr)] -> repr (Body repr) -> 
    State GOOLState (repr (Method repr))
  destructor :: Label -> [repr (StateVar repr)] -> 
    State GOOLState (repr (Method repr))

  docMain :: repr (Body repr) -> State GOOLState (repr (Method repr))

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    repr (Type repr) -> [repr (Parameter repr)] -> repr (Body repr) -> 
    State GOOLState (repr (Method repr))
  mainFunction  :: repr (Body repr) -> State GOOLState (repr (Method repr))
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> 
    State GOOLState (repr (Method repr)) -> State GOOLState (repr (Method repr))

  -- Second label is class name, rest is same as inOutFunc
  inOutMethod :: Label -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> [repr (Variable repr)] -> [repr (Variable repr)] -> 
    [repr (Variable repr)] -> repr (Body repr) -> 
    State GOOLState (repr (Method repr))
  -- Second label is class name, rest is same as docInOutFunc
  docInOutMethod :: Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> String -> [(String, repr (Variable repr))] -> 
    [(String, repr (Variable repr))] -> [(String, repr (Variable repr))] -> 
    repr (Body repr) -> State GOOLState (repr (Method repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [repr (Variable repr)] -> [repr (Variable repr)] -> [repr (Variable repr)] 
    -> repr (Body repr) -> State GOOLState (repr (Method repr))
  -- Parameters are: function name, scope, permanence, brief description, input descriptions and variables, output descriptions and variables, descriptions and variables for parameters that are both input and output, function body
  docInOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, repr (Variable repr))] -> [(String, repr 
    (Variable repr))] -> [(String, repr (Variable repr))] -> repr (Body repr)
    -> State GOOLState (repr (Method repr))

  parameters :: repr (Method repr) -> [repr (Parameter repr)]

class (MethodTypeSym repr, BlockCommentSym repr) => 
  InternalMethod repr where
  intMethod      :: Bool -> Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> repr (MethodType repr) -> [repr (Parameter repr)] 
    -> repr (Body repr) -> State GOOLState (repr (Method repr))
  intFunc      :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Body repr) -> 
    State GOOLState (repr (Method repr))
  commentedFunc :: State GOOLState (repr (BlockComment repr)) -> 
    State GOOLState (repr (Method repr)) -> State GOOLState (repr (Method repr))

  isMainMethod :: repr (Method repr) -> Bool
  methodDoc :: repr (Method repr) -> Doc

class (ScopeSym repr, PermanenceSym repr, TypeSym repr, StatementSym repr,
  InternalStateVar repr) => StateVarSym repr where
  type StateVar repr
  stateVar :: repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr) -> repr (StateVar repr)
  stateVarDef :: Label -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr) -> repr (Value repr) -> repr (StateVar repr)
  constVar :: Label -> repr (Scope repr) ->  repr (Variable repr) -> 
    repr (Value repr) -> repr (StateVar repr)
  privMVar :: repr (Variable repr) -> repr (StateVar repr)
  pubMVar  :: repr (Variable repr) -> repr (StateVar repr)
  pubGVar  :: repr (Variable repr) -> repr (StateVar repr)

class InternalStateVar repr where
  stateVarDoc :: repr (StateVar repr) -> Doc
  stateVarFromData :: Doc -> repr (StateVar repr)

class (MethodSym repr, InternalClass repr) => ClassSym repr 
  where
  type Class repr
  buildClass :: Label -> Maybe Label -> repr (Scope repr) -> 
    [repr (StateVar repr)] -> [repr (Method repr)] -> repr (Class repr)
  enum :: Label -> [Label] -> repr (Scope repr) -> repr (Class repr)
  privClass :: Label -> Maybe Label -> [repr (StateVar repr)] -> 
    [repr (Method repr)] -> repr (Class repr)
  pubClass :: Label -> Maybe Label -> [repr (StateVar repr)] -> 
    [repr (Method repr)] -> repr (Class repr)

  docClass :: String -> repr (Class repr) -> repr (Class repr)

  commentedClass :: State GOOLState (repr (BlockComment repr)) -> 
    repr (Class repr) -> repr (Class repr)

class InternalClass repr where
  classDoc :: repr (Class repr) -> Doc
  classFromData :: State GOOLState Doc -> repr (Class repr)

class (ClassSym repr, InternalMod repr) => ModuleSym repr where
  type Module repr
  buildModule :: Label -> [Library] -> [repr (Method repr)] -> 
    [repr (Class repr)] -> repr (Module repr)
    
  moduleName :: repr (Module repr) -> String

class InternalMod repr where
  isMainModule :: repr (Module repr) -> Bool
  moduleDoc :: repr (Module repr) -> Doc
  modFromData :: String -> Bool -> Doc -> repr (Module repr)
  updateModuleDoc :: (Doc -> Doc) -> repr (Module repr) -> repr (Module repr)
    
class BlockCommentSym repr where
  type BlockComment repr
  blockComment :: [String] -> repr (BlockComment repr)
  docComment :: State GOOLState [String] -> State GOOLState (repr (BlockComment repr))

  blockCommentDoc :: repr (BlockComment repr) -> Doc
