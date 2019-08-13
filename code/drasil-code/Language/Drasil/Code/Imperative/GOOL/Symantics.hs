{-# LANGUAGE TypeFamilies #-}

module Language.Drasil.Code.Imperative.GOOL.Symantics (
  -- Types
  Label, Library,
  -- Typeclasses
  PackageSym(..), ProgramSym(..), RenderSym(..), InternalFile(..), 
  AuxiliarySym(..), KeywordSym(..), PermanenceSym(..), BodySym(..), 
  ControlBlockSym(..), BlockSym(..), StateTypeSym(..), UnaryOpSym(..), 
  BinaryOpSym(..), VariableSym(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), MethodTypeSym(..), ParameterSym(..), 
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..), 
  BlockCommentSym(..)
) where

import Language.Drasil.Code.Code (CodeType)
import Language.Drasil.CodeSpec (Comments)
import Text.PrettyPrint.HughesPJ (Doc)

type Label = String
type Library = String

class (ProgramSym repr, AuxiliarySym repr) => PackageSym repr where
  type Package repr 
  package :: repr (Program repr) -> [repr (Auxiliary repr)] -> 
    repr (Package repr)

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

class (KeywordSym repr, RenderSym repr) => AuxiliarySym repr where
  type Auxiliary repr
  doxConfig :: String -> repr (Program repr) -> repr (Auxiliary repr)

  optimizeDox :: repr (Keyword repr)

  makefile :: [Comments] -> repr (Program repr) -> repr (Auxiliary repr)

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

class (BodySym repr, ControlStatementSym repr) => ControlBlockSym repr where
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

class (StateTypeSym repr) => VariableSym repr where
  type Variable repr
  var          :: Label -> repr (StateType repr) -> repr (Variable repr)
  const        :: Label -> repr (StateType repr) -> repr (Variable repr)
  extVar       :: Library -> Label -> repr (StateType repr) -> 
    repr (Variable repr)
  self         :: Label -> repr (Variable repr)
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

  variableName :: repr (Variable repr) -> String
  variableType :: repr (Variable repr) -> repr (StateType repr)
  variableDoc  :: repr (Variable repr) -> Doc

class (VariableSym repr, StateVarSym repr) => ValueSym repr where
  type Value repr
  litTrue   :: repr (Value repr)
  litFalse  :: repr (Value repr)
  litChar   :: Char -> repr (Value repr)
  litFloat  :: Double -> repr (Value repr)
  litInt    :: Integer -> repr (Value repr)
  litString :: String -> repr (Value repr)

  --other operators ($)
  ($:)  :: Label -> Label -> repr (Value repr)
  infixl 9 $:

  valueOf       :: repr (Variable repr) -> repr (Value repr)
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> repr (Value repr)
  enumElement  :: Label -> Label -> repr (Value repr)

  argsList  :: repr (Value repr)

  valueType :: repr (Value repr) -> repr (StateType repr)
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

class (ValueExpression repr) => InternalValue repr where
  inputFunc       :: repr (Value repr)
  printFunc       :: repr (Value repr)
  printLnFunc     :: repr (Value repr)
  printFileFunc   :: repr (Value repr) -> repr (Value repr)
  printFileLnFunc :: repr (Value repr) -> repr (Value repr)

  cast :: repr (StateType repr) -> repr (Value repr) -> repr (Value repr)

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

  listIndexExists :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  argExists       :: Integer -> repr (Value repr)

  indexOf :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

class (ValueSym repr, ValueExpression repr) => FunctionSym repr where
  type Function repr
  func           :: Label -> repr (StateType repr) -> [repr (Value repr)] -> 
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

class (ValueSym repr, InternalValue repr, FunctionSym repr, Selector repr) => 
  SelectorFunction repr where
  listAccess :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
  listSet    :: repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Value repr)
  at         :: repr (Value repr) -> Label -> repr (Value repr)

class (ValueSym repr, InternalValue repr) => InternalFunction repr where
  getFunc        :: repr (Variable repr) -> repr (Function repr)
  setFunc        :: repr (StateType repr) -> repr (Variable repr) -> 
    repr (Value repr) -> repr (Function repr)

  listSizeFunc       :: repr (Function repr)
  listAddFunc        :: repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Function repr)
  listAppendFunc         :: repr (Value repr) -> repr (Function repr)

  iterBeginFunc :: repr (StateType repr) -> repr (Function repr)
  iterEndFunc   :: repr (StateType repr) -> repr (Function repr)

  listAccessFunc :: repr (StateType repr) -> repr (Value repr) -> 
    repr (Function repr)
  listSetFunc    :: repr (Value repr) -> repr (Value repr) -> 
    repr (Value repr) -> repr (Function repr)

  atFunc :: repr (StateType repr) -> Label -> repr (Function repr)

class (Selector repr) => InternalStatement repr where
  -- newLn, printFunc, value to print, maybe a file to print to 
  printSt :: Bool -> repr (Value repr) -> repr (Value repr) -> 
    Maybe (repr (Value repr)) -> repr (Statement repr)

  state     :: repr (Statement repr) -> repr (Statement repr)
  loopState :: repr (Statement repr) -> repr (Statement repr)

class (ValueSym repr, Selector repr, SelectorFunction repr, FunctionSym repr,
  InternalFunction repr, InternalStatement repr) => StatementSym repr where
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
  objDecNewVoid    :: repr (Variable repr) -> repr (Statement repr)
  extObjDecNewVoid :: Library -> repr (Variable repr) -> repr (Statement repr)
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

  initObserverList :: repr (StateType repr) -> [repr (Value repr)] -> 
    repr (Statement repr)
  addObserver      :: repr (Value repr) -> repr (Statement repr)

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [repr (Value repr)] -> [repr (Variable repr)] -> 
    [repr (Variable repr)] -> repr (Statement repr)
  extInOutCall :: Library -> Label -> [repr (Value repr)] ->
    [repr (Variable repr)] -> [repr (Variable repr)] -> repr (Statement repr)

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
  forEach  :: Label -> repr (Value repr) -> repr (Body repr) -> 
    repr (Statement repr)
  while    :: repr (Value repr) -> repr (Body repr) -> repr (Statement repr) 

  tryCatch :: repr (Body repr) -> repr (Body repr) -> repr (Statement repr)

  checkState      :: Label -> [(repr (Value repr), repr (Body repr))] -> 
    repr (Body repr) -> repr (Statement repr)
  notifyObservers :: repr (Function repr) -> repr (StateType repr) -> 
    repr (Statement repr)

  getFileInputAll  :: repr (Value repr) -> repr (Variable repr) -> 
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
  -- Parameters are: brief description, input descriptions, output descriptions, descriptions of parameters that are both input and output, function
  docInOutFunc :: String -> [String] -> [String] -> [String] -> 
    repr (Method repr) -> repr (Method repr)

  commentedFunc :: repr (BlockComment repr) -> repr (Method repr) -> 
    repr (Method repr)

  parameters :: repr (Method repr) -> [repr (Parameter repr)]

class (ScopeSym repr, InternalScope repr, PermanenceSym repr, StateTypeSym repr)
   => StateVarSym repr where
  type StateVar repr
  stateVar :: Int -> repr (Scope repr) -> repr (Permanence repr) ->
    repr (Variable repr) -> repr (StateVar repr)
  privMVar :: Int -> repr (Variable repr) -> repr (StateVar repr)
  pubMVar  :: Int -> repr (Variable repr) -> repr (StateVar repr)
  pubGVar  :: Int -> repr (Variable repr) -> repr (StateVar repr)

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
