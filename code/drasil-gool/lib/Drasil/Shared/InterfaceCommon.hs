{-# LANGUAGE TypeFamilyDependencies #-}
module Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, NamedArgs, MixedCall,
  MixedCtorCall, PosCall, PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, BodySym(..), bodyStatements, oneLiner, BlockSym(..), TypeSym(..),
  TypeElim(..), VariableSym(..), ScopeSym(..), convScope, VariableElim(..),
  listOf, listVar, ValueSym(..), Argument(..), Literal(..), litZero,
  MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp, libFuncApp,
  exists, List(..), Set(..), InternalList(..), listSlice, listIndexExists, at,
  ThunkSym(..), VectorType(..), VectorDecl(..), VectorThunk(..),
  VectorExpression(..), ThunkAssign(..), StatementSym(..), AssignStatement(..),
  (&=), assignToListIndex, DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), ifNoElse, switchAsIf,
  VisibilitySym(..), ParameterSym(..), MethodSym(..), convType
  ) where

import Drasil.Shared.CodeType (CodeType(..))

import qualified Data.Kind as K (Type)
import Data.Bifunctor (first)
import CodeLang.Drasil (Comment)
import Drasil.Shared.AST (ScopeData(..), ScopeTag(..))

type Label = String
type Library = String

-- In relation to GOOL, the type variable r can be considered as short for "representation"

-- Functions in GOOL's interface beginning with "ext" are to be used to access items from other modules in the same program/project
-- Functions in GOOL's interface beginning with "lib" are to be used to access items from different libraries/projects

class (VectorType r, VectorDecl r, VectorThunk r,
  VectorExpression r, ThunkAssign r, AssignStatement r, DeclStatement r,
  IOStatement r, StringStatement r, FunctionSym r, FuncAppStatement r,
  CommentStatement r, ControlStatement r, InternalList r, Argument r, Literal r,
  MathConstant r, VariableValue r, CommandLineArgs r, NumericExpression r,
  BooleanExpression r, Comparison r, ValueExpression r, List r, Set r, TypeElim r,
  VariableElim r, MethodSym r, ScopeSym r
  ) => SharedProg r

-- Shared between OO and Procedural --

class (BlockSym r) => BodySym r where
  type Body r = t | t -> r
  body           :: [Block r] -> Body r

  addComments :: Label -> Body r -> Body r

bodyStatements :: (BodySym r) => [Statement r] -> Body r
bodyStatements sts = body [block sts]

oneLiner :: (BodySym r) => Statement r -> Body r
oneLiner s = bodyStatements [s]

class (StatementSym r) => BlockSym r where
  type Block r = t | t -> r
  block   :: [Statement r] -> Block r

class TypeSym r where
  type Type r = t | t -> r
  bool          :: Type r
  int           :: Type r -- This is 32-bit signed ints except in Python, 
                            -- which has unlimited precision ints; and Julia,
                            -- Which defaults to 64-bit signed ints
  float         :: Type r
  double        :: Type r
  char          :: Type r
  string        :: Type r
  infile        :: Type r
  outfile       :: Type r
  listType      :: Type r -> Type r
  setType       :: Type r -> Type r
  arrayType     :: Type r -> Type r
  listInnerType :: Type r -> Type r
  funcType      :: [Type r] -> Type r -> Type r
  void          :: Type r

class (TypeSym r) => TypeElim r where
  getType :: Type r -> CodeType
  getTypeString :: Type r -> String

class ScopeSym r where
  type Scope r = t | t -> r
  global :: Scope r -- Definite global scope
  mainFn :: Scope r -- Main program - either main function or global scope
  local  :: Scope r -- Definite local scope

class (TypeSym r) => VariableSym r where
  type Variable r = t | t -> r
  var       :: Label -> Type r -> Variable r
  constant  :: Label -> Type r -> Variable r
  extVar    :: Library -> Label -> Type r -> Variable r
  arrayElem :: Integer -> Variable r -> Variable r

class (VariableSym r) => VariableElim r where
  variableName :: Variable r -> String
  variableType :: Variable r -> Type r

listVar :: (VariableSym r) => Label -> Type r -> Variable r
listVar n t = var n (listType t)

listOf :: (VariableSym r) => Label -> Type r -> Variable r
listOf = listVar

class (TypeSym r) => ValueSym r where
  type Value r = t | t -> r
  valueType :: Value r -> Type r

class (ValueSym r) => Argument r where
  pointerArg :: Value r -> Value r

class (ValueSym r) => Literal r where
  litTrue   :: Value r
  litFalse  :: Value r
  litChar   :: Char -> Value r
  litDouble :: Double -> Value r
  litFloat  :: Float -> Value r
  litInt    :: Integer -> Value r
  litString :: String -> Value r
  litArray  :: Type r -> [Value r] -> Value r
  litList   :: Type r -> [Value r] -> Value r
  litSet    :: Type r -> [Value r] -> Value r

litZero :: (TypeElim r, Literal r) => Type r -> Value r
litZero t =
  case getType t of
    Integer -> litInt 0
    Float -> litFloat 0
    Double -> litDouble 0
    _ -> error "litZero expects a numeric type"
class (ValueSym r) => MathConstant r where
  pi :: Value r

class (VariableSym r, ValueSym r) => VariableValue r where
  valueOf       :: Variable r -> Value r

class (ValueSym r) => CommandLineArgs r where
  arg          :: Integer -> Value r
  argsList     :: Value r
  argExists    :: Integer -> Value r

class (ValueSym r) => NumericExpression r where
  (#~)  :: Value r -> Value r
  infixl 8 #~ -- Negation
  (#/^) :: Value r -> Value r
  infixl 7 #/^ -- Square root
  (#|)  :: Value r -> Value r
  infixl 7 #| -- Absolute value
  (#+)  :: Value r -> Value r -> Value r
  infixl 5 #+
  (#-)  :: Value r -> Value r -> Value r
  infixl 5 #-
  (#*)  :: Value r -> Value r -> Value r
  infixl 6 #*
  (#/)  :: Value r -> Value r -> Value r
  infixl 6 #/
  (#%)  :: Value r -> Value r -> Value r
  infixl 6 #% -- Modulo
  (#^)  :: Value r -> Value r -> Value r
  infixl 7 #^ -- Exponentiation

  log    :: Value r -> Value r
  ln     :: Value r -> Value r
  exp    :: Value r -> Value r
  sin    :: Value r -> Value r
  cos    :: Value r -> Value r
  tan    :: Value r -> Value r
  csc    :: Value r -> Value r
  sec    :: Value r -> Value r
  cot    :: Value r -> Value r
  arcsin :: Value r -> Value r
  arccos :: Value r -> Value r
  arctan :: Value r -> Value r
  floor  :: Value r -> Value r
  ceil   :: Value r -> Value r

class (ValueSym r) => BooleanExpression r where
  (?!)  :: Value r -> Value r
  infixr 6 ?! -- Boolean 'not'
  (?&&) :: Value r -> Value r -> Value r
  infixl 2 ?&&
  (?||) :: Value r -> Value r -> Value r
  infixl 1 ?||

class (ValueSym r) => Comparison r where
  (?<)  :: Value r -> Value r -> Value r
  infixl 4 ?<
  (?<=) :: Value r -> Value r -> Value r
  infixl 4 ?<=
  (?>)  :: Value r -> Value r -> Value r
  infixl 4 ?>
  (?>=) :: Value r -> Value r -> Value r
  infixl 4 ?>=
  (?==) :: Value r -> Value r -> Value r
  infixl 3 ?==
  (?!=) :: Value r -> Value r -> Value r
  infixl 3 ?!=

type NamedArgs r = [(Variable r, Value r)]
-- Function call with both positional and named arguments
type MixedCall r = Label -> Type r -> [Value r] -> NamedArgs r -> Value r
-- Constructor call with both positional and named arguments
type MixedCtorCall r = Type r -> [Value r] -> NamedArgs r -> Value r
-- Function call with only positional arguments
type PosCall r = Label -> Type r -> [Value r] -> Value r
-- Constructor call with only positional arguments
type PosCtorCall r = Type r -> [Value r] -> Value r

-- for values that can include expressions
class (VariableSym r, ValueSym r) => ValueExpression r where
  -- An inline if-statement, aka the ternary operator.  Inputs:
  -- Condition, True-value, False-value
  inlineIf     :: Value r -> Value r -> Value r -> Value r

  funcAppMixedArgs     ::            MixedCall r
  extFuncAppMixedArgs  :: Library -> MixedCall r
  libFuncAppMixedArgs  :: Library -> MixedCall r

  lambda :: [Variable r] -> Value r -> Value r

  notNull :: Value r -> Value r

funcApp          :: (ValueExpression r) =>            PosCall r
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs :: (ValueExpression r) =>            Label -> Type r ->
  NamedArgs r -> Value r
funcAppNamedArgs n t = funcAppMixedArgs n t []

extFuncApp       :: (ValueExpression r) => Library -> PosCall r
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp       :: (ValueExpression r) => Library -> PosCall r
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

exists :: (ValueExpression r) => Value r -> Value r
exists = notNull

class (ValueSym r) => List r where
  -- | Does any necessary conversions from GOOL's zero-indexed assumptions to
  --   the target language's assumptions
  intToIndex :: Value r -> Value r
  -- | Does any necessary conversions from the target language's indexing
  --   assumptions assumptions to GOOL's zero-indexed assumptions
  indexToInt :: Value r -> Value r
  -- | Finds the size of a list.
  --   Arguments are: List
  listSize   :: Value r -> Value r
  -- | Inserts a value into a list.
  --   Arguments are: List, Index, Value
  listAdd    :: Value r -> Value r -> Value r -> Value r
  -- | Appens a value to a list.
  --   Arguments are: List, Value
  listAppend :: Value r -> Value r -> Value r
  -- | Gets the value of an index of a list.
  --   Arguments are: List, Index
  listAccess :: Value r -> Value r -> Value r
  -- | Sets the value of an index of a list.
  --   Arguments are: List, Index, Value
  listSet    :: Value r -> Value r -> Value r -> Value r
  -- | Finds the index of the first occurrence of a value in a list.
  --   Arguments are: List, Value
  indexOf :: Value r -> Value r -> Value r

class (ValueSym r) => Set r where
  -- | Checks membership
  -- Arguments are: Set, Value
  contains :: Value r -> Value r -> Value r
  -- | Inserts a value into a set
  -- Arguments are: Set, Value
  setAdd :: Value r -> Value r -> Value r
  -- | Removes a value from a set
  -- Arguments are: Set, Value
  setRemove :: Value r -> Value r -> Value r
  -- | Removes a value from a set
  -- Arguments are: Set, Set
  setUnion :: Value r -> Value r -> Value r

class (ValueSym r) => InternalList r where
  listSlice'      :: Maybe (Value r) -> Maybe (Value r) -> Maybe (Value r)
    -> Variable r -> Value r -> Block r

-- | Creates a slice of a list and assigns it to a variable.
--   Arguments are: 
--   Variable to assign
--   List to read from
--   (optional) Start index inclusive.
--      (if Nothing, then list start if step > 0, list end if step < 0)
--   (optional) End index exclusive.
--      (if Nothing, then list end if step > 0, list start if step > 0)
--   (optional) Step (if Nothing, then defaults to 1)
listSlice :: (InternalList r) => Variable r -> Value r -> Maybe (Value r) ->
  Maybe (Value r) -> Maybe (Value r) -> Block r
listSlice vnew vold b e s = listSlice' b e s vnew vold

listIndexExists :: (List r, Comparison r) => Value r -> Value r -> Value r
listIndexExists lst index = listSize lst ?> index

at :: (List r) => Value r -> Value r -> Value r
at = listAccess

-- Vector Typeclasses --

class ThunkSym r where
  -- K.Type -> K.Type annotation needed because r is not applied here so its
  -- kind cannot be inferred (whereas for Value, r is applied in the type
  -- signature of valueType
  type Thunk (r :: K.Type) = t | t -> r

class (VariableSym r, ThunkSym r, StatementSym r) => ThunkAssign r where
  thunkAssign :: Variable r -> Thunk r -> Statement r

class TypeSym r => VectorType r where
  vecType :: Type r -> Type r

class (DeclStatement r) => VectorDecl r where
  -- First argument is size of the vector
  vecDec :: Integer -> Variable r -> Scope r -> Statement r
  vecDecDef :: Variable r -> Scope r -> [Value r] -> Statement r

class (VariableSym r, ThunkSym r) => VectorThunk r where
  vecThunk :: Variable r -> Thunk r

class (ThunkSym r, ValueSym r) => VectorExpression r where
  vecScale :: Value r -> Thunk r -> Thunk r
  vecAdd :: Thunk r -> Thunk r -> Thunk r
  vecIndex :: Value r -> Thunk r -> Value r
  vecDot :: Thunk r -> Thunk r -> Thunk r

class (ValueSym r) => StatementSym r where
  type Statement r = t | t -> r
  valStmt :: Value r -> Statement r -- converts value to statement
  emptyStmt :: Statement r
  multi     :: [Statement r] -> Statement r

class (VariableSym r, StatementSym r) => AssignStatement r where
  (&-=)  :: Variable r -> Value r -> Statement r
  infixl 1 &-=
  (&+=)  :: Variable r -> Value r -> Statement r
  infixl 1 &+=
  (&++)  :: Variable r -> Statement r
  infixl 8 &++
  (&--)  :: Variable r -> Statement r
  infixl 8 &--

  assign :: Variable r -> Value r -> Statement r

(&=) :: (AssignStatement r) => Variable r -> Value r -> Statement r
infixr 1 &=
(&=) = assign

assignToListIndex :: (StatementSym r, VariableValue r, List r) => Variable r
  -> Value r -> Value r -> Statement r
assignToListIndex lst index v = valStmt $ listSet (valueOf lst) index v

class (VariableSym r, StatementSym r, ScopeSym r) => DeclStatement r where
  varDec       :: Variable r -> Scope r -> Statement r
  varDecDef    :: Variable r -> Scope r -> Value r -> Statement r
  -- First argument is size of the list
  listDec      :: Integer -> Variable r -> Scope r -> Statement r
  listDecDef   :: Variable r -> Scope r -> [Value r] -> Statement r
  setDec       :: Variable r -> Scope r -> Statement r
  setDecDef    :: Variable r -> Scope r -> Value r -> Statement r
  -- First argument is size of the array
  arrayDec     :: Integer -> Variable r -> Scope r -> Statement r
  arrayDecDef  :: Variable r -> Scope r -> [Value r] -> Statement r
  constDecDef  :: Variable r -> Scope r -> Value r -> Statement r
  funcDecDef   :: Variable r -> Scope r -> [Variable r] -> Body r
    -> Statement r


class (VariableSym r, StatementSym r) => IOStatement r where
  print      :: Value r -> Statement r
  printLn    :: Value r -> Statement r
  printStr   :: String -> Statement r
  printStrLn :: String -> Statement r

  -- First argument is file handle, second argument is value to print
  printFile      :: Value r -> Value r -> Statement r
  printFileLn    :: Value r -> Value r -> Statement r
  printFileStr   :: Value r -> String -> Statement r
  printFileStrLn :: Value r -> String -> Statement r

  getInput         :: Variable r -> Statement r
  discardInput     :: Statement r
  getFileInput     :: Value r -> Variable r -> Statement r
  discardFileInput :: Value r -> Statement r

  openFileR :: Variable r -> Value r -> Statement r
  openFileW :: Variable r -> Value r -> Statement r
  openFileA :: Variable r -> Value r -> Statement r
  closeFile :: Value r -> Statement r

  getFileInputLine :: Value r -> Variable r -> Statement r
  discardFileLine  :: Value r -> Statement r
  getFileInputAll  :: Value r -> Variable r -> Statement r

class (VariableSym r, StatementSym r) => StringStatement r where
  -- Parameters are: char to split on, variable to store result in, string to split
  stringSplit :: Char -> Variable r -> Value r -> Statement r

  stringListVals  :: [Variable r] -> Value r -> Statement r
  stringListLists :: [Variable r] -> Value r -> Statement r

class (ValueSym r) => FunctionSym r where
  type Function r = t | t -> r

-- The three lists are inputs, outputs, and both, respectively
type InOutCall r = Label -> [Value r] -> [Variable r] -> [Variable r] ->
  Statement r

class (VariableSym r, StatementSym r) => FuncAppStatement r where
  inOutCall    ::            InOutCall r
  extInOutCall :: Library -> InOutCall r

class (StatementSym r) => CommentStatement r where
  comment :: Comment -> Statement r

class (BodySym r, VariableSym r) => ControlStatement r where
  break :: Statement r
  continue :: Statement r

  returnStmt :: Value r -> Statement r

  throw :: Label -> Statement r

  -- | String of if-else statements.
  --   Arguments: List of predicates and bodies (if this then that),
  --   Body for else branch
  ifCond     :: [(Value r, Body r)] -> Body r -> Statement r
  switch     :: Value r -> [(Value r, Body r)] -> Body r -> Statement r

  ifExists :: Value r -> Body r -> Body r -> Statement r

  for      :: Statement r -> Value r -> Statement r -> Body r ->
    Statement r
  -- Iterator variable, start value, end value, step value, loop body
  forRange :: Variable r -> Value r -> Value r -> Value r -> Body r ->
    Statement r
  forEach  :: Variable r -> Value r -> Body r -> Statement r
  while    :: Value r -> Body r -> Statement r

  tryCatch :: Body r -> Body r -> Statement r

  assert :: Value r -> Value r -> Statement r

ifNoElse :: (ControlStatement r) => [(Value r, Body r)] -> Statement r
ifNoElse bs = ifCond bs $ body []

switchAsIf :: (ControlStatement r, Comparison r) => Value r ->
  [(Value r, Body r)] -> Body r -> Statement r
switchAsIf v = ifCond . map (first (v ?==))

class VisibilitySym r where
  type Visibility r = t | t -> r
  private :: Visibility r
  public  :: Visibility r

class (VariableSym r) => ParameterSym r where
  type Parameter r
  param :: Variable r -> Parameter r
  pointerParam :: Variable r -> Parameter r

-- The three lists are inputs, outputs, and both, respectively
type InOutFunc r = [Variable r] -> [Variable r] -> [Variable r] ->
  Body r -> Method r
-- Parameters are: brief description of function, input descriptions and 
-- variables, output descriptions and variables, descriptions and variables 
-- for parameters that are both input and output, function body
type DocInOutFunc r = String -> [(String, Variable r)] ->
  [(String, Variable r)] -> [(String, Variable r)] -> Body r -> Method r

class (BodySym r, ParameterSym r, VisibilitySym r) => MethodSym r
  where
  type Method r = t | t -> r
  docMain :: Body r -> Method r

  function :: Label -> Visibility r -> Type r -> [Parameter r] ->
    Body r -> Method r
  mainFunction  :: Body r -> Method r
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> Method r -> Method r

  inOutFunc :: Label -> Visibility r -> InOutFunc r
  docInOutFunc :: Label -> Visibility r -> DocInOutFunc r

-- Utility

convType :: (TypeSym r) => CodeType -> Type r
convType Boolean = bool
convType Integer = int
convType Float = float
convType Double = double
convType Char = char
convType String = string
convType (List t) = listType (convType t)
convType (Set t) = setType (convType t)
convType (Array t) = arrayType (convType t)
convType (Func ps r) = funcType (map convType ps) (convType r)
convType Void = void
convType InFile = infile
convType OutFile = outfile
convType (Object _) = error "Objects not supported"

convScope :: (ScopeSym r) => ScopeData -> Scope r
convScope (SD {scopeTag = Global}) = global
convScope (SD {scopeTag = Local}) = local
