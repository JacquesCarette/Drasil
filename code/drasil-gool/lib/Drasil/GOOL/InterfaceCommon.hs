{-# LANGUAGE TypeFamilies #-}

module Drasil.GOOL.InterfaceCommon (
  -- Types
  Label, Library, MSBody, MSBlock, VSFunction, VSType, SVariable, SValue,
  VSThunk, MSStatement, MSParameter, SMethod, NamedArgs, MixedCall,
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

import Drasil.GOOL.CodeType (CodeType(..))
import Drasil.GOOL.State (MS, VS)

import qualified Data.Kind as K (Type)
import Data.Bifunctor (first)
import CodeLang.Drasil (Comment)
import Drasil.GOOL.AST (ScopeData(..), ScopeTag(..))

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

type MSBody a = MS (a (Body a))

class (BlockSym r) => BodySym r where
  type Body r
  body           :: [MSBlock r] -> MSBody r

  addComments :: Label -> MSBody r -> MSBody r

bodyStatements :: (BodySym r) => [MSStatement r] -> MSBody r
bodyStatements sts = body [block sts]

oneLiner :: (BodySym r) => MSStatement r -> MSBody r
oneLiner s = bodyStatements [s]

type MSBlock a = MS (a (Block a))

class (StatementSym r) => BlockSym r where
  type Block r
  block   :: [MSStatement r] -> MSBlock r

type VSType a = VS (a (Type a))

class TypeSym r where
  type Type r
  bool          :: VSType r
  int           :: VSType r -- This is 32-bit signed ints except in Python, 
                            -- which has unlimited precision ints; and Julia,
                            -- Which defaults to 64-bit signed ints
  float         :: VSType r
  double        :: VSType r
  char          :: VSType r
  string        :: VSType r
  infile        :: VSType r
  outfile       :: VSType r
  listType      :: VSType r -> VSType r
  setType       :: VSType r -> VSType r
  arrayType     :: VSType r -> VSType r
  listInnerType :: VSType r -> VSType r
  funcType      :: [VSType r] -> VSType r -> VSType r
  void          :: VSType r

class (TypeSym r) => TypeElim r where
  getType :: r (Type r) -> CodeType
  getTypeString :: r (Type r) -> String

class ScopeSym r where
  type Scope r
  global :: r (Scope r) -- Definite global scope
  mainFn :: r (Scope r) -- Main program - either main function or global scope
  local  :: r (Scope r) -- Definite local scope

type SVariable a = VS (a (Variable a))

class (TypeSym r) => VariableSym r where
  type Variable r
  var       :: Label -> VSType r -> SVariable r
  constant  :: Label -> VSType r -> SVariable r
  extVar    :: Library -> Label -> VSType r -> SVariable r
  arrayElem :: Integer -> SVariable r -> SVariable r

class (VariableSym r) => VariableElim r where
  variableName :: r (Variable r) -> String
  variableType :: r (Variable r) -> r (Type r)

listVar :: (VariableSym r) => Label -> VSType r -> SVariable r
listVar n t = var n (listType t)

listOf :: (VariableSym r) => Label -> VSType r -> SVariable r
listOf = listVar

type SValue a = VS (a (Value a))

class (TypeSym r) => ValueSym r where
  type Value r
  valueType :: r (Value r) -> r (Type r)

class (ValueSym r) => Argument r where
  pointerArg :: SValue r -> SValue r

class (ValueSym r) => Literal r where
  litTrue   :: SValue r
  litFalse  :: SValue r
  litChar   :: Char -> SValue r
  litDouble :: Double -> SValue r
  litFloat  :: Float -> SValue r
  litInt    :: Integer -> SValue r
  litString :: String -> SValue r
  litArray  :: VSType r -> [SValue r] -> SValue r
  litList   :: VSType r -> [SValue r] -> SValue r
  litSet    :: VSType r -> [SValue r] -> SValue r

litZero :: (TypeElim r, Literal r) => VSType r -> SValue r
litZero t = do
  t' <- t
  case getType t' of
    Integer -> litInt 0
    Float -> litFloat 0
    Double -> litDouble 0
    _ -> error "litZero expects a numeric type"

class (ValueSym r) => MathConstant r where
  pi :: SValue r

class (VariableSym r, ValueSym r) => VariableValue r where
  valueOf       :: SVariable r -> SValue r

class (ValueSym r) => CommandLineArgs r where
  arg          :: Integer -> SValue r
  argsList     :: SValue r
  argExists    :: Integer -> SValue r

class (ValueSym r) => NumericExpression r where
  (#~)  :: SValue r -> SValue r
  infixl 8 #~ -- Negation
  (#/^) :: SValue r -> SValue r
  infixl 7 #/^ -- Square root
  (#|)  :: SValue r -> SValue r
  infixl 7 #| -- Absolute value
  (#+)  :: SValue r -> SValue r -> SValue r
  infixl 5 #+
  (#-)  :: SValue r -> SValue r -> SValue r
  infixl 5 #-
  (#*)  :: SValue r -> SValue r -> SValue r
  infixl 6 #*
  (#/)  :: SValue r -> SValue r -> SValue r
  infixl 6 #/
  (#%)  :: SValue r -> SValue r -> SValue r
  infixl 6 #% -- Modulo
  (#^)  :: SValue r -> SValue r -> SValue r
  infixl 7 #^ -- Exponentiation

  log    :: SValue r -> SValue r
  ln     :: SValue r -> SValue r
  exp    :: SValue r -> SValue r
  sin    :: SValue r -> SValue r
  cos    :: SValue r -> SValue r
  tan    :: SValue r -> SValue r
  csc    :: SValue r -> SValue r
  sec    :: SValue r -> SValue r
  cot    :: SValue r -> SValue r
  arcsin :: SValue r -> SValue r
  arccos :: SValue r -> SValue r
  arctan :: SValue r -> SValue r
  floor  :: SValue r -> SValue r
  ceil   :: SValue r -> SValue r

class (ValueSym r) => BooleanExpression r where
  (?!)  :: SValue r -> SValue r
  infixr 6 ?! -- Boolean 'not'
  (?&&) :: SValue r -> SValue r -> SValue r
  infixl 2 ?&&
  (?||) :: SValue r -> SValue r -> SValue r
  infixl 1 ?||

class (ValueSym r) => Comparison r where
  (?<)  :: SValue r -> SValue r -> SValue r
  infixl 4 ?<
  (?<=) :: SValue r -> SValue r -> SValue r
  infixl 4 ?<=
  (?>)  :: SValue r -> SValue r -> SValue r
  infixl 4 ?>
  (?>=) :: SValue r -> SValue r -> SValue r
  infixl 4 ?>=
  (?==) :: SValue r -> SValue r -> SValue r
  infixl 3 ?==
  (?!=) :: SValue r -> SValue r -> SValue r
  infixl 3 ?!=

type NamedArgs r = [(SVariable r, SValue r)]
-- Function call with both positional and named arguments
type MixedCall r = Label -> VSType r -> [SValue r] -> NamedArgs r -> SValue r
-- Constructor call with both positional and named arguments
type MixedCtorCall r = VSType r -> [SValue r] -> NamedArgs r -> SValue r
-- Function call with only positional arguments
type PosCall r = Label -> VSType r -> [SValue r] -> SValue r
-- Constructor call with only positional arguments
type PosCtorCall r = VSType r -> [SValue r] -> SValue r

-- for values that can include expressions
class (VariableSym r, ValueSym r) => ValueExpression r where
  -- An inline if-statement, aka the ternary operator.  Inputs:
  -- Condition, True-value, False-value
  inlineIf     :: SValue r -> SValue r -> SValue r -> SValue r
  
  funcAppMixedArgs     ::            MixedCall r
  extFuncAppMixedArgs  :: Library -> MixedCall r
  libFuncAppMixedArgs  :: Library -> MixedCall r

  lambda :: [SVariable r] -> SValue r -> SValue r

  notNull :: SValue r -> SValue r

funcApp          :: (ValueExpression r) =>            PosCall r
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs :: (ValueExpression r) =>            Label -> VSType r ->
  NamedArgs r -> SValue r
funcAppNamedArgs n t = funcAppMixedArgs n t []

extFuncApp       :: (ValueExpression r) => Library -> PosCall r
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp       :: (ValueExpression r) => Library -> PosCall r
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

exists :: (ValueExpression r) => SValue r -> SValue r
exists = notNull

class (ValueSym r) => List r where
  -- | Does any necessary conversions from GOOL's zero-indexed assumptions to
  --   the target language's assumptions
  intToIndex :: SValue r -> SValue r
  -- | Does any necessary conversions from the target language's indexing
  --   assumptions assumptions to GOOL's zero-indexed assumptions
  indexToInt :: SValue r -> SValue r
  -- | Finds the size of a list.
  --   Arguments are: List
  listSize   :: SValue r -> SValue r
  -- | Inserts a value into a list.
  --   Arguments are: List, Index, Value
  listAdd    :: SValue r -> SValue r -> SValue r -> SValue r
  -- | Appens a value to a list.
  --   Arguments are: List, Value
  listAppend :: SValue r -> SValue r -> SValue r
  -- | Gets the value of an index of a list.
  --   Arguments are: List, Index
  listAccess :: SValue r -> SValue r -> SValue r
  -- | Sets the value of an index of a list.
  --   Arguments are: List, Index, Value
  listSet    :: SValue r -> SValue r -> SValue r -> SValue r
  -- | Finds the index of the first occurrence of a value in a list.
  --   Arguments are: List, Value
  indexOf :: SValue r -> SValue r -> SValue r

class (ValueSym r) => Set r where
  -- | Checks membership
  -- Arguments are: Set, Value
  contains :: SValue r -> SValue r -> SValue r
  -- | Inserts a value into a set
  -- Arguments are: Set, Value
  setAdd :: SValue r -> SValue r -> SValue r
  -- | Removes a value from a set
  -- Arguments are: Set, Value
  setRemove :: SValue r -> SValue r -> SValue r
  -- | Removes a value from a set
  -- Arguments are: Set, Set
  setUnion :: SValue r -> SValue r -> SValue r

class (ValueSym r) => InternalList r where
  listSlice'      :: Maybe (SValue r) -> Maybe (SValue r) -> Maybe (SValue r) 
    -> SVariable r -> SValue r -> MSBlock r

-- | Creates a slice of a list and assigns it to a variable.
--   Arguments are: 
--   Variable to assign
--   List to read from
--   (optional) Start index inclusive.
--      (if Nothing, then list start if step > 0, list end if step < 0)
--   (optional) End index exclusive.
--      (if Nothing, then list end if step > 0, list start if step > 0)
--   (optional) Step (if Nothing, then defaults to 1)
listSlice :: (InternalList r) => SVariable r -> SValue r -> Maybe (SValue r) -> 
  Maybe (SValue r) -> Maybe (SValue r) -> MSBlock r
listSlice vnew vold b e s = listSlice' b e s vnew vold

listIndexExists :: (List r, Comparison r) => SValue r -> SValue r -> SValue r
listIndexExists lst index = listSize lst ?> index

at :: (List r) => SValue r -> SValue r -> SValue r
at = listAccess

-- Vector Typeclasses --

type VSThunk a = VS (a (Thunk a))

class ThunkSym r where
  -- K.Type -> K.Type annotation needed because r is not applied here so its
  -- kind cannot be inferred (whereas for Value, r is applied in the type
  -- signature of valueType
  type Thunk (r :: K.Type -> K.Type)

class (VariableSym r, ThunkSym r, StatementSym r) => ThunkAssign r where
  thunkAssign :: SVariable r -> VSThunk r -> MSStatement r

class TypeSym r => VectorType r where
  vecType :: VSType r -> VSType r

class (DeclStatement r) => VectorDecl r where
  -- First argument is size of the vector
  vecDec :: Integer -> SVariable r -> r (Scope r) -> MSStatement r
  vecDecDef :: SVariable r -> r (Scope r) -> [SValue r] -> MSStatement r

class (VariableSym r, ThunkSym r) => VectorThunk r where
  vecThunk :: SVariable r -> VSThunk r

class (ThunkSym r, ValueSym r) => VectorExpression r where
  vecScale :: SValue r -> VSThunk r -> VSThunk r
  vecAdd :: VSThunk r -> VSThunk r -> VSThunk r
  vecIndex :: SValue r -> VSThunk r -> SValue r
  vecDot :: VSThunk r -> VSThunk r -> VSThunk r

type MSStatement a = MS (a (Statement a))

class (ValueSym r) => StatementSym r where
  type Statement r
  valStmt :: SValue r -> MSStatement r -- converts value to statement
  emptyStmt :: MSStatement r
  multi     :: [MSStatement r] -> MSStatement r

class (VariableSym r, StatementSym r) => AssignStatement r where
  (&-=)  :: SVariable r -> SValue r -> MSStatement r
  infixl 1 &-=
  (&+=)  :: SVariable r -> SValue r -> MSStatement r
  infixl 1 &+=
  (&++)  :: SVariable r -> MSStatement r
  infixl 8 &++
  (&--)  :: SVariable r -> MSStatement r
  infixl 8 &--

  assign :: SVariable r -> SValue r -> MSStatement r

(&=) :: (AssignStatement r) => SVariable r -> SValue r -> MSStatement r
infixr 1 &=
(&=) = assign

assignToListIndex :: (StatementSym r, VariableValue r, List r) => SVariable r 
  -> SValue r -> SValue r -> MSStatement r
assignToListIndex lst index v = valStmt $ listSet (valueOf lst) index v

class (VariableSym r, StatementSym r, ScopeSym r) => DeclStatement r where
  varDec       :: SVariable r -> r (Scope r) -> MSStatement r
  varDecDef    :: SVariable r -> r (Scope r) -> SValue r -> MSStatement r
  -- First argument is size of the list
  listDec      :: Integer -> SVariable r -> r (Scope r) -> MSStatement r
  listDecDef   :: SVariable r -> r (Scope r) -> [SValue r] -> MSStatement r
  setDec       :: SVariable r -> r (Scope r) -> MSStatement r
  setDecDef    :: SVariable r -> r (Scope r) -> SValue r -> MSStatement r
  -- First argument is size of the array
  arrayDec     :: Integer -> SVariable r -> r (Scope r) -> MSStatement r
  arrayDecDef  :: SVariable r -> r (Scope r) -> [SValue r] -> MSStatement r
  constDecDef  :: SVariable r -> r (Scope r) -> SValue r -> MSStatement r
  funcDecDef   :: SVariable r -> r (Scope r) -> [SVariable r] -> MSBody r
    -> MSStatement r


class (VariableSym r, StatementSym r) => IOStatement r where
  print      :: SValue r -> MSStatement r
  printLn    :: SValue r -> MSStatement r
  printStr   :: String -> MSStatement r
  printStrLn :: String -> MSStatement r

  -- First argument is file handle, second argument is value to print
  printFile      :: SValue r -> SValue r -> MSStatement r
  printFileLn    :: SValue r -> SValue r -> MSStatement r
  printFileStr   :: SValue r -> String -> MSStatement r
  printFileStrLn :: SValue r -> String -> MSStatement r

  getInput         :: SVariable r -> MSStatement r
  discardInput     :: MSStatement r
  getFileInput     :: SValue r -> SVariable r -> MSStatement r
  discardFileInput :: SValue r -> MSStatement r

  openFileR :: SVariable r -> SValue r -> MSStatement r
  openFileW :: SVariable r -> SValue r -> MSStatement r
  openFileA :: SVariable r -> SValue r -> MSStatement r
  closeFile :: SValue r -> MSStatement r

  getFileInputLine :: SValue r -> SVariable r -> MSStatement r
  discardFileLine  :: SValue r -> MSStatement r
  getFileInputAll  :: SValue r -> SVariable r -> MSStatement r

class (VariableSym r, StatementSym r) => StringStatement r where
  -- Parameters are: char to split on, variable to store result in, string to split
  stringSplit :: Char -> SVariable r -> SValue r -> MSStatement r

  stringListVals  :: [SVariable r] -> SValue r -> MSStatement r
  stringListLists :: [SVariable r] -> SValue r -> MSStatement r

type VSFunction a = VS (a (Function a))

class (ValueSym r) => FunctionSym r where
  type Function r

-- The three lists are inputs, outputs, and both, respectively
type InOutCall r = Label -> [SValue r] -> [SVariable r] -> [SVariable r] -> 
  MSStatement r

class (VariableSym r, StatementSym r) => FuncAppStatement r where
  inOutCall    ::            InOutCall r
  extInOutCall :: Library -> InOutCall r

class (StatementSym r) => CommentStatement r where
  comment :: Comment -> MSStatement r

class (BodySym r, VariableSym r) => ControlStatement r where
  break :: MSStatement r
  continue :: MSStatement r

  returnStmt :: SValue r -> MSStatement r

  throw :: Label -> MSStatement r

  -- | String of if-else statements.
  --   Arguments: List of predicates and bodies (if this then that),
  --   Body for else branch
  ifCond     :: [(SValue r, MSBody r)] -> MSBody r -> MSStatement r
  switch     :: SValue r -> [(SValue r, MSBody r)] -> MSBody r -> MSStatement r

  ifExists :: SValue r -> MSBody r -> MSBody r -> MSStatement r

  for      :: MSStatement r -> SValue r -> MSStatement r -> MSBody r -> 
    MSStatement r
  -- Iterator variable, start value, end value, step value, loop body
  forRange :: SVariable r -> SValue r -> SValue r -> SValue r -> MSBody r -> 
    MSStatement r
  forEach  :: SVariable r -> SValue r -> MSBody r -> MSStatement r
  while    :: SValue r -> MSBody r -> MSStatement r 

  tryCatch :: MSBody r -> MSBody r -> MSStatement r

  assert :: SValue r -> SValue r -> MSStatement r

ifNoElse :: (ControlStatement r) => [(SValue r, MSBody r)] -> MSStatement r
ifNoElse bs = ifCond bs $ body []

switchAsIf :: (ControlStatement r, Comparison r) => SValue r -> 
  [(SValue r, MSBody r)] -> MSBody r -> MSStatement r
switchAsIf v = ifCond . map (first (v ?==))

class VisibilitySym r where
  type Visibility r
  private :: r (Visibility r)
  public  :: r (Visibility r)

type MSParameter a = MS (a (Parameter a))

class (VariableSym r) => ParameterSym r where
  type Parameter r
  param :: SVariable r -> MSParameter r
  pointerParam :: SVariable r -> MSParameter r

type SMethod a = MS (a (Method a))

-- The three lists are inputs, outputs, and both, respectively
type InOutFunc r = [SVariable r] -> [SVariable r] -> [SVariable r] -> 
  MSBody r -> SMethod r
-- Parameters are: brief description of function, input descriptions and 
-- variables, output descriptions and variables, descriptions and variables 
-- for parameters that are both input and output, function body
type DocInOutFunc r = String -> [(String, SVariable r)] -> 
  [(String, SVariable r)] -> [(String, SVariable r)] -> MSBody r -> SMethod r

class (BodySym r, ParameterSym r, VisibilitySym r) => MethodSym r
  where
  type Method r
  docMain :: MSBody r -> SMethod r

  function :: Label -> r (Visibility r) -> VSType r -> [MSParameter r] -> 
    MSBody r -> SMethod r
  mainFunction  :: MSBody r -> SMethod r
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> SMethod r -> SMethod r

  inOutFunc :: Label -> r (Visibility r) -> InOutFunc r
  docInOutFunc :: Label -> r (Visibility r) -> DocInOutFunc r

-- Utility

convType :: (TypeSym r) => CodeType -> VSType r
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

convScope :: (ScopeSym r) => ScopeData -> r (Scope r)
convScope (SD {scopeTag = Global}) = global
convScope (SD {scopeTag = Local}) = local
