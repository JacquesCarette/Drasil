{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, MSBody, MSBlock, VSFunction, VSBinder, SVariable, SValue,
  MSStatement, MSParameter, SMethod, NamedArgs, MixedCall, MixedCtorCall,
  PosCall, PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, UnRepr(..), BodySym(..), bodyStatements, oneLiner, BlockSym(..),
  TypeSym(..), getCodeType, getTypeString, VariableSym(..), ScopeSym(..),
  convScope, VariableElim(..), listOf, listVar, ValueSym(..), Argument(..),
  Literal(..), litZero, MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp, libFuncApp, exists,
  IndexTranslator(..), Reference(..), Array(..), List(..), Set(..),
  InternalList(..), listSlice, listIndexExists, at, StatementSym(..),
  AssignStatement(..), (&=), DeclStatement(..), IOStatement(..),
  StringStatement(..), FunctionSym(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), ifNoElse, switchAsIf,
  VisibilitySym(..), ParameterSym(..), MethodSym(..), BinderSym(..),
  BinderElim(..), convType
  ) where

import Data.Bifunctor (first)

import Drasil.Shared.AST (ScopeData(..), ScopeTag(..), TypeData(..), BinderD)
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.State (MS, VS)

type Label = String
type Library = String

-- In relation to GOOL, the type variable r can be considered as short for "representation"

-- Functions in GOOL's interface beginning with "ext" are to be used to access items from other modules in the same program/project
-- Functions in GOOL's interface beginning with "lib" are to be used to access items from different libraries/projects

-- TODO [Brandon Bosman, 06/09/2026]: UnRepr can be removed from SharedProg
-- if we can root out its use from drasil-code

class (UnRepr r TypeData, TypeElim r s, AssignStatement r s, DeclStatement r s,
  IOStatement r s, StringStatement r s, FunctionSym r s, FuncAppStatement r s,
  CommentStatement r s, ControlStatement r s, InternalList r s, Argument r s,
  Literal r s, MathConstant r s, VariableValue r s, CommandLineArgs r s,
  NumericExpression r s, BooleanExpression r s, Comparison r s,
  ValueExpression r s, IndexTranslator r s, Array r s, List r s, Set r s,
  VariableElim r s, MethodSym r s, ScopeSym r, BinderSym r s, Reference r s
  ) => SharedProg r s

-- Shared between OO and Procedural --

class UnRepr repr contents where
  unRepr :: repr contents -> contents

type MSBody a = MS (a (Body a))

class (BlockSym r s) => BodySym r s where
  type Body r
  body           :: [MSBlock r] -> MSBody r

  addComments :: Label -> MSBody r -> MSBody r

bodyStatements :: (BodySym r s) => [MSStatement r] -> MSBody r
bodyStatements sts = body [block sts]

oneLiner :: (BodySym r s) => MSStatement r -> MSBody r
oneLiner s = bodyStatements [s]

type MSBlock a = MS (a (Block a))

class (StatementSym r s) => BlockSym r s where
  type Block r
  block   :: [MSStatement r] -> MSBlock r

class TypeSym r s | r -> s where
  bool          :: VS (r s)
  int           :: VS (r s) -- This is 32-bit signed ints except in Python,
                            -- which has unlimited precision ints; and Julia,
                            -- Which defaults to 64-bit signed ints
  float         :: VS (r s)
  double        :: VS (r s)
  char          :: VS (r s)
  string        :: VS (r s)
  infile        :: VS (r s)
  outfile       :: VS (r s)
  referenceType :: VS (r s) -> VS (r s)
  listType      :: VS (r s) -> VS (r s)
  setType       :: VS (r s) -> VS (r s)
  arrayType     :: VS (r s) -> VS (r s)
  innerType     :: VS (r s) -> VS (r s)
  funcType      :: [VS (r s)] -> VS (r s) -> VS (r s)
  void          :: VS (r s)

-- TODO [Brandon Bosman, 06/09/2026]: Think about separating GOOL and GProc implementations of this
-- | A helper function for extracting the String representation from an `r TypeData`
getTypeString :: (UnRepr r TypeData) => r TypeData -> String
getTypeString = typeString . unRepr

class ScopeSym r where
  global :: r ScopeData -- Definite global scope
  mainFn :: r ScopeData -- Main program - either main function or global scope
  local  :: r ScopeData -- Definite local scope

type SVariable a = VS (a (Variable a))

class (TypeSym r s) => VariableSym r s where
  type Variable r
  -- | An instance- or function-level variable, separate from its instance (i.e. `v`, not `o.v`)
  var       :: Label -> VS (r s) -> SVariable r
  -- | An instance- or function-level constant, separate from its instance (i.e. `v`, not `o.v`)
  constant  :: Label -> VS (r s) -> SVariable r
  -- | An instance- or module-level variable from an external library.
  -- Given library `Lib`, variable name `v`, and variable type `t`,
  -- it performs the necessary imports and creates `Lib.v`
  extVar    :: Library -> Label -> VS (r s) -> SVariable r

class (VariableSym r s) => VariableElim r s where
  variableName :: r (Variable r) -> String
  variableType :: r (Variable r) -> r s

listVar :: (VariableSym r s) => Label -> VS (r s) -> SVariable r
listVar n t = var n (listType t)

listOf :: (VariableSym r s) => Label -> VS (r s) -> SVariable r
listOf = listVar

type SValue a = VS (a (Value a))

class (TypeSym r s) => ValueSym r s where
  type Value r
  valueType :: r (Value r) -> r s

class (TypeSym r s) => TypeElim r s where
  getCodeType :: r s -> CodeType

class (ValueSym r s) => Argument r s where
  pointerArg :: SValue r -> SValue r

class (ValueSym r s) => Literal r s where
  litTrue   :: SValue r
  litFalse  :: SValue r
  litChar   :: Char -> SValue r
  litDouble :: Double -> SValue r
  litFloat  :: Float -> SValue r
  litInt    :: Integer -> SValue r
  litString :: String -> SValue r
  litArray  :: VS (r s) -> [SValue r] -> SValue r
  litList   :: VS (r s) -> [SValue r] -> SValue r
  litSet    :: VS (r s) -> [SValue r] -> SValue r

litZero :: (Literal r s, TypeElim r s) => VS (r s) -> SValue r
litZero t = do
  t' <- t
  case getCodeType t' of
    Integer -> litInt 0
    Float -> litFloat 0
    Double -> litDouble 0
    _ -> error "litZero expects a numeric type"

class (ValueSym r s) => MathConstant r s where
  pi :: SValue r

class (VariableSym r s, ValueSym r s) => VariableValue r s where
  valueOf       :: SVariable r -> SValue r

class (ValueSym r s) => CommandLineArgs r s where
  arg          :: Integer -> SValue r
  argsList     :: SValue r
  argExists    :: Integer -> SValue r

class (ValueSym r s) => NumericExpression r s where
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

class (ValueSym r s) => BooleanExpression r s where
  (?!)  :: SValue r -> SValue r
  infixr 6 ?! -- Boolean 'not'
  (?&&) :: SValue r -> SValue r -> SValue r
  infixl 2 ?&&
  (?||) :: SValue r -> SValue r -> SValue r
  infixl 1 ?||

class (ValueSym r s) => Comparison r s where
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

type NamedArgs r s = [(SVariable r, SValue r)]
-- Function call with both positional and named arguments
type MixedCall r s = Label -> VS (r s) -> [SValue r] -> NamedArgs r s -> SValue r
-- Constructor call with both positional and named arguments
type MixedCtorCall r s = VS (r s) -> [SValue r] -> NamedArgs r s -> SValue r
-- Function call with only positional arguments
type PosCall r s = Label -> VS (r s) -> [SValue r] -> SValue r
-- Constructor call with only positional arguments
type PosCtorCall r s = VS (r s) -> [SValue r] -> SValue r

type VSBinder a = VS (a BinderD)

class (TypeSym r s) => BinderSym r s where
  binder :: Label -> VS (r s) -> VSBinder r

class (BinderSym r s) => BinderElim r s where
  binderName :: r BinderD -> String
  binderType :: r BinderD -> r s

-- for values that can include expressions
class (VariableSym r s, ValueSym r s) => ValueExpression r s where
  -- An inline if-statement, aka the ternary operator.  Inputs:
  -- Condition, True-value, False-value
  inlineIf     :: SValue r -> SValue r -> SValue r -> SValue r

  funcAppMixedArgs     ::            MixedCall r s
  extFuncAppMixedArgs  :: Library -> MixedCall r s
  libFuncAppMixedArgs  :: Library -> MixedCall r s

  lambda :: [VSBinder r] -> SValue r -> SValue r

  notNull :: SValue r -> SValue r

funcApp          :: (ValueExpression r s) =>            PosCall r s
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs :: (ValueExpression r s) =>            Label -> VS (r s) ->
  NamedArgs r s -> SValue r
funcAppNamedArgs n t = funcAppMixedArgs n t []

extFuncApp       :: (ValueExpression r s) => Library -> PosCall r s
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp       :: (ValueExpression r s) => Library -> PosCall r s
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

exists :: (ValueExpression r s) => SValue r -> SValue r
exists = notNull

class (ValueSym r s) => IndexTranslator r s where
  -- | Does any necessary conversions from GOOL's zero-indexed assumptions to
  --   the target language's assumptions
  intToIndex :: SValue r -> SValue r
  -- | Does any necessary conversions from the target language's indexing
  --   assumptions assumptions to GOOL's zero-indexed assumptions
  indexToInt :: SValue r -> SValue r
  -- | Finds the size of a list.
  --   Arguments are: List

class (TypeSym r s, ValueSym r s) => Reference r s where
  -- | Given a value, convert it to a reference to that value
  makeRef :: SValue r -> SValue r
  -- | Given a value that may be a reference type,
  -- apply any necessary dereference operation.
  maybeDeref :: SValue r -> SValue r

class (IndexTranslator r s) => Array r s where
  -- TODO [Brandon Bosman, 05/19/2026]: Change return type to SValue
  -- | Given array `a` and index `i`, creates `a[i]`
  arrayElem :: SValue r -> SVariable r -> SVariable r
  -- TODO [Brandon Bosman, 06/03/2026]: Consider switching to a polymorphic `length`
  -- for Array, List, and Set
  -- | Given an array, return its length
  arrayLength :: SValue r -> SValue r
  -- TODO [Brandon Bosman, 05/21/2026]: Consider switching this to a polymorphic `copy`,
  -- more like how `print` currently works
  -- | Given a source array, create a (shallow) copy of it
  arrayCopy :: SValue r -> SValue r

class (IndexTranslator r s) => List r s where
  listSize   :: SValue r -> SValue r
  -- | Inserts a value into a list.
  --   Arguments are: List, Index, Value
  listAdd    :: SValue r -> SValue r -> SValue r -> MSStatement r
  -- | Appens a value to a list.
  --   Arguments are: List, Value
  listAppend :: SValue r -> SValue r -> MSStatement r
  -- | Gets the value of an index of a list.
  --   Arguments are: List, Index
  listAccess :: SValue r -> SValue r -> SValue r
  -- | Sets the value of an index of a list.
  --   Arguments are: List, Index, Value
  listSet    :: SValue r -> SValue r -> SValue r -> MSStatement r
  -- | Finds the index of the first occurrence of a value in a list.
  --   Arguments are: List, Value
  indexOf :: SValue r -> SValue r -> SValue r

class (ValueSym r s) => Set r s where
  -- | Checks membership
  -- Arguments are: Set, Value
  contains :: SValue r -> SValue r -> SValue r
  -- | Inserts a value into a set
  -- Arguments are: Set, Value
  setAdd :: SValue r -> SValue r -> SValue r -- TODO [Brandon Bosman, 06/24/2026]: Make this MSStatement
  -- | Removes a value from a set
  -- Arguments are: Set, Value
  setRemove :: SValue r -> SValue r -> SValue r -- TODO [Brandon Bosman, 06/24/2026]: Make this MSStatement
  -- | Removes a value from a set
  -- Arguments are: Set, Set
  setUnion :: SValue r -> SValue r -> SValue r -- TODO [Brandon Bosman, 06/24/2026]: See if we should make this MSStatement

class (ValueSym r s) => InternalList r s where
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
listSlice :: (InternalList r s) => SVariable r -> SValue r -> Maybe (SValue r) ->
  Maybe (SValue r) -> Maybe (SValue r) -> MSBlock r
listSlice vnew vold b e s = listSlice' b e s vnew vold

listIndexExists :: (List r s, Comparison r s) => SValue r -> SValue r -> SValue r
listIndexExists lst index = listSize lst ?> index

at :: (List r s) => SValue r -> SValue r -> SValue r
at = listAccess

type MSStatement a = MS (a (Statement a))

class (ValueSym r s) => StatementSym r s where
  type Statement r
  valStmt :: SValue r -> MSStatement r -- converts value to statement
  emptyStmt :: MSStatement r
  multi     :: [MSStatement r] -> MSStatement r

class (VariableSym r s, StatementSym r s) => AssignStatement r s where
  (&-=)  :: SVariable r -> SValue r -> MSStatement r
  infixl 1 &-=
  (&+=)  :: SVariable r -> SValue r -> MSStatement r
  infixl 1 &+=
  (&++)  :: SVariable r -> MSStatement r
  infixl 8 &++
  (&--)  :: SVariable r -> MSStatement r
  infixl 8 &--

  assign :: SVariable r -> SValue r -> MSStatement r

(&=) :: (AssignStatement r s) => SVariable r -> SValue r -> MSStatement r
infixr 1 &=
(&=) = assign

class (VariableSym r s, StatementSym r s, ScopeSym r) => DeclStatement r s where
  -- | Declare a variable without giving it a value.
  -- Not for use with arrays; use `arrayDec` instead.
  varDec       :: SVariable r -> r ScopeData -> MSStatement r
  -- | Declare a variable and give it a value.
  -- Not for use with arrays; use `arrayDecDef` instead.
  varDecDef    :: SVariable r -> r ScopeData -> SValue r -> MSStatement r
  -- First argument is size of the list
  listDec      :: Integer -> SVariable r -> r ScopeData -> MSStatement r
  listDecDef   :: SVariable r -> r ScopeData -> [SValue r] -> MSStatement r
  setDec       :: SVariable r -> r ScopeData -> MSStatement r
  setDecDef    :: SVariable r -> r ScopeData -> SValue r -> MSStatement r
  -- First argument is size of the array
  arrayDec     :: Integer -> SVariable r -> r ScopeData -> MSStatement r
  arrayDecDef  :: SVariable r -> r ScopeData -> [SValue r] -> MSStatement r
  constDecDef  :: SVariable r -> r ScopeData -> SValue r -> MSStatement r
  funcDecDef   :: SVariable r -> r ScopeData -> [SVariable r] -> MSBody r
    -> MSStatement r

class (VariableSym r s, StatementSym r s) => IOStatement r s where
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

class (VariableSym r s, StatementSym r s) => StringStatement r s where
  -- Parameters are: char to split on, variable to store result in, string to split
  stringSplit :: Char -> SVariable r -> SValue r -> MSStatement r
  stringListVals  :: [SVariable r] -> SValue r -> MSStatement r
  -- Given a list of variables and a value containing a list of strings,
  -- assign the ith element of hte list of strings into the ith variable
  stringListLists :: [SVariable r] -> SValue r -> MSStatement r

type VSFunction a = VS (a (Function a))

class (ValueSym r s) => FunctionSym r s where
  type Function r

-- The three lists are inputs, outputs, and both, respectively
type InOutCall r = Label -> [SValue r] -> [SVariable r] -> [SVariable r] ->
  MSStatement r

class (VariableSym r s, StatementSym r s) => FuncAppStatement r s where
  inOutCall    ::            InOutCall r
  extInOutCall :: Library -> InOutCall r

class (StatementSym r s) => CommentStatement r s where
  comment :: String -> MSStatement r

class (BodySym r s, VariableSym r s) => ControlStatement r s where
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

ifNoElse :: (ControlStatement r s) => [(SValue r, MSBody r)] -> MSStatement r
ifNoElse bs = ifCond bs $ body []

switchAsIf :: (ControlStatement r s, Comparison r s) => SValue r ->
  [(SValue r, MSBody r)] -> MSBody r -> MSStatement r
switchAsIf v = ifCond . map (first (v ?==))

class VisibilitySym r where
  type Visibility r
  private :: r (Visibility r)
  public  :: r (Visibility r)

type MSParameter a = MS (a (Parameter a))

class (VariableSym r s) => ParameterSym r s where
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

class (BodySym r s, ParameterSym r s, VisibilitySym r) => MethodSym r s
  where
  type Method r
  docMain :: MSBody r -> SMethod r

  function :: Label -> r (Visibility r) -> VS (r s) -> [MSParameter r] ->
    MSBody r -> SMethod r
  mainFunction  :: MSBody r -> SMethod r
  -- Parameters are: function description, parameter descriptions,
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> SMethod r -> SMethod r

  inOutFunc :: Label -> r (Visibility r) -> InOutFunc r
  docInOutFunc :: Label -> r (Visibility r) -> DocInOutFunc r

-- Utility

convType :: (TypeSym r s) => CodeType -> VS (r s)
convType Boolean = bool
convType Integer = int
convType Float = float
convType Double = double
convType Char = char
convType String = string
convType (Reference t) = referenceType (convType t)
convType (List t) = listType (convType t)
convType (Set t) = setType (convType t)
convType (Array t) = arrayType (convType t)
convType (Func ps r) = funcType (map convType ps) (convType r)
convType Void = void
convType InFile = infile
convType OutFile = outfile
convType (Object _) = error "Objects not supported"

convScope :: (ScopeSym r) => ScopeData -> r ScopeData
convScope (SD {scopeTag = Global}) = global
convScope (SD {scopeTag = Local}) = local
