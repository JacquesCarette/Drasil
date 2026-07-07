{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, MSBody, MSBlock, VSFunction, VSBinder, SVariable, SValue,
  MSParameter, SMethod, NamedArgs, MixedCall, MixedCtorCall, PosCall,
  PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, UnRepr(..), BodySym(..), bodyStatements, oneLiner, BlockSym(..),
  TypeSym(..), TypeElim(..), getTypeString, VariableSym(..), ScopeSym(..),
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

class (UnRepr r TypeData, TypeElim r tp, AssignStatement r tp smt,
  DeclStatement r tp smt, IOStatement r tp smt, StringStatement r tp smt,
  FunctionSym r tp, FuncAppStatement r tp smt, CommentStatement r tp smt,
  ControlStatement r tp smt, InternalList r tp, Argument r tp, Literal r tp,
  MathConstant r tp, VariableValue r tp, CommandLineArgs r tp,
  NumericExpression r tp, BooleanExpression r tp, Comparison r tp,
  ValueExpression r tp, IndexTranslator r tp, Array r tp, List r tp smt,
  Set r tp, VariableElim r tp, MethodSym r tp vis smt, ScopeSym r,
  BinderSym r tp, Reference r tp, VariableValue r tp
  ) => SharedProg r tp vis smt

-- Shared between OO and Procedural --

class UnRepr repr contents where
  unRepr :: repr contents -> contents

type MSBody a = MS (a (Body a))

class (BlockSym r tp smt) => BodySym r tp smt where
  type Body r
  body           :: [MSBlock r] -> MSBody r

  addComments :: Label -> MSBody r -> MSBody r

bodyStatements :: (BodySym r tp smt) => [MS (r smt)] -> MSBody r
bodyStatements sts = body [block sts]

oneLiner :: (BodySym r tp smt) => MS (r smt) -> MSBody r
oneLiner tp = bodyStatements [tp]

type MSBlock a = MS (a (Block a))

class (StatementSym r tp smt) => BlockSym r tp smt where
  type Block r
  block   :: [MS (r smt)] -> MSBlock r

class TypeSym r tp | r -> tp where
  bool          :: VS (r tp)
  int           :: VS (r tp) -- This is 32-bit signed ints except in Python,
                            -- which has unlimited precision ints; and Julia,
                            -- Which defaults to 64-bit signed ints
  float         :: VS (r tp)
  double        :: VS (r tp)
  char          :: VS (r tp)
  string        :: VS (r tp)
  infile        :: VS (r tp)
  outfile       :: VS (r tp)
  referenceType :: VS (r tp) -> VS (r tp)
  listType      :: VS (r tp) -> VS (r tp)
  setType       :: VS (r tp) -> VS (r tp)
  arrayType     :: VS (r tp) -> VS (r tp)
  innerType     :: VS (r tp) -> VS (r tp)
  funcType      :: [VS (r tp)] -> VS (r tp) -> VS (r tp)
  void          :: VS (r tp)

-- TODO [Brandon Bosman, 06/09/2026]: Think about separating GOOL and GProc implementations of this
-- | A helper function for extracting the String representation from an `r TypeData`
getTypeString :: (UnRepr r TypeData) => r TypeData -> String
getTypeString = typeString . unRepr

class ScopeSym r where
  global :: r ScopeData -- Definite global scope
  mainFn :: r ScopeData -- Main program - either main function or global scope
  local  :: r ScopeData -- Definite local scope

type SVariable a = VS (a (Variable a))

class (TypeSym r tp) => VariableSym r tp where
  type Variable r
  -- | An instance- or function-level variable, separate from its instance (i.e. `v`, not `o.v`)
  var       :: Label -> VS (r tp) -> SVariable r
  -- | An instance- or function-level constant, separate from its instance (i.e. `v`, not `o.v`)
  constant  :: Label -> VS (r tp) -> SVariable r
  -- | An instance- or module-level variable from an external library.
  -- Given library `Lib`, variable name `v`, and variable type `t`,
  -- it performs the necessary imports and creates `Lib.v`
  extVar    :: Library -> Label -> VS (r tp) -> SVariable r

class (VariableSym r tp) => VariableElim r tp where
  variableName :: r (Variable r) -> String
  variableType :: r (Variable r) -> r tp

listVar :: (VariableSym r tp) => Label -> VS (r tp) -> SVariable r
listVar n t = var n (listType t)

listOf :: (VariableSym r tp) => Label -> VS (r tp) -> SVariable r
listOf = listVar

type SValue a = VS (a (Value a))

class (TypeSym r tp) => ValueSym r tp where
  type Value r
  valueType :: r (Value r) -> r tp

class (TypeSym r tp) => TypeElim r tp where
  getCodeType :: r tp -> CodeType

class (ValueSym r tp) => Argument r tp where
  pointerArg :: SValue r -> SValue r

class (ValueSym r tp) => Literal r tp where
  litTrue   :: SValue r
  litFalse  :: SValue r
  litChar   :: Char -> SValue r
  litDouble :: Double -> SValue r
  litFloat  :: Float -> SValue r
  litInt    :: Integer -> SValue r
  litString :: String -> SValue r
  litArray  :: VS (r tp) -> [SValue r] -> SValue r
  litList   :: VS (r tp) -> [SValue r] -> SValue r
  litSet    :: VS (r tp) -> [SValue r] -> SValue r

litZero :: (Literal r tp, TypeElim r tp) => VS (r tp) -> SValue r
litZero t = do
  t' <- t
  case getCodeType t' of
    Integer -> litInt 0
    Float -> litFloat 0
    Double -> litDouble 0
    _ -> error "litZero expects a numeric type"

class (ValueSym r tp) => MathConstant r tp where
  pi :: SValue r

class (VariableSym r tp, ValueSym r tp) => VariableValue r tp where
  valueOf       :: SVariable r -> SValue r

class (ValueSym r tp) => CommandLineArgs r tp where
  arg          :: Integer -> SValue r
  argsList     :: SValue r
  argExists    :: Integer -> SValue r

class (ValueSym r tp) => NumericExpression r tp where
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

class (ValueSym r tp) => BooleanExpression r tp where
  (?!)  :: SValue r -> SValue r
  infixr 6 ?! -- Boolean 'not'
  (?&&) :: SValue r -> SValue r -> SValue r
  infixl 2 ?&&
  (?||) :: SValue r -> SValue r -> SValue r
  infixl 1 ?||

class (ValueSym r tp) => Comparison r tp where
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

type NamedArgs r tp = [(SVariable r, SValue r)]
-- Function call with both positional and named arguments
type MixedCall r tp = Label -> VS (r tp) -> [SValue r] -> NamedArgs r tp -> SValue r
-- Constructor call with both positional and named arguments
type MixedCtorCall r tp = VS (r tp) -> [SValue r] -> NamedArgs r tp -> SValue r
-- Function call with only positional arguments
type PosCall r tp = Label -> VS (r tp) -> [SValue r] -> SValue r
-- Constructor call with only positional arguments
type PosCtorCall r tp = VS (r tp) -> [SValue r] -> SValue r

type VSBinder a = VS (a BinderD)

class (TypeSym r tp) => BinderSym r tp where
  binder :: Label -> VS (r tp) -> VSBinder r

class (BinderSym r tp) => BinderElim r tp where
  binderName :: r BinderD -> String
  binderType :: r BinderD -> r tp

-- for values that can include expressions
class (VariableSym r tp, ValueSym r tp) => ValueExpression r tp where
  -- An inline if-statement, aka the ternary operator.  Inputs:
  -- Condition, True-value, False-value
  inlineIf     :: SValue r -> SValue r -> SValue r -> SValue r

  funcAppMixedArgs     ::            MixedCall r tp
  extFuncAppMixedArgs  :: Library -> MixedCall r tp
  libFuncAppMixedArgs  :: Library -> MixedCall r tp

  lambda :: [VSBinder r] -> SValue r -> SValue r

  notNull :: SValue r -> SValue r

funcApp          :: (ValueExpression r tp) =>            PosCall r tp
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs :: (ValueExpression r tp) =>            Label -> VS (r tp) ->
  NamedArgs r tp -> SValue r
funcAppNamedArgs n t = funcAppMixedArgs n t []

extFuncApp       :: (ValueExpression r tp) => Library -> PosCall r tp
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp       :: (ValueExpression r tp) => Library -> PosCall r tp
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

exists :: (ValueExpression r tp) => SValue r -> SValue r
exists = notNull

class (ValueSym r tp) => IndexTranslator r tp where
  -- | Does any necessary conversions from GOOL's zero-indexed assumptions to
  --   the target language's assumptions
  intToIndex :: SValue r -> SValue r
  -- | Does any necessary conversions from the target language's indexing
  --   assumptions assumptions to GOOL's zero-indexed assumptions
  indexToInt :: SValue r -> SValue r
  -- | Finds the size of a list.
  --   Arguments are: List

class (TypeSym r tp, ValueSym r tp) => Reference r tp where
  -- | Given a value, convert it to a reference to that value
  makeRef :: SValue r -> SValue r
  -- | Given a value that may be a reference type,
  -- apply any necessary dereference operation.
  maybeDeref :: SValue r -> SValue r

class (IndexTranslator r tp) => Array r tp where
  -- TODO [Brandon Bosman, 05/19/2026]: Change return type to SValue
  -- | Given array `a` and index `i`, creates `a[i]`
  arrayElem :: SValue r -> SValue r -> SVariable r
  -- TODO [Brandon Bosman, 06/03/2026]: Consider switching to a polymorphic `length`
  -- for Array, List, and Set
  -- | Given an array, return its length
  arrayLength :: SValue r -> SValue r
  -- TODO [Brandon Bosman, 05/21/2026]: Consider switching this to a polymorphic `copy`,
  -- more like how `print` currently works
  -- | Given a source array, create a (shallow) copy of it
  arrayCopy :: SValue r -> SValue r

class (IndexTranslator r tp, StatementSym r tp smt) => List r tp smt where
  listSize   :: SValue r -> SValue r
  -- | Inserts a value into a list.
  --   Arguments are: List, Index, Value
  listAdd    :: SValue r -> SValue r -> SValue r -> MS (r smt)
  -- | Appens a value to a list.
  --   Arguments are: List, Value
  listAppend :: SValue r -> SValue r -> MS (r smt)
  -- | Gets the value of an index of a list.
  --   Arguments are: List, Index
  listAccess :: SValue r -> SValue r -> SValue r
  -- | Sets the value of an index of a list.
  --   Arguments are: List, Index, Value
  listSet    :: SValue r -> SValue r -> SValue r -> MS (r smt)
  -- | Finds the index of the first occurrence of a value in a list.
  --   Arguments are: List, Value
  indexOf :: SValue r -> SValue r -> SValue r

class (ValueSym r tp) => Set r tp where
  -- | Checks membership
  -- Arguments are: Set, Value
  contains :: SValue r -> SValue r -> SValue r
  -- | Inserts a value into a set
  -- Arguments are: Set, Value
  setAdd :: SValue r -> SValue r -> SValue r -- TODO [Brandon Bosman, 06/24/2026]: Make this a Statement
  -- | Removes a value from a set
  -- Arguments are: Set, Value
  setRemove :: SValue r -> SValue r -> SValue r -- TODO [Brandon Bosman, 06/24/2026]: Make this a SStatement
  -- | Removes a value from a set
  -- Arguments are: Set, Set
  setUnion :: SValue r -> SValue r -> SValue r -- TODO [Brandon Bosman, 06/24/2026]: See if we should make this a Statement

class (ValueSym r tp) => InternalList r tp where
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
listSlice :: (InternalList r tp) => SVariable r -> SValue r ->
  Maybe (SValue r) -> Maybe (SValue r) -> Maybe (SValue r) -> MSBlock r
listSlice vnew vold b e tp = listSlice' b e tp vnew vold

listIndexExists :: (List r tp smt, Comparison r tp) => SValue r -> SValue r -> SValue r
listIndexExists lst index = listSize lst ?> index

at :: (List r tp smt) => SValue r -> SValue r -> SValue r
at = listAccess

class (ValueSym r tp) => StatementSym r tp smt | r -> smt where
  valStmt :: SValue r -> MS (r smt) -- converts value to statement
  emptyStmt :: MS (r smt)
  multi     :: [MS (r smt)] -> MS (r smt)

class (VariableSym r tp, StatementSym r tp smt) => AssignStatement r tp smt where
  (&-=)  :: SVariable r -> SValue r -> MS (r smt)
  infixl 1 &-=
  (&+=)  :: SVariable r -> SValue r -> MS (r smt)
  infixl 1 &+=
  (&++)  :: SVariable r -> MS (r smt)
  infixl 8 &++
  (&--)  :: SVariable r -> MS (r smt)
  infixl 8 &--

  assign :: SVariable r -> SValue r -> MS (r smt)

(&=) :: (AssignStatement r tp smt) => SVariable r -> SValue r -> MS (r smt)
infixr 1 &=
(&=) = assign

class (VariableSym r tp, StatementSym r tp smt, ScopeSym r) => DeclStatement r tp smt where
  -- | Declare a variable without giving it a value.
  -- Not for use with arrays; use `arrayDec` instead.
  varDec       :: SVariable r -> r ScopeData -> MS (r smt)
  -- | Declare a variable and give it a value.
  -- Not for use with arrays; use `arrayDecDef` instead.
  varDecDef    :: SVariable r -> r ScopeData -> SValue r -> MS (r smt)
  -- First argument is size of the list
  listDec      :: Integer -> SVariable r -> r ScopeData -> MS (r smt)
  listDecDef   :: SVariable r -> r ScopeData -> [SValue r] -> MS (r smt)
  setDec       :: SVariable r -> r ScopeData -> MS (r smt)
  setDecDef    :: SVariable r -> r ScopeData -> SValue r -> MS (r smt)
  -- First argument is size of the array
  arrayDec     :: Integer -> SVariable r -> r ScopeData -> MS (r smt)
  arrayDecDef  :: SVariable r -> r ScopeData -> [SValue r] -> MS (r smt)
  constDecDef  :: SVariable r -> r ScopeData -> SValue r -> MS (r smt)
  funcDecDef   :: SVariable r -> r ScopeData -> [SVariable r] -> MSBody r
    -> MS (r smt)

class (VariableSym r tp, StatementSym r tp smt) => IOStatement r tp smt where
  print      :: SValue r -> MS (r smt)
  printLn    :: SValue r -> MS (r smt)
  printStr   :: String -> MS (r smt)
  printStrLn :: String -> MS (r smt)

  -- First argument is file handle, second argument is value to print
  printFile      :: SValue r -> SValue r -> MS (r smt)
  printFileLn    :: SValue r -> SValue r -> MS (r smt)
  printFileStr   :: SValue r -> String -> MS (r smt)
  printFileStrLn :: SValue r -> String -> MS (r smt)

  getInput         :: SVariable r -> MS (r smt)
  discardInput     :: MS (r smt)
  getFileInput     :: SValue r -> SVariable r -> MS (r smt)
  discardFileInput :: SValue r -> MS (r smt)

  openFileR :: SVariable r -> SValue r -> MS (r smt)
  openFileW :: SVariable r -> SValue r -> MS (r smt)
  openFileA :: SVariable r -> SValue r -> MS (r smt)
  closeFile :: SValue r -> MS (r smt)

  getFileInputLine :: SValue r -> SVariable r -> MS (r smt)
  discardFileLine  :: SValue r -> MS (r smt)
  getFileInputAll  :: SValue r -> SVariable r -> MS (r smt)

class (VariableSym r tp, StatementSym r tp smt) => StringStatement r tp smt where
  -- Parameters are: char to split on, variable to store result in, string to split
  stringSplit :: Char -> SVariable r -> SValue r -> MS (r smt)
  stringListVals  :: [SVariable r] -> SValue r -> MS (r smt)
  -- Given a list of variables and a value containing a list of strings,
  -- assign the ith element of hte list of strings into the ith variable
  stringListLists :: [SVariable r] -> SValue r -> MS (r smt)

type VSFunction a = VS (a (Function a))

class (ValueSym r tp) => FunctionSym r tp where
  type Function r

-- The three lists are inputs, outputs, and both, respectively
type InOutCall r smt = Label -> [SValue r] -> [SVariable r] -> [SVariable r] ->
  MS (r smt)

class (VariableSym r tp, StatementSym r tp smt) => FuncAppStatement r tp smt where
  inOutCall    ::            InOutCall r smt
  extInOutCall :: Library -> InOutCall r smt

class (StatementSym r tp smt) => CommentStatement r tp smt where
  comment :: String -> MS (r smt)

class (BodySym r tp smt, VariableSym r tp) => ControlStatement r tp smt where
  break :: MS (r smt)
  continue :: MS (r smt)

  returnStmt :: SValue r -> MS (r smt)

  throw :: Label -> MS (r smt)

  -- | String of if-else statements.
  --   Arguments: List of predicates and bodies (if this then that),
  --   Body for else branch
  ifCond     :: [(SValue r, MSBody r)] -> MSBody r -> MS (r smt)
  switch     :: SValue r -> [(SValue r, MSBody r)] -> MSBody r -> MS (r smt)

  ifExists :: SValue r -> MSBody r -> MSBody r -> MS (r smt)

  for      :: MS (r smt) -> SValue r -> MS (r smt) -> MSBody r ->
    MS (r smt)
  -- Iterator variable, start value, end value, step value, loop body
  forRange :: SVariable r -> SValue r -> SValue r -> SValue r -> MSBody r ->
    MS (r smt)
  forEach  :: SVariable r -> SValue r -> MSBody r -> MS (r smt)
  while    :: SValue r -> MSBody r -> MS (r smt)

  tryCatch :: MSBody r -> MSBody r -> MS (r smt)

  assert :: SValue r -> SValue r -> MS (r smt)

ifNoElse :: (ControlStatement r tp smt) => [(SValue r, MSBody r)] -> MS (r smt)
ifNoElse bs = ifCond bs $ body []

switchAsIf :: (ControlStatement r tp smt, Comparison r tp) => SValue r ->
  [(SValue r, MSBody r)] -> MSBody r -> MS (r smt)
switchAsIf v = ifCond . map (first (v ?==))

class VisibilitySym r vis | r -> vis where
  private :: r vis
  public  :: r vis

type MSParameter a = MS (a (Parameter a))

class (VariableSym r tp) => ParameterSym r tp where
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

class (BodySym r tp smt, ParameterSym r tp, VisibilitySym r vis) => MethodSym r tp vis smt
  where
  type Method r
  docMain :: MSBody r -> SMethod r

  function :: Label -> r vis -> VS (r tp) -> [MSParameter r] ->
    MSBody r -> SMethod r
  mainFunction  :: MSBody r -> SMethod r
  -- Parameters are: function description, parameter descriptions,
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> SMethod r -> SMethod r

  inOutFunc :: Label -> r vis -> InOutFunc r
  docInOutFunc :: Label -> r vis -> DocInOutFunc r

-- Utility

convType :: (TypeSym r tp) => CodeType -> VS (r tp)
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
