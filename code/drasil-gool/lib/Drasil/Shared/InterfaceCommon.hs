{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, Body, Block, VSBinder, Variable, SVariable, Value, NamedArgs,
  MixedCall, MixedCtorCall, PosCall, PosCtorCall, InOutCall, InOutFunc,
  DocInOutFunc,
  -- Typeclasses
  UnRepr(..), BodySym(..), bodyStatements, oneLiner, BlockSym(..), TypeSym(..),
  TypeElim(..), getTypeString, VariableSym(..), ScopeSym(..), convScope,
  VariableElim(..), listOf, listVar, ValueSym(..), Argument(..), Literal(..),
  litZero, MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp, libFuncApp, exists,
  IndexTranslator(..), Reference(..), Array(..), List(..), ListStatement(..),
  Set(..), NativeVector(..), InternalList(..), listSlice, listIndexExists, at,
  EmptyStatement(..), MultiStatement(..), ValueStatement(..),
  AssignStatement(..), (&=), DeclStatement(..), PrintConsole(..),
  ReadConsole(..), FileHandling(..), PrintFile(..), ReadFile(..),
  StringStatement(..), FuncAppStatement(..), CommentStatement(..),
  ControlStatement(..), ifNoElse, switchAsIf, VisibilitySym(..),
  ParameterSym(..), MethodSym(..), BinderSym(..), BinderElim(..), convType
  ) where

import Data.Bifunctor (first)
import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.AST (ScopeData(..), ScopeTag(..), TypeData(..), BinderD,
  ParamData, VarData, ValData)
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.State (MS, VS)

type Label = String
type Library = String

-- In relation to GOOL, the type variable r can be considered as short for "representation"

-- Functions in GOOL's interface beginning with "ext" are to be used to access items from other modules in the same program/project
-- Functions in GOOL's interface beginning with "lib" are to be used to access items from different libraries/projects

class UnRepr repr contents where
  unRepr :: repr contents -> contents

type Body = Doc

-- | Class for representing a `Body`, which is basically a lexical scope of code.
-- Examples include a function body, the branch(es) of an `if`-statement, etc.
class BodySym r  bod block | r -> bod block where
  -- | Given a list of `block`s, create a `Body` of them.
  body           :: [MS (r block)] -> MS (r bod)
  -- | Given a comment and a body, add the comment as a header for the body.
  addComments :: Label -> MS (r bod) -> MS (r bod)

bodyStatements
  :: (BlockSym r block stmt, BodySym r bod block) => [MS (r stmt)] -> MS (r bod)
bodyStatements sts = body [block sts]

oneLiner
  :: (BlockSym r block stmt, BodySym r bod block) => MS (r stmt) -> MS (r bod)
oneLiner tp = bodyStatements [tp]

type Block = Doc

-- | Class for representing a `block` of code.
-- A `block` is a series of statements grouped together,
-- not for use by the compiler/interpreter
-- but to improve readability of the generated code.
-- See the bottom of page 2 of Brook's GOOL paper from 2020 for more details.
class BlockSym r block stmt | r -> block stmt where
  block   :: [MS (r stmt)] -> MS (r block)

-- | Class for representing a type.
class TypeSym r typ | r -> typ where
  bool          :: VS (r typ)
  int           :: VS (r typ) -- This is 32-bit signed ints except in Python,
                            -- which has unlimited precision ints; and Julia,
                            -- Which defaults to 64-bit signed ints
  float         :: VS (r typ)
  double        :: VS (r typ)
  char          :: VS (r typ)
  string        :: VS (r typ)
  infile        :: VS (r typ)
  outfile       :: VS (r typ)
  referenceType :: VS (r typ) -> VS (r typ)
  listType      :: VS (r typ) -> VS (r typ)
  setType       :: VS (r typ) -> VS (r typ)
  arrayType     :: VS (r typ) -> VS (r typ)
  innerType     :: VS (r typ) -> VS (r typ)
  funcType      :: [VS (r typ)] -> VS (r typ) -> VS (r typ)
  void          :: VS (r typ)

-- TODO [Brandon Bosman, 06/09/2026]: Think about separating GOOL and GProc implementations of this
-- | A helper function for extracting the String representation from an `r typ`
getTypeString :: (UnRepr r TypeData) => r TypeData -> String
getTypeString = typeString . unRepr

-- TODO [Brandon Bosman, 07/22/2026]: rework this so that GOOL handles scopes automatically
-- | Class for representing the lexical scope of a variable.
-- Currently only differentiates `global` and `local`,
-- allowing individual renderers to define which of them the main function is.
class ScopeSym r where
  global :: r ScopeData -- Definite global scope
  mainFn :: r ScopeData -- Main program - either main function or global scope
  local  :: r ScopeData -- Definite local scope

type Variable = VarData
type SVariable a = VS (a Variable)

-- | Class for representing variables.
class VariableSym r typ | r -> typ where
  -- | An instance- or function-level variable, separate from its instance (i.e. `v`, not `o.v`)
  var       :: Label -> VS (r typ) -> SVariable r
  -- | An instance- or function-level constant, separate from its instance (i.e. `v`, not `o.v`)
  constant  :: Label -> VS (r typ) -> SVariable r
  -- | An instance- or module-level variable from an external library.
  -- Given library `Lib`, variable name `v`, and variable type `t`,
  -- it performs the necessary imports and creates `Lib.v`
  extVar    :: Library -> Label -> VS (r typ) -> SVariable r

class VariableElim r typ | r -> typ where
  variableName :: r Variable -> String
  variableType :: r Variable -> r typ

listVar
  :: (TypeSym r typ, VariableSym r typ)
  => Label -> VS (r typ) -> SVariable r
listVar n t = var n (listType t)

listOf
  :: (TypeSym r typ, VariableSym r typ)
  => Label -> VS (r typ) -> SVariable r
listOf = listVar

type Value = ValData

-- | Class for representing a value.
class ValueSym r typ | r -> typ where
  valueType :: r Value -> r typ

class TypeElim r typ | r -> typ where
  getCodeType :: r typ -> CodeType

class Argument r where
  pointerArg :: VS (r Value) -> VS (r Value)

class Literal r typ | r -> typ where
  litTrue   :: VS (r Value)
  litFalse  :: VS (r Value)
  litChar   :: Char -> VS (r Value)
  litDouble :: Double -> VS (r Value)
  litFloat  :: Float -> VS (r Value)
  litInt    :: Integer -> VS (r Value)
  litString :: String -> VS (r Value)
  litArray  :: VS (r typ) -> [VS (r Value)] -> VS (r Value)
  litList   :: VS (r typ) -> [VS (r Value)] -> VS (r Value)
  litSet    :: VS (r typ) -> [VS (r Value)] -> VS (r Value)

litZero :: (Literal r typ, TypeElim r typ) => VS (r typ) -> VS (r Value)
litZero t = do
  t' <- t
  case getCodeType t' of
    Integer -> litInt 0
    Float -> litFloat 0
    Double -> litDouble 0
    _ -> error "litZero expects a numeric type"

class MathConstant r where
  pi :: VS (r Value)

class VariableValue r where
  valueOf       :: SVariable r -> VS (r Value)

class CommandLineArgs r where
  arg          :: Integer -> VS (r Value)
  argsList     :: VS (r Value)
  argExists    :: Integer -> VS (r Value)

class NumericExpression r where
  (#~)  :: VS (r Value) -> VS (r Value)
  infixl 8 #~ -- Negation
  (#/^) :: VS (r Value) -> VS (r Value)
  infixl 7 #/^ -- Square root
  (#|)  :: VS (r Value) -> VS (r Value)
  infixl 7 #| -- Absolute value
  (#+)  :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 5 #+
  (#-)  :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 5 #-
  (#*)  :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 6 #*
  (#/)  :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 6 #/
  (#%)  :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 6 #% -- Modulo
  (#^)  :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 7 #^ -- Exponentiation

  log    :: VS (r Value) -> VS (r Value)
  ln     :: VS (r Value) -> VS (r Value)
  exp    :: VS (r Value) -> VS (r Value)
  sin    :: VS (r Value) -> VS (r Value)
  cos    :: VS (r Value) -> VS (r Value)
  tan    :: VS (r Value) -> VS (r Value)
  csc    :: VS (r Value) -> VS (r Value)
  sec    :: VS (r Value) -> VS (r Value)
  cot    :: VS (r Value) -> VS (r Value)
  arcsin :: VS (r Value) -> VS (r Value)
  arccos :: VS (r Value) -> VS (r Value)
  arctan :: VS (r Value) -> VS (r Value)
  floor  :: VS (r Value) -> VS (r Value)
  ceil   :: VS (r Value) -> VS (r Value)

class BooleanExpression r where
  (?!)  :: VS (r Value) -> VS (r Value)
  infixr 6 ?! -- Boolean 'not'
  (?&&) :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 2 ?&&
  (?||) :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 1 ?||

class Comparison r where
  (?<)  :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 4 ?<
  (?<=) :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 4 ?<=
  (?>)  :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 4 ?>
  (?>=) :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 4 ?>=
  (?==) :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 3 ?==
  (?!=) :: VS (r Value) -> VS (r Value) -> VS (r Value)
  infixl 3 ?!=

type NamedArgs r = [(SVariable r, VS (r Value))]
-- Function call with both positional and named arguments
type MixedCall r typ = Label -> VS (r typ) -> [VS (r Value)] -> NamedArgs r -> VS (r Value)
-- Constructor call with both positional and named arguments
type MixedCtorCall r typ = VS (r typ) -> [VS (r Value)] -> NamedArgs r -> VS (r Value)
-- Function call with only positional arguments
type PosCall r typ = Label -> VS (r typ) -> [VS (r Value)] -> VS (r Value)
-- Constructor call with only positional arguments
type PosCtorCall r typ = VS (r typ) -> [VS (r Value)] -> VS (r Value)

type VSBinder a = VS (a BinderD)

-- | A class for representing a binder, i.e. the binding of a variable name
-- to a type, scope, etc.
-- As of July 2026, integration of this typeclass is still WIP, blocked
-- by issues with our variable map.
class BinderSym r typ | r -> typ where
  binder :: Label -> VS (r typ) -> VSBinder r

class BinderElim r typ | r -> typ where
  binderName :: r BinderD -> String
  binderType :: r BinderD -> r typ

-- | A class for representing values that can include expressions
class ValueExpression r typ | r -> typ where
  -- An inline if-statement, aka the ternary operator.  Inputs:
  -- Condition, True-value, False-value
  inlineIf     :: VS (r Value) -> VS (r Value) -> VS (r Value) -> VS (r Value)

  funcAppMixedArgs     ::            MixedCall r typ
  extFuncAppMixedArgs  :: Library -> MixedCall r typ
  libFuncAppMixedArgs  :: Library -> MixedCall r typ

  lambda :: [VSBinder r] -> VS (r Value) -> VS (r Value)

  notNull :: VS (r Value) -> VS (r Value)

funcApp          :: (ValueExpression r typ) => PosCall r typ
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs
  :: (ValueExpression r typ)
  => Label -> VS (r typ) -> NamedArgs r -> VS (r Value)
funcAppNamedArgs n t = funcAppMixedArgs n t []

extFuncApp       :: (ValueExpression r typ) => Library -> PosCall r typ
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp       :: (ValueExpression r typ) => Library -> PosCall r typ
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

exists :: (ValueExpression r typ) => VS (r Value) -> VS (r Value)
exists = notNull

-- | Helper class for representing the conversion between integers and array indices.
-- GOOL is 0-indexed, so languages like Julia that are not 0-indexed
-- need to convert between integers and indices.
class IndexTranslator r where
  -- | Does any necessary conversions from GOOL's zero-indexed assumptions to
  --   the target language's assumptions
  intToIndex :: VS (r Value) -> VS (r Value)
  -- | Does any necessary conversions from the target language's indexing
  --   assumptions assumptions to GOOL's zero-indexed assumptions
  indexToInt :: VS (r Value) -> VS (r Value)

-- | A class for representing references.
-- By "reference" we basically mean "C++ pointer" or "OCaml reference".
class Reference r where
  -- | Given a value, convert it to a reference to that value
  makeRef :: VS (r Value) -> VS (r Value)
  -- | Given a value that may be a reference type,
  -- apply any necessary dereference operation.
  maybeDeref :: VS (r Value) -> VS (r Value)

class Array r where
  -- TODO [Brandon Bosman, 05/19/2026]: Change return type to VS Value
  -- | Given array `a` and index `i`, creates `a[i]`
  arrayElem :: VS (r Value) -> VS (r Value) -> SVariable r
  -- TODO [Brandon Bosman, 06/03/2026]: Consider switching to a polymorphic `length`
  -- for Array, List, and Set
  -- | Given an array, return its length
  arrayLength :: VS (r Value) -> VS (r Value)
  -- TODO [Brandon Bosman, 05/21/2026]: Consider switching this to a polymorphic `copy`,
  -- more like how `print` currently works
  -- | Given a source array, create a (shallow) copy of it
  arrayCopy :: VS (r Value) -> VS (r Value)

class List r where
  -- | Finds the size of a list.
  --   Arguments are: List
  listSize   :: VS (r Value) -> VS (r Value)
  -- | Gets the value of an index of a list.
  --   Arguments are: List, Index
  listAccess :: VS (r Value) -> VS (r Value) -> VS (r Value)
  -- | Finds the index of the first occurrence of a value in a list.
  --   Arguments are: List, Value
  indexOf :: VS (r Value) -> VS (r Value) -> VS (r Value)

class ListStatement r stmt | r -> stmt where
  -- | Inserts a value into a list.
  --   Arguments are: List, Index, Value
  listAdd    :: VS (r Value) -> VS (r Value) -> VS (r Value) -> MS (r stmt)
  -- | Appens a value to a list.
  --   Arguments are: List, Value
  listAppend :: VS (r Value) -> VS (r Value) -> MS (r stmt)
  -- | Sets the value of an index of a list.
  --   Arguments are: List, Index, Value
  listSet    :: VS (r Value) -> VS (r Value) -> VS (r Value) -> MS (r stmt)

class Set r where
  -- | Checks membership
  -- Arguments are: Set, Value
  contains :: VS (r Value) -> VS (r Value) -> VS (r Value)
  -- | Inserts a value into a set
  -- Arguments are: Set, Value
  setAdd :: VS (r Value) -> VS (r Value) -> VS (r Value) -- TODO [Brandon Bosman, 06/24/2026]: Make this a Statement
  -- | Removes a value from a set
  -- Arguments are: Set, Value
  setRemove :: VS (r Value) -> VS (r Value) -> VS (r Value) -- TODO [Brandon Bosman, 06/24/2026]: Make this a SStatement
  -- | Removes a value from a set
  -- Arguments are: Set, Set
  setUnion :: VS (r Value) -> VS (r Value) -> VS (r Value) -- TODO [Brandon Bosman, 06/24/2026]: See if we should make this a Statement

-- | Vector operations for languages with native vector support (e.g. MATLAB,
--   Julia). Expression-based: every operation takes and returns 'VS Value's, so
--   operations compose like math (e.g. @vecAdd (vecScale s a) b@).
--   Vectors have their own 'vecType' and 'litVec' so callers don't depend on
--   how vectors are represented; these default to 'listType' and 'litList'.
class NativeVector r typ | r -> typ where
  -- | The type of a vector with the given element type.
  --   For most languages it will be 'listType'
  vecType :: VS (r typ) -> VS (r typ)
  -- | A vector literal with the given element type and elements.
  --   For most languages it will be 'litList'.
  litVec :: VS (r typ) -> [VS (r Value)] -> VS (r Value)
  -- | Scales a vector by a scalar.
  --   Arguments are: Scalar, Vector
  vecScale :: VS (r Value) -> VS (r Value) -> VS (r Value)
  -- | Adds two vectors elementwise.
  --   Arguments are: Vector, Vector
  vecAdd :: VS (r Value) -> VS (r Value) -> VS (r Value)
  -- | Gets the element of a vector at an index.
  --   Arguments are: Vector, Index
  vecIndex :: VS (r Value) -> VS (r Value) -> VS (r Value)
  -- | Dot product of two vectors (returns a scalar).
  --   Arguments are: Vector, Vector
  vecDot :: VS (r Value) -> VS (r Value) -> VS (r Value)
  -- | Euclidean norm (magnitude) of a vector (returns a scalar).
  --   Argument is: Vector
  vecMag :: VS (r Value) -> VS (r Value)
  -- | Unit vector in the direction of a vector (returns a vector).
  --   Argument is: Vector
  vecUnit :: VS (r Value) -> VS (r Value)

class InternalList r block | r -> block where
  listSlice'      :: Maybe (VS (r Value)) -> Maybe (VS (r Value)) -> Maybe (VS (r Value))
    -> SVariable r -> VS (r Value) -> MS (r block)

-- | Creates a slice of a list and assigns it to a variable.
--   Arguments are:
--   Variable to assign
--   List to read from
--   (optional) Start index inclusive.
--      (if Nothing, then list start if step > 0, list end if step < 0)
--   (optional) End index exclusive.
--      (if Nothing, then list end if step > 0, list start if step > 0)
--   (optional) Step (if Nothing, then defaults to 1)
listSlice
  :: (InternalList r block)
  => SVariable r
  -> VS (r Value)
  -> Maybe (VS (r Value))
  -> Maybe (VS (r Value))
  -> Maybe (VS (r Value))
  -> MS (r block)
listSlice vnew vold b e tp = listSlice' b e tp vnew vold

listIndexExists
  :: (List r, Comparison r)
  => VS (r Value) -> VS (r Value) -> VS (r Value)
listIndexExists lst index = listSize lst ?> index

at :: (List r) => VS (r Value) -> VS (r Value) -> VS (r Value)
at = listAccess

class EmptyStatement r stmt | r -> stmt where
  -- | Empty statement
  emptyStmt :: MS (r stmt)

class MultiStatement r stmt | r -> stmt where
  -- | Consolidates a list of statements into a single statement
  multi     :: [MS (r stmt)] -> MS (r stmt)

class ValueStatement r stmt | r -> stmt where
  -- | Converts a value to statement
  valStmt :: VS (r Value) -> MS (r stmt)

class AssignStatement r stmt | r -> stmt where
  (&-=)  :: SVariable r -> VS (r Value) -> MS (r stmt)
  infixl 1 &-=
  (&+=)  :: SVariable r -> VS (r Value) -> MS (r stmt)
  infixl 1 &+=
  (&++)  :: SVariable r -> MS (r stmt)
  infixl 8 &++
  (&--)  :: SVariable r -> MS (r stmt)
  infixl 8 &--

  assign :: SVariable r -> VS (r Value) -> MS (r stmt)

(&=) :: (AssignStatement r stmt) => SVariable r -> VS (r Value) -> MS (r stmt)
infixr 1 &=
(&=) = assign

class DeclStatement r stmt bod | r -> stmt bod where
  -- | Declare a variable without giving it a value.
  -- Not for use with arrays; use `arrayDec` instead.
  varDec       :: SVariable r -> r ScopeData -> MS (r stmt)
  -- | Declare a variable and give it a value.
  -- Not for use with arrays; use `arrayDecDef` instead.
  varDecDef    :: SVariable r -> r ScopeData -> VS (r Value) -> MS (r stmt)
  -- | Given the size of the list, the variable to store the list in,
  -- and the scope of the variable, declare a list of the given size.
  listDec      :: Integer -> SVariable r -> r ScopeData -> MS (r stmt)
  listDecDef   :: SVariable r -> r ScopeData -> [VS (r Value)] -> MS (r stmt)
  setDec       :: SVariable r -> r ScopeData -> MS (r stmt)
  setDecDef    :: SVariable r -> r ScopeData -> VS (r Value) -> MS (r stmt)
  -- | Given the size of the aray, the default value to fill the array with,
  -- the variable to store the array in, and the scope of the variable,
  -- declare an array of the given size.
  arrayDec     :: Integer -> VS (r Value) -> SVariable r -> r ScopeData -> MS (r stmt)
  arrayDecDef  :: SVariable r -> r ScopeData -> [VS (r Value)] -> MS (r stmt)
  constDecDef  :: SVariable r -> r ScopeData -> VS (r Value) -> MS (r stmt)
  funcDecDef   :: SVariable r -> r ScopeData -> [SVariable r] -> MS (r bod)
    -> MS (r stmt)

class PrintConsole r stmt | r -> stmt where
  print      :: VS (r Value) -> MS (r stmt)
  printLn    :: VS (r Value) -> MS (r stmt)
  -- TODO [Brandon Bosman, 07/23/2026]: Could these be helpers?
  printStr   :: String -> MS (r stmt)
  printStrLn :: String -> MS (r stmt)

class ReadConsole r stmt | r -> stmt where
  getInput         :: SVariable r -> MS (r stmt)
  discardInput     :: MS (r stmt)

class FileHandling r stmt | r -> stmt where
  openFileR :: SVariable r -> VS (r Value) -> MS (r stmt)
  openFileW :: SVariable r -> VS (r Value) -> MS (r stmt)
  openFileA :: SVariable r -> VS (r Value) -> MS (r stmt)
  closeFile :: VS (r Value) -> MS (r stmt)

class PrintFile r stmt | r -> stmt where
  -- | Given the file handle and value to print, print the value to the file.
  printFile      :: VS (r Value) -> VS (r Value) -> MS (r stmt)
  printFileLn    :: VS (r Value) -> VS (r Value) -> MS (r stmt)
  printFileStr   :: VS (r Value) -> String -> MS (r stmt)
  printFileStrLn :: VS (r Value) -> String -> MS (r stmt)

class ReadFile r stmt | r -> stmt where
  getFileInput     :: VS (r Value) -> SVariable r -> MS (r stmt)
  discardFileInput :: VS (r Value) -> MS (r stmt)
  getFileInputLine :: VS (r Value) -> SVariable r -> MS (r stmt)
  discardFileLine  :: VS (r Value) -> MS (r stmt)
  getFileInputAll  :: VS (r Value) -> SVariable r -> MS (r stmt)

class StringStatement r stmt | r -> stmt where
  -- | Given a char to split on, variable to store result in, and string to split,
  -- generates a statement splitting the string into a list of strings
  -- delimited by the char.
  stringSplit :: Char -> SVariable r -> VS (r Value) -> MS (r stmt)
  stringListVals  :: [SVariable r] -> VS (r Value) -> MS (r stmt)
  -- | Given a list of variables and a value containing a list of strings,
  -- assign the ith element of the list of strings into the ith variable
  stringListLists :: [SVariable r] -> VS (r Value) -> MS (r stmt)

-- The three lists are inputs, outputs, and both, respectively
type InOutCall r stmt =
     Label
  -> [VS (r Value)]
  -> [SVariable r]
  -> [SVariable r]
  -> MS (r stmt)

class FuncAppStatement r stmt | r -> stmt where
  inOutCall    ::            InOutCall r stmt
  extInOutCall :: Library -> InOutCall r stmt

class CommentStatement r stmt | r -> stmt where
  comment :: String -> MS (r stmt)

class ControlStatement r stmt bod | r -> stmt bod where
  break :: MS (r stmt)
  continue :: MS (r stmt)

  returnStmt :: VS (r Value) -> MS (r stmt)

  throw :: Label -> MS (r stmt)

  -- | String of if-else statements.
  --   Arguments: List of predicates and bodies (if this then that),
  --   Body for else branch
  ifCond     :: [(VS (r Value), MS (r bod))] -> MS (r bod) -> MS (r stmt)
  switch     :: VS (r Value) -> [(VS (r Value), MS (r bod))] -> MS (r bod) -> MS (r stmt)

  ifExists :: VS (r Value) -> MS (r bod) -> MS (r bod) -> MS (r stmt)

  for      :: MS (r stmt) -> VS (r Value) -> MS (r stmt) -> MS (r bod) ->
    MS (r stmt)
  -- Iterator variable, start value, end value, step value, loop body
  forRange :: SVariable r -> VS (r Value) -> VS (r Value) -> VS (r Value) -> MS (r bod) ->
    MS (r stmt)
  forEach  :: SVariable r -> VS (r Value) -> MS (r bod) -> MS (r stmt)
  while    :: VS (r Value) -> MS (r bod) -> MS (r stmt)

  tryCatch :: MS (r bod) -> MS (r bod) -> MS (r stmt)

  assert :: VS (r Value) -> VS (r Value) -> MS (r stmt)

ifNoElse
  :: (BodySym r bod block, ControlStatement r stmt bod)
  => [(VS (r Value), MS (r bod))] -> MS (r stmt)
ifNoElse bs = ifCond bs $ body []

switchAsIf
  :: (ControlStatement r stmt bod, Comparison r)
  => VS (r Value) -> [(VS (r Value), MS (r bod))] -> MS (r bod) -> MS (r stmt)
switchAsIf v = ifCond . map (first (v ?==))

-- TODO [Brandon Bosman, 07/22/2026]: move this to InterfaceGOOL
-- | A class for representing "Visibility", of a class member,
-- i.e. whether it is public or private.
class VisibilitySym r vis | r -> vis where
  private :: r vis
  public  :: r vis

-- | A class for representing function/method parameters.
class ParameterSym r where
  param :: SVariable r -> MS (r ParamData)
  -- | A parameter that is an "alias" type, e.g. a C++ reference.
  -- This is a minor hack, to get around us not having/wanting
  -- "alias types" in GOOL.
  pointerParam :: SVariable r -> MS (r ParamData)

-- The three lists are inputs, outputs, and both, respectively
type InOutFunc r mthd bod = [SVariable r] -> [SVariable r] -> [SVariable r] ->
  MS (r bod) -> MS (r mthd)
-- Parameters are: brief description of function, input descriptions and
-- variables, output descriptions and variables, descriptions and variables
-- for parameters that are both input and output, function body
type DocInOutFunc r mthd bod = String -> [(String, SVariable r)] ->
  [(String, SVariable r)] -> [(String, SVariable r)] -> MS (r bod) -> MS (r mthd)

-- | A class for representing functions/methods.
-- Usually 'MethodData' is used for the representation.
class MethodSym r vis typ mthd bod | r -> vis typ mthd bod
  where
  docMain :: MS (r bod) -> MS (r mthd)

  function :: Label -> r vis -> VS (r typ) -> [MS (r ParamData)] ->
    MS (r bod) -> MS (r mthd)
  mainFunction  :: MS (r bod) -> MS (r mthd)
  -- Parameters are: function description, parameter descriptions,
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> MS (r mthd) -> MS (r mthd)

  inOutFunc :: Label -> r vis -> InOutFunc r mthd bod
  docInOutFunc :: Label -> r vis -> DocInOutFunc r mthd bod

-- Utility

convType :: (TypeSym r typ) => CodeType -> VS (r typ)
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
