{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, Body, Block, VSBinder, Variable, Value, NamedArgs, MixedCall,
  MixedCtorCall, PosCall, PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
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
  VarData, ValData)
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
class ScopeSym r scope | r -> scope where
  global :: r scope -- Definite global scope
  mainFn :: r scope -- Main program - either main function or global scope
  local  :: r scope -- Definite local scope

type Variable = VarData

-- | Class for representing variables.
class VariableSym r typ | r -> typ where
  -- | An instance- or function-level variable, separate from its instance (i.e. `v`, not `o.v`)
  var       :: Label -> VS (r typ) -> VS (r Variable)
  -- | An instance- or function-level constant, separate from its instance (i.e. `v`, not `o.v`)
  constant  :: Label -> VS (r typ) -> VS (r Variable)
  -- | An instance- or module-level variable from an external library.
  -- Given library `Lib`, variable name `v`, and variable type `t`,
  -- it performs the necessary imports and creates `Lib.v`
  extVar    :: Library -> Label -> VS (r typ) -> VS (r Variable)

class VariableElim r typ | r -> typ where
  variableName :: r Variable -> String
  variableType :: r Variable -> r typ

listVar
  :: (TypeSym r typ, VariableSym r typ)
  => Label -> VS (r typ) -> VS (r Variable)
listVar n t = var n (listType t)

listOf
  :: (TypeSym r typ, VariableSym r typ)
  => Label -> VS (r typ) -> VS (r Variable)
listOf = listVar

type Value = ValData

-- | Class for representing a value.
class ValueSym r typ val | r -> typ val where
  valueType :: r val -> r typ

class TypeElim r typ | r -> typ where
  getCodeType :: r typ -> CodeType

class Argument r val | r -> val where
  pointerArg :: VS (r val) -> VS (r val)

class Literal r typ val | r -> typ val where
  litTrue   :: VS (r val)
  litFalse  :: VS (r val)
  litChar   :: Char -> VS (r val)
  litDouble :: Double -> VS (r val)
  litFloat  :: Float -> VS (r val)
  litInt    :: Integer -> VS (r val)
  litString :: String -> VS (r val)
  litArray  :: VS (r typ) -> [VS (r val)] -> VS (r val)
  litList   :: VS (r typ) -> [VS (r val)] -> VS (r val)
  litSet    :: VS (r typ) -> [VS (r val)] -> VS (r val)

litZero :: (Literal r typ val, TypeElim r typ) => VS (r typ) -> VS (r val)
litZero t = do
  t' <- t
  case getCodeType t' of
    Integer -> litInt 0
    Float -> litFloat 0
    Double -> litDouble 0
    _ -> error "litZero expects a numeric type"

class MathConstant r val | r -> val where
  pi :: VS (r val)

class VariableValue r val | r -> val where
  valueOf       :: VS (r Variable) -> VS (r val)

class CommandLineArgs r val | r -> val where
  arg          :: Integer -> VS (r val)
  argsList     :: VS (r val)
  argExists    :: Integer -> VS (r val)

class NumericExpression r val | r -> val where
  (#~)  :: VS (r val) -> VS (r val)
  infixl 8 #~ -- Negation
  (#/^) :: VS (r val) -> VS (r val)
  infixl 7 #/^ -- Square root
  (#|)  :: VS (r val) -> VS (r val)
  infixl 7 #| -- Absolute value
  (#+)  :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 5 #+
  (#-)  :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 5 #-
  (#*)  :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 6 #*
  (#/)  :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 6 #/
  (#%)  :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 6 #% -- Modulo
  (#^)  :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 7 #^ -- Exponentiation

  log    :: VS (r val) -> VS (r val)
  ln     :: VS (r val) -> VS (r val)
  exp    :: VS (r val) -> VS (r val)
  sin    :: VS (r val) -> VS (r val)
  cos    :: VS (r val) -> VS (r val)
  tan    :: VS (r val) -> VS (r val)
  csc    :: VS (r val) -> VS (r val)
  sec    :: VS (r val) -> VS (r val)
  cot    :: VS (r val) -> VS (r val)
  arcsin :: VS (r val) -> VS (r val)
  arccos :: VS (r val) -> VS (r val)
  arctan :: VS (r val) -> VS (r val)
  floor  :: VS (r val) -> VS (r val)
  ceil   :: VS (r val) -> VS (r val)

class BooleanExpression r val | r -> val where
  (?!)  :: VS (r val) -> VS (r val)
  infixr 6 ?! -- Boolean 'not'
  (?&&) :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 2 ?&&
  (?||) :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 1 ?||

class Comparison r val | r -> val where
  (?<)  :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 4 ?<
  (?<=) :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 4 ?<=
  (?>)  :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 4 ?>
  (?>=) :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 4 ?>=
  (?==) :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 3 ?==
  (?!=) :: VS (r val) -> VS (r val) -> VS (r val)
  infixl 3 ?!=

type NamedArgs r val = [(VS (r Variable), VS (r val))]
-- Function call with both positional and named arguments
type MixedCall r typ val = Label -> VS (r typ) -> [VS (r val)] -> NamedArgs r val -> VS (r val)
-- Constructor call with both positional and named arguments
type MixedCtorCall r typ val = VS (r typ) -> [VS (r val)] -> NamedArgs r val -> VS (r val)
-- Function call with only positional arguments
type PosCall r typ val = Label -> VS (r typ) -> [VS (r val)] -> VS (r val)
-- Constructor call with only positional arguments
type PosCtorCall r typ val = VS (r typ) -> [VS (r val)] -> VS (r val)

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
class ValueExpression r typ val | r -> typ val where
  -- An inline if-statement, aka the ternary operator.  Inputs:
  -- Condition, True-value, False-value
  inlineIf     :: VS (r val) -> VS (r val) -> VS (r val) -> VS (r val)

  funcAppMixedArgs     ::            MixedCall r typ val
  extFuncAppMixedArgs  :: Library -> MixedCall r typ val
  libFuncAppMixedArgs  :: Library -> MixedCall r typ val

  lambda :: [VSBinder r] -> VS (r val) -> VS (r val)

  notNull :: VS (r val) -> VS (r val)

funcApp          :: (ValueExpression r typ val) => PosCall r typ val
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs
  :: (ValueExpression r typ val)
  => Label -> VS (r typ) -> NamedArgs r val -> VS (r val)
funcAppNamedArgs n t = funcAppMixedArgs n t []

extFuncApp       :: (ValueExpression r typ val) => Library -> PosCall r typ val
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp       :: (ValueExpression r typ val) => Library -> PosCall r typ val
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

exists :: (ValueExpression r typ val) => VS (r val) -> VS (r val)
exists = notNull

-- | Helper class for representing the conversion between integers and array indices.
-- GOOL is 0-indexed, so languages like Julia that are not 0-indexed
-- need to convert between integers and indices.
class IndexTranslator r val | r -> val where
  -- | Does any necessary conversions from GOOL's zero-indexed assumptions to
  --   the target language's assumptions
  intToIndex :: VS (r val) -> VS (r val)
  -- | Does any necessary conversions from the target language's indexing
  --   assumptions assumptions to GOOL's zero-indexed assumptions
  indexToInt :: VS (r val) -> VS (r val)

-- | A class for representing references.
-- By "reference" we basically mean "C++ pointer" or "OCaml reference".
class Reference r val | r -> val where
  -- | Given a value, convert it to a reference to that value
  makeRef :: VS (r val) -> VS (r val)
  -- | Given a value that may be a reference type,
  -- apply any necessary dereference operation.
  maybeDeref :: VS (r val) -> VS (r val)

class Array r val | r -> val where
  -- TODO [Brandon Bosman, 05/19/2026]: Change return type to VS val
  -- | Given array `a` and index `i`, creates `a[i]`
  arrayElem :: VS (r val) -> VS (r val) -> VS (r Variable)
  -- TODO [Brandon Bosman, 06/03/2026]: Consider switching to a polymorphic `length`
  -- for Array, List, and Set
  -- | Given an array, return its length
  arrayLength :: VS (r val) -> VS (r val)
  -- TODO [Brandon Bosman, 05/21/2026]: Consider switching this to a polymorphic `copy`,
  -- more like how `print` currently works
  -- | Given a source array, create a (shallow) copy of it
  arrayCopy :: VS (r val) -> VS (r val)

class List r val | r -> val where
  -- | Finds the size of a list.
  --   Arguments are: List
  listSize   :: VS (r val) -> VS (r val)
  -- | Gets the value of an index of a list.
  --   Arguments are: List, Index
  listAccess :: VS (r val) -> VS (r val) -> VS (r val)
  -- | Finds the index of the first occurrence of a value in a list.
  --   Arguments are: List, val
  indexOf :: VS (r val) -> VS (r val) -> VS (r val)

class ListStatement r val stmt | r -> val stmt where
  -- | Inserts a value into a list.
  --   Arguments are: List, Index, val
  listAdd    :: VS (r val) -> VS (r val) -> VS (r val) -> MS (r stmt)
  -- | Appens a value to a list.
  --   Arguments are: List, val
  listAppend :: VS (r val) -> VS (r val) -> MS (r stmt)
  -- | Sets the value of an index of a list.
  --   Arguments are: List, Index, val
  listSet    :: VS (r val) -> VS (r val) -> VS (r val) -> MS (r stmt)

class Set r val | r -> val where
  -- | Checks membership
  -- Arguments are: Set, val
  contains :: VS (r val) -> VS (r val) -> VS (r val)
  -- | Inserts a value into a set
  -- Arguments are: Set, val
  setAdd :: VS (r val) -> VS (r val) -> VS (r val) -- TODO [Brandon Bosman, 06/24/2026]: Make this a Statement
  -- | Removes a value from a set
  -- Arguments are: Set, val
  setRemove :: VS (r val) -> VS (r val) -> VS (r val) -- TODO [Brandon Bosman, 06/24/2026]: Make this a SStatement
  -- | Removes a value from a set
  -- Arguments are: Set, Set
  setUnion :: VS (r val) -> VS (r val) -> VS (r val) -- TODO [Brandon Bosman, 06/24/2026]: See if we should make this a Statement

-- | Vector operations for languages with native vector support (e.g. MATLAB,
--   Julia). Expression-based: every operation takes and returns 'VS val's, so
--   operations compose like math (e.g. @vecAdd (vecScale s a) b@).
--   Vectors have their own 'vecType' and 'litVec' so callers don't depend on
--   how vectors are represented; these default to 'listType' and 'litList'.
class NativeVector r typ val | r -> typ val where
  -- | The type of a vector with the given element type.
  --   For most languages it will be 'listType'
  vecType :: VS (r typ) -> VS (r typ)
  -- | A vector literal with the given element type and elements.
  --   For most languages it will be 'litList'.
  litVec :: VS (r typ) -> [VS (r val)] -> VS (r val)
  -- | Scales a vector by a scalar.
  --   Arguments are: Scalar, Vector
  vecScale :: VS (r val) -> VS (r val) -> VS (r val)
  -- | Adds two vectors elementwise.
  --   Arguments are: Vector, Vector
  vecAdd :: VS (r val) -> VS (r val) -> VS (r val)
  -- | Gets the element of a vector at an index.
  --   Arguments are: Vector, Index
  vecIndex :: VS (r val) -> VS (r val) -> VS (r val)
  -- | Dot product of two vectors (returns a scalar).
  --   Arguments are: Vector, Vector
  vecDot :: VS (r val) -> VS (r val) -> VS (r val)
  -- | Euclidean norm (magnitude) of a vector (returns a scalar).
  --   Argument is: Vector
  vecMag :: VS (r val) -> VS (r val)
  -- | Unit vector in the direction of a vector (returns a vector).
  --   Argument is: Vector
  vecUnit :: VS (r val) -> VS (r val)

class InternalList r val block | r -> val block where
  listSlice'      :: Maybe (VS (r val)) -> Maybe (VS (r val)) -> Maybe (VS (r val))
    -> VS (r Variable) -> VS (r val) -> MS (r block)

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
  :: (InternalList r val block)
  => VS (r Variable)
  -> VS (r val)
  -> Maybe (VS (r val))
  -> Maybe (VS (r val))
  -> Maybe (VS (r val))
  -> MS (r block)
listSlice vnew vold b e tp = listSlice' b e tp vnew vold

listIndexExists
  :: (List r val, Comparison r val)
  => VS (r val) -> VS (r val) -> VS (r val)
listIndexExists lst index = listSize lst ?> index

at :: (List r val) => VS (r val) -> VS (r val) -> VS (r val)
at = listAccess

class EmptyStatement r stmt | r -> stmt where
  -- | Empty statement
  emptyStmt :: MS (r stmt)

class MultiStatement r stmt | r -> stmt where
  -- | Consolidates a list of statements into a single statement
  multi     :: [MS (r stmt)] -> MS (r stmt)

class ValueStatement r val stmt | r -> val stmt where
  -- | Converts a value to statement
  valStmt :: VS (r val) -> MS (r stmt)

class AssignStatement r val stmt | r -> val stmt where
  (&-=)  :: VS (r Variable) -> VS (r val) -> MS (r stmt)
  infixl 1 &-=
  (&+=)  :: VS (r Variable) -> VS (r val) -> MS (r stmt)
  infixl 1 &+=
  (&++)  :: VS (r Variable) -> MS (r stmt)
  infixl 8 &++
  (&--)  :: VS (r Variable) -> MS (r stmt)
  infixl 8 &--

  assign :: VS (r Variable) -> VS (r val) -> MS (r stmt)

(&=) :: (AssignStatement r val stmt) => VS (r Variable) -> VS (r val) -> MS (r stmt)
infixr 1 &=
(&=) = assign

class DeclStatement r scope val stmt bod | r -> scope val stmt bod where
  -- | Declare a variable without giving it a value.
  -- Not for use with arrays; use `arrayDec` instead.
  varDec       :: VS (r Variable) -> r scope -> MS (r stmt)
  -- | Declare a variable and give it a value.
  -- Not for use with arrays; use `arrayDecDef` instead.
  varDecDef    :: VS (r Variable) -> r scope -> VS (r val) -> MS (r stmt)
  -- | Given the size of the list, the variable to store the list in,
  -- and the scope of the variable, declare a list of the given size.
  listDec      :: Integer -> VS (r Variable) -> r scope -> MS (r stmt)
  listDecDef   :: VS (r Variable) -> r scope -> [VS (r val)] -> MS (r stmt)
  setDec       :: VS (r Variable) -> r scope -> MS (r stmt)
  setDecDef    :: VS (r Variable) -> r scope -> VS (r val) -> MS (r stmt)
  -- | Given the size of the aray, the default value to fill the array with,
  -- the variable to store the array in, and the scope of the variable,
  -- declare an array of the given size.
  arrayDec     :: Integer -> VS (r val) -> VS (r Variable) -> r scope -> MS (r stmt)
  arrayDecDef  :: VS (r Variable) -> r scope -> [VS (r val)] -> MS (r stmt)
  constDecDef  :: VS (r Variable) -> r scope -> VS (r val) -> MS (r stmt)
  funcDecDef   :: VS (r Variable) -> r scope -> [VS (r Variable)] -> MS (r bod)
    -> MS (r stmt)

class PrintConsole r val stmt | r -> val stmt where
  print      :: VS (r val) -> MS (r stmt)
  printLn    :: VS (r val) -> MS (r stmt)
  -- TODO [Brandon Bosman, 07/23/2026]: Could these be helpers?
  printStr   :: String -> MS (r stmt)
  printStrLn :: String -> MS (r stmt)

class ReadConsole r stmt | r -> stmt where
  getInput         :: VS (r Variable) -> MS (r stmt)
  discardInput     :: MS (r stmt)

class FileHandling r val stmt | r -> val stmt where
  openFileR :: VS (r Variable) -> VS (r val) -> MS (r stmt)
  openFileW :: VS (r Variable) -> VS (r val) -> MS (r stmt)
  openFileA :: VS (r Variable) -> VS (r val) -> MS (r stmt)
  closeFile :: VS (r val) -> MS (r stmt)

class PrintFile r val stmt | r -> val stmt where
  -- | Given the file handle and value to print, print the value to the file.
  printFile      :: VS (r val) -> VS (r val) -> MS (r stmt)
  printFileLn    :: VS (r val) -> VS (r val) -> MS (r stmt)
  printFileStr   :: VS (r val) -> String -> MS (r stmt)
  printFileStrLn :: VS (r val) -> String -> MS (r stmt)

class ReadFile r val stmt | r -> val stmt where
  getFileInput     :: VS (r val) -> VS (r Variable) -> MS (r stmt)
  discardFileInput :: VS (r val) -> MS (r stmt)
  getFileInputLine :: VS (r val) -> VS (r Variable) -> MS (r stmt)
  discardFileLine  :: VS (r val) -> MS (r stmt)
  getFileInputAll  :: VS (r val) -> VS (r Variable) -> MS (r stmt)

class StringStatement r val stmt | r -> val stmt where
  -- | Given a char to split on, variable to store result in, and string to split,
  -- generates a statement splitting the string into a list of strings
  -- delimited by the char.
  stringSplit :: Char -> VS (r Variable) -> VS (r val) -> MS (r stmt)
  stringListVals  :: [VS (r Variable)] -> VS (r val) -> MS (r stmt)
  -- | Given a list of variables and a value containing a list of strings,
  -- assign the ith element of the list of strings into the ith variable
  stringListLists :: [VS (r Variable)] -> VS (r val) -> MS (r stmt)

-- The three lists are inputs, outputs, and both, respectively
type InOutCall r val stmt =
     Label
  -> [VS (r val)]
  -> [VS (r Variable)]
  -> [VS (r Variable)]
  -> MS (r stmt)

class FuncAppStatement r val stmt | r -> val stmt where
  inOutCall    ::            InOutCall r val stmt
  extInOutCall :: Library -> InOutCall r val stmt

class CommentStatement r stmt | r -> stmt where
  comment :: String -> MS (r stmt)

class ControlStatement r val stmt bod | r -> val stmt bod where
  break :: MS (r stmt)
  continue :: MS (r stmt)

  returnStmt :: VS (r val) -> MS (r stmt)

  throw :: Label -> MS (r stmt)

  -- | String of if-else statements.
  --   Arguments: List of predicates and bodies (if this then that),
  --   Body for else branch
  ifCond     :: [(VS (r val), MS (r bod))] -> MS (r bod) -> MS (r stmt)
  switch     :: VS (r val) -> [(VS (r val), MS (r bod))] -> MS (r bod) -> MS (r stmt)

  ifExists :: VS (r val) -> MS (r bod) -> MS (r bod) -> MS (r stmt)

  for      :: MS (r stmt) -> VS (r val) -> MS (r stmt) -> MS (r bod) ->
    MS (r stmt)
  -- Iterator variable, start value, end value, step value, loop body
  forRange :: VS (r Variable) -> VS (r val) -> VS (r val) -> VS (r val) -> MS (r bod) ->
    MS (r stmt)
  forEach  :: VS (r Variable) -> VS (r val) -> MS (r bod) -> MS (r stmt)
  while    :: VS (r val) -> MS (r bod) -> MS (r stmt)

  tryCatch :: MS (r bod) -> MS (r bod) -> MS (r stmt)

  assert :: VS (r val) -> VS (r val) -> MS (r stmt)

ifNoElse
  :: (BodySym r bod block, ControlStatement r val stmt bod)
  => [(VS (r val), MS (r bod))] -> MS (r stmt)
ifNoElse bs = ifCond bs $ body []

switchAsIf
  :: (ControlStatement r val stmt bod, Comparison r val)
  => VS (r val) -> [(VS (r val), MS (r bod))] -> MS (r bod) -> MS (r stmt)
switchAsIf v = ifCond . map (first (v ?==))

-- TODO [Brandon Bosman, 07/22/2026]: move this to InterfaceGOOL
-- | A class for representing "Visibility", of a class member,
-- i.e. whether it is public or private.
class VisibilitySym r vis | r -> vis where
  private :: r vis
  public  :: r vis

-- | A class for representing function/method parameters.
class ParameterSym r param | r -> param where
  param :: VS (r Variable) -> MS (r param)
  -- | A parameter that is an "alias" type, e.g. a C++ reference.
  -- This is a minor hack, to get around us not having/wanting
  -- "alias types" in GOOL.
  pointerParam :: VS (r Variable) -> MS (r param)

-- The three lists are inputs, outputs, and both, respectively
type InOutFunc r mthd bod = [VS (r Variable)] -> [VS (r Variable)] -> [VS (r Variable)] ->
  MS (r bod) -> MS (r mthd)
-- Parameters are: brief description of function, input descriptions and
-- variables, output descriptions and variables, descriptions and variables
-- for parameters that are both input and output, function body
type DocInOutFunc r mthd bod = String -> [(String, VS (r Variable))] ->
  [(String, VS (r Variable))] -> [(String, VS (r Variable))] -> MS (r bod) -> MS (r mthd)

-- | A class for representing functions/methods.
-- Usually 'MethodData' is used for the representation.
class MethodSym r vis typ param mthd bod | r -> vis typ param mthd bod
  where
  docMain :: MS (r bod) -> MS (r mthd)

  function :: Label -> r vis -> VS (r typ) -> [MS (r param)] ->
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

convScope :: (ScopeSym r scope) => ScopeData -> r scope
convScope (SD {scopeTag = Global}) = global
convScope (SD {scopeTag = Local}) = local
