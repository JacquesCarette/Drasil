{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, Body, Block, VSBinder, Variable, SVariable, Value, SValue,
  NamedArgs, MixedCall, MixedCtorCall, PosCall, PosCtorCall, InOutCall,
  InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, SharedStatement, UnRepr(..), BodySym(..), bodyStatements, oneLiner,
  BlockSym(..), TypeSym(..), TypeElim(..), getTypeString, VariableSym(..),
  ScopeSym(..), convScope, VariableElim(..), listOf, listVar, ValueSym(..),
  Argument(..), Literal(..), litZero, MathConstant(..), VariableValue(..),
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp,
  libFuncApp, exists, IndexTranslator(..), Reference(..), Array(..), List(..),
  Set(..), NativeVector(..), InternalList(..), listSlice, listIndexExists, at,
  StatementSym(..), AssignStatement(..), (&=), DeclStatement(..),
  IOStatement(..), StringStatement(..), FunctionSym, FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), ifNoElse, switchAsIf,
  VisibilitySym(..), ParameterSym(..), MethodSym(..), BinderSym(..),
  BinderElim(..), convType
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

-- TODO [Brandon Bosman, 06/09/2026]: UnRepr can be removed from SharedProg
-- if we can root out its use from drasil-code

-- | Wrapper typeclass that bundles everything common between what is essential
-- for generating object-oriented and procedural programs.
class (UnRepr r TypeData, SharedStatement r smt, FunctionSym r, InternalList r,
  VariableValue r, IndexTranslator r, TypeElim r,
  VariableElim r, MethodSym r vis smt mthd, ScopeSym r, BinderSym r
  ) => SharedProg r vis smt mthd

class (Array r, AssignStatement r smt, Argument r, BooleanExpression r,
  CommandLineArgs r, CommentStatement r smt, Comparison r,
  ControlStatement r smt, DeclStatement r smt, FuncAppStatement r smt,
  IOStatement r smt, List r smt, Literal r, MathConstant r, NumericExpression r,
  ParameterSym r, Reference r, Set r, StringStatement r smt, ValueExpression r,
  VariableValue r
  ) => SharedStatement r smt

-- Shared between OO and Procedural --

class UnRepr repr contents where
  unRepr :: repr contents -> contents

type Body = Doc

-- | Class for representing a `Body`, which is basically a lexical scope of code.
-- Examples include a function body, the branch(es) of an `if`-statement, etc.
class (BlockSym r smt) => BodySym r smt where
  -- | Given a list of `Block`s, create a `Body` of them.
  body           :: [MS (r Block)] -> MS (r Body)
  -- | Given a comment and a body, add the comment as a header for the body.
  addComments :: Label -> MS (r Body) -> MS (r Body)

bodyStatements :: (BodySym r smt) => [MS (r smt)] -> MS (r Body)
bodyStatements sts = body [block sts]

oneLiner :: (BodySym r smt) => MS (r smt) -> MS (r Body)
oneLiner tp = bodyStatements [tp]

type Block = Doc

-- | Class for representing a `Block` of code.
-- A `Block` is a series of statements grouped together,
-- not for use by the compiler/interpreter
-- but to improve readability of the generated code.
-- See the bottom of page 2 of Brook's GOOL paper from 2020 for more details.
class (StatementSym r smt) => BlockSym r smt where
  block   :: [MS (r smt)] -> MS (r Block)

-- | Class for representing a type.
class TypeSym r where
  bool          :: VS (r TypeData)
  int           :: VS (r TypeData) -- This is 32-bit signed ints except in Python,
                            -- which has unlimited precision ints; and Julia,
                            -- Which defaults to 64-bit signed ints
  float         :: VS (r TypeData)
  double        :: VS (r TypeData)
  char          :: VS (r TypeData)
  string        :: VS (r TypeData)
  infile        :: VS (r TypeData)
  outfile       :: VS (r TypeData)
  referenceType :: VS (r TypeData) -> VS (r TypeData)
  listType      :: VS (r TypeData) -> VS (r TypeData)
  setType       :: VS (r TypeData) -> VS (r TypeData)
  arrayType     :: VS (r TypeData) -> VS (r TypeData)
  innerType     :: VS (r TypeData) -> VS (r TypeData)
  funcType      :: [VS (r TypeData)] -> VS (r TypeData) -> VS (r TypeData)
  void          :: VS (r TypeData)

-- TODO [Brandon Bosman, 06/09/2026]: Think about separating GOOL and GProc implementations of this
-- | A helper function for extracting the String representation from an `r TypeData`
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
class (TypeSym r) => VariableSym r where
  -- | An instance- or function-level variable, separate from its instance (i.e. `v`, not `o.v`)
  var       :: Label -> VS (r TypeData) -> SVariable r
  -- | An instance- or function-level constant, separate from its instance (i.e. `v`, not `o.v`)
  constant  :: Label -> VS (r TypeData) -> SVariable r
  -- | An instance- or module-level variable from an external library.
  -- Given library `Lib`, variable name `v`, and variable type `t`,
  -- it performs the necessary imports and creates `Lib.v`
  extVar    :: Library -> Label -> VS (r TypeData) -> SVariable r

class (VariableSym r) => VariableElim r where
  variableName :: r Variable -> String
  variableType :: r Variable -> r TypeData

listVar :: (VariableSym r) => Label -> VS (r TypeData) -> SVariable r
listVar n t = var n (listType t)

listOf :: (VariableSym r) => Label -> VS (r TypeData) -> SVariable r
listOf = listVar

type Value = ValData
type SValue a = VS (a Value)

-- | Class for representing a value.
class (TypeSym r) => ValueSym r where
  valueType :: r Value -> r TypeData

class (TypeSym r) => TypeElim r where
  getCodeType :: r TypeData -> CodeType

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
  litArray  :: VS (r TypeData) -> [SValue r] -> SValue r
  litList   :: VS (r TypeData) -> [SValue r] -> SValue r
  litSet    :: VS (r TypeData) -> [SValue r] -> SValue r

litZero :: (Literal r, TypeElim r) => VS (r TypeData) -> SValue r
litZero t = do
  t' <- t
  case getCodeType t' of
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
type MixedCall r = Label -> VS (r TypeData) -> [SValue r] -> NamedArgs r -> SValue r
-- Constructor call with both positional and named arguments
type MixedCtorCall r = VS (r TypeData) -> [SValue r] -> NamedArgs r -> SValue r
-- Function call with only positional arguments
type PosCall r = Label -> VS (r TypeData) -> [SValue r] -> SValue r
-- Constructor call with only positional arguments
type PosCtorCall r = VS (r TypeData) -> [SValue r] -> SValue r

type VSBinder a = VS (a BinderD)

-- | A class for representing a binder, i.e. the binding of a variable name
-- to a type, scope, etc.
-- As of July 2026, integration of this typeclass is still WIP, blocked
-- by issues with our variable map.
class (TypeSym r) => BinderSym r where
  binder :: Label -> VS (r TypeData) -> VSBinder r

class (BinderSym r) => BinderElim r where
  binderName :: r BinderD -> String
  binderType :: r BinderD -> r TypeData

-- | A class for representing values that can include expressions
class (VariableSym r, ValueSym r) => ValueExpression r where
  -- An inline if-statement, aka the ternary operator.  Inputs:
  -- Condition, True-value, False-value
  inlineIf     :: SValue r -> SValue r -> SValue r -> SValue r

  funcAppMixedArgs     ::            MixedCall r
  extFuncAppMixedArgs  :: Library -> MixedCall r
  libFuncAppMixedArgs  :: Library -> MixedCall r

  lambda :: [VSBinder r] -> SValue r -> SValue r

  notNull :: SValue r -> SValue r

funcApp          :: (ValueExpression r) => PosCall r
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs :: (ValueExpression r) => Label -> VS (r TypeData) ->
  NamedArgs r -> SValue r
funcAppNamedArgs n t = funcAppMixedArgs n t []

extFuncApp       :: (ValueExpression r) => Library -> PosCall r
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp       :: (ValueExpression r) => Library -> PosCall r
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

exists :: (ValueExpression r) => SValue r -> SValue r
exists = notNull

-- | Helper class for representing the conversion between integers and array indices.
-- GOOL is 0-indexed, so languages like Julia that are not 0-indexed
-- need to convert between integers and indices.
class (ValueSym r) => IndexTranslator r where
  -- | Does any necessary conversions from GOOL's zero-indexed assumptions to
  --   the target language's assumptions
  intToIndex :: SValue r -> SValue r
  -- | Does any necessary conversions from the target language's indexing
  --   assumptions assumptions to GOOL's zero-indexed assumptions
  indexToInt :: SValue r -> SValue r

-- | A class for representing references.
-- By "reference" we basically mean "C++ pointer" or "OCaml reference".
class (TypeSym r, ValueSym r) => Reference r where
  -- | Given a value, convert it to a reference to that value
  makeRef :: SValue r -> SValue r
  -- | Given a value that may be a reference type,
  -- apply any necessary dereference operation.
  maybeDeref :: SValue r -> SValue r

class (IndexTranslator r) => Array r where
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

class (IndexTranslator r, StatementSym r smt) => List r smt where
  -- | Finds the size of a list.
  --   Arguments are: List
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

class (ValueSym r) => Set r where
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

-- | Vector operations for languages with native vector support (e.g. MATLAB,
--   Julia). Expression-based: every operation takes and returns 'SValue's, so
--   operations compose like math (e.g. @vecAdd (vecScale s a) b@).
--   Vectors have their own 'vecType' and 'litVec' so callers don't depend on
--   how vectors are represented; these default to 'listType' and 'litList'.
class (IndexTranslator r, Literal r) => NativeVector r where
  -- | The type of a vector with the given element type.
  --   Defaults to 'listType'; a language may override it to use a distinct
  --   vector representation.
  vecType :: VS (r TypeData) -> VS (r TypeData)
  vecType = listType
  -- | A vector literal with the given element type and elements.
  --   Defaults to 'litList'.
  litVec :: VS (r TypeData) -> [SValue r] -> SValue r
  litVec = litList
  -- | Scales a vector by a scalar.
  --   Arguments are: Scalar, Vector
  vecScale :: SValue r -> SValue r -> SValue r
  -- | Adds two vectors elementwise.
  --   Arguments are: Vector, Vector
  vecAdd :: SValue r -> SValue r -> SValue r
  -- | Gets the element of a vector at an index.
  --   Arguments are: Vector, Index
  vecIndex :: SValue r -> SValue r -> SValue r
  -- | Dot product of two vectors (returns a scalar).
  --   Arguments are: Vector, Vector
  vecDot :: SValue r -> SValue r -> SValue r
  -- | Euclidean norm (magnitude) of a vector (returns a scalar).
  --   Argument is: Vector
  vecMag :: SValue r -> SValue r
  -- | Unit vector in the direction of a vector (returns a vector).
  --   Argument is: Vector
  vecUnit :: SValue r -> SValue r

class (ValueSym r) => InternalList r where
  listSlice'      :: Maybe (SValue r) -> Maybe (SValue r) -> Maybe (SValue r)
    -> SVariable r -> SValue r -> MS (r Block)

-- | Creates a slice of a list and assigns it to a variable.
--   Arguments are:
--   Variable to assign
--   List to read from
--   (optional) Start index inclusive.
--      (if Nothing, then list start if step > 0, list end if step < 0)
--   (optional) End index exclusive.
--      (if Nothing, then list end if step > 0, list start if step > 0)
--   (optional) Step (if Nothing, then defaults to 1)
listSlice :: (InternalList r) => SVariable r -> SValue r ->
  Maybe (SValue r) -> Maybe (SValue r) -> Maybe (SValue r) -> MS (r Block)
listSlice vnew vold b e tp = listSlice' b e tp vnew vold

listIndexExists :: (List r smt, Comparison r) => SValue r -> SValue r -> SValue r
listIndexExists lst index = listSize lst ?> index

at :: (List r smt) => SValue r -> SValue r -> SValue r
at = listAccess

-- | A class for representing statements.
-- Usually `(Doc, Terminator)` is used for the representation.
class (ValueSym r) => StatementSym r smt | r -> smt where
  -- | Converts a value to statement
  valStmt :: SValue r -> MS (r smt)
  -- | Empty statement
  emptyStmt :: MS (r smt)
  -- | Consolidates a list of statements into a single statement
  multi     :: [MS (r smt)] -> MS (r smt)

class (VariableSym r, StatementSym r smt) => AssignStatement r smt where
  (&-=)  :: SVariable r -> SValue r -> MS (r smt)
  infixl 1 &-=
  (&+=)  :: SVariable r -> SValue r -> MS (r smt)
  infixl 1 &+=
  (&++)  :: SVariable r -> MS (r smt)
  infixl 8 &++
  (&--)  :: SVariable r -> MS (r smt)
  infixl 8 &--

  assign :: SVariable r -> SValue r -> MS (r smt)

(&=) :: (AssignStatement r smt) => SVariable r -> SValue r -> MS (r smt)
infixr 1 &=
(&=) = assign

class (VariableSym r, StatementSym r smt, ScopeSym r) => DeclStatement r smt where
  -- | Declare a variable without giving it a value.
  -- Not for use with arrays; use `arrayDec` instead.
  varDec       :: SVariable r -> r ScopeData -> MS (r smt)
  -- | Declare a variable and give it a value.
  -- Not for use with arrays; use `arrayDecDef` instead.
  varDecDef    :: SVariable r -> r ScopeData -> SValue r -> MS (r smt)
  -- | Given the size of the list, the variable to store the list in,
  -- and the scope of the variable, declare a list of the given size.
  listDec      :: Integer -> SVariable r -> r ScopeData -> MS (r smt)
  listDecDef   :: SVariable r -> r ScopeData -> [SValue r] -> MS (r smt)
  setDec       :: SVariable r -> r ScopeData -> MS (r smt)
  setDecDef    :: SVariable r -> r ScopeData -> SValue r -> MS (r smt)
  -- | Given the size of the aray, the default value to fill the array with,
  -- the variable to store the array in, and the scope of the variable,
  -- declare an array of the given size.
  arrayDec     :: Integer -> SValue r -> SVariable r -> r ScopeData -> MS (r smt)
  arrayDecDef  :: SVariable r -> r ScopeData -> [SValue r] -> MS (r smt)
  constDecDef  :: SVariable r -> r ScopeData -> SValue r -> MS (r smt)
  funcDecDef   :: SVariable r -> r ScopeData -> [SVariable r] -> MS (r Body)
    -> MS (r smt)

class (VariableSym r, StatementSym r smt) => IOStatement r smt where
  print      :: SValue r -> MS (r smt)
  printLn    :: SValue r -> MS (r smt)
  printStr   :: String -> MS (r smt)
  printStrLn :: String -> MS (r smt)

  -- | Given the file handle and value to print, print the value to the file.
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

class (VariableSym r, StatementSym r smt) => StringStatement r smt where
  -- | Given a char to split on, variable to store result in, and string to split,
  -- generates a statement splitting the string into a list of strings
  -- delimited by the char.
  stringSplit :: Char -> SVariable r -> SValue r -> MS (r smt)
  stringListVals  :: [SVariable r] -> SValue r -> MS (r smt)
  -- | Given a list of variables and a value containing a list of strings,
  -- assign the ith element of the list of strings into the ith variable
  stringListLists :: [SVariable r] -> SValue r -> MS (r smt)

class (ValueSym r) => FunctionSym r where

-- The three lists are inputs, outputs, and both, respectively
type InOutCall r smt = Label -> [SValue r] -> [SVariable r] -> [SVariable r] ->
  MS (r smt)

class (VariableSym r, StatementSym r smt) => FuncAppStatement r smt where
  inOutCall    ::            InOutCall r smt
  extInOutCall :: Library -> InOutCall r smt

class (StatementSym r smt) => CommentStatement r smt where
  comment :: String -> MS (r smt)

class (BodySym r smt, VariableSym r) => ControlStatement r smt where
  break :: MS (r smt)
  continue :: MS (r smt)

  returnStmt :: SValue r -> MS (r smt)

  throw :: Label -> MS (r smt)

  -- | String of if-else statements.
  --   Arguments: List of predicates and bodies (if this then that),
  --   Body for else branch
  ifCond     :: [(SValue r, MS (r Body))] -> MS (r Body) -> MS (r smt)
  switch     :: SValue r -> [(SValue r, MS (r Body))] -> MS (r Body) -> MS (r smt)

  ifExists :: SValue r -> MS (r Body) -> MS (r Body) -> MS (r smt)

  for      :: MS (r smt) -> SValue r -> MS (r smt) -> MS (r Body) ->
    MS (r smt)
  -- Iterator variable, start value, end value, step value, loop body
  forRange :: SVariable r -> SValue r -> SValue r -> SValue r -> MS (r Body) ->
    MS (r smt)
  forEach  :: SVariable r -> SValue r -> MS (r Body) -> MS (r smt)
  while    :: SValue r -> MS (r Body) -> MS (r smt)

  tryCatch :: MS (r Body) -> MS (r Body) -> MS (r smt)

  assert :: SValue r -> SValue r -> MS (r smt)

ifNoElse :: (ControlStatement r smt) => [(SValue r, MS (r Body))] -> MS (r smt)
ifNoElse bs = ifCond bs $ body []

switchAsIf :: (ControlStatement r smt, Comparison r) => SValue r ->
  [(SValue r, MS (r Body))] -> MS (r Body) -> MS (r smt)
switchAsIf v = ifCond . map (first (v ?==))

-- TODO [Brandon Bosman, 07/22/2026]: move this to InterfaceGOOL
-- | A class for representing "Visibility", of a class member,
-- i.e. whether it is public or private.
class VisibilitySym r vis | r -> vis where
  private :: r vis
  public  :: r vis

-- | A class for representing function/method parameters.
class (VariableSym r) => ParameterSym r where
  param :: SVariable r -> MS (r ParamData)
  -- | A parameter that is an "alias" type, e.g. a C++ reference.
  -- This is a minor hack, to get around us not having/wanting
  -- "alias types" in GOOL.
  pointerParam :: SVariable r -> MS (r ParamData)

-- The three lists are inputs, outputs, and both, respectively
type InOutFunc r mthd = [SVariable r] -> [SVariable r] -> [SVariable r] ->
  MS (r Body) -> MS (r mthd)
-- Parameters are: brief description of function, input descriptions and
-- variables, output descriptions and variables, descriptions and variables
-- for parameters that are both input and output, function body
type DocInOutFunc r mthd = String -> [(String, SVariable r)] ->
  [(String, SVariable r)] -> [(String, SVariable r)] -> MS (r Body) -> MS (r mthd)

-- | A class for representing functions/methods.
-- Usually 'MethodData' is used for the representation.
class (BodySym r smt, ParameterSym r, VisibilitySym r vis) => MethodSym r vis smt mthd | r -> mthd
  where
  docMain :: MS (r Body) -> MS (r mthd)

  function :: Label -> r vis -> VS (r TypeData) -> [MS (r ParamData)] ->
    MS (r Body) -> MS (r mthd)
  mainFunction  :: MS (r Body) -> MS (r mthd)
  -- Parameters are: function description, parameter descriptions,
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> MS (r mthd) -> MS (r mthd)

  inOutFunc :: Label -> r vis -> InOutFunc r mthd
  docInOutFunc :: Label -> r vis -> DocInOutFunc r mthd

-- Utility

convType :: (TypeSym r) => CodeType -> VS (r TypeData)
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
