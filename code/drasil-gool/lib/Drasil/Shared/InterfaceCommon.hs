{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, MSBody, MSBlock, VSFunction, VSType, VSBinder,
  SVariable, SValue, MSStatement, MSParameter, SMethod, NamedArgs, MixedCall,
  MixedCtorCall, PosCall, PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, UnRepr(..), BodySym(..), bodyStatements, oneLiner, BlockSym(..),
  TypeSym(..), getCodeType, getTypeString, VariableSym(..), ScopeSym(..),
  convScope, VariableElim(..), listOf, listVar, ValueSym(..), Argument(..),
  Literal(..), litZero, MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), funcApp, funcAppNamedArgs, extFuncApp, libFuncApp, exists,
  IndexTranslator(..), Array(..), List(..), Set(..), InternalList(..), listSlice,
  listIndexExists, at, StatementSym(..), AssignStatement(..), (&=),
  DeclStatement(..), IOStatement(..), StringStatement(..), FunctionSym(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..), ifNoElse,
  switchAsIf, VisibilitySym(..), ParameterSym(..), MethodSym(..), BinderSym(..),
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

class (UnRepr r TypeData, AssignStatement r, DeclStatement r, IOStatement r,
  StringStatement r, FunctionSym r, FuncAppStatement r, CommentStatement r,
  ControlStatement r, InternalList r, Argument r, Literal r, MathConstant r,
  VariableValue r, CommandLineArgs r, NumericExpression r, BooleanExpression r,
  Comparison r, ValueExpression r, IndexTranslator r, Array r, List r, Set r,
  VariableElim r, MethodSym r, ScopeSym r, BinderSym r
  ) => SharedProg r

-- Shared between OO and Procedural --

class UnRepr repr contents where
  unRepr :: repr contents -> contents

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

type VSType a = VS (a TypeData)

class TypeSym r where
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
  referenceType :: VSType r -> VSType r
  listType      :: VSType r -> VSType r
  setType       :: VSType r -> VSType r
  arrayType     :: VSType r -> VSType r
  innerType     :: VSType r -> VSType r
  funcType      :: [VSType r] -> VSType r -> VSType r
  void          :: VSType r

-- | A helper function for extracting the CodeType from an `r TypeData`
getCodeType :: (UnRepr r TypeData) => r TypeData -> CodeType
getCodeType = cType . unRepr

-- TODO [Brandon Bosman, 06/09/2026]: Think about separating GOOL and GProc implementations of this
-- | A helper function for extracting the String representation from an `r TypeData`
getTypeString :: (UnRepr r TypeData) => r TypeData -> String
getTypeString = typeString . unRepr

class ScopeSym r where
  global :: r ScopeData -- Definite global scope
  mainFn :: r ScopeData -- Main program - either main function or global scope
  local  :: r ScopeData -- Definite local scope

type SVariable a = VS (a (Variable a))

class (TypeSym r) => VariableSym r where
  type Variable r
  -- | An instance- or function-level variable, separate from its instance (i.e. `v`, not `o.v`)
  var       :: Label -> VSType r -> SVariable r
  -- | An instance- or function-level constant, separate from its instance (i.e. `v`, not `o.v`)
  constant  :: Label -> VSType r -> SVariable r
  -- | An instance- or module-level variable from an external library.
  -- Given library `Lib`, variable name `v`, and variable type `t`,
  -- it performs the necessary imports and creates `Lib.v`
  extVar    :: Library -> Label -> VSType r -> SVariable r

class (VariableSym r) => VariableElim r where
  variableName :: r (Variable r) -> String
  variableType :: r (Variable r) -> r TypeData

listVar :: (VariableSym r) => Label -> VSType r -> SVariable r
listVar n t = var n (listType t)

listOf :: (VariableSym r) => Label -> VSType r -> SVariable r
listOf = listVar

type SValue a = VS (a (Value a))

class (TypeSym r) => ValueSym r where
  type Value r
  valueType :: r (Value r) -> r TypeData

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

litZero :: (Literal r, UnRepr r TypeData) => VSType r -> SValue r
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
type MixedCall r = Label -> VSType r -> [SValue r] -> NamedArgs r -> SValue r
-- Constructor call with both positional and named arguments
type MixedCtorCall r = VSType r -> [SValue r] -> NamedArgs r -> SValue r
-- Function call with only positional arguments
type PosCall r = Label -> VSType r -> [SValue r] -> SValue r
-- Constructor call with only positional arguments
type PosCtorCall r = VSType r -> [SValue r] -> SValue r

type VSBinder a = VS (a BinderD)

class (TypeSym r) => BinderSym r where
  binder :: Label -> VSType r -> VSBinder r

class (BinderSym r) => BinderElim r where
  binderName :: r BinderD -> String
  binderType :: r BinderD -> r TypeData

-- for values that can include expressions
class (VariableSym r, ValueSym r) => ValueExpression r where
  -- An inline if-statement, aka the ternary operator.  Inputs:
  -- Condition, True-value, False-value
  inlineIf     :: SValue r -> SValue r -> SValue r -> SValue r

  funcAppMixedArgs     ::            MixedCall r
  extFuncAppMixedArgs  :: Library -> MixedCall r
  libFuncAppMixedArgs  :: Library -> MixedCall r

  lambda :: [VSBinder r] -> SValue r -> SValue r

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

class (ValueSym r) => IndexTranslator r where
  -- | Does any necessary conversions from GOOL's zero-indexed assumptions to
  --   the target language's assumptions
  intToIndex :: SValue r -> SValue r
  -- | Does any necessary conversions from the target language's indexing
  --   assumptions assumptions to GOOL's zero-indexed assumptions
  indexToInt :: SValue r -> SValue r
  -- | Finds the size of a list.
  --   Arguments are: List

class (IndexTranslator r) => Array r where
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

class (IndexTranslator r) => List r where
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
  listSet    :: SValue r -> SValue r -> SValue r -> MSStatement r
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

class (VariableSym r, StatementSym r, ScopeSym r) => DeclStatement r where
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
  comment :: String -> MSStatement r

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
