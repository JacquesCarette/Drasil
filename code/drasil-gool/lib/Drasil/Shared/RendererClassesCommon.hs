{-# LANGUAGE TypeFamilyDependencies #-}

module Drasil.Shared.RendererClassesCommon (
  CommonRenderSym, ImportSym(..), ImportElim(..),
  RenderBody(..), BodyElim(..), RenderBlock(..), BlockElim(..), RenderType(..),
  InternalTypeElim(..), UnaryOpSym(..), BinaryOpSym(..),
  OpElim(..), RenderVariable(..), InternalVarElim(..), RenderValue(..),
  ValueElim(..), InternalListFunc(..), RenderFunction(..), FunctionElim(..),
  InternalAssignStmt(..), InternalIOStmt(..), InternalControlStmt(..),
  RenderStatement(..), StatementElim(..), RenderVisibility(..),
  VisibilityElim(..), MethodTypeSym(..), RenderParam(..), ParamElim(..),
  RenderMethod(..), MethodElim(..), BlockCommentSym(..), BlockCommentElim(..),
  ScopeElim(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Library, Block, Function,
  Type, Variable, Value, Statement, Parameter, Method, MixedCall,
  BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..), VariableSym(..),
  VariableElim(..), ValueSym(..), Argument(..), Literal(..), MathConstant(..),
  VariableValue(..), ValueExpression(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..), List(..),
  InternalList(..), VectorExpression(..), StatementSym(..), AssignStatement(..),
  DeclStatement(..), IOStatement(..), StringStatement(..), FunctionSym(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  VisibilitySym(..), ParameterSym(..), MethodSym(..), ScopeSym(..))
import Drasil.Shared.CodeType (CodeType)
import Drasil.Shared.AST (Binding, Terminator, VisibilityTag, ScopeData)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)
import qualified GHC.Arr as CLike

class (AssignStatement r, DeclStatement r, IOStatement r, 
  StringStatement r, FuncAppStatement r, CommentStatement r, ControlStatement
  r, Argument r, Literal r, MathConstant r, VariableValue r, CommandLineArgs r,
  NumericExpression r, BooleanExpression r, Comparison r, List r,
  InternalList r, VectorExpression r, TypeElim r, VariableElim r, RenderBlock r,
  BlockElim r, RenderBody r, BodyElim r, InternalListFunc r, RenderFunction r,
  FunctionElim r, OpElim r, RenderParam r, ParamElim r, RenderVisibility r,
  VisibilityElim r, InternalAssignStmt r, InternalIOStmt r,
  InternalControlStmt r, RenderStatement r, StatementElim r, RenderType r,
  InternalTypeElim r, RenderValue r, ValueElim r, RenderVariable r,
  InternalVarElim r, ImportSym r, ImportElim r, UnaryOpSym r, BinaryOpSym r,
  BlockCommentSym r, BlockCommentElim r, ValueExpression r, RenderMethod r,
  MethodElim r, ParameterSym r, ScopeElim r
  ) => CommonRenderSym r


-- TODO: split into multiple files, and create ProcRenderSym (or rename them both to RenderSym?)

-- Common Typeclasses --

class ImportSym r where
  type Import r = t | t -> r
  -- For importing an external library
  langImport :: Label -> Import r
  -- For importing a local (same project) module
  modImport :: Label -> Import r

class ImportElim r where
  import' :: Import r -> Doc

class RenderBody r where
  multiBody :: [Body r] -> Body r

class BodyElim r where
  body :: Body r -> Doc

class RenderBlock r where
  multiBlock :: [Block r] -> Block r

class BlockElim r where
  block :: Block r -> Doc

class RenderType r where
  multiType :: [Type r] -> Type r
  typeFromData :: CodeType -> String -> Doc -> Type r

class InternalTypeElim r where
  type' :: Type r -> Doc

class UnaryOpSym r where
  type UnaryOp r = t | t -> r
  notOp    :: UnaryOp r
  negateOp :: UnaryOp r
  sqrtOp   :: UnaryOp r
  absOp    :: UnaryOp r
  logOp    :: UnaryOp r
  lnOp     :: UnaryOp r
  expOp    :: UnaryOp r
  sinOp    :: UnaryOp r
  cosOp    :: UnaryOp r
  tanOp    :: UnaryOp r
  asinOp   :: UnaryOp r
  acosOp   :: UnaryOp r
  atanOp   :: UnaryOp r
  floorOp  :: UnaryOp r
  ceilOp   :: UnaryOp r
  
class BinaryOpSym r where
  type BinaryOp r = t | t -> r
  equalOp        :: BinaryOp r
  notEqualOp     :: BinaryOp r
  greaterOp      :: BinaryOp r
  greaterEqualOp :: BinaryOp r
  lessOp         :: BinaryOp r
  lessEqualOp    :: BinaryOp r
  plusOp         :: BinaryOp r
  minusOp        :: BinaryOp r
  multOp         :: BinaryOp r
  divideOp       :: BinaryOp r
  powerOp        :: BinaryOp r
  moduloOp       :: BinaryOp r
  andOp          :: BinaryOp r
  orOp           :: BinaryOp r

class OpElim r where
  uOp :: UnaryOp r -> Doc
  bOp :: BinaryOp r -> Doc
  uOpPrec :: UnaryOp r -> Int
  bOpPrec :: BinaryOp r -> Int

class ScopeElim r where
  scopeData :: Scope r -> ScopeData
  
class RenderVariable r where
  varFromData :: Binding -> String -> Type r -> Doc -> Variable r
  
-- This seemingly only gets used when we are 
class InternalVarElim r where
  -- [FIXME: Reed M, 09/10/25]
  -- We use @variableBind@ when building some lvalue helper functions in
  -- CommonPseudoOO and CLike. In particular, we need to be able to distinguish
  -- between assignments to static and non-static variables. Instead, we should
  -- either track the difference as part of the representation types by delaying
  -- our rendering choice, or just directly at the type level by adding two
  -- associated types @Field r@ and @StaticField r@ to a putative class like
  --
  -- @
  -- class (OOTypeSym r) => OOFieldSym r where
  --   type Field r = t | t -> r
  --   type StaticField r = t | t -> r
  --   staticConstField :: Label -> Type r -> StaticField r
  --   staticMutField   :: Label -> Type r -> StaticField r
  --   constField :: Label -> Type r -> Field r
  --   mutField :: Label -> Type r -> Field r
  -- @
  variableBind :: Variable r -> Binding
  -- [FIXME: Reed M, 09/10/25]
  -- This just needs to get banished
  variable  :: Variable r -> Doc

class RenderValue r where
  inputFunc       :: Value r
  printFunc       :: Value r
  printLnFunc     :: Value r
  printFileFunc   :: Value r -> Value r
  printFileLnFunc :: Value r -> Value r

  cast :: Type r -> Value r -> Value r

  -- | Very generic internal function for generating calls, to reduce repeated 
  -- code throughout generators.
  -- Parameters are: maybe name of external module, maybe Doc for object 
  -- variable (including separator between object and function) for method 
  -- calls.
  call :: Maybe Library -> Maybe Doc -> MixedCall r

  valFromData :: Maybe Int -> Maybe Integer -> Type r -> Doc -> Value r

class ValueElim r where
  valuePrec :: Value r -> Maybe Int
  valueInt :: Value r -> Maybe Integer
  value :: Value r -> Doc

class InternalListFunc r where
  -- | List
  listSizeFunc   :: Value r -> Function r
  -- | List, Index, Value
  listAddFunc    :: Value r -> Value r -> Value r -> Function r
  -- | List, Value
  listAppendFunc :: Value r -> Value r -> Function r
  -- | List, Index
  listAccessFunc :: Type r -> Value r -> Function r
  -- | List, Index, Value
  listSetFunc    :: Value r -> Value r -> Value r -> Function r

class RenderFunction r where
  funcFromData :: Doc -> Type r -> Function r
  
class FunctionElim r where
  functionType :: Function r -> Type r
  function :: Function r -> Doc

class InternalAssignStmt r where
  multiAssign       :: [Variable r] -> [Value r] -> Statement r 

class InternalIOStmt r where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (Value r) -> Value r -> Value r -> Statement r
    
class InternalControlStmt r where
  multiReturn :: [Value r] -> Statement r

class RenderStatement r where
  stmt     :: Statement r -> Statement r
  loopStmt :: Statement r -> Statement r

  stmtFromData :: Doc -> Terminator -> Statement r

class StatementElim r where
  statement :: Statement r -> Doc
  statementTerm :: Statement r -> Terminator

class RenderVisibility r where
  visibilityFromData :: VisibilityTag -> Doc -> Visibility r
  
class VisibilityElim r where
  visibility :: Visibility r -> Doc

class RenderParam r where
  paramFromData :: Variable r -> Doc -> Parameter r
  
class ParamElim r where
  parameterName :: Parameter r -> Label
  parameterType :: Parameter r -> Type r
  parameter     :: Parameter r -> Doc

class BlockCommentSym r where
  type BlockComment r = t | t -> r
  blockComment :: [String] -> BlockComment r
  -- | Converts a list of strings into a block comment
  docComment :: [String] -> BlockComment r

class BlockCommentElim r where
  blockComment' :: BlockComment r -> Doc

class (TypeSym r) => MethodTypeSym r where
  type MethodType r = t | t -> r
  mType    :: Type r -> MethodType r
    
class (MethodTypeSym r, BlockCommentSym r) => RenderMethod r where
  -- | Takes a BlockComment and a method and generates a function.
  commentedFunc :: BlockComment r -> Method r -> Method r
  mthdFromData :: VisibilityTag -> Doc -> Method r

class MethodElim r where
  method :: Method r -> Doc