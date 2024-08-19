{-# LANGUAGE TypeFamilies #-}

module Drasil.GOOL.RendererClassesCommon (
  CommonRenderSym, ImportSym(..), ImportElim(..),
  RenderBody(..), BodyElim(..), RenderBlock(..), BlockElim(..), RenderType(..),
  InternalTypeElim(..), VSUnOp, UnaryOpSym(..), VSBinOp, BinaryOpSym(..),
  OpElim(..), RenderVariable(..), InternalVarElim(..), RenderValue(..),
  ValueElim(..), InternalListFunc(..), RenderFunction(..), FunctionElim(..),
  InternalAssignStmt(..), InternalIOStmt(..), InternalControlStmt(..),
  RenderStatement(..), StatementElim(..), RenderVisibility(..), VisibilityElim(..),
  MSMthdType, MethodTypeSym(..), RenderParam(..), ParamElim(..),
  RenderMethod(..), MethodElim(..), BlockCommentSym(..), BlockCommentElim(..),
  ScopeElim(..)
) where

import Drasil.GOOL.InterfaceCommon (Label, Library, MSBody, MSBlock, VSFunction,
  VSType, SVariable, SValue, MSStatement, MSParameter, SMethod, MixedCall,
  BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..), VariableSym(..),
  VariableElim(..), ValueSym(..), Argument(..), Literal(..), MathConstant(..),
  VariableValue(..), ValueExpression(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..), List(..),
  InternalList(..), VectorExpression(..), StatementSym(..), AssignStatement(..),
  DeclStatement(..), IOStatement(..), StringStatement(..), FunctionSym(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  VisibilitySym(..), ParameterSym(..), MethodSym(..), ScopeSym(..))
import Drasil.GOOL.CodeType (CodeType)
import Drasil.GOOL.AST (Binding, Terminator, VisibilityTag, ScopeData)
import Drasil.GOOL.State (MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

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
  type Import r
  langImport :: Label -> r (Import r)
  modImport :: Label -> r (Import r)

class ImportElim r where
  import' :: r (Import r) -> Doc

class RenderBody r where
  multiBody :: [MSBody r] -> MSBody r

class BodyElim r where
  body :: r (Body r) -> Doc

class RenderBlock r where
  multiBlock :: [MSBlock r] -> MSBlock r

class BlockElim r where
  block :: r (Block r) -> Doc

class RenderType r where
  multiType :: [VSType r] -> VSType r
  typeFromData :: CodeType -> String -> Doc -> VSType r

class InternalTypeElim r where
  type' :: r (Type r) -> Doc

type VSUnOp a = VS (a (UnaryOp a))

class UnaryOpSym r where
  type UnaryOp r
  notOp    :: VSUnOp r
  negateOp :: VSUnOp r
  sqrtOp   :: VSUnOp r
  absOp    :: VSUnOp r
  logOp    :: VSUnOp r
  lnOp     :: VSUnOp r
  expOp    :: VSUnOp r
  sinOp    :: VSUnOp r
  cosOp    :: VSUnOp r
  tanOp    :: VSUnOp r
  asinOp   :: VSUnOp r
  acosOp   :: VSUnOp r
  atanOp   :: VSUnOp r
  floorOp  :: VSUnOp r
  ceilOp   :: VSUnOp r
  
type VSBinOp a = VS (a (BinaryOp a))

class BinaryOpSym r where
  type BinaryOp r
  equalOp        :: VSBinOp r
  notEqualOp     :: VSBinOp r
  greaterOp      :: VSBinOp r
  greaterEqualOp :: VSBinOp r
  lessOp         :: VSBinOp r
  lessEqualOp    :: VSBinOp r
  plusOp         :: VSBinOp r
  minusOp        :: VSBinOp r
  multOp         :: VSBinOp r
  divideOp       :: VSBinOp r
  powerOp        :: VSBinOp r
  moduloOp       :: VSBinOp r
  andOp          :: VSBinOp r
  orOp           :: VSBinOp r

class OpElim r where
  uOp :: r (UnaryOp r) -> Doc
  bOp :: r (BinaryOp r) -> Doc
  uOpPrec :: r (UnaryOp r) -> Int
  bOpPrec :: r (BinaryOp r) -> Int

class ScopeElim r where
  scopeData :: r (Scope r) -> ScopeData
  
class RenderVariable r where
  varFromData :: Binding -> String -> VSType r -> Doc -> SVariable r
    
class InternalVarElim r where
  variableBind :: r (Variable r) -> Binding
  variable  :: r (Variable r) -> Doc

class RenderValue r where
  inputFunc       :: SValue r
  printFunc       :: SValue r
  printLnFunc     :: SValue r
  printFileFunc   :: SValue r -> SValue r
  printFileLnFunc :: SValue r -> SValue r

  cast :: VSType r -> SValue r -> SValue r

  -- | Very generic internal function for generating calls, to reduce repeated 
  -- code throughout generators.
  -- Parameters are: maybe name of external module, maybe Doc for object 
  -- variable (including separator between object and function) for method 
  -- calls.
  call :: Maybe Library -> Maybe Doc -> MixedCall r

  valFromData :: Maybe Int -> Maybe Integer -> VSType r -> Doc -> SValue r

class ValueElim r where
  valuePrec :: r (Value r) -> Maybe Int
  valueInt :: r (Value r) -> Maybe Integer
  value :: r (Value r) -> Doc

class InternalListFunc r where
  -- | List
  listSizeFunc   :: SValue r -> VSFunction r
  -- | List, Index, Value
  listAddFunc    :: SValue r -> SValue r -> SValue r -> VSFunction r
  -- | List, Value
  listAppendFunc :: SValue r -> SValue r -> VSFunction r
  -- | List, Index
  listAccessFunc :: VSType r -> SValue r -> VSFunction r
  -- | List, Index, Value
  listSetFunc    :: SValue r -> SValue r -> SValue r -> VSFunction r

class RenderFunction r where
  funcFromData :: Doc -> VSType r -> VSFunction r
  
class FunctionElim r where
  functionType :: r (Function r) -> r (Type r)
  function :: r (Function r) -> Doc

class InternalAssignStmt r where
  multiAssign       :: [SVariable r] -> [SValue r] -> MSStatement r 

class InternalIOStmt r where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (SValue r) -> SValue r -> SValue r -> MSStatement r
    
class InternalControlStmt r where
  multiReturn :: [SValue r] -> MSStatement r

class RenderStatement r where
  stmt     :: MSStatement r -> MSStatement r
  loopStmt :: MSStatement r -> MSStatement r

  emptyStmt   :: MSStatement r

  stmtFromData :: Doc -> Terminator -> MSStatement r

class StatementElim r where
  statement :: r (Statement r) -> Doc
  statementTerm :: r (Statement r) -> Terminator

class RenderVisibility r where
  visibilityFromData :: VisibilityTag -> Doc -> r (Visibility r)
  
class VisibilityElim r where
  visibility :: r (Visibility r) -> Doc

class RenderParam r where
  paramFromData :: SVariable r -> Doc -> MSParameter r
  
class ParamElim r where
  parameterName :: r (Parameter r) -> Label
  parameterType :: r (Parameter r) -> r (Type r)
  parameter     :: r (Parameter r) -> Doc

class BlockCommentSym r where
  type BlockComment r
  blockComment :: [String] -> r (BlockComment r)
  -- | Converts a list of strings into a block comment
  docComment :: State a [String] -> State a (r (BlockComment r))

class BlockCommentElim r where
  blockComment' :: r (BlockComment r) -> Doc

type MSMthdType a = MS (a (MethodType a))

class (TypeSym r) => MethodTypeSym r where
  type MethodType r
  mType    :: VSType r -> MSMthdType r
    
class (MethodTypeSym r, BlockCommentSym r) => RenderMethod r where
  -- | Takes a BlockComment and a method and generates a function.
  commentedFunc :: MS (r (BlockComment r)) -> SMethod r -> SMethod r
  mthdFromData :: VisibilityTag -> Doc -> SMethod r

class MethodElim r where
  method :: r (Method r) -> Doc