{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Drasil.Shared.RendererClassesCommon (
  CommonRenderSym, ImportSym(..), import', RenderBody(..), BodyElim(..),
  RenderBlock(..), BlockElim(..), RenderType(..), VSUnOp, UnaryOpSym(..),
  VSBinOp, BinaryOpSym(..), OpElim(..), RenderVariable(..), InternalVarElim(..),
  InternalBinderElim(..), RenderValue(..), ValueElim(..), InternalListFunc(..),
  RenderFunction(..), FunctionElim(..), InternalAssignStmt(..),
  InternalIOStmt(..), InternalControlStmt(..), RenderStatement(..),
  StatementElim(..), RenderVisibility(..), VisibilityElim(..), MethodTypeSym(..),
  RenderParam(..), ParamElim(..), RenderMethod(..), MethodElim(..),
  BlockCommentSym(..), BlockCommentElim(..), ScopeElim(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Library, Variable, SVariable, Value,
  SValue, MixedCall, TypeSym(..), VariableElim(..), Argument(..), Literal(..),
  MathConstant(..), VariableSym, ValueSym, VariableValue(..),
  ValueExpression(..), CommandLineArgs(..), NumericExpression(..),
  BooleanExpression(..), Comparison(..), IndexTranslator(..), List(..),
  ListStatement, InternalList(..), AssignStatement(..), ScopeSym,
  DeclStatement(..), StringStatement(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), ParameterSym(..), BinderElim(..),
  UnRepr(..), BodySym, BlockSym)
import Drasil.Shared.AST (AttachmentTag, Terminator, VisibilityTag, ScopeData,
  OpData, BinderD, ParamData, FuncData)
import Drasil.Shared.State (MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

class (BodySym r bod block, BlockSym r block stmt, AssignStatement r stmt,
  ScopeSym r, DeclStatement r stmt bod, StringStatement r stmt, FuncAppStatement r stmt,
  CommentStatement r stmt, ControlStatement r stmt bod, Argument r,
  Literal r typ, MathConstant r, ValueSym r typ, VariableSym r typ,
  VariableValue r, CommandLineArgs r, NumericExpression r, BooleanExpression r,
  Comparison r, IndexTranslator r, List r, ListStatement r stmt,
  InternalList r block, VariableElim r typ, BinderElim r typ,
  RenderBlock r block, BlockElim r block, RenderBody r bod, BodyElim r bod,
  InternalListFunc r typ, RenderFunction r typ, FunctionElim r typ, OpElim r,
  RenderParam r, ParamElim r typ, RenderVisibility r vis, VisibilityElim r vis,
  InternalAssignStmt r stmt, InternalIOStmt r stmt, InternalControlStmt r stmt,
  RenderStatement r stmt, StatementElim r stmt, RenderType r typ,
  RenderValue r typ, ValueElim r, RenderVariable r typ, InternalVarElim r,
  InternalBinderElim r, ImportSym r, UnaryOpSym r, BinaryOpSym r,
  BlockCommentSym r, BlockCommentElim r, ValueExpression r typ, TypeSym r typ,
  MethodTypeSym r typ, RenderMethod r mthd, MethodElim r mthd, ParameterSym r,
  ScopeElim r
  ) => CommonRenderSym r vis typ stmt mthd bod block

-- Common Typeclasses --

class ImportSym r where
  -- For importing an external library
  langImport :: Label -> r Doc
  -- For importing a local (same project) module
  modImport :: Label -> r Doc

import' :: (UnRepr r Doc) => r Doc -> Doc
import' = unRepr

class RenderBody r bod | r -> bod where
  multiBody :: [MS (r bod)] -> MS (r bod)

class BodyElim r bod | r -> bod where
  body :: r bod -> Doc

class RenderBlock r block | r -> block where
  multiBlock :: [MS (r block)] -> MS (r block)

class BlockElim r block | r -> block where
  block :: r block -> Doc

class RenderType r typ | r -> typ where
  multiType :: [VS (r typ)] -> VS (r typ)

type VSUnOp a = VS (a OpData)

class UnaryOpSym r where
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

type VSBinOp a = VS (a OpData)

class BinaryOpSym r where
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
  uOp :: r OpData -> Doc
  bOp :: r OpData -> Doc
  uOpPrec :: r OpData -> Int
  bOpPrec :: r OpData -> Int

class ScopeElim r where
  scopeData :: r ScopeData -> ScopeData

class RenderVariable r typ | r -> typ where
  varFromData :: AttachmentTag -> String -> VS (r typ) -> Doc -> SVariable r

class InternalVarElim r where
  variableBind :: r Variable -> AttachmentTag
  variable  :: r Variable -> Doc

class InternalBinderElim r where
  binderElim  :: r BinderD -> Doc

class RenderValue r typ | r -> typ where
  inputFunc       :: SValue r
  printFunc       :: SValue r
  printLnFunc     :: SValue r
  printFileFunc   :: SValue r -> SValue r
  printFileLnFunc :: SValue r -> SValue r

  cast :: VS (r typ) -> SValue r -> SValue r

  -- | Very generic internal function for generating calls, to reduce repeated
  -- code throughout generators.
  -- Parameters are: maybe name of external module, maybe Doc for object
  -- variable (including separator between object and function) for method
  -- calls.
  call :: Maybe Library -> Maybe Doc -> MixedCall r typ

  valFromData :: Maybe Int -> Maybe Integer -> VS (r typ) -> Doc -> SValue r

class ValueElim r where
  valuePrec :: r Value -> Maybe Int
  valueInt :: r Value -> Maybe Integer
  value :: r Value -> Doc

class InternalListFunc r typ | r -> typ where
  -- | List, Index
  listAccessFunc :: VS (r typ) -> SValue r -> VS (r FuncData)

class RenderFunction r typ | r -> typ where
  funcFromData :: Doc -> VS (r typ) -> VS (r FuncData)

class FunctionElim r typ | r -> typ where
  functionType :: r FuncData -> r typ
  function :: r FuncData -> Doc

class InternalAssignStmt r stmt | r -> stmt where
  multiAssign       :: [SVariable r] -> [SValue r] -> MS (r stmt)

class InternalIOStmt r stmt | r -> stmt where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (SValue r) -> SValue r -> SValue r -> MS (r stmt)

class InternalControlStmt r stmt | r -> stmt where
  multiReturn :: [SValue r] -> MS (r stmt)

class RenderStatement r stmt | r -> stmt where
  stmt     :: MS (r stmt) -> MS (r stmt)
  loopStmt :: MS (r stmt) -> MS (r stmt)

  stmtFromData :: Doc -> Terminator -> MS (r stmt)

class StatementElim r stmt | r -> stmt where
  statement :: r stmt -> Doc
  statementTerm :: r stmt -> Terminator

class RenderVisibility r vis | r -> vis where
  visibilityFromData :: VisibilityTag -> Doc -> r vis

class VisibilityElim r vis | r -> vis where
  visibility :: r vis -> Doc

class RenderParam r where
  paramFromData :: SVariable r -> Doc -> MS (r ParamData)

class ParamElim r typ | r -> typ where
  parameterName :: r ParamData -> Label
  parameterType :: r ParamData -> r typ
  parameter     :: r ParamData -> Doc

class BlockCommentSym r where
  blockComment :: [String] -> r Doc
  -- | Converts a list of strings into a block comment
  docComment :: State a [String] -> State a (r Doc)

class BlockCommentElim r where
  blockComment' :: r Doc -> Doc

class MethodTypeSym r typ | r -> typ where
  mType :: VS (r typ) -> MS (r typ)

class RenderMethod r mthd | r -> mthd where
  -- | Takes a BlockComment and a method and generates a function.
  commentedFunc :: MS (r Doc) -> MS (r mthd) -> MS (r mthd)
  mthdFromData :: VisibilityTag -> Doc -> MS (r mthd)

class MethodElim r mthd | r -> mthd where
  method :: r mthd -> Doc
