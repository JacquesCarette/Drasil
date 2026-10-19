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

import Drasil.Shared.InterfaceCommon (Label, Library, MixedCall, TypeSym(..),
  VariableElim(..), Argument(..), Literal(..), MathConstant(..), VariableSym,
  ValueSym, VariableValue(..), ValueExpression(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  IndexTranslator(..), List(..), ListStatement, InternalList(..),
  AssignStatement(..), ScopeSym, DeclStatement(..), StringStatement(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  ParameterSym(..), BinderElim(..), UnRepr(..), BodySym, BlockSym)
import Drasil.Shared.AST (AttachmentTag, Terminator, VisibilityTag, OpData,
  FuncData)
import Drasil.Shared.State (MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

class (BodySym r bod block, BlockSym r block stmt,
  AssignStatement r stmt var val, ScopeSym r scope,
  DeclStatement r bod stmt var scope val, StringStatement r stmt var val,
  FuncAppStatement r stmt var val, CommentStatement r stmt,
  ControlStatement r bod stmt var val, Argument r val, Literal r val typ,
  MathConstant r val, ValueSym r val typ, VariableSym r var typ,
  VariableValue r var val, CommandLineArgs r val, NumericExpression r val,
  BooleanExpression r val, Comparison r val, IndexTranslator r val, List r val,
  ListStatement r stmt val, InternalList r block var val, VariableElim r var typ,
  BinderElim r binder typ, RenderBlock r block, BlockElim r block, RenderBody r bod,
  BodyElim r bod, InternalListFunc r val typ, RenderFunction r typ,
  FunctionElim r typ, OpElim r, RenderParam r param var, ParamElim r param typ,
  RenderVisibility r vis, VisibilityElim r vis,
  InternalAssignStmt r stmt var val, InternalIOStmt r stmt val,
  InternalControlStmt r stmt val, RenderStatement r stmt, StatementElim r stmt,
  RenderType r typ, RenderValue r var val typ, ValueElim r val,
  RenderVariable r var typ, InternalVarElim r var, InternalBinderElim r binder,
  ImportSym r, UnaryOpSym r, BinaryOpSym r, BlockCommentSym r,
  BlockCommentElim r, ValueExpression r var val binder typ, TypeSym r typ,
  MethodTypeSym r typ, RenderMethod r mthd, MethodElim r mthd,
  ParameterSym r param var, ScopeElim r scope
  ) => CommonRenderSym r mthd vis param bod block stmt var scope val binder typ

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

class ScopeElim r scope | r -> scope where
  scopeData :: r scope -> scope

class RenderVariable r var typ | r -> var typ where
  varFromData :: AttachmentTag -> String -> VS (r typ) -> Doc -> VS (r var)

class InternalVarElim r var | r -> var where
  variableBind :: r var -> AttachmentTag
  variable  :: r var -> Doc

class InternalBinderElim r binder where
  binderElim  :: r binder -> Doc

class RenderValue r var val typ | r -> var val typ where
  inputFunc       :: VS (r val)
  printFunc       :: VS (r val)
  printLnFunc     :: VS (r val)
  printFileFunc   :: VS (r val) -> VS (r val)
  printFileLnFunc :: VS (r val) -> VS (r val)

  cast :: VS (r typ) -> VS (r val) -> VS (r val)

  -- | Very generic internal function for generating calls, to reduce repeated
  -- code throughout generators.
  -- Parameters are: maybe name of external module, maybe Doc for object
  -- variable (including separator between object and function) for method
  -- calls.
  call :: Maybe Library -> Maybe Doc -> MixedCall r var val typ

  valFromData :: Maybe Int -> Maybe Integer -> VS (r typ) -> Doc -> VS (r val)

class ValueElim r val | r -> val where
  valuePrec :: r val -> Maybe Int
  valueInt :: r val -> Maybe Integer
  value :: r val -> Doc

class InternalListFunc r val typ | r -> val typ where
  -- | List, Index
  listAccessFunc :: VS (r typ) -> VS (r val) -> VS (r FuncData)

class RenderFunction r typ | r -> typ where
  funcFromData :: Doc -> VS (r typ) -> VS (r FuncData)

class FunctionElim r typ | r -> typ where
  functionType :: r FuncData -> r typ
  function :: r FuncData -> Doc

class InternalAssignStmt r stmt var val | r -> stmt var val where
  multiAssign :: [VS (r var)] -> [VS (r val)] -> MS (r stmt)

class InternalIOStmt r stmt val | r -> stmt val where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (VS (r val)) -> VS (r val) -> VS (r val) -> MS (r stmt)

class InternalControlStmt r stmt val | r -> stmt val where
  multiReturn :: [VS (r val)] -> MS (r stmt)

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

class RenderParam r param var | r -> param var where
  paramFromData :: VS (r var) -> Doc -> MS (r param)

class ParamElim r param typ | r -> param typ where
  parameterName :: r param -> Label
  parameterType :: r param -> r typ
  parameter     :: r param -> Doc

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
