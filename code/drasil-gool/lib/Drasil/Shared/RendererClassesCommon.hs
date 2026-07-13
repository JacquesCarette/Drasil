{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.Shared.RendererClassesCommon (
  CommonRenderSym, ImportSym(..), import', RenderBody(..), BodyElim(..),
  RenderBlock(..), BlockElim(..), RenderType(..), VSUnOp, UnaryOpSym(..),
  VSBinOp, BinaryOpSym(..), OpElim(..), RenderVariable(..), InternalVarElim(..),
  InternalBinderElim(..), RenderValue(..), ValueElim(..), InternalListFunc(..),
  RenderFunction(..), FunctionElim(..), InternalAssignStmt(..),
  InternalIOStmt(..), InternalControlStmt(..), RenderStatement(..),
  StatementElim(..), RenderVisibility(..), VisibilityElim(..), MSMthdType,
  MethodTypeSym(..), RenderParam(..), ParamElim(..), RenderMethod(..),
  MethodElim(..), BlockCommentSym(..), BlockCommentElim(..), ScopeElim(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Library, MSBody, MSBlock, SVariable,
  SValue, MixedCall, BodySym(..), BlockSym(..), TypeSym(..), VariableSym(..),
  VariableElim(..), ValueSym(..), Argument(..), Literal(..), MathConstant(..),
  VariableValue(..), ValueExpression(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  IndexTranslator(..), List(..), InternalList(..), AssignStatement(..),
  DeclStatement(..), IOStatement(..), StringStatement(..), FuncAppStatement(..),
  CommentStatement(..), ControlStatement(..), ParameterSym(..), MethodSym(..),
  BinderElim(..), UnRepr(..))
import Drasil.Shared.AST (AttachmentTag, Terminator, VisibilityTag, ScopeData,
  OpData, BinderD, TypeData, ParamData, FuncData)
import Drasil.Shared.State (MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

class (AssignStatement r smt, DeclStatement r smt, IOStatement r smt,
  StringStatement r smt, FuncAppStatement r smt, CommentStatement r smt,
  ControlStatement r smt, Argument r, Literal r, MathConstant r,
  VariableValue r, CommandLineArgs r, NumericExpression r,
  BooleanExpression r, Comparison r, IndexTranslator r, List r smt,
  InternalList r, VariableElim r, BinderElim r, RenderBlock r,
  BlockElim r, RenderBody r, BodyElim r, InternalListFunc r,
  RenderFunction r, FunctionElim r, OpElim r, RenderParam r,
  ParamElim r, RenderVisibility r vis, VisibilityElim r vis,
  InternalAssignStmt r smt, InternalIOStmt r smt, InternalControlStmt r smt,
  RenderStatement r smt, StatementElim r smt, RenderType r, RenderValue r,
  ValueElim r, RenderVariable r, InternalVarElim r, InternalBinderElim r,
  ImportSym r, UnaryOpSym r, BinaryOpSym r, BlockCommentSym r,
  BlockCommentElim r, ValueExpression r, RenderMethod r, MethodElim r,
  ParameterSym r, ScopeElim r
  ) => CommonRenderSym r vis smt

-- Common Typeclasses --

class ImportSym r where
  -- For importing an external library
  langImport :: Label -> r Doc
  -- For importing a local (same project) module
  modImport :: Label -> r Doc

import' :: (UnRepr r Doc) => r Doc -> Doc
import' = unRepr

class RenderBody r where
  multiBody :: [MSBody r] -> MSBody r

class BodyElim r where
  body :: r (Body r) -> Doc

class RenderBlock r where
  multiBlock :: [MSBlock r] -> MSBlock r

class BlockElim r where
  block :: r (Block r) -> Doc

class RenderType r where
  multiType :: [VS (r TypeData)] -> VS (r TypeData)

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

class RenderVariable r where
  varFromData :: AttachmentTag -> String -> VS (r TypeData) -> Doc -> SVariable r

class InternalVarElim r where
  variableBind :: r (Variable r) -> AttachmentTag
  variable  :: r (Variable r) -> Doc

class InternalBinderElim r where
  binderElim  :: r BinderD -> Doc

class RenderValue r where
  inputFunc       :: SValue r
  printFunc       :: SValue r
  printLnFunc     :: SValue r
  printFileFunc   :: SValue r -> SValue r
  printFileLnFunc :: SValue r -> SValue r

  cast :: VS (r TypeData) -> SValue r -> SValue r

  -- | Very generic internal function for generating calls, to reduce repeated
  -- code throughout generators.
  -- Parameters are: maybe name of external module, maybe Doc for object
  -- variable (including separator between object and function) for method
  -- calls.
  call :: Maybe Library -> Maybe Doc -> MixedCall r

  valFromData :: Maybe Int -> Maybe Integer -> VS (r TypeData) -> Doc -> SValue r

class ValueElim r where
  valuePrec :: r (Value r) -> Maybe Int
  valueInt :: r (Value r) -> Maybe Integer
  value :: r (Value r) -> Doc

class InternalListFunc r where
  -- | List, Index
  listAccessFunc :: VS (r TypeData) -> SValue r -> VS (r FuncData)

class RenderFunction r where
  funcFromData :: Doc -> VS (r TypeData) -> VS (r FuncData)

class FunctionElim r where
  functionType :: r FuncData -> r TypeData
  function :: r FuncData -> Doc

class InternalAssignStmt r smt | r -> smt where
  multiAssign       :: [SVariable r] -> [SValue r] -> MS (r smt)

class InternalIOStmt r smt | r -> smt where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (SValue r) -> SValue r -> SValue r -> MS (r smt)

class InternalControlStmt r smt | r -> smt where
  multiReturn :: [SValue r] -> MS (r smt)

class RenderStatement r smt | r -> smt where
  stmt     :: MS (r smt) -> MS (r smt)
  loopStmt :: MS (r smt) -> MS (r smt)

  stmtFromData :: Doc -> Terminator -> MS (r smt)

class StatementElim r smt | r -> smt where
  statement :: r smt -> Doc
  statementTerm :: r smt -> Terminator

class RenderVisibility r vis | r -> vis where
  visibilityFromData :: VisibilityTag -> Doc -> r vis

class VisibilityElim r vis | r -> vis where
  visibility :: r vis -> Doc

class RenderParam r where
  paramFromData :: SVariable r -> Doc -> MS (r ParamData)

class ParamElim r where
  parameterName :: r ParamData -> Label
  parameterType :: r ParamData -> r TypeData
  parameter     :: r ParamData -> Doc

class BlockCommentSym r where
  blockComment :: [String] -> r Doc
  -- | Converts a list of strings into a block comment
  docComment :: State a [String] -> State a (r Doc)

class BlockCommentElim r where
  blockComment' :: r Doc -> Doc

type MSMthdType a = MS (a (MethodType a))

class (TypeSym r) => MethodTypeSym r where
  type MethodType r
  mType    :: VS (r TypeData) -> MSMthdType r

class (MethodTypeSym r, BlockCommentSym r) => RenderMethod r where
  -- | Takes a BlockComment and a method and generates a function.
  commentedFunc :: MS (r Doc) -> MS (r (Method r)) -> MS (r (Method r))
  mthdFromData :: VisibilityTag -> Doc -> MS (r (Method r))

class MethodElim r where
  method :: r (Method r) -> Doc
