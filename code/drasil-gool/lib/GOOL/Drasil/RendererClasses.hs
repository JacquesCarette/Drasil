{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.RendererClasses (
  RenderSym, RenderFile(..), ImportSym(..), ImportElim(..), PermElim(..), 
  RenderBody(..), BodyElim(..), RenderBlock(..), BlockElim(..), RenderType(..), 
  InternalTypeElim(..), VSUnOp, UnaryOpSym(..), VSBinOp, BinaryOpSym(..), 
  OpElim(..), RenderVariable(..), InternalVarElim(..), RenderValue(..), 
  ValueElim(..), InternalGetSet(..), InternalListFunc(..), RenderFunction(..),
  FunctionElim(..), InternalAssignStmt(..), InternalIOStmt(..),
  InternalControlStmt(..), RenderStatement(..), StatementElim(..),
  RenderScope(..), ScopeElim(..), MSMthdType, MethodTypeSym(..),
  RenderParam(..), ParamElim(..), RenderMethod(..), MethodElim(..),
  StateVarElim(..), ParentSpec, RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), BlockCommentSym(..), BlockCommentElim(..)
) where

import GOOL.Drasil.ClassInterface (Label, Library, SFile, MSBody, MSBlock, 
  VSType, SVariable, SValue, VSFunction, MSStatement, MSParameter,
  SMethod, CSStateVar, SClass, FSModule, MixedCall, FileSym(..),
  PermanenceSym(..), BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..),
  VariableSym(..), VariableElim(..), ValueSym(..), Argument(..), Literal(..),
  MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), InternalValueExp(..), FunctionSym(..), GetSet(..),
  List(..), InternalList(..), VectorExpression(..), StatementSym(..),
  AssignStatement(..), DeclStatement(..), IOStatement(..), StringStatement(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  StatePattern(..), ObserverPattern(..), StrategyPattern(..), ScopeSym(..),
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..),
  ModuleSym(..))
import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.AST (Binding, Terminator, ScopeTag)
import GOOL.Drasil.State (FS, CS, MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

class (FileSym r, AssignStatement r, DeclStatement r, IOStatement r, 
  StringStatement r, FuncAppStatement r, CommentStatement r, ControlStatement
  r, Argument r, Literal r, MathConstant r, VariableValue r, CommandLineArgs r,
  NumericExpression r, BooleanExpression r, Comparison r, ValueExpression r,
  InternalValueExp r, GetSet r, List r, InternalList r, VectorExpression r,
  StatePattern r, ObserverPattern r, StrategyPattern r, TypeElim r,
  VariableElim r, RenderBlock r, BlockElim r, RenderBody r, BodyElim r,
  RenderClass r, ClassElim r, RenderFile r, InternalGetSet r, InternalListFunc
  r, RenderFunction r, FunctionElim r, RenderMethod r, MethodElim r, RenderMod
  r, ModuleElim r, OpElim r, RenderParam r, ParamElim r, PermElim r,
  RenderScope r, ScopeElim r, InternalAssignStmt r, InternalIOStmt r,
  InternalControlStmt r, RenderStatement r, StatementElim r, StateVarElim r,
  RenderType r, InternalTypeElim r, RenderValue r, ValueElim r, RenderVariable
  r, InternalVarElim r, ImportSym r, ImportElim r, UnaryOpSym r, BinaryOpSym r,
  BlockCommentElim r) => RenderSym r

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header 
  -- files. FIXME: Remove them (generation of pre-processor guards can be 
  -- handled by fileDoc instead)
  top :: r (Module r) -> r (Block r) 
  bottom :: r (Block r)

  commentedMod :: SFile r -> FS (r (BlockComment r)) -> SFile r

  fileFromData :: FilePath -> FSModule r -> SFile r

class ImportSym r where
  type Import r
  langImport :: Label -> r (Import r)
  modImport :: Label -> r (Import r)

class ImportElim r where
  import' :: r (Import r) -> Doc

class PermElim r where
  perm :: r (Permanence r) -> Doc
  binding :: r (Permanence r) -> Binding

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

  valFromData :: Maybe Int -> VSType r -> Doc -> SValue r

class ValueElim r where
  valuePrec :: r (Value r) -> Maybe Int
  value :: r (Value r) -> Doc

class InternalGetSet r where
  getFunc :: SVariable r -> VSFunction r
  setFunc :: VSType r -> SVariable r -> SValue r -> VSFunction r

class InternalListFunc r where
  listSizeFunc   :: VSFunction r
  listAddFunc    :: SValue r -> SValue r -> SValue r -> VSFunction r
  listAppendFunc :: SValue r -> VSFunction r
  listAccessFunc :: VSType r -> SValue r -> VSFunction r
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

class RenderScope r where
  scopeFromData :: ScopeTag -> Doc -> r (Scope r)
  
class ScopeElim r where
  scope :: r (Scope r) -> Doc

type MSMthdType a = MS (a (MethodType a))

class (TypeSym r) => MethodTypeSym r where
  type MethodType r
  mType    :: VSType r -> MSMthdType r
  construct :: Label -> MSMthdType r

class RenderParam r where
  paramFromData :: SVariable r -> Doc -> MSParameter r
  
class ParamElim r where
  parameterName :: r (Parameter r) -> Label
  parameterType :: r (Parameter r) -> r (Type r)
  parameter     :: r (Parameter r) -> Doc

class (MethodTypeSym r, BlockCommentSym r) => 
  RenderMethod r where
  intMethod     :: Bool -> Label -> r (Scope r) -> r (Permanence r) -> 
    MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
  intFunc       :: Bool -> Label -> r (Scope r) -> r (Permanence r) 
    -> MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
  commentedFunc :: MS (r (BlockComment r)) -> SMethod r -> SMethod r
    
  destructor :: [CSStateVar r] -> SMethod r

  mthdFromData :: ScopeTag -> Doc -> SMethod r

class MethodElim r where
  method :: r (Method r) -> Doc

class StateVarElim r where  
  stateVar :: r (StateVar r) -> Doc

type ParentSpec = Doc

class (BlockCommentSym r) => RenderClass r where
  -- | Converts a class to an SClass (Doc with State in all renderers)
  --   Parameters: name, scope (public/private), parent (inherit/implement),
  --   variables, constructor(s), methods
  intClass :: Label -> r (Scope r) -> r ParentSpec -> [CSStateVar r] 
    -> [SMethod r] -> [SMethod r] -> SClass r
    
  inherit :: Maybe Label -> r ParentSpec
  implements :: [Label] -> r ParentSpec

  commentedClass :: CS (r (BlockComment r)) -> SClass r -> SClass r
  
class ClassElim r where
  class' :: r (Class r) -> Doc

class RenderMod r where
  modFromData :: String -> FS Doc -> FSModule r
  updateModuleDoc :: (Doc -> Doc) -> r (Module r) -> r (Module r)
  
class ModuleElim r where
  module' :: r (Module r) -> Doc

class BlockCommentSym r where
  type BlockComment r
  blockComment :: [String] -> r (BlockComment r)
  docComment :: State a [String] -> State a (r (BlockComment r))

class BlockCommentElim r where
  blockComment' :: r (BlockComment r) -> Doc
