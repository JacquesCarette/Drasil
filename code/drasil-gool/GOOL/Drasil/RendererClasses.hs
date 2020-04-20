{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.RendererClasses (
  RenderSym, RenderFile(..), ImportSym(..), ImportElim(..), PermElim(..), 
  RenderBody(..), BodyElim(..), RenderBlock(..), BlockElim(..), 
  RenderType(..), InternalTypeElim(..), VSUnOp, UnaryOpSym(..), VSBinOp, 
  BinaryOpSym(..), OpElim(..), RenderOp(..), InternalVariable(..), 
  InternalVarElim(..), InternalValue(..), ValueElim(..), InternalGetSet(..), 
  InternalListFunc(..), InternalIterator(..), InternalFunction(..), 
  FunctionElim(..), InternalAssignStmt(..), InternalIOStmt(..),
  InternalControlStmt(..), InternalStatement(..), StatementElim(..), 
  InternalScope(..), ScopeElim(..), MSMthdType, MethodTypeSym(..), 
  InternalParam(..), ParamElim(..), InternalMethod(..), MethodElim(..), 
  InternalStateVar(..), StateVarElim(..), ParentSpec, InternalClass(..), 
  ClassElim(..), InternalMod(..), ModuleElim(..), BlockCommentSym(..), 
  BlockCommentElim(..)
) where

import GOOL.Drasil.ClassInterface (Label, Library, SFile, MSBody, MSBlock, 
  VSType, SVariable, SValue, VSFunction, MSStatement, MSParameter, SMethod, 
  CSStateVar, SClass, FSModule, NamedArgs, FileSym(..), PermanenceSym(..), 
  BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..),
  VariableSym(..), VariableElim(..), ValueSym(..), Literal(..), 
  MathConstant(..), VariableValue(..), CommandLineArgs(..), 
  NumericExpression(..), BooleanExpression(..), Comparison(..), 
  ValueExpression(..), InternalValueExp(..), FunctionSym(..), GetSet(..), 
  List(..), InternalList(..), Iterator(..), StatementSym(..), 
  AssignStatement(..), DeclStatement(..), IOStatement(..), StringStatement(..), 
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..), 
  StatePattern(..), ObserverPattern(..), StrategyPattern(..), ScopeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.AST (Binding, Terminator, ScopeTag)
import GOOL.Drasil.State (FS, CS, MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

class (FileSym r, AssignStatement r, DeclStatement r, IOStatement r, 
  StringStatement r, FuncAppStatement r, CommentStatement r, ControlStatement r,
  Literal r, MathConstant r, VariableValue r, CommandLineArgs r,
  NumericExpression r, BooleanExpression r, Comparison r, ValueExpression r, 
  InternalValueExp r, GetSet r, List r, InternalList r, Iterator r, 
  StatePattern r, ObserverPattern r, StrategyPattern r, TypeElim r, 
  VariableElim r, RenderBlock r, BlockElim r, RenderBody r, BodyElim r, 
  InternalClass r, ClassElim r, RenderFile r, InternalGetSet r, 
  InternalListFunc r, InternalIterator r, InternalFunction r, FunctionElim r, 
  InternalMethod r, MethodElim r, InternalMod r, ModuleElim r, OpElim r, 
  RenderOp r, InternalParam r, ParamElim r, PermElim r, InternalScope r, 
  ScopeElim r, InternalAssignStmt r, InternalIOStmt r, InternalControlStmt r, 
  InternalStatement r, StatementElim r, InternalStateVar r, StateVarElim r, 
  RenderType r, InternalTypeElim r, InternalValue r, ValueElim r, 
  InternalVariable r, InternalVarElim r, ImportSym r, ImportElim r, 
  UnaryOpSym r, BinaryOpSym r, BlockCommentElim r) => RenderSym r

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header 
  -- files. FIXME: Remove them (generation of pre-processor guards can be 
  -- handled by fileDoc instead)
  top :: r (Module r) -> r (Block r) 
  bottom :: r (Block r)

  commentedMod :: FS (r (BlockComment r)) -> SFile r -> SFile r

  fileFromData :: FS FilePath -> FSModule r -> SFile r

class ImportSym r where
  type Import r
  langImport :: Label -> r (Import r)
  modImport :: Label -> r (Import r)

class ImportElim r where
  importDoc :: r (Import r) -> Doc

class PermElim r where
  permDoc :: r (Permanence r) -> Doc
  binding :: r (Permanence r) -> Binding

class RenderBody r where
  docBody :: MS Doc -> MSBody r
  multiBody :: [MSBody r] -> MSBody r

class BodyElim r where
  bodyDoc :: r (Body r) -> Doc

class RenderBlock r where
  docBlock :: MS Doc -> MSBlock r
  multiBlock :: [MSBlock r] -> MSBlock r

class BlockElim r where
  blockDoc :: r (Block r) -> Doc

class RenderType r where
  typeFromData :: CodeType -> String -> Doc -> r (Type r)

class InternalTypeElim r where
  getTypeDoc :: r (Type r) -> Doc

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
  uOpDoc :: r (UnaryOp r) -> Doc
  bOpDoc :: r (BinaryOp r) -> Doc
  uOpPrec :: r (UnaryOp r) -> Int
  bOpPrec :: r (BinaryOp r) -> Int

class RenderOp r where
  uOpFromData :: Int -> Doc -> VSUnOp r
  bOpFromData :: Int -> Doc -> VSBinOp r
  
class InternalVariable r where
  varFromData :: Binding -> String -> r (Type r) -> Doc -> r (Variable r)
    
class InternalVarElim r where
  variableBind :: r (Variable r) -> Binding
  variableDoc  :: r (Variable r) -> Doc

class InternalValue r where
  inputFunc       :: SValue r
  printFunc       :: SValue r
  printLnFunc     :: SValue r
  printFileFunc   :: SValue r -> SValue r
  printFileLnFunc :: SValue r -> SValue r

  cast :: VSType r -> SValue r -> SValue r

  -- Very generic internal function for generating calls, to reduce repeated code throughout generators
  -- Maybe library, function name, return type, maybe object doc, regular arguments, named arguments
  call :: Maybe Library -> Label -> VSType r -> Maybe Doc -> [SValue r] 
    -> NamedArgs r -> SValue r

  valFromData :: Maybe Int -> r (Type r) -> Doc -> r (Value r)

class ValueElim r where
  valuePrec :: r (Value r) -> Maybe Int
  valueDoc :: r (Value r) -> Doc

class InternalGetSet r where
  getFunc :: SVariable r -> VSFunction r
  setFunc :: VSType r -> SVariable r -> SValue r -> VSFunction r

class InternalListFunc r where
  listSizeFunc   :: VSFunction r
  listAddFunc    :: SValue r -> SValue r -> SValue r -> VSFunction r
  listAppendFunc :: SValue r -> VSFunction r
  listAccessFunc :: VSType r -> SValue r -> VSFunction r
  listSetFunc    :: SValue r -> SValue r -> SValue r -> VSFunction r

class InternalIterator r where
  iterBeginFunc :: VSType r -> VSFunction r
  iterEndFunc   :: VSType r -> VSFunction r

class InternalFunction r where
  funcFromData :: Doc -> VSType r -> VSFunction r
  
class FunctionElim r where
  functionType :: r (Function r) -> r (Type r)
  functionDoc :: r (Function r) -> Doc

class InternalAssignStmt r where
  multiAssign       :: [SVariable r] -> [SValue r] -> MSStatement r 

class InternalIOStmt r where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (SValue r) -> SValue r -> SValue r -> MSStatement r
    
class InternalControlStmt r where
  multiReturn :: [SValue r] -> MSStatement r

class InternalStatement r where
  stmt     :: MSStatement r -> MSStatement r
  loopStmt :: MSStatement r -> MSStatement r

  emptyStmt   :: MSStatement r

  stmtFromData :: Doc -> Terminator -> r (Statement r)

class StatementElim r where
  statementDoc :: r (Statement r) -> Doc
  statementTerm :: r (Statement r) -> Terminator

class InternalScope r where
  scopeFromData :: ScopeTag -> Doc -> r (Scope r)
  
class ScopeElim r where
  scopeDoc :: r (Scope r) -> Doc

type MSMthdType a = MS (a (MethodType a))

class (TypeSym r) => MethodTypeSym r where
  type MethodType r
  mType    :: VSType r -> MSMthdType r
  construct :: Label -> MSMthdType r

class InternalParam r where
  paramFromData :: r (Variable r) -> Doc -> r (Parameter r)
  
class ParamElim r where
  parameterName :: r (Parameter r) -> Label
  parameterType :: r (Parameter r) -> r (Type r)
  parameterDoc  :: r (Parameter r) -> Doc

class (MethodTypeSym r, BlockCommentSym r, StateVarSym r) => 
  InternalMethod r where
  intMethod     :: Bool -> Label -> r (Scope r) -> r (Permanence r) -> 
    MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
  intFunc       :: Bool -> Label -> r (Scope r) -> r (Permanence r) 
    -> MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
  commentedFunc :: MS (r (BlockComment r)) -> SMethod r -> SMethod r
    
  destructor :: [CSStateVar r] -> SMethod r

  methodFromData :: ScopeTag -> Doc -> r (Method r)

class MethodElim r where
  methodDoc :: r (Method r) -> Doc

class InternalStateVar r where
  stateVarFromData :: CS Doc -> CSStateVar r

class StateVarElim r where  
  stateVarDoc :: r (StateVar r) -> Doc

type ParentSpec = Doc

class (BlockCommentSym r) => InternalClass r where
  intClass :: Label -> r (Scope r) -> r ParentSpec -> [CSStateVar r] 
    -> [SMethod r] -> SClass r
    
  inherit :: Maybe Label -> r ParentSpec
  implements :: [Label] -> r ParentSpec

  commentedClass :: CS (r (BlockComment r)) -> SClass r -> SClass r

  classFromData :: CS (r Doc) -> SClass r
  
class ClassElim r where
  classDoc :: r (Class r) -> Doc

class InternalMod r where
  modFromData :: String -> FS Doc -> FSModule r
  updateModuleDoc :: (Doc -> Doc) -> r (Module r) -> r (Module r)
  
class ModuleElim r where
  moduleDoc :: r (Module r) -> Doc

class BlockCommentSym r where
  type BlockComment r
  blockComment :: [String] -> r (BlockComment r)
  docComment :: State a [String] -> State a (r (BlockComment r))

class BlockCommentElim r where
  blockCommentDoc :: r (BlockComment r) -> Doc
