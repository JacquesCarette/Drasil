{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.RendererClasses (
  RenderSym, InternalFile(..), ImportSym(..), ImportElim(..), PermElim(..), 
  InternalBody(..), BodyElim(..), InternalBlock(..), BlockElim(..), InternalType(..), InternalTypeElim(..), VSUnOp, UnaryOpSym(..),
  VSBinOp, BinaryOpSym(..), OpElim(..), OpIntro(..), InternalVarElim(..), InternalValue(..), ValueElim(..),
  InternalGetSet(..), InternalListFunc(..), InternalIterator(..), InternalFunction(..), FunctionElim(..), InternalAssignStmt(..), InternalIOStmt(..), InternalControlStmt(..), InternalStatement(..), StatementElim(..), InternalScope(..), 
  MethodTypeSym(..), InternalParam(..), InternalMethod(..), 
  InternalStateVar(..), ParentSpec, InternalClass(..), InternalMod(..), 
  BlockCommentSym(..)
) where

import GOOL.Drasil.ClassInterface (Label, Library, SFile, MSBody, MSBlock, 
  VSType, SVariable, SValue, VSFunction, MSStatement, MSParameter, SMethod, 
  CSStateVar, SClass, FSModule, NamedArgs, FileSym(..), PermanenceSym(..), BodySym(..), 
  BlockSym(..), TypeSym(..), VariableSym(..), ValueSym(..), FunctionSym(..), 
  StatementSym(..), ScopeSym(..), ParameterSym(..), MethodSym(..), 
  StateVarSym(..), ClassSym(..), ModuleSym(..))
import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.AST (Binding, Terminator, ScopeTag)
import GOOL.Drasil.State (FS, CS, MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

class (FileSym r, InternalBlock r, BlockElim r, InternalBody r, BodyElim r, InternalClass r, 
  InternalFile r, InternalGetSet r, InternalListFunc r, InternalIterator r, InternalFunction r, FunctionElim r, InternalMethod r, 
  InternalMod r, OpElim r, OpIntro r, InternalParam r, ParamElim r, PermElim r, 
  InternalScope r, ScopeElim r, InternalAssignStmt r, InternalIOStmt r, InternalControlStmt r, InternalStatement r, StatementElim r, InternalStateVar r, 
  InternalType r, InternalTypeElim r, InternalValue r, ValueElim r, InternaVariable r, InternalVarElim r,
  ImportSym r, ImportElim r, UnaryOpSym r, BinaryOpSym r) => RenderSym r

class (BlockCommentSym r) => InternalFile r where
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

class InternalBody r where
  docBody :: MS Doc -> MSBody r
  multiBody :: [MSBody r] -> MSBody r

class BodyElim r where
  bodyDoc :: r (Body r) -> Doc

class InternalBlock r where
  docBlock :: MS Doc -> MSBlock r
  multiBlock :: [MSBlock r] -> MSBlock r

class BlockElim r where
  blockDoc :: r (Block r) -> Doc

class InternalType r where
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

class OpIntro r where
  uOpFromData :: Int -> Doc -> VSUnOp r
  bOpFromData :: Int -> Doc -> VSBinOp r
  
class InternalVariable r where
  varFromData :: Binding -> String -> r (Type r) -> Doc -> 
    r (Variable r)
    
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
  printSt :: Bool -> Maybe (SValue r) -> SValue r -> SValue r -> 
    MSStatement r
    
class InternalControlStmt r where
  multiReturn :: [SValue r] -> MSStatement r

class InternalStatement r where
  state     :: MSStatement r -> MSStatement r
  loopState :: MSStatement r -> MSStatement r

  emptyState   :: MSStatement r

  stateFromData :: Doc -> Terminator -> r (Statement r)

class StatementElim r where
  statementDoc :: r (Statement r) -> Doc
  statementTerm :: r (Statement r) -> Terminator

class InternalScope r where
  scopeFromData :: ScopeTag -> Doc -> r (Scope r)
  
class ScopeElim r where
  scopeDoc :: r (Scope r) -> Doc

class (TypeSym r) => MethodTypeSym r where
  type MethodType r
  mType    :: VSType r -> MS (r (MethodType r))
  construct :: Label -> MS (r (MethodType r))

class InternalParam r where
  paramFromData :: r (Variable r) -> Doc -> r (Parameter r)
  
class ParamElim r where
  parameterName :: r (Parameter r) -> Label
  parameterType :: r (Parameter r) -> r (Type r)
  parameterDoc  :: r (Parameter r) -> Doc

class (MethodTypeSym r, BlockCommentSym r, StateVarSym r) => 
  InternalMethod r where
  intMethod     :: Bool -> Label -> r (Scope r) -> r (Permanence r) 
    -> MS (r (MethodType r)) -> [MSParameter r] -> MSBody r -> 
    SMethod r
  intFunc       :: Bool -> Label -> r (Scope r) -> r (Permanence r) 
    -> MS (r (MethodType r)) -> [MSParameter r] -> MSBody r -> 
    SMethod r
  commentedFunc :: MS (r (BlockComment r)) -> SMethod r -> SMethod r
    
  destructor :: [CSStateVar r] -> SMethod r

  methodDoc :: r (Method r) -> Doc
  methodFromData :: ScopeTag -> Doc -> r (Method r)

class InternalStateVar r where
  stateVarDoc :: r (StateVar r) -> Doc
  stateVarFromData :: CS Doc -> CSStateVar r

type ParentSpec = Doc

class (BlockCommentSym r) => InternalClass r where
  intClass :: Label -> r (Scope r) -> r ParentSpec -> [CSStateVar r]
    -> [SMethod r] -> SClass r
    
  inherit :: Maybe Label -> r ParentSpec
  implements :: [Label] -> r ParentSpec

  commentedClass :: CS (r (BlockComment r)) -> SClass r -> SClass r

  classDoc :: r (Class r) -> Doc
  classFromData :: CS (r Doc) -> SClass r

class InternalMod r where
  moduleDoc :: r (Module r) -> Doc
  modFromData :: String -> FS Doc -> FSModule r
  updateModuleDoc :: (Doc -> Doc) -> r (Module r) -> r (Module r)

class BlockCommentSym r where
  type BlockComment r
  blockComment :: [String] -> r (BlockComment r)
  docComment :: State a [String] -> State a (r (BlockComment r))

  blockCommentDoc :: r (BlockComment r) -> Doc
