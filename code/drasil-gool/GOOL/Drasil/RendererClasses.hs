{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.RendererClasses (
  RenderSym, InternalFile(..), ImportSym(..), InternalPerm(..), 
  InternalBody(..), InternalBlock(..), InternalType(..), VSUnOp, UnaryOpSym(..),
  VSBinOp, BinaryOpSym(..), InternalOp(..), InternalVariable(..), InternalValue(..),
  InternalGetSet(..), InternalListFunc(..), InternalIterator(..), InternalFunction(..), InternalAssignStmt(..), InternalIOStmt(..), InternalControlStmt(..), InternalStatement(..), InternalScope(..), 
  MethodTypeSym(..), InternalParam(..), InternalMethod(..), 
  InternalStateVar(..), ParentSpec, InternalClass(..), InternalMod(..), 
  BlockCommentSym(..)
) where

import GOOL.Drasil.ClassInterface (Label, Library, SFile, MSBody, MSBlock, 
  VSType, SVariable, SValue, VSFunction, MSStatement, MSParameter, SMethod, 
  CSStateVar, SClass, FSModule, NamedArg, FileSym(..), PermanenceSym(..), BodySym(..), 
  BlockSym(..), TypeSym(..), VariableSym(..), ValueSym(..), FunctionSym(..), 
  StatementSym(..), ScopeSym(..), ParameterSym(..), MethodSym(..), 
  StateVarSym(..), ClassSym(..), ModuleSym(..))
import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.AST (Binding, Terminator, ScopeTag)
import GOOL.Drasil.State (FS, CS, MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

class (FileSym repr, InternalBlock repr, InternalBody repr, InternalClass repr, 
  InternalFile repr, InternalGetSet repr, InternalListFunc repr, InternalIterator repr, InternalFunction repr, InternalMethod repr, 
  InternalMod repr, InternalOp repr, InternalParam repr, InternalPerm repr, 
  InternalScope repr, InternalAssignStmt repr, InternalIOStmt repr, InternalControlStmt repr, InternalStatement repr, InternalStateVar repr, 
  InternalType repr, InternalValue repr, InternalVariable repr,
  ImportSym repr, UnaryOpSym repr, BinaryOpSym repr) => RenderSym repr

class (BlockCommentSym repr) => InternalFile repr where
  top :: repr (Module repr) -> repr (Block repr)
  bottom :: repr (Block repr)

  commentedMod :: FS (repr (BlockComment repr)) -> SFile repr -> SFile repr

  fileFromData :: FS FilePath -> FSModule repr -> SFile repr

class ImportSym repr where
  type Import repr
  langImport :: Label -> repr (Import repr)
  modImport :: Label -> repr (Import repr)

  importDoc :: repr (Import repr) -> Doc

class InternalPerm repr where
  permDoc :: repr (Permanence repr) -> Doc
  binding :: repr (Permanence repr) -> Binding

class InternalBody repr where
  bodyDoc :: repr (Body repr) -> Doc
  docBody :: MS Doc -> MSBody repr
  multiBody :: [MSBody repr] -> MSBody repr

class InternalBlock repr where
  blockDoc :: repr (Block repr) -> Doc
  docBlock :: MS Doc -> MSBlock repr
  multiBlock :: [MSBlock repr] -> MSBlock repr

class InternalType repr where
  getTypeDoc :: repr (Type repr) -> Doc
  typeFromData :: CodeType -> String -> Doc -> repr (Type repr)

type VSUnOp a = VS (a (UnaryOp a))

class UnaryOpSym repr where
  type UnaryOp repr
  notOp    :: VSUnOp repr
  negateOp :: VSUnOp repr
  sqrtOp   :: VSUnOp repr
  absOp    :: VSUnOp repr
  logOp    :: VSUnOp repr
  lnOp     :: VSUnOp repr
  expOp    :: VSUnOp repr
  sinOp    :: VSUnOp repr
  cosOp    :: VSUnOp repr
  tanOp    :: VSUnOp repr
  asinOp   :: VSUnOp repr
  acosOp   :: VSUnOp repr
  atanOp   :: VSUnOp repr
  floorOp  :: VSUnOp repr
  ceilOp   :: VSUnOp repr

type VSBinOp a = VS (a (BinaryOp a))

class BinaryOpSym repr where
  type BinaryOp repr
  equalOp        :: VSBinOp repr
  notEqualOp     :: VSBinOp repr
  greaterOp      :: VSBinOp repr
  greaterEqualOp :: VSBinOp repr
  lessOp         :: VSBinOp repr
  lessEqualOp    :: VSBinOp repr
  plusOp         :: VSBinOp repr
  minusOp        :: VSBinOp repr
  multOp         :: VSBinOp repr
  divideOp       :: VSBinOp repr
  powerOp        :: VSBinOp repr
  moduloOp       :: VSBinOp repr
  andOp          :: VSBinOp repr
  orOp           :: VSBinOp repr

class InternalOp repr where
  uOpDoc :: repr (UnaryOp repr) -> Doc
  bOpDoc :: repr (BinaryOp repr) -> Doc
  uOpPrec :: repr (UnaryOp repr) -> Int
  bOpPrec :: repr (BinaryOp repr) -> Int

  uOpFromData :: Int -> Doc -> VSUnOp repr
  bOpFromData :: Int -> Doc -> VSBinOp repr

class InternalVariable repr where
  variableBind :: repr (Variable repr) -> Binding
  variableDoc  :: repr (Variable repr) -> Doc
  varFromData :: Binding -> String -> repr (Type repr) -> Doc -> 
    repr (Variable repr)

class InternalValue repr where
  inputFunc       :: SValue repr
  printFunc       :: SValue repr
  printLnFunc     :: SValue repr
  printFileFunc   :: SValue repr -> SValue repr
  printFileLnFunc :: SValue repr -> SValue repr

  cast :: VSType repr -> SValue repr -> SValue repr

  -- Very generic internal function for generating calls, to reduce repeated code throughout generators
  -- Maybe library, function name, return type, maybe object doc, regular arguments, named arguments
  call :: Maybe Library -> Label -> VSType repr -> Maybe Doc -> [SValue repr] 
    -> [NamedArg repr] -> SValue repr

  valuePrec :: repr (Value repr) -> Maybe Int
  valueDoc :: repr (Value repr) -> Doc
  valFromData :: Maybe Int -> repr (Type repr) -> Doc -> repr (Value repr)

class InternalGetSet repr where
  getFunc :: SVariable repr -> VSFunction repr
  setFunc :: VSType repr -> SVariable repr -> SValue repr -> VSFunction repr

class InternalListFunc repr where
  listSizeFunc   :: VSFunction repr
  listAddFunc    :: SValue repr -> SValue repr -> SValue repr -> VSFunction repr
  listAppendFunc :: SValue repr -> VSFunction repr
  listAccessFunc :: VSType repr -> SValue repr -> VSFunction repr
  listSetFunc    :: SValue repr -> SValue repr -> SValue repr -> VSFunction repr

class InternalIterator repr where
  iterBeginFunc :: VSType repr -> VSFunction repr
  iterEndFunc   :: VSType repr -> VSFunction repr

class InternalFunction repr where
  functionType :: repr (Function repr) -> repr (Type repr)
  functionDoc :: repr (Function repr) -> Doc

  funcFromData :: Doc -> VSType repr -> VSFunction repr

class InternalAssignStmt repr where
  multiAssign       :: [SVariable repr] -> [SValue repr] -> MSStatement repr 

class InternalIOStmt repr where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (SValue repr) -> SValue repr -> SValue repr -> 
    MSStatement repr
    
class InternalControlStmt repr where
  multiReturn :: [SValue repr] -> MSStatement repr

class InternalStatement repr where
  state     :: MSStatement repr -> MSStatement repr
  loopState :: MSStatement repr -> MSStatement repr

  emptyState   :: MSStatement repr
  statementDoc :: repr (Statement repr) -> Doc
  statementTerm :: repr (Statement repr) -> Terminator

  stateFromData :: Doc -> Terminator -> repr (Statement repr)

class InternalScope repr where
  scopeDoc :: repr (Scope repr) -> Doc
  scopeFromData :: ScopeTag -> Doc -> repr (Scope repr)

class (TypeSym repr) => MethodTypeSym repr where
  type MethodType repr
  mType    :: VSType repr -> MS (repr (MethodType repr))
  construct :: Label -> MS (repr (MethodType repr))

class InternalParam repr where
  parameterName :: repr (Parameter repr) -> Label
  parameterType :: repr (Parameter repr) -> repr (Type repr)
  parameterDoc  :: repr (Parameter repr) -> Doc
  paramFromData :: repr (Variable repr) -> Doc -> repr (Parameter repr)

class (MethodTypeSym repr, BlockCommentSym repr, StateVarSym repr) => 
  InternalMethod repr where
  intMethod     :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> MS (repr (MethodType repr)) -> [MSParameter repr] -> MSBody repr -> 
    SMethod repr
  intFunc       :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> MS (repr (MethodType repr)) -> [MSParameter repr] -> MSBody repr -> 
    SMethod repr
  commentedFunc :: MS (repr (BlockComment repr)) -> SMethod repr -> SMethod repr
    
  destructor :: [CSStateVar repr] -> SMethod repr

  methodDoc :: repr (Method repr) -> Doc
  methodFromData :: ScopeTag -> Doc -> repr (Method repr)

class InternalStateVar repr where
  stateVarDoc :: repr (StateVar repr) -> Doc
  stateVarFromData :: CS Doc -> CSStateVar repr

type ParentSpec = Doc

class (BlockCommentSym repr) => InternalClass repr where
  intClass :: Label -> repr (Scope repr) -> repr ParentSpec -> [CSStateVar repr]
    -> [SMethod repr] -> SClass repr
    
  inherit :: Maybe Label -> repr ParentSpec
  implements :: [Label] -> repr ParentSpec

  commentedClass :: CS (repr (BlockComment repr)) -> SClass repr -> SClass repr

  classDoc :: repr (Class repr) -> Doc
  classFromData :: CS (repr Doc) -> SClass repr

class InternalMod repr where
  moduleDoc :: repr (Module repr) -> Doc
  modFromData :: String -> FS Doc -> FSModule repr
  updateModuleDoc :: (Doc -> Doc) -> repr (Module repr) -> repr (Module repr)

class BlockCommentSym repr where
  type BlockComment repr
  blockComment :: [String] -> repr (BlockComment repr)
  docComment :: State a [String] -> State a (repr (BlockComment repr))

  blockCommentDoc :: repr (BlockComment repr) -> Doc
