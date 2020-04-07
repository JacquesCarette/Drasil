{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.RendererClasses (
  RenderSym, InternalFile(..), KeywordSym(..), ImportSym(..), InternalPerm(..),
  InternalBody(..), InternalBlock(..), InternalType(..), UnaryOpSym(..), 
  BinaryOpSym(..), InternalOp(..), InternalVariable(..), InternalValue(..),
  InternalFunction(..), InternalStatement(..), InternalScope(..), 
  MethodTypeSym(..), InternalParam(..), InternalMethod(..), 
  InternalStateVar(..), InternalClass(..), InternalMod(..), BlockCommentSym(..)
) where

import GOOL.Drasil.ClassInterface (Label, Library, FileSym(..), 
  PermanenceSym(..), BodySym(..), BlockSym(..), TypeSym(..), VariableSym(..), 
  ValueSym(..), FunctionSym(..), StatementSym(..), ScopeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.AST (Binding, Terminator, ScopeTag)
import GOOL.Drasil.State (FS, CS, MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

class (FileSym repr, InternalBlock repr, InternalBody repr, InternalClass repr, 
  InternalFile repr, InternalFunction repr, InternalMethod repr, 
  InternalMod repr, InternalOp repr, InternalParam repr, InternalPerm repr, 
  InternalScope repr, InternalStatement repr, InternalStateVar repr, 
  InternalType repr, InternalValue repr, InternalVariable repr, KeywordSym repr,
  ImportSym repr, UnaryOpSym repr, BinaryOpSym repr) => RenderSym repr

class (BlockCommentSym repr) => InternalFile repr where
  top :: repr (Module repr) -> repr (Block repr)
  bottom :: repr (Block repr)

  commentedMod :: FS (repr (BlockComment repr)) -> FS (repr (RenderFile repr)) 
    -> FS (repr (RenderFile repr))

  fileFromData :: FS FilePath -> FS (repr (Module repr)) -> 
    FS (repr (RenderFile repr))

class KeywordSym repr where
  type Keyword repr
  endStatement     :: repr (Keyword repr)
  endStatementLoop :: repr (Keyword repr)

  inherit :: Label -> repr (Keyword repr)
  implements :: [Label] -> repr (Keyword repr)

  list     :: repr (Keyword repr)

  blockStart :: repr (Keyword repr)
  blockEnd   :: repr (Keyword repr)

  ifBodyStart :: repr (Keyword repr)
  elseIf      :: repr (Keyword repr)

  iterForEachLabel :: repr (Keyword repr)
  iterInLabel      :: repr (Keyword repr)

  commentStart      :: repr (Keyword repr)
  blockCommentStart :: repr (Keyword repr)
  blockCommentEnd   :: repr (Keyword repr)
  docCommentStart   :: repr (Keyword repr)
  docCommentEnd     :: repr (Keyword repr)

  keyFromDoc :: Doc -> repr (Keyword repr)
  keyDoc :: repr (Keyword repr) -> Doc

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
  docBody :: MS Doc -> MS (repr (Body repr))
  multiBody :: [MS (repr (Body repr))] -> MS (repr (Body repr))

class InternalBlock repr where
  blockDoc :: repr (Block repr) -> Doc
  docBlock :: MS Doc -> MS (repr (Block repr))
  multiBlock :: [MS (repr (Block repr))] -> MS (repr (Block repr))

class InternalType repr where
  getTypeDoc :: repr (Type repr) -> Doc
  typeFromData :: CodeType -> String -> Doc -> repr (Type repr)

class UnaryOpSym repr where
  type UnaryOp repr
  notOp    :: VS (repr (UnaryOp repr))
  negateOp :: VS (repr (UnaryOp repr))
  sqrtOp   :: VS (repr (UnaryOp repr))
  absOp    :: VS (repr (UnaryOp repr))
  logOp    :: VS (repr (UnaryOp repr))
  lnOp     :: VS (repr (UnaryOp repr))
  expOp    :: VS (repr (UnaryOp repr))
  sinOp    :: VS (repr (UnaryOp repr))
  cosOp    :: VS (repr (UnaryOp repr))
  tanOp    :: VS (repr (UnaryOp repr))
  asinOp   :: VS (repr (UnaryOp repr))
  acosOp   :: VS (repr (UnaryOp repr))
  atanOp   :: VS (repr (UnaryOp repr))
  floorOp  :: VS (repr (UnaryOp repr))
  ceilOp   :: VS (repr (UnaryOp repr))

class BinaryOpSym repr where
  type BinaryOp repr
  equalOp        :: VS (repr (BinaryOp repr))
  notEqualOp     :: VS (repr (BinaryOp repr))
  greaterOp      :: VS (repr (BinaryOp repr))
  greaterEqualOp :: VS (repr (BinaryOp repr))
  lessOp         :: VS (repr (BinaryOp repr))
  lessEqualOp    :: VS (repr (BinaryOp repr))
  plusOp         :: VS (repr (BinaryOp repr))
  minusOp        :: VS (repr (BinaryOp repr))
  multOp         :: VS (repr (BinaryOp repr))
  divideOp       :: VS (repr (BinaryOp repr))
  powerOp        :: VS (repr (BinaryOp repr))
  moduloOp       :: VS (repr (BinaryOp repr))
  andOp          :: VS (repr (BinaryOp repr))
  orOp           :: VS (repr (BinaryOp repr))

class InternalOp repr where
  uOpDoc :: repr (UnaryOp repr) -> Doc
  bOpDoc :: repr (BinaryOp repr) -> Doc
  uOpPrec :: repr (UnaryOp repr) -> Int
  bOpPrec :: repr (BinaryOp repr) -> Int

  uOpFromData :: Int -> Doc -> VS (repr (UnaryOp repr))
  bOpFromData :: Int -> Doc -> VS (repr (BinaryOp repr))

class InternalVariable repr where
  variableBind :: repr (Variable repr) -> Binding
  variableDoc  :: repr (Variable repr) -> Doc
  varFromData :: Binding -> String -> repr (Type repr) -> Doc -> 
    repr (Variable repr)

class InternalValue repr where
  inputFunc       :: VS (repr (Value repr))
  printFunc       :: VS (repr (Value repr))
  printLnFunc     :: VS (repr (Value repr))
  printFileFunc   :: VS (repr (Value repr)) -> VS (repr (Value repr))
  printFileLnFunc :: VS (repr (Value repr)) -> VS (repr (Value repr))

  cast :: VS (repr (Type repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))

  -- Very generic internal function for generating calls, to reduce repeated code throughout generators
  -- Maybe library, function name, return type, maybe object doc, regular arguments, named arguments
  call :: Maybe Library -> Label -> VS (repr (Type repr)) -> 
    Maybe Doc -> [VS (repr (Value repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))

  valuePrec :: repr (Value repr) -> Maybe Int
  valueDoc :: repr (Value repr) -> Doc
  valFromData :: Maybe Int -> repr (Type repr) -> Doc -> repr (Value repr)

class InternalFunction repr where
  getFunc        :: VS (repr (Variable repr)) -> VS (repr (Function repr))
  setFunc        :: VS (repr (Type repr)) -> VS (repr (Variable repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Function repr))

  listSizeFunc       :: VS (repr (Function repr))
  listAddFunc        :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Function repr))
  listAppendFunc         :: VS (repr (Value repr)) -> VS (repr (Function repr))

  iterBeginFunc :: VS (repr (Type repr)) -> VS (repr (Function repr))
  iterEndFunc   :: VS (repr (Type repr)) -> VS (repr (Function repr))

  listAccessFunc :: VS (repr (Type repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Function repr))
  listSetFunc    :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Function repr))

  functionType :: repr (Function repr) -> repr (Type repr)
  functionDoc :: repr (Function repr) -> Doc

  funcFromData :: Doc -> VS (repr (Type repr)) -> VS (repr (Function repr))

class InternalStatement repr where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (VS (repr (Value repr))) -> VS (repr (Value repr)) 
    -> VS (repr (Value repr)) -> MS (repr (Statement repr))

  state     :: MS (repr (Statement repr)) -> MS (repr (Statement repr))
  loopState :: MS (repr (Statement repr)) -> MS (repr (Statement repr))

  emptyState   :: MS (repr (Statement repr))
  statementDoc :: repr (Statement repr) -> Doc
  statementTerm :: repr (Statement repr) -> Terminator

  stateFromData :: Doc -> Terminator -> repr (Statement repr)

class InternalScope repr where
  scopeDoc :: repr (Scope repr) -> Doc
  scopeFromData :: ScopeTag -> Doc -> repr (Scope repr)

class (TypeSym repr) => MethodTypeSym repr where
  type MethodType repr
  mType    :: VS (repr (Type repr)) -> MS (repr (MethodType repr))
  construct :: Label -> MS (repr (MethodType repr))

class InternalParam repr where
  parameterName :: repr (Parameter repr) -> Label
  parameterType :: repr (Parameter repr) -> repr (Type repr)
  parameterDoc  :: repr (Parameter repr) -> Doc
  paramFromData :: repr (Variable repr) -> Doc -> repr (Parameter repr)

class (MethodTypeSym repr, BlockCommentSym repr) => InternalMethod repr where
  intMethod     :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> MS (repr (MethodType repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  intFunc       :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> MS (repr (MethodType repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  commentedFunc :: MS (repr (BlockComment repr)) -> MS (repr (Method repr)) -> 
    MS (repr (Method repr))
    
  destructor :: [CS (repr (StateVar repr))] -> MS (repr (Method repr))

  methodDoc :: repr (Method repr) -> Doc
  methodFromData :: ScopeTag -> Doc -> repr (Method repr)

class InternalStateVar repr where
  stateVarDoc :: repr (StateVar repr) -> Doc
  stateVarFromData :: CS Doc -> CS (repr (StateVar repr))

class (BlockCommentSym repr) => InternalClass repr where
  intClass :: Label -> repr (Scope repr) -> repr (Keyword repr) ->
    [CS (repr (StateVar repr))] -> [MS (repr (Method repr))] -> 
    CS (repr (Class repr))

  commentedClass :: CS (repr (BlockComment repr)) -> 
    CS (repr (Class repr)) -> CS (repr (Class repr))

  classDoc :: repr (Class repr) -> Doc
  classFromData :: CS Doc -> CS (repr (Class repr))

class InternalMod repr where
  moduleDoc :: repr (Module repr) -> Doc
  modFromData :: String -> FS Doc -> FS (repr (Module repr))
  updateModuleDoc :: (Doc -> Doc) -> repr (Module repr) -> repr (Module repr)

class BlockCommentSym repr where
  type BlockComment repr
  blockComment :: [String] -> repr (BlockComment repr)
  docComment :: State a [String] -> State a (repr (BlockComment repr))

  blockCommentDoc :: repr (BlockComment repr) -> Doc
