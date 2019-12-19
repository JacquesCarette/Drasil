{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.Symantics (
  -- Types
  Label, Library,
  -- Typeclasses
  ProgramSym(..), RenderSym, FileSym(..), InternalFile(..),  KeywordSym(..), 
  ImportSym(..), PermanenceSym(..), InternalPerm(..), BodySym(..), 
  ControlBlockSym(..), listSlice, BlockSym(..), InternalBlock(..), TypeSym(..), 
  InternalType(..), UnaryOpSym(..), BinaryOpSym(..), InternalOp(..), 
  VariableSym(..), InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), 
  Selector(..), InternalSelector(..), objMethodCall, objMethodCallNoParams,
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), MethodTypeSym(..), ParameterSym(..), 
  InternalParam(..), MethodSym(..), InternalMethod(..), StateVarSym(..), 
  InternalStateVar(..), ClassSym(..), InternalClass(..), ModuleSym(..), 
  InternalMod(..), BlockCommentSym(..)
) where

import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.Data (Binding, Terminator, FileType, ScopeTag)
import GOOL.Drasil.State (GS, FS, MS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

type Label = String
type Library = String

class (FileSym repr, InternalBlock repr, InternalClass repr, InternalFile repr, 
  InternalFunction repr, InternalMethod repr, InternalMod repr, InternalOp repr,
  InternalParam repr, InternalPerm repr, InternalScope repr, 
  InternalStatement repr, InternalStateVar repr, InternalType repr, 
  InternalValue repr, InternalVariable repr, KeywordSym repr, ImportSym repr, 
  UnaryOpSym repr, BinaryOpSym repr) => RenderSym repr

class (FileSym repr) => ProgramSym repr where
  type Program repr
  prog :: Label -> [FS (repr (RenderFile repr))] -> 
    GS (repr (Program repr))

class (ModuleSym repr) => FileSym repr where 
  type RenderFile repr
  fileDoc :: FS (repr (Module repr)) -> FS (repr (RenderFile repr))

  -- Module description, list of author names, date as a String, file to comment
  docMod :: String -> [String] -> String -> 
    FS (repr (RenderFile repr)) -> 
    FS (repr (RenderFile repr))

  commentedMod :: FS (repr (BlockComment repr)) -> FS (repr (RenderFile repr)) 
    -> FS (repr (RenderFile repr))

class InternalFile repr where
  top :: repr (Module repr) -> repr (Block repr)
  bottom :: repr (Block repr)

  fileFromData :: FileType -> FS FilePath -> 
    FS (repr (Module repr)) -> 
    FS (repr (RenderFile repr))

class (PermanenceSym repr) => KeywordSym repr where
  type Keyword repr
  endStatement     :: repr (Keyword repr)
  endStatementLoop :: repr (Keyword repr)

  inherit :: Label -> repr (Keyword repr)

  list     :: repr (Permanence repr) -> repr (Keyword repr)

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

  keyDoc :: repr (Keyword repr) -> Doc

class ImportSym repr where
  type Import repr
  langImport :: Label -> repr (Import repr)
  modImport :: Label -> repr (Import repr)

  importDoc :: repr (Import repr) -> Doc

class PermanenceSym repr where
  type Permanence repr
  static_  :: repr (Permanence repr)
  dynamic_ :: repr (Permanence repr)

class InternalPerm repr where
  permDoc :: repr (Permanence repr) -> Doc
  binding :: repr (Permanence repr) -> Binding

class (BlockSym repr) => BodySym repr where
  type Body repr
  body           :: [FS (repr (Block repr))] -> FS (repr (Body repr))
  bodyStatements :: [FS (repr (Statement repr))] -> FS (repr (Body repr))
  oneLiner       :: FS (repr (Statement repr)) -> FS (repr (Body repr))

  addComments :: Label -> FS (repr (Body repr)) -> FS (repr (Body repr))

  bodyDoc :: repr (Body repr) -> Doc

class (StatementSym repr) => BlockSym repr where
  type Block repr
  block   :: [FS (repr (Statement repr))] -> FS (repr (Block repr))

class InternalBlock repr where
  blockDoc :: repr (Block repr) -> Doc
  docBlock :: FS Doc -> FS (repr (Block repr))

class (PermanenceSym repr) => TypeSym repr where
  type Type repr
  bool          :: FS (repr (Type repr))
  int           :: FS (repr (Type repr))
  float         :: FS (repr (Type repr))
  char          :: FS (repr (Type repr))
  string        :: FS (repr (Type repr))
  infile        :: FS (repr (Type repr))
  outfile       :: FS (repr (Type repr))
  listType      :: repr (Permanence repr) -> FS (repr (Type repr)) -> 
    FS (repr (Type repr))
  listInnerType :: FS (repr (Type repr)) -> FS (repr (Type repr))
  obj           :: Label -> FS (repr (Type repr))
  enumType      :: Label -> FS (repr (Type repr))
  iterator      :: FS (repr (Type repr)) -> FS (repr (Type repr))
  void          :: FS (repr (Type repr))

  getType :: repr (Type repr) -> CodeType
  getTypeString :: repr (Type repr) -> String
  getTypeDoc :: repr (Type repr) -> Doc

class InternalType repr where
  typeFromData :: CodeType -> String -> Doc -> repr (Type repr)

class (ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, FS (repr (Body repr)))] -> 
    Maybe (FS (repr (Value repr))) -> Maybe (FS (repr (Variable repr))) -> 
    FS (repr (Block repr))

  listSlice'      :: Maybe (FS (repr (Value repr))) -> 
    Maybe (FS (repr (Value repr))) -> Maybe (FS (repr (Value repr))) ->
    FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Block repr))

listSlice :: (ControlBlockSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> Maybe (FS (repr (Value repr))) -> 
  Maybe (FS (repr (Value repr))) -> Maybe (FS (repr (Value repr))) -> 
  FS (repr (Block repr))
listSlice vnew vold b e s = listSlice' b e s vnew vold

class UnaryOpSym repr where
  type UnaryOp repr
  notOp    :: FS (repr (UnaryOp repr))
  negateOp :: FS (repr (UnaryOp repr))
  sqrtOp   :: FS (repr (UnaryOp repr))
  absOp    :: FS (repr (UnaryOp repr))
  logOp    :: FS (repr (UnaryOp repr))
  lnOp     :: FS (repr (UnaryOp repr))
  expOp    :: FS (repr (UnaryOp repr))
  sinOp    :: FS (repr (UnaryOp repr))
  cosOp    :: FS (repr (UnaryOp repr))
  tanOp    :: FS (repr (UnaryOp repr))
  asinOp   :: FS (repr (UnaryOp repr))
  acosOp   :: FS (repr (UnaryOp repr))
  atanOp   :: FS (repr (UnaryOp repr))
  floorOp  :: FS (repr (UnaryOp repr))
  ceilOp   :: FS (repr (UnaryOp repr))

class BinaryOpSym repr where
  type BinaryOp repr
  equalOp        :: FS (repr (BinaryOp repr))
  notEqualOp     :: FS (repr (BinaryOp repr))
  greaterOp      :: FS (repr (BinaryOp repr))
  greaterEqualOp :: FS (repr (BinaryOp repr))
  lessOp         :: FS (repr (BinaryOp repr))
  lessEqualOp    :: FS (repr (BinaryOp repr))
  plusOp         :: FS (repr (BinaryOp repr))
  minusOp        :: FS (repr (BinaryOp repr))
  multOp         :: FS (repr (BinaryOp repr))
  divideOp       :: FS (repr (BinaryOp repr))
  powerOp        :: FS (repr (BinaryOp repr))
  moduloOp       :: FS (repr (BinaryOp repr))
  andOp          :: FS (repr (BinaryOp repr))
  orOp           :: FS (repr (BinaryOp repr))

class InternalOp repr where
  uOpDoc :: repr (UnaryOp repr) -> Doc
  bOpDoc :: repr (BinaryOp repr) -> Doc
  uOpPrec :: repr (UnaryOp repr) -> Int
  bOpPrec :: repr (BinaryOp repr) -> Int

  uOpFromData :: Int -> Doc -> FS (repr (UnaryOp repr))
  bOpFromData :: Int -> Doc -> FS (repr (BinaryOp repr))

class (TypeSym repr) => VariableSym repr where
  type Variable repr
  var          :: Label -> FS (repr (Type repr)) -> FS (repr (Variable repr))
  staticVar    :: Label -> FS (repr (Type repr)) -> FS (repr (Variable repr))
  const        :: Label -> FS (repr (Type repr)) -> FS (repr (Variable repr))
  extVar       :: Library -> Label -> FS (repr (Type repr)) -> 
    FS (repr (Variable repr))
  self         :: Label -> FS (repr (Variable repr))
  classVar     :: FS (repr (Type repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Variable repr))
  extClassVar  :: FS (repr (Type repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Variable repr))
  objVar       :: FS (repr (Variable repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Variable repr))
  objVarSelf   :: Label -> FS (repr (Variable repr)) -> FS (repr (Variable repr))
  enumVar      :: Label -> Label -> FS (repr (Variable repr))
  listVar      :: Label -> repr (Permanence repr) -> FS (repr (Type repr)) -> 
    FS (repr (Variable repr))
  listOf       :: Label -> FS (repr (Type repr)) -> FS (repr (Variable repr))
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> FS (repr (Type repr)) -> FS (repr (Variable repr))

  ($->) :: FS (repr (Variable repr)) -> FS (repr (Variable repr)) -> FS (repr (Variable repr))
  infixl 9 $->

  variableBind :: repr (Variable repr) -> Binding
  variableName :: repr (Variable repr) -> String
  variableType :: repr (Variable repr) -> repr (Type repr)
  variableDoc  :: repr (Variable repr) -> Doc

class InternalVariable repr where
  varFromData :: Binding -> String -> repr (Type repr) -> Doc -> 
    repr (Variable repr)

class (VariableSym repr) => ValueSym repr where
  type Value repr
  litTrue   :: FS (repr (Value repr))
  litFalse  :: FS (repr (Value repr))
  litChar   :: Char -> FS (repr (Value repr))
  litFloat  :: Double -> FS (repr (Value repr))
  litInt    :: Integer -> FS (repr (Value repr))
  litString :: String -> FS (repr (Value repr))

  pi :: FS (repr (Value repr))

  --other operators ($)
  ($:)  :: Label -> Label -> FS (repr (Value repr))
  infixl 9 $:

  valueOf       :: FS (repr (Variable repr)) -> FS (repr (Value repr))
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> FS (repr (Value repr))
  enumElement  :: Label -> Label -> FS (repr (Value repr))

  argsList  :: FS (repr (Value repr))

  valueType :: repr (Value repr) -> repr (Type repr)
  valueDoc :: repr (Value repr) -> Doc

class (ValueSym repr) => 
  NumericExpression repr where
  (#~)  :: FS (repr (Value repr)) -> FS (repr (Value repr))
  infixl 8 #~
  (#/^) :: FS (repr (Value repr)) -> FS (repr (Value repr))
  infixl 7 #/^
  (#|)  :: FS (repr (Value repr)) -> FS (repr (Value repr))
  infixl 7 #|
  (#+)  :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 5 #+
  (#-)  :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 5 #-
  (#*)  :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 6 #*
  (#/)  :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 6 #/
  (#%)  :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 6 #%
  (#^)  :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 7 #^

  log    :: FS (repr (Value repr)) -> FS (repr (Value repr))
  ln     :: FS (repr (Value repr)) -> FS (repr (Value repr))
  exp    :: FS (repr (Value repr)) -> FS (repr (Value repr))
  sin    :: FS (repr (Value repr)) -> FS (repr (Value repr))
  cos    :: FS (repr (Value repr)) -> FS (repr (Value repr))
  tan    :: FS (repr (Value repr)) -> FS (repr (Value repr))
  csc    :: FS (repr (Value repr)) -> FS (repr (Value repr))
  sec    :: FS (repr (Value repr)) -> FS (repr (Value repr))
  cot    :: FS (repr (Value repr)) -> FS (repr (Value repr))
  arcsin :: FS (repr (Value repr)) -> FS (repr (Value repr))
  arccos :: FS (repr (Value repr)) -> FS (repr (Value repr))
  arctan :: FS (repr (Value repr)) -> FS (repr (Value repr))
  floor  :: FS (repr (Value repr)) -> FS (repr (Value repr))
  ceil   :: FS (repr (Value repr)) -> FS (repr (Value repr))

-- I considered having two separate classes, BooleanExpressions and BooleanComparisons,
-- but this would require cyclic constraints, since it is feasible to have
-- BooleanComparisons of BooleanExpressions and also BooleanExpressions of BooleanComparisons.
-- This has the drawback of requiring a NumericExpression constraint for the first
-- 3 functions here, even though they don't really need it.
class (ValueSym repr, NumericExpression repr) => 
  BooleanExpression repr where
  (?!)  :: FS (repr (Value repr)) -> FS (repr (Value repr))
  infixr 6 ?!
  (?&&) :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 2 ?&&
  (?||) :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 1 ?||

  (?<)  :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 4 ?<
  (?<=) :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 4 ?<=
  (?>)  :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 4 ?>
  (?>=) :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 4 ?>=
  (?==) :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 3 ?==
  (?!=) :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  infixl 3 ?!=

class (ValueSym repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Value repr))
  funcApp      :: Label -> FS (repr (Type repr)) -> [FS (repr (Value repr))] -> 
    FS (repr (Value repr))
  selfFuncApp  :: Label -> Label -> FS (repr (Type repr)) -> 
    [FS (repr (Value repr))] -> FS (repr (Value repr))
  extFuncApp   :: Library -> Label -> FS (repr (Type repr)) -> 
    [FS (repr (Value repr))] -> FS (repr (Value repr))
  newObj     :: FS (repr (Type repr)) -> [FS (repr (Value repr))] -> 
    FS (repr (Value repr))
  extNewObj  :: Library -> FS (repr (Type repr)) -> [FS (repr (Value repr))] -> 
    FS (repr (Value repr))

  exists  :: FS (repr (Value repr)) -> FS (repr (Value repr))
  notNull :: FS (repr (Value repr)) -> FS (repr (Value repr))

class InternalValue repr where
  inputFunc       :: FS (repr (Value repr))
  printFunc       :: FS (repr (Value repr))
  printLnFunc     :: FS (repr (Value repr))
  printFileFunc   :: FS (repr (Value repr)) -> FS (repr (Value repr))
  printFileLnFunc :: FS (repr (Value repr)) -> FS (repr (Value repr))

  cast :: FS (repr (Type repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))

  valuePrec :: repr (Value repr) -> Maybe Int
  valFromData :: Maybe Int -> repr (Type repr) -> Doc -> repr (Value repr)

-- The cyclic constraints issue arises here too. I've constrained this by ValueExpression,
-- but really one might want one of these values as part of an expression, so the
-- constraint would have to go both ways. I'm not sure what the solution is for
-- these sorts of problems, other than removing the constraints altogether, but 
-- then what is the purpose of splitting the typeclasses into smaller typeclasses?
-- I'm leaving it as is for now, even though I suspect this will change in the future.
class (FunctionSym repr) => Selector repr where
  objAccess :: FS (repr (Value repr)) -> FS (repr (Function repr)) -> 
    FS (repr (Value repr))
  ($.)      :: FS (repr (Value repr)) -> FS (repr (Function repr)) -> 
    FS (repr (Value repr))
  infixl 9 $.

  selfAccess :: Label -> FS (repr (Function repr)) -> FS (repr (Value repr))

  listIndexExists :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  argExists       :: Integer -> FS (repr (Value repr))

  indexOf :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))

class (FunctionSym repr) => InternalSelector repr where
  objMethodCall' :: Label -> FS (repr (Type repr)) -> FS (repr (Value repr)) -> 
    [FS (repr (Value repr))] -> FS (repr (Value repr))
  objMethodCallNoParams' :: Label -> FS (repr (Type repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Value repr))

objMethodCall :: (InternalSelector repr) => FS (repr (Type repr)) -> 
  FS (repr (Value repr)) -> Label -> [FS (repr (Value repr))] -> 
  FS (repr (Value repr))
objMethodCall t o f = objMethodCall' f t o

objMethodCallNoParams :: (InternalSelector repr) => FS (repr (Type repr)) -> 
  FS (repr (Value repr)) -> Label -> FS (repr (Value repr))
objMethodCallNoParams t o f = objMethodCallNoParams' f t o

class (ValueExpression repr) => FunctionSym repr where
  type Function repr
  func :: Label -> FS (repr (Type repr)) -> [FS (repr (Value repr))] -> 
    FS (repr (Function repr))

  get :: FS (repr (Value repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Value repr))
  set :: FS (repr (Value repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Value repr))

  listSize   :: FS (repr (Value repr)) -> FS (repr (Value repr))
  listAdd    :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Value repr))
  listAppend :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))

  iterBegin :: FS (repr (Value repr)) -> FS (repr (Value repr))
  iterEnd   :: FS (repr (Value repr)) -> FS (repr (Value repr))

class (Selector repr, InternalSelector repr) => SelectorFunction repr where
  listAccess :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))
  listSet    :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Value repr))
  at         :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr))

class InternalFunction repr where
  getFunc        :: FS (repr (Variable repr)) -> FS (repr (Function repr))
  setFunc        :: FS (repr (Type repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Function repr))

  listSizeFunc       :: FS (repr (Function repr))
  listAddFunc        :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Function repr))
  listAppendFunc         :: FS (repr (Value repr)) -> FS (repr (Function repr))

  iterBeginFunc :: FS (repr (Type repr)) -> FS (repr (Function repr))
  iterEndFunc   :: FS (repr (Type repr)) -> FS (repr (Function repr))

  listAccessFunc :: FS (repr (Type repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Function repr))
  listSetFunc    :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Function repr))

  functionType :: repr (Function repr) -> repr (Type repr)
  functionDoc :: repr (Function repr) -> Doc

  funcFromData :: Doc -> FS (repr (Type repr)) -> FS (repr (Function repr))

class InternalStatement repr where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (FS (repr (Value repr))) -> FS (repr (Value repr)) 
    -> FS (repr (Value repr)) -> FS (repr (Statement repr))

  state     :: FS (repr (Statement repr)) -> FS (repr (Statement repr))
  loopState :: FS (repr (Statement repr)) -> FS (repr (Statement repr))

  emptyState   :: FS (repr (Statement repr))
  statementDoc :: repr (Statement repr) -> Doc
  statementTerm :: repr (Statement repr) -> Terminator

  stateFromData :: Doc -> Terminator -> repr (Statement repr)

class (SelectorFunction repr) => StatementSym repr where
  type Statement repr
  (&=)   :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  infixr 1 &=
  (&-=)  :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  infixl 1 &-=
  (&+=)  :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  infixl 1 &+=
  (&++)  :: FS (repr (Variable repr)) -> FS (repr (Statement repr))
  infixl 8 &++
  (&~-)  :: FS (repr (Variable repr)) -> FS (repr (Statement repr))
  infixl 8 &~-

  assign            :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  assignToListIndex :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Statement repr))
  multiAssign       :: [FS (repr (Variable repr))] -> [FS (repr (Value repr))] ->
    FS (repr (Statement repr)) 

  varDec           :: FS (repr (Variable repr)) -> FS (repr (Statement repr))
  varDecDef        :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  listDec          :: Integer -> FS (repr (Variable repr)) -> 
    FS (repr (Statement repr))
  listDecDef       :: FS (repr (Variable repr)) -> [FS (repr (Value repr))] -> 
    FS (repr (Statement repr))
  objDecDef        :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  objDecNew        :: FS (repr (Variable repr)) -> [FS (repr (Value repr))] -> 
    FS (repr (Statement repr))
  extObjDecNew     :: Library -> FS (repr (Variable repr)) -> 
    [FS (repr (Value repr))] -> FS (repr (Statement repr))
  objDecNewNoParams    :: FS (repr (Variable repr)) -> FS (repr (Statement repr))
  extObjDecNewNoParams :: Library -> FS (repr (Variable repr)) -> 
    FS (repr (Statement repr))
  constDecDef      :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))

  print      :: FS (repr (Value repr)) -> FS (repr (Statement repr))
  printLn    :: FS (repr (Value repr)) -> FS (repr (Statement repr))
  printStr   :: String -> FS (repr (Statement repr))
  printStrLn :: String -> FS (repr (Statement repr))

  printFile      :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  printFileLn    :: FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  printFileStr   :: FS (repr (Value repr)) -> String -> 
    FS (repr (Statement repr))
  printFileStrLn :: FS (repr (Value repr)) -> String -> 
    FS (repr (Statement repr))

  getInput         :: FS (repr (Variable repr)) -> FS (repr (Statement repr))
  discardInput     :: FS (repr (Statement repr))
  getFileInput     :: FS (repr (Value repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Statement repr))
  discardFileInput :: FS (repr (Value repr)) -> FS (repr (Statement repr))

  openFileR :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  openFileW :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  openFileA :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  closeFile :: FS (repr (Value repr)) -> FS (repr (Statement repr))

  getFileInputLine :: FS (repr (Value repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Statement repr))
  discardFileLine  :: FS (repr (Value repr)) -> FS (repr (Statement repr))
  stringSplit      :: Char -> FS (repr (Variable repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Statement repr))

  stringListVals :: [FS (repr (Variable repr))] -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr))
  stringListLists :: [FS (repr (Variable repr))] -> FS (repr (Value repr)) ->
    FS (repr (Statement repr))

  break :: FS (repr (Statement repr))
  continue :: FS (repr (Statement repr))

  returnState :: FS (repr (Value repr)) -> FS (repr (Statement repr))
  multiReturn :: [FS (repr (Value repr))] -> FS (repr (Statement repr))

  valState :: FS (repr (Value repr)) -> FS (repr (Statement repr))

  comment :: Label -> FS (repr (Statement repr))

  free :: FS (repr (Variable repr)) -> FS (repr (Statement repr))

  throw :: Label -> FS (repr (Statement repr))

  initState   :: Label -> Label -> FS (repr (Statement repr))
  changeState :: Label -> Label -> FS (repr (Statement repr))

  initObserverList :: FS (repr (Type repr)) -> [FS (repr (Value repr))] -> 
    FS (repr (Statement repr))
  addObserver      :: FS (repr (Value repr)) -> FS (repr (Statement repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [FS (repr (Value repr))] -> [FS (repr (Variable repr))] 
    -> [FS (repr (Variable repr))] -> FS (repr (Statement repr))
  selfInOutCall :: Label -> Label -> [FS (repr (Value repr))] -> 
    [FS (repr (Variable repr))] -> [FS (repr (Variable repr))] -> 
    FS (repr (Statement repr))
  extInOutCall :: Library -> Label -> [FS (repr (Value repr))] ->
    [FS (repr (Variable repr))] -> [FS (repr (Variable repr))] -> 
    FS (repr (Statement repr))

  multi     :: [FS (repr (Statement repr))] -> FS (repr (Statement repr))

class (BodySym repr) => ControlStatementSym repr where
  ifCond     :: [(FS (repr (Value repr)), FS (repr (Body repr)))] -> 
    FS (repr (Body repr)) -> FS (repr (Statement repr))
  ifNoElse   :: [(FS (repr (Value repr)), FS (repr (Body repr)))] -> 
    FS (repr (Statement repr))
  switch     :: FS (repr (Value repr)) -> [(FS (repr (Value repr)), 
    FS (repr (Body repr)))] -> FS (repr (Body repr)) -> 
    FS (repr (Statement repr)) -- is there value in separating Literals into their own type?
  switchAsIf :: FS (repr (Value repr)) -> [(FS (repr (Value repr)), 
    FS (repr (Body repr)))] -> FS (repr (Body repr)) -> 
    FS (repr (Statement repr))

  ifExists :: FS (repr (Value repr)) -> FS (repr (Body repr)) -> 
    FS (repr (Body repr)) -> FS (repr (Statement repr))

  for      :: FS (repr (Statement repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Statement repr)) -> FS (repr (Body repr)) -> 
    FS (repr (Statement repr))
  forRange :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Value repr)) -> FS (repr (Value repr)) -> FS (repr (Body repr)) 
    -> FS (repr (Statement repr))
  forEach  :: FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (Body repr)) -> FS (repr (Statement repr))
  while    :: FS (repr (Value repr)) -> FS (repr (Body repr)) -> 
    FS (repr (Statement repr)) 

  tryCatch :: FS (repr (Body repr)) -> FS (repr (Body repr)) -> 
    FS (repr (Statement repr))

  checkState      :: Label -> [(FS (repr (Value repr)), FS (repr (Body repr)))] 
    -> FS (repr (Body repr)) -> FS (repr (Statement repr))
  notifyObservers :: FS (repr (Function repr)) -> FS (repr (Type repr)) -> 
    FS (repr (Statement repr))

  getFileInputAll  :: FS (repr (Value repr)) -> FS (repr (Variable repr)) -> 
    FS (repr (Statement repr))

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class InternalScope repr where
  scopeDoc :: repr (Scope repr) -> Doc

class (TypeSym repr) => MethodTypeSym repr where
  type MethodType repr
  mType    :: FS (repr (Type repr)) -> FS (repr (MethodType repr))
  construct :: Label -> FS (repr (MethodType repr))

class ParameterSym repr where
  type Parameter repr
  param :: FS (repr (Variable repr)) -> MS (repr (Parameter repr))
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: FS (repr (Variable repr)) -> MS (repr (Parameter repr))

class InternalParam repr where
  parameterName :: repr (Parameter repr) -> Label
  parameterType :: repr (Parameter repr) -> repr (Type repr)
  parameterDoc  :: repr (Parameter repr) -> Doc
  paramFromData :: repr (Variable repr) -> Doc -> repr (Parameter repr)

class (StateVarSym repr, ParameterSym repr, ControlBlockSym repr) => 
  MethodSym repr where
  type Method repr
  -- Second label is class name
  method      :: Label -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> FS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    FS (repr (Body repr)) -> MS (repr (Method repr))
  getMethod   :: Label -> FS (repr (Variable repr)) -> MS (repr (Method repr))
  setMethod   :: Label -> FS (repr (Variable repr)) -> MS (repr (Method repr)) 
  privMethod  :: Label -> Label -> FS (repr (Type repr)) -> 
    [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
    MS (repr (Method repr))
  pubMethod   :: Label -> Label -> FS (repr (Type repr)) -> 
    [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
    MS (repr (Method repr))
  constructor :: Label -> [MS (repr (Parameter repr))] -> FS (repr (Body repr)) 
    -> MS (repr (Method repr))
  destructor :: Label -> [FS (repr (StateVar repr))] -> MS (repr (Method repr))

  docMain :: FS (repr (Body repr)) -> MS (repr (Method repr))

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    FS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    FS (repr (Body repr)) -> MS (repr (Method repr))
  mainFunction  :: FS (repr (Body repr)) -> MS (repr (Method repr))
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> 
    MS (repr (Method repr)) -> MS (repr (Method repr))

  -- Second label is class name, rest is same as inOutFunc
  inOutMethod :: Label -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> [FS (repr (Variable repr))] -> [FS (repr (Variable repr))] -> 
    [FS (repr (Variable repr))] -> FS (repr (Body repr)) -> 
    MS (repr (Method repr))
  -- Second label is class name, rest is same as docInOutFunc
  docInOutMethod :: Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> String -> [(String, FS (repr (Variable repr)))] 
    -> [(String, FS (repr (Variable repr)))] -> 
    [(String, FS (repr (Variable repr)))] -> FS (repr (Body repr)) -> 
    MS (repr (Method repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [FS (repr (Variable repr))] -> [FS (repr (Variable repr))] -> 
    [FS (repr (Variable repr))] -> FS (repr (Body repr)) -> 
    MS (repr (Method repr))
  -- Parameters are: function name, scope, permanence, brief description, input descriptions and variables, output descriptions and variables, descriptions and variables for parameters that are both input and output, function body
  docInOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, FS (repr (Variable repr)))] -> [(String, FS (repr 
    (Variable repr)))] -> [(String, FS (repr (Variable repr)))] -> 
    FS (repr (Body repr)) -> MS (repr (Method repr))

class (MethodTypeSym repr, BlockCommentSym repr) => InternalMethod repr where
  intMethod     :: Bool -> Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> FS (repr (MethodType repr)) -> 
    [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
    MS (repr (Method repr))
  intFunc       :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> FS (repr (MethodType repr)) -> [MS (repr (Parameter repr))] -> 
    FS (repr (Body repr)) -> MS (repr (Method repr))
  commentedFunc :: MS (repr (BlockComment repr)) -> MS (repr (Method repr)) -> 
    MS (repr (Method repr))

  methodDoc :: repr (Method repr) -> Doc
  methodFromData :: ScopeTag -> Doc -> repr (Method repr)

class (ScopeSym repr, PermanenceSym repr, TypeSym repr, StatementSym repr) =>
  StateVarSym repr where
  type StateVar repr
  stateVar :: repr (Scope repr) -> repr (Permanence repr) ->
    FS (repr (Variable repr)) -> FS (repr (StateVar repr))
  stateVarDef :: Label -> repr (Scope repr) -> repr (Permanence repr) ->
    FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
    FS (repr (StateVar repr))
  constVar :: Label -> repr (Scope repr) ->  FS (repr (Variable repr)) -> 
    FS (repr (Value repr)) -> FS (repr (StateVar repr))
  privMVar :: FS (repr (Variable repr)) -> FS (repr (StateVar repr))
  pubMVar  :: FS (repr (Variable repr)) -> FS (repr (StateVar repr))
  pubGVar  :: FS (repr (Variable repr)) -> FS (repr (StateVar repr))

class InternalStateVar repr where
  stateVarDoc :: repr (StateVar repr) -> Doc
  stateVarFromData :: FS Doc -> FS (repr (StateVar repr))

class (MethodSym repr) => ClassSym repr where
  type Class repr
  buildClass :: Label -> Maybe Label -> repr (Scope repr) -> 
    [FS (repr (StateVar repr))] -> 
    [MS (repr (Method repr))] -> 
    FS (repr (Class repr))
  enum :: Label -> [Label] -> repr (Scope repr) -> 
    FS (repr (Class repr))
  privClass :: Label -> Maybe Label -> [FS (repr (StateVar repr))] 
    -> [MS (repr (Method repr))] -> 
    FS (repr (Class repr))
  pubClass :: Label -> Maybe Label -> [FS (repr (StateVar repr))] 
    -> [MS (repr (Method repr))] -> 
    FS (repr (Class repr))

  docClass :: String -> FS (repr (Class repr)) ->
    FS (repr (Class repr))

  commentedClass :: FS (repr (BlockComment repr)) -> 
    FS (repr (Class repr)) -> FS (repr (Class repr))

class InternalClass repr where
  classDoc :: repr (Class repr) -> Doc
  classFromData :: FS Doc -> FS (repr (Class repr))

class (ClassSym repr) => ModuleSym repr where
  type Module repr
  buildModule :: Label -> [Library] -> [MS (repr (Method repr))] -> 
    [FS (repr (Class repr))] -> FS (repr (Module repr))

class InternalMod repr where
  moduleDoc :: repr (Module repr) -> Doc
  modFromData :: String -> FS Bool -> FS Doc -> FS (repr (Module repr))
  updateModuleDoc :: (Doc -> Doc) -> repr (Module repr) -> repr (Module repr)
    
class BlockCommentSym repr where
  type BlockComment repr
  blockComment :: [String] -> repr (BlockComment repr)
  docComment :: State a [String] -> State a (repr (BlockComment repr))

  blockCommentDoc :: repr (BlockComment repr) -> Doc
