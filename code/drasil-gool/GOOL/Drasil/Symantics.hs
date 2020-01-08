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
  InternalMod(..), BlockCommentSym(..),
  ODEInfo(..), odeInfo, ODEOptions(..), odeOptions, ODEMethod(..)
) where

import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.Data (Binding, Terminator, FileType, ScopeTag)
import GOOL.Drasil.State (GS, FS, CS, MS)

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
  body           :: [MS (repr (Block repr))] -> MS (repr (Body repr))
  bodyStatements :: [MS (repr (Statement repr))] -> MS (repr (Body repr))
  oneLiner       :: MS (repr (Statement repr)) -> MS (repr (Body repr))

  addComments :: Label -> MS (repr (Body repr)) -> MS (repr (Body repr))

  bodyDoc :: repr (Body repr) -> Doc

class (StatementSym repr) => BlockSym repr where
  type Block repr
  block   :: [MS (repr (Statement repr))] -> MS (repr (Block repr))

class InternalBlock repr where
  blockDoc :: repr (Block repr) -> Doc
  docBlock :: MS Doc -> MS (repr (Block repr))
  multiBlock :: [MS (repr (Block repr))] -> MS (repr (Block repr))

class (PermanenceSym repr) => TypeSym repr where
  type Type repr
  bool          :: MS (repr (Type repr))
  int           :: MS (repr (Type repr))
  float         :: MS (repr (Type repr))
  char          :: MS (repr (Type repr))
  string        :: MS (repr (Type repr))
  infile        :: MS (repr (Type repr))
  outfile       :: MS (repr (Type repr))
  listType      :: repr (Permanence repr) -> MS (repr (Type repr)) -> 
    MS (repr (Type repr))
  listInnerType :: MS (repr (Type repr)) -> MS (repr (Type repr))
  obj           :: Label -> MS (repr (Type repr))
  enumType      :: Label -> MS (repr (Type repr))
  iterator      :: MS (repr (Type repr)) -> MS (repr (Type repr))
  void          :: MS (repr (Type repr))

  getType :: repr (Type repr) -> CodeType
  getTypeString :: repr (Type repr) -> String
  getTypeDoc :: repr (Type repr) -> Doc

class InternalType repr where
  typeFromData :: CodeType -> String -> Doc -> repr (Type repr)

class (ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, MS (repr (Body repr)))] -> 
    Maybe (MS (repr (Value repr))) -> Maybe (MS (repr (Variable repr))) -> 
    MS (repr (Block repr))

  listSlice'      :: Maybe (MS (repr (Value repr))) -> 
    Maybe (MS (repr (Value repr))) -> Maybe (MS (repr (Value repr))) ->
    MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Block repr))

  solveODE :: ODEInfo repr -> ODEOptions repr -> MS (repr (Block repr))
  
listSlice :: (ControlBlockSym repr) => MS (repr (Variable repr)) -> 
  MS (repr (Value repr)) -> Maybe (MS (repr (Value repr))) -> 
  Maybe (MS (repr (Value repr))) -> Maybe (MS (repr (Value repr))) -> 
  MS (repr (Block repr))
listSlice vnew vold b e s = listSlice' b e s vnew vold

class UnaryOpSym repr where
  type UnaryOp repr
  notOp    :: MS (repr (UnaryOp repr))
  negateOp :: MS (repr (UnaryOp repr))
  sqrtOp   :: MS (repr (UnaryOp repr))
  absOp    :: MS (repr (UnaryOp repr))
  logOp    :: MS (repr (UnaryOp repr))
  lnOp     :: MS (repr (UnaryOp repr))
  expOp    :: MS (repr (UnaryOp repr))
  sinOp    :: MS (repr (UnaryOp repr))
  cosOp    :: MS (repr (UnaryOp repr))
  tanOp    :: MS (repr (UnaryOp repr))
  asinOp   :: MS (repr (UnaryOp repr))
  acosOp   :: MS (repr (UnaryOp repr))
  atanOp   :: MS (repr (UnaryOp repr))
  floorOp  :: MS (repr (UnaryOp repr))
  ceilOp   :: MS (repr (UnaryOp repr))

class BinaryOpSym repr where
  type BinaryOp repr
  equalOp        :: MS (repr (BinaryOp repr))
  notEqualOp     :: MS (repr (BinaryOp repr))
  greaterOp      :: MS (repr (BinaryOp repr))
  greaterEqualOp :: MS (repr (BinaryOp repr))
  lessOp         :: MS (repr (BinaryOp repr))
  lessEqualOp    :: MS (repr (BinaryOp repr))
  plusOp         :: MS (repr (BinaryOp repr))
  minusOp        :: MS (repr (BinaryOp repr))
  multOp         :: MS (repr (BinaryOp repr))
  divideOp       :: MS (repr (BinaryOp repr))
  powerOp        :: MS (repr (BinaryOp repr))
  moduloOp       :: MS (repr (BinaryOp repr))
  andOp          :: MS (repr (BinaryOp repr))
  orOp           :: MS (repr (BinaryOp repr))

class InternalOp repr where
  uOpDoc :: repr (UnaryOp repr) -> Doc
  bOpDoc :: repr (BinaryOp repr) -> Doc
  uOpPrec :: repr (UnaryOp repr) -> Int
  bOpPrec :: repr (BinaryOp repr) -> Int

  uOpFromData :: Int -> Doc -> MS (repr (UnaryOp repr))
  bOpFromData :: Int -> Doc -> MS (repr (BinaryOp repr))

class (TypeSym repr) => VariableSym repr where
  type Variable repr
  var          :: Label -> MS (repr (Type repr)) -> MS (repr (Variable repr))
  staticVar    :: Label -> MS (repr (Type repr)) -> MS (repr (Variable repr))
  const        :: Label -> MS (repr (Type repr)) -> MS (repr (Variable repr))
  extVar       :: Library -> Label -> MS (repr (Type repr)) -> 
    MS (repr (Variable repr))
  self         :: MS (repr (Variable repr))
  classVar     :: MS (repr (Type repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Variable repr))
  extClassVar  :: MS (repr (Type repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Variable repr))
  objVar       :: MS (repr (Variable repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Variable repr))
  objVarSelf   :: MS (repr (Variable repr)) -> MS (repr (Variable repr))
  enumVar      :: Label -> Label -> MS (repr (Variable repr))
  listVar      :: Label -> repr (Permanence repr) -> MS (repr (Type repr)) -> 
    MS (repr (Variable repr))
  listOf       :: Label -> MS (repr (Type repr)) -> MS (repr (Variable repr))
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> MS (repr (Type repr)) -> MS (repr (Variable repr))

  ($->) :: MS (repr (Variable repr)) -> MS (repr (Variable repr)) -> MS (repr (Variable repr))
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
  litTrue   :: MS (repr (Value repr))
  litFalse  :: MS (repr (Value repr))
  litChar   :: Char -> MS (repr (Value repr))
  litFloat  :: Double -> MS (repr (Value repr))
  litInt    :: Integer -> MS (repr (Value repr))
  litString :: String -> MS (repr (Value repr))

  pi :: MS (repr (Value repr))

  --other operators ($)
  ($:)  :: Label -> Label -> MS (repr (Value repr))
  infixl 9 $:

  valueOf       :: MS (repr (Variable repr)) -> MS (repr (Value repr))
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> MS (repr (Value repr))
  enumElement  :: Label -> Label -> MS (repr (Value repr))

  argsList  :: MS (repr (Value repr))

  valueType :: repr (Value repr) -> repr (Type repr)
  valueDoc :: repr (Value repr) -> Doc

class (ValueSym repr) => 
  NumericExpression repr where
  (#~)  :: MS (repr (Value repr)) -> MS (repr (Value repr))
  infixl 8 #~
  (#/^) :: MS (repr (Value repr)) -> MS (repr (Value repr))
  infixl 7 #/^
  (#|)  :: MS (repr (Value repr)) -> MS (repr (Value repr))
  infixl 7 #|
  (#+)  :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 5 #+
  (#-)  :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 5 #-
  (#*)  :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 6 #*
  (#/)  :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 6 #/
  (#%)  :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 6 #%
  (#^)  :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 7 #^

  log    :: MS (repr (Value repr)) -> MS (repr (Value repr))
  ln     :: MS (repr (Value repr)) -> MS (repr (Value repr))
  exp    :: MS (repr (Value repr)) -> MS (repr (Value repr))
  sin    :: MS (repr (Value repr)) -> MS (repr (Value repr))
  cos    :: MS (repr (Value repr)) -> MS (repr (Value repr))
  tan    :: MS (repr (Value repr)) -> MS (repr (Value repr))
  csc    :: MS (repr (Value repr)) -> MS (repr (Value repr))
  sec    :: MS (repr (Value repr)) -> MS (repr (Value repr))
  cot    :: MS (repr (Value repr)) -> MS (repr (Value repr))
  arcsin :: MS (repr (Value repr)) -> MS (repr (Value repr))
  arccos :: MS (repr (Value repr)) -> MS (repr (Value repr))
  arctan :: MS (repr (Value repr)) -> MS (repr (Value repr))
  floor  :: MS (repr (Value repr)) -> MS (repr (Value repr))
  ceil   :: MS (repr (Value repr)) -> MS (repr (Value repr))

-- I considered having two separate classes, BooleanExpressions and BooleanComparisons,
-- but this would require cyclic constraints, since it is feasible to have
-- BooleanComparisons of BooleanExpressions and also BooleanExpressions of BooleanComparisons.
-- This has the drawback of requiring a NumericExpression constraint for the first
-- 3 functions here, even though they don't really need it.
class (ValueSym repr, NumericExpression repr) => 
  BooleanExpression repr where
  (?!)  :: MS (repr (Value repr)) -> MS (repr (Value repr))
  infixr 6 ?!
  (?&&) :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 2 ?&&
  (?||) :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 1 ?||

  (?<)  :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 4 ?<
  (?<=) :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 4 ?<=
  (?>)  :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 4 ?>
  (?>=) :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 4 ?>=
  (?==) :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 3 ?==
  (?!=) :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  infixl 3 ?!=

class (ValueSym repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Value repr))
  funcApp      :: Label -> MS (repr (Type repr)) -> [MS (repr (Value repr))] -> 
    MS (repr (Value repr))
  selfFuncApp  :: Label -> MS (repr (Type repr)) -> 
    [MS (repr (Value repr))] -> MS (repr (Value repr))
  extFuncApp   :: Library -> Label -> MS (repr (Type repr)) -> 
    [MS (repr (Value repr))] -> MS (repr (Value repr))
  newObj     :: MS (repr (Type repr)) -> [MS (repr (Value repr))] -> 
    MS (repr (Value repr))
  extNewObj  :: Library -> MS (repr (Type repr)) -> [MS (repr (Value repr))] -> 
    MS (repr (Value repr))

  exists  :: MS (repr (Value repr)) -> MS (repr (Value repr))
  notNull :: MS (repr (Value repr)) -> MS (repr (Value repr))

class InternalValue repr where
  inputFunc       :: MS (repr (Value repr))
  printFunc       :: MS (repr (Value repr))
  printLnFunc     :: MS (repr (Value repr))
  printFileFunc   :: MS (repr (Value repr)) -> MS (repr (Value repr))
  printFileLnFunc :: MS (repr (Value repr)) -> MS (repr (Value repr))

  cast :: MS (repr (Type repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))

  valuePrec :: repr (Value repr) -> Maybe Int
  valFromData :: Maybe Int -> repr (Type repr) -> Doc -> repr (Value repr)

-- The cyclic constraints issue arises here too. I've constrained this by ValueExpression,
-- but really one might want one of these values as part of an expression, so the
-- constraint would have to go both ways. I'm not sure what the solution is for
-- these sorts of problems, other than removing the constraints altogether, but 
-- then what is the purpose of splitting the typeclasses into smaller typeclasses?
-- I'm leaving it as is for now, even though I suspect this will change in the future.
class (FunctionSym repr) => Selector repr where
  objAccess :: MS (repr (Value repr)) -> MS (repr (Function repr)) -> 
    MS (repr (Value repr))
  ($.)      :: MS (repr (Value repr)) -> MS (repr (Function repr)) -> 
    MS (repr (Value repr))
  infixl 9 $.

  selfAccess :: MS (repr (Function repr)) -> MS (repr (Value repr))

  listIndexExists :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  argExists       :: Integer -> MS (repr (Value repr))

  indexOf :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))

class (FunctionSym repr) => InternalSelector repr where
  objMethodCall' :: Label -> MS (repr (Type repr)) -> MS (repr (Value repr)) -> 
    [MS (repr (Value repr))] -> MS (repr (Value repr))
  objMethodCallNoParams' :: Label -> MS (repr (Type repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Value repr))

objMethodCall :: (InternalSelector repr) => MS (repr (Type repr)) -> 
  MS (repr (Value repr)) -> Label -> [MS (repr (Value repr))] -> 
  MS (repr (Value repr))
objMethodCall t o f = objMethodCall' f t o

objMethodCallNoParams :: (InternalSelector repr) => MS (repr (Type repr)) -> 
  MS (repr (Value repr)) -> Label -> MS (repr (Value repr))
objMethodCallNoParams t o f = objMethodCallNoParams' f t o

class (ValueExpression repr) => FunctionSym repr where
  type Function repr
  func :: Label -> MS (repr (Type repr)) -> [MS (repr (Value repr))] -> 
    MS (repr (Function repr))

  get :: MS (repr (Value repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Value repr))
  set :: MS (repr (Value repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Value repr))

  listSize   :: MS (repr (Value repr)) -> MS (repr (Value repr))
  listAdd    :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Value repr))
  listAppend :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))

  iterBegin :: MS (repr (Value repr)) -> MS (repr (Value repr))
  iterEnd   :: MS (repr (Value repr)) -> MS (repr (Value repr))

class (Selector repr, InternalSelector repr) => SelectorFunction repr where
  listAccess :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))
  listSet    :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Value repr))
  at         :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr))

class InternalFunction repr where
  getFunc        :: MS (repr (Variable repr)) -> MS (repr (Function repr))
  setFunc        :: MS (repr (Type repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Function repr))

  listSizeFunc       :: MS (repr (Function repr))
  listAddFunc        :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Function repr))
  listAppendFunc         :: MS (repr (Value repr)) -> MS (repr (Function repr))

  iterBeginFunc :: MS (repr (Type repr)) -> MS (repr (Function repr))
  iterEndFunc   :: MS (repr (Type repr)) -> MS (repr (Function repr))

  listAccessFunc :: MS (repr (Type repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Function repr))
  listSetFunc    :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Function repr))

  functionType :: repr (Function repr) -> repr (Type repr)
  functionDoc :: repr (Function repr) -> Doc

  funcFromData :: Doc -> MS (repr (Type repr)) -> MS (repr (Function repr))

class InternalStatement repr where
  -- newLn, maybe a file to print to, printFunc, value to print
  printSt :: Bool -> Maybe (MS (repr (Value repr))) -> MS (repr (Value repr)) 
    -> MS (repr (Value repr)) -> MS (repr (Statement repr))

  state     :: MS (repr (Statement repr)) -> MS (repr (Statement repr))
  loopState :: MS (repr (Statement repr)) -> MS (repr (Statement repr))

  emptyState   :: MS (repr (Statement repr))
  statementDoc :: repr (Statement repr) -> Doc
  statementTerm :: repr (Statement repr) -> Terminator

  stateFromData :: Doc -> Terminator -> repr (Statement repr)

class (SelectorFunction repr) => StatementSym repr where
  type Statement repr
  (&=)   :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  infixr 1 &=
  (&-=)  :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  infixl 1 &-=
  (&+=)  :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  infixl 1 &+=
  (&++)  :: MS (repr (Variable repr)) -> MS (repr (Statement repr))
  infixl 8 &++
  (&~-)  :: MS (repr (Variable repr)) -> MS (repr (Statement repr))
  infixl 8 &~-

  assign            :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  assignToListIndex :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Statement repr))
  multiAssign       :: [MS (repr (Variable repr))] -> [MS (repr (Value repr))] ->
    MS (repr (Statement repr)) 

  varDec           :: MS (repr (Variable repr)) -> MS (repr (Statement repr))
  varDecDef        :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  listDec          :: Integer -> MS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  listDecDef       :: MS (repr (Variable repr)) -> [MS (repr (Value repr))] -> 
    MS (repr (Statement repr))
  objDecDef        :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  objDecNew        :: MS (repr (Variable repr)) -> [MS (repr (Value repr))] -> 
    MS (repr (Statement repr))
  extObjDecNew     :: Library -> MS (repr (Variable repr)) -> 
    [MS (repr (Value repr))] -> MS (repr (Statement repr))
  objDecNewNoParams    :: MS (repr (Variable repr)) -> MS (repr (Statement repr))
  extObjDecNewNoParams :: Library -> MS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  constDecDef      :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))

  print      :: MS (repr (Value repr)) -> MS (repr (Statement repr))
  printLn    :: MS (repr (Value repr)) -> MS (repr (Statement repr))
  printStr   :: String -> MS (repr (Statement repr))
  printStrLn :: String -> MS (repr (Statement repr))

  printFile      :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  printFileLn    :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  printFileStr   :: MS (repr (Value repr)) -> String -> 
    MS (repr (Statement repr))
  printFileStrLn :: MS (repr (Value repr)) -> String -> 
    MS (repr (Statement repr))

  getInput         :: MS (repr (Variable repr)) -> MS (repr (Statement repr))
  discardInput     :: MS (repr (Statement repr))
  getFileInput     :: MS (repr (Value repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  discardFileInput :: MS (repr (Value repr)) -> MS (repr (Statement repr))

  openFileR :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  openFileW :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  openFileA :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  closeFile :: MS (repr (Value repr)) -> MS (repr (Statement repr))

  getFileInputLine :: MS (repr (Value repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  discardFileLine  :: MS (repr (Value repr)) -> MS (repr (Statement repr))
  stringSplit      :: Char -> MS (repr (Variable repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Statement repr))

  stringListVals :: [MS (repr (Variable repr))] -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  stringListLists :: [MS (repr (Variable repr))] -> MS (repr (Value repr)) ->
    MS (repr (Statement repr))

  break :: MS (repr (Statement repr))
  continue :: MS (repr (Statement repr))

  returnState :: MS (repr (Value repr)) -> MS (repr (Statement repr))
  multiReturn :: [MS (repr (Value repr))] -> MS (repr (Statement repr))

  valState :: MS (repr (Value repr)) -> MS (repr (Statement repr))

  comment :: Label -> MS (repr (Statement repr))

  free :: MS (repr (Variable repr)) -> MS (repr (Statement repr))

  throw :: Label -> MS (repr (Statement repr))

  initState   :: Label -> Label -> MS (repr (Statement repr))
  changeState :: Label -> Label -> MS (repr (Statement repr))

  initObserverList :: MS (repr (Type repr)) -> [MS (repr (Value repr))] -> 
    MS (repr (Statement repr))
  addObserver      :: MS (repr (Value repr)) -> MS (repr (Statement repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [MS (repr (Value repr))] -> [MS (repr (Variable repr))] 
    -> [MS (repr (Variable repr))] -> MS (repr (Statement repr))
  selfInOutCall :: Label -> [MS (repr (Value repr))] -> 
    [MS (repr (Variable repr))] -> [MS (repr (Variable repr))] -> 
    MS (repr (Statement repr))
  extInOutCall :: Library -> Label -> [MS (repr (Value repr))] ->
    [MS (repr (Variable repr))] -> [MS (repr (Variable repr))] -> 
    MS (repr (Statement repr))

  multi     :: [MS (repr (Statement repr))] -> MS (repr (Statement repr))

class (BodySym repr) => ControlStatementSym repr where
  ifCond     :: [(MS (repr (Value repr)), MS (repr (Body repr)))] -> 
    MS (repr (Body repr)) -> MS (repr (Statement repr))
  ifNoElse   :: [(MS (repr (Value repr)), MS (repr (Body repr)))] -> 
    MS (repr (Statement repr))
  switch     :: MS (repr (Value repr)) -> [(MS (repr (Value repr)), 
    MS (repr (Body repr)))] -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr)) -- is there value in separating Literals into their own type?
  switchAsIf :: MS (repr (Value repr)) -> [(MS (repr (Value repr)), 
    MS (repr (Body repr)))] -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr))

  ifExists :: MS (repr (Value repr)) -> MS (repr (Body repr)) -> 
    MS (repr (Body repr)) -> MS (repr (Statement repr))

  for      :: MS (repr (Statement repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Statement repr)) -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr))
  forRange :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Value repr)) -> MS (repr (Value repr)) -> MS (repr (Body repr)) 
    -> MS (repr (Statement repr))
  forEach  :: MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    MS (repr (Body repr)) -> MS (repr (Statement repr))
  while    :: MS (repr (Value repr)) -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr)) 

  tryCatch :: MS (repr (Body repr)) -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr))

  checkState      :: Label -> [(MS (repr (Value repr)), MS (repr (Body repr)))] 
    -> MS (repr (Body repr)) -> MS (repr (Statement repr))
  notifyObservers :: MS (repr (Function repr)) -> MS (repr (Type repr)) -> 
    MS (repr (Statement repr))

  getFileInputAll  :: MS (repr (Value repr)) -> MS (repr (Variable repr)) -> 
    MS (repr (Statement repr))

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class InternalScope repr where
  scopeDoc :: repr (Scope repr) -> Doc

class (TypeSym repr) => MethodTypeSym repr where
  type MethodType repr
  mType    :: MS (repr (Type repr)) -> MS (repr (MethodType repr))
  construct :: Label -> MS (repr (MethodType repr))

class ParameterSym repr where
  type Parameter repr
  param :: MS (repr (Variable repr)) -> MS (repr (Parameter repr))
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: MS (repr (Variable repr)) -> MS (repr (Parameter repr))

class InternalParam repr where
  parameterName :: repr (Parameter repr) -> Label
  parameterType :: repr (Parameter repr) -> repr (Type repr)
  parameterDoc  :: repr (Parameter repr) -> Doc
  paramFromData :: repr (Variable repr) -> Doc -> repr (Parameter repr)

class (StateVarSym repr, ParameterSym repr, ControlBlockSym repr) => 
  MethodSym repr where
  type Method repr
  method      :: Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> MS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  getMethod   :: MS (repr (Variable repr)) -> MS (repr (Method repr))
  setMethod   :: MS (repr (Variable repr)) -> MS (repr (Method repr)) 
  privMethod  :: Label -> MS (repr (Type repr)) -> [MS (repr (Parameter repr))] 
    -> MS (repr (Body repr)) -> MS (repr (Method repr))
  pubMethod   :: Label -> MS (repr (Type repr)) -> [MS (repr (Parameter repr))] 
    -> MS (repr (Body repr)) -> MS (repr (Method repr))
  constructor :: [MS (repr (Parameter repr))] -> MS (repr (Body repr)) 
    -> MS (repr (Method repr))
  destructor :: [CS (repr (StateVar repr))] -> MS (repr (Method repr))

  docMain :: MS (repr (Body repr)) -> MS (repr (Method repr))

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    MS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  mainFunction  :: MS (repr (Body repr)) -> MS (repr (Method repr))
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> 
    MS (repr (Method repr)) -> MS (repr (Method repr))

  inOutMethod :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [MS (repr (Variable repr))] -> [MS (repr (Variable repr))] -> 
    [MS (repr (Variable repr))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr))
  docInOutMethod :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, MS (repr (Variable repr)))] -> 
    [(String, MS (repr (Variable repr)))] -> 
    [(String, MS (repr (Variable repr)))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [MS (repr (Variable repr))] -> [MS (repr (Variable repr))] -> 
    [MS (repr (Variable repr))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr))
  -- Parameters are: function name, scope, permanence, brief description, input descriptions and variables, output descriptions and variables, descriptions and variables for parameters that are both input and output, function body
  docInOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, MS (repr (Variable repr)))] -> [(String, 
    MS (repr (Variable repr)))] -> [(String, MS (repr (Variable repr)))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))

class (MethodTypeSym repr, BlockCommentSym repr) => InternalMethod repr where
  intMethod     :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> MS (repr (MethodType repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  intFunc       :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> MS (repr (MethodType repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  commentedFunc :: MS (repr (BlockComment repr)) -> MS (repr (Method repr)) -> 
    MS (repr (Method repr))

  methodDoc :: repr (Method repr) -> Doc
  methodFromData :: ScopeTag -> Doc -> repr (Method repr)

class (ScopeSym repr, PermanenceSym repr, TypeSym repr, StatementSym repr) =>
  StateVarSym repr where
  type StateVar repr
  stateVar :: repr (Scope repr) -> repr (Permanence repr) ->
    MS (repr (Variable repr)) -> CS (repr (StateVar repr))
  stateVarDef :: Label -> repr (Scope repr) -> repr (Permanence repr) ->
    MS (repr (Variable repr)) -> MS (repr (Value repr)) -> 
    CS (repr (StateVar repr))
  constVar :: Label -> repr (Scope repr) ->  MS (repr (Variable repr)) -> 
    MS (repr (Value repr)) -> CS (repr (StateVar repr))
  privMVar :: MS (repr (Variable repr)) -> CS (repr (StateVar repr))
  pubMVar  :: MS (repr (Variable repr)) -> CS (repr (StateVar repr))
  pubGVar  :: MS (repr (Variable repr)) -> CS (repr (StateVar repr))

class InternalStateVar repr where
  stateVarDoc :: repr (StateVar repr) -> Doc
  stateVarFromData :: CS Doc -> CS (repr (StateVar repr))

class (MethodSym repr) => ClassSym repr where
  type Class repr
  buildClass :: Label -> Maybe Label -> repr (Scope repr) -> 
    [CS (repr (StateVar repr))] -> [MS (repr (Method repr))] -> 
    CS (repr (Class repr))
  enum :: Label -> [Label] -> repr (Scope repr) -> CS (repr (Class repr))
  privClass :: Label -> Maybe Label -> [CS (repr (StateVar repr))] 
    -> [MS (repr (Method repr))] -> CS (repr (Class repr))
  pubClass :: Label -> Maybe Label -> [CS (repr (StateVar repr))] 
    -> [MS (repr (Method repr))] -> CS (repr (Class repr))

  docClass :: String -> CS (repr (Class repr)) -> CS (repr (Class repr))

  commentedClass :: CS (repr (BlockComment repr)) -> 
    CS (repr (Class repr)) -> CS (repr (Class repr))

class InternalClass repr where
  classDoc :: repr (Class repr) -> Doc
  classFromData :: CS Doc -> CS (repr (Class repr))

class (ClassSym repr) => ModuleSym repr where
  type Module repr
  buildModule :: Label -> [MS (repr (Method repr))] -> 
    [CS (repr (Class repr))] -> FS (repr (Module repr))

class InternalMod repr where
  moduleDoc :: repr (Module repr) -> Doc
  modFromData :: String -> FS Doc -> FS (repr (Module repr))
  updateModuleDoc :: (Doc -> Doc) -> repr (Module repr) -> repr (Module repr)
    
class BlockCommentSym repr where
  type BlockComment repr
  blockComment :: [String] -> repr (BlockComment repr)
  docComment :: State a [String] -> State a (repr (BlockComment repr))

  blockCommentDoc :: repr (BlockComment repr) -> Doc

-- Data

data ODEInfo repr = ODEInfo {
  indepVar :: MS (repr (Variable repr)),
  depVar :: MS (repr (Variable repr)),
  tInit :: MS (repr (Value repr)),
  tFinal :: MS (repr (Value repr)),
  initVal :: MS (repr (Value repr)),
  ode :: MS (repr (Value repr))
}

odeInfo :: MS (repr (Variable repr)) -> MS (repr (Variable repr)) -> 
  MS (repr (Value repr)) -> MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
  MS (repr (Value repr)) -> ODEInfo repr
odeInfo = ODEInfo

data ODEOptions repr = ODEOptions {
  absTol :: MS (repr (Value repr)),
  relTol :: MS (repr (Value repr)),
  stepSize :: MS (repr (Value repr)),
  solveMethod :: ODEMethod
}

odeOptions :: MS (repr (Value repr)) -> MS (repr (Value repr)) -> 
  MS (repr (Value repr)) -> ODEMethod -> ODEOptions repr
odeOptions = ODEOptions

data ODEMethod = RK45 | BDF | Adams