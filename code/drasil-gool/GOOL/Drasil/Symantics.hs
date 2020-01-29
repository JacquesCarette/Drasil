{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.Symantics (
  -- Types
  Label, Library,
  -- Typeclasses
  ProgramSym(..), RenderSym, FileSym(..), InternalFile(..),  KeywordSym(..), 
  ImportSym(..), PermanenceSym(..), InternalPerm(..), BodySym(..), 
  InternalBody(..), BlockSym(..), InternalBlock(..), TypeSym(..), 
  InternalType(..), ControlBlockSym(..), listSlice, UnaryOpSym(..), 
  BinaryOpSym(..), InternalOp(..), VariableSym(..), InternalVariable(..), 
  ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), InternalValue(..), Selector(..), InternalSelector(..), 
  objMethodCall, objMethodCallNoParams, FunctionSym(..), SelectorFunction(..), 
  InternalFunction(..), InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), MethodTypeSym(..), ParameterSym(..), 
  InternalParam(..), MethodSym(..), initializer, nonInitConstructor, 
  InternalMethod(..), StateVarSym(..), InternalStateVar(..), ClassSym(..), 
  InternalClass(..), ModuleSym(..), InternalMod(..), BlockCommentSym(..),
  ODEInfo(..), odeInfo, ODEOptions(..), odeOptions, ODEMethod(..)
) where

import GOOL.Drasil.CodeType (CodeType)
import GOOL.Drasil.Data (Binding, Terminator, FileType, ScopeTag)
import GOOL.Drasil.State (GS, FS, CS, MS, VS)

import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

type Label = String
type Library = String

class (FileSym repr, InternalBlock repr, InternalBody repr, InternalClass repr, 
  InternalFile repr, InternalFunction repr, InternalMethod repr, 
  InternalMod repr, InternalOp repr, InternalParam repr, InternalPerm repr, 
  InternalScope repr, InternalStatement repr, InternalStateVar repr, 
  InternalType repr, InternalValue repr, InternalVariable repr, KeywordSym repr,
  ImportSym repr, UnaryOpSym repr, BinaryOpSym repr) => RenderSym repr

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

class InternalBody repr where
  bodyDoc :: repr (Body repr) -> Doc
  docBody :: MS Doc -> MS (repr (Body repr))
  multiBody :: [MS (repr (Body repr))] -> MS (repr (Body repr))

class (StatementSym repr) => BlockSym repr where
  type Block repr
  block   :: [MS (repr (Statement repr))] -> MS (repr (Block repr))

class InternalBlock repr where
  blockDoc :: repr (Block repr) -> Doc
  docBlock :: MS Doc -> MS (repr (Block repr))
  multiBlock :: [MS (repr (Block repr))] -> MS (repr (Block repr))

class (PermanenceSym repr) => TypeSym repr where
  type Type repr
  bool          :: VS (repr (Type repr))
  int           :: VS (repr (Type repr))
  float         :: VS (repr (Type repr))
  char          :: VS (repr (Type repr))
  string        :: VS (repr (Type repr))
  infile        :: VS (repr (Type repr))
  outfile       :: VS (repr (Type repr))
  listType      :: repr (Permanence repr) -> VS (repr (Type repr)) -> 
    VS (repr (Type repr))
  arrayType     :: VS (repr (Type repr)) -> VS (repr (Type repr))
  listInnerType :: VS (repr (Type repr)) -> VS (repr (Type repr))
  obj           :: Label -> VS (repr (Type repr))
  enumType      :: Label -> VS (repr (Type repr))
  funcType      :: [VS (repr (Type repr))] -> VS (repr (Type repr)) -> 
    VS (repr (Type repr))
  iterator      :: VS (repr (Type repr)) -> VS (repr (Type repr))
  void          :: VS (repr (Type repr))

  getType :: repr (Type repr) -> CodeType
  getTypeString :: repr (Type repr) -> String
  getTypeDoc :: repr (Type repr) -> Doc

class InternalType repr where
  typeFromData :: CodeType -> String -> Doc -> repr (Type repr)

class (ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, MS (repr (Body repr)))] -> 
    Maybe (VS (repr (Value repr))) -> Maybe (VS (repr (Variable repr))) -> 
    MS (repr (Block repr))

  listSlice'      :: Maybe (VS (repr (Value repr))) -> 
    Maybe (VS (repr (Value repr))) -> Maybe (VS (repr (Value repr))) ->
    VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Block repr))

  solveODE :: ODEInfo repr -> ODEOptions repr -> MS (repr (Block repr))
  
listSlice :: (ControlBlockSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> Maybe (VS (repr (Value repr))) -> 
  Maybe (VS (repr (Value repr))) -> Maybe (VS (repr (Value repr))) -> 
  MS (repr (Block repr))
listSlice vnew vold b e s = listSlice' b e s vnew vold

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

class (TypeSym repr) => VariableSym repr where
  type Variable repr
  var          :: Label -> VS (repr (Type repr)) -> VS (repr (Variable repr))
  staticVar    :: Label -> VS (repr (Type repr)) -> VS (repr (Variable repr))
  const        :: Label -> VS (repr (Type repr)) -> VS (repr (Variable repr))
  extVar       :: Library -> Label -> VS (repr (Type repr)) -> 
    VS (repr (Variable repr))
  self         :: VS (repr (Variable repr))
  classVar     :: VS (repr (Type repr)) -> VS (repr (Variable repr)) -> 
    VS (repr (Variable repr))
  extClassVar  :: VS (repr (Type repr)) -> VS (repr (Variable repr)) -> 
    VS (repr (Variable repr))
  objVar       :: VS (repr (Variable repr)) -> VS (repr (Variable repr)) -> 
    VS (repr (Variable repr))
  objVarSelf   :: VS (repr (Variable repr)) -> VS (repr (Variable repr))
  enumVar      :: Label -> Label -> VS (repr (Variable repr))
  listVar      :: Label -> repr (Permanence repr) -> VS (repr (Type repr)) -> 
    VS (repr (Variable repr))
  listOf       :: Label -> VS (repr (Type repr)) -> VS (repr (Variable repr))
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> VS (repr (Type repr)) -> VS (repr (Variable repr))

  ($->) :: VS (repr (Variable repr)) -> VS (repr (Variable repr)) -> VS (repr (Variable repr))
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
  litTrue   :: VS (repr (Value repr))
  litFalse  :: VS (repr (Value repr))
  litChar   :: Char -> VS (repr (Value repr))
  litFloat  :: Double -> VS (repr (Value repr))
  litInt    :: Integer -> VS (repr (Value repr))
  litString :: String -> VS (repr (Value repr))

  pi :: VS (repr (Value repr))

  --other operators ($)
  ($:)  :: Label -> Label -> VS (repr (Value repr))
  infixl 9 $:

  valueOf       :: VS (repr (Variable repr)) -> VS (repr (Value repr))
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> VS (repr (Value repr))
  enumElement  :: Label -> Label -> VS (repr (Value repr))

  argsList  :: VS (repr (Value repr))

  valueType :: repr (Value repr) -> repr (Type repr)
  valueDoc :: repr (Value repr) -> Doc

class (ValueSym repr) => 
  NumericExpression repr where
  (#~)  :: VS (repr (Value repr)) -> VS (repr (Value repr))
  infixl 8 #~
  (#/^) :: VS (repr (Value repr)) -> VS (repr (Value repr))
  infixl 7 #/^
  (#|)  :: VS (repr (Value repr)) -> VS (repr (Value repr))
  infixl 7 #|
  (#+)  :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 5 #+
  (#-)  :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 5 #-
  (#*)  :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 6 #*
  (#/)  :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 6 #/
  (#%)  :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 6 #%
  (#^)  :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 7 #^

  log    :: VS (repr (Value repr)) -> VS (repr (Value repr))
  ln     :: VS (repr (Value repr)) -> VS (repr (Value repr))
  exp    :: VS (repr (Value repr)) -> VS (repr (Value repr))
  sin    :: VS (repr (Value repr)) -> VS (repr (Value repr))
  cos    :: VS (repr (Value repr)) -> VS (repr (Value repr))
  tan    :: VS (repr (Value repr)) -> VS (repr (Value repr))
  csc    :: VS (repr (Value repr)) -> VS (repr (Value repr))
  sec    :: VS (repr (Value repr)) -> VS (repr (Value repr))
  cot    :: VS (repr (Value repr)) -> VS (repr (Value repr))
  arcsin :: VS (repr (Value repr)) -> VS (repr (Value repr))
  arccos :: VS (repr (Value repr)) -> VS (repr (Value repr))
  arctan :: VS (repr (Value repr)) -> VS (repr (Value repr))
  floor  :: VS (repr (Value repr)) -> VS (repr (Value repr))
  ceil   :: VS (repr (Value repr)) -> VS (repr (Value repr))

-- I considered having two separate classes, BooleanExpressions and BooleanComparisons,
-- but this would require cyclic constraints, since it is feasible to have
-- BooleanComparisons of BooleanExpressions and also BooleanExpressions of BooleanComparisons.
-- This has the drawback of requiring a NumericExpression constraint for the first
-- 3 functions here, even though they don't really need it.
class (ValueSym repr, NumericExpression repr) => 
  BooleanExpression repr where
  (?!)  :: VS (repr (Value repr)) -> VS (repr (Value repr))
  infixr 6 ?!
  (?&&) :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 2 ?&&
  (?||) :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 1 ?||

  (?<)  :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 4 ?<
  (?<=) :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 4 ?<=
  (?>)  :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 4 ?>
  (?>=) :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 4 ?>=
  (?==) :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 3 ?==
  (?!=) :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  infixl 3 ?!=

class (ValueSym repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr))
  funcApp      :: Label -> VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
    VS (repr (Value repr))
  selfFuncApp  :: Label -> VS (repr (Type repr)) -> 
    [VS (repr (Value repr))] -> VS (repr (Value repr))
  extFuncApp   :: Library -> Label -> VS (repr (Type repr)) -> 
    [VS (repr (Value repr))] -> VS (repr (Value repr))
  newObj     :: VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
    VS (repr (Value repr))
  extNewObj  :: Library -> VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
    VS (repr (Value repr))

  lambda :: [VS (repr (Variable repr))] -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))

  exists  :: VS (repr (Value repr)) -> VS (repr (Value repr))
  notNull :: VS (repr (Value repr)) -> VS (repr (Value repr))

class InternalValue repr where
  inputFunc       :: VS (repr (Value repr))
  printFunc       :: VS (repr (Value repr))
  printLnFunc     :: VS (repr (Value repr))
  printFileFunc   :: VS (repr (Value repr)) -> VS (repr (Value repr))
  printFileLnFunc :: VS (repr (Value repr)) -> VS (repr (Value repr))

  cast :: VS (repr (Type repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))

  valuePrec :: repr (Value repr) -> Maybe Int
  valFromData :: Maybe Int -> repr (Type repr) -> Doc -> repr (Value repr)

-- The cyclic constraints issue arises here too. I've constrained this by ValueExpression,
-- but really one might want one of these values as part of an expression, so the
-- constraint would have to go both ways. I'm not sure what the solution is for
-- these sorts of problems, other than removing the constraints altogether, but 
-- then what is the purpose of splitting the typeclasses into smaller typeclasses?
-- I'm leaving it as is for now, even though I suspect this will change in the future.
class (FunctionSym repr) => Selector repr where
  objAccess :: VS (repr (Value repr)) -> VS (repr (Function repr)) -> 
    VS (repr (Value repr))
  ($.)      :: VS (repr (Value repr)) -> VS (repr (Function repr)) -> 
    VS (repr (Value repr))
  infixl 9 $.

  selfAccess :: VS (repr (Function repr)) -> VS (repr (Value repr))

  listIndexExists :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  argExists       :: Integer -> VS (repr (Value repr))

  indexOf :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))

class (FunctionSym repr) => InternalSelector repr where
  objMethodCall' :: Label -> VS (repr (Type repr)) -> VS (repr (Value repr)) -> 
    [VS (repr (Value repr))] -> VS (repr (Value repr))
  objMethodCallNoParams' :: Label -> VS (repr (Type repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr))

objMethodCall :: (InternalSelector repr) => VS (repr (Type repr)) -> 
  VS (repr (Value repr)) -> Label -> [VS (repr (Value repr))] -> 
  VS (repr (Value repr))
objMethodCall t o f = objMethodCall' f t o

objMethodCallNoParams :: (InternalSelector repr) => VS (repr (Type repr)) -> 
  VS (repr (Value repr)) -> Label -> VS (repr (Value repr))
objMethodCallNoParams t o f = objMethodCallNoParams' f t o

class (ValueExpression repr) => FunctionSym repr where
  type Function repr
  func :: Label -> VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
    VS (repr (Function repr))

  get :: VS (repr (Value repr)) -> VS (repr (Variable repr)) -> 
    VS (repr (Value repr))
  set :: VS (repr (Value repr)) -> VS (repr (Variable repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr))

  listSize   :: VS (repr (Value repr)) -> VS (repr (Value repr))
  listAdd    :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr))
  listAppend :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))

  iterBegin :: VS (repr (Value repr)) -> VS (repr (Value repr))
  iterEnd   :: VS (repr (Value repr)) -> VS (repr (Value repr))

class (Selector repr, InternalSelector repr) => SelectorFunction repr where
  listAccess :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  listSet    :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr))
  at         :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))

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

class (SelectorFunction repr) => StatementSym repr where
  type Statement repr
  (&=)   :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  infixr 1 &=
  (&-=)  :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  infixl 1 &-=
  (&+=)  :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  infixl 1 &+=
  (&++)  :: VS (repr (Variable repr)) -> MS (repr (Statement repr))
  infixl 8 &++
  (&~-)  :: VS (repr (Variable repr)) -> MS (repr (Statement repr))
  infixl 8 &~-

  assign            :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  assignToListIndex :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> MS (repr (Statement repr))
  multiAssign       :: [VS (repr (Variable repr))] -> [VS (repr (Value repr))] ->
    MS (repr (Statement repr)) 

  varDec           :: VS (repr (Variable repr)) -> MS (repr (Statement repr))
  varDecDef        :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  listDec          :: Integer -> VS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  listDecDef       :: VS (repr (Variable repr)) -> [VS (repr (Value repr))] -> 
    MS (repr (Statement repr))
  objDecDef        :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  objDecNew        :: VS (repr (Variable repr)) -> [VS (repr (Value repr))] -> 
    MS (repr (Statement repr))
  extObjDecNew     :: Library -> VS (repr (Variable repr)) -> 
    [VS (repr (Value repr))] -> MS (repr (Statement repr))
  objDecNewNoParams    :: VS (repr (Variable repr)) -> MS (repr (Statement repr))
  extObjDecNewNoParams :: Library -> VS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  constDecDef      :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  funcDecDef       :: VS (repr (Variable repr)) -> [VS (repr (Variable repr))] 
    -> VS (repr (Value repr)) -> MS (repr (Statement repr))

  print      :: VS (repr (Value repr)) -> MS (repr (Statement repr))
  printLn    :: VS (repr (Value repr)) -> MS (repr (Statement repr))
  printStr   :: String -> MS (repr (Statement repr))
  printStrLn :: String -> MS (repr (Statement repr))

  printFile      :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  printFileLn    :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  printFileStr   :: VS (repr (Value repr)) -> String -> 
    MS (repr (Statement repr))
  printFileStrLn :: VS (repr (Value repr)) -> String -> 
    MS (repr (Statement repr))

  getInput         :: VS (repr (Variable repr)) -> MS (repr (Statement repr))
  discardInput     :: MS (repr (Statement repr))
  getFileInput     :: VS (repr (Value repr)) -> VS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  discardFileInput :: VS (repr (Value repr)) -> MS (repr (Statement repr))

  openFileR :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  openFileW :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  openFileA :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  closeFile :: VS (repr (Value repr)) -> MS (repr (Statement repr))

  getFileInputLine :: VS (repr (Value repr)) -> VS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  discardFileLine  :: VS (repr (Value repr)) -> MS (repr (Statement repr))
  stringSplit      :: Char -> VS (repr (Variable repr)) -> 
    VS (repr (Value repr)) -> MS (repr (Statement repr))

  stringListVals :: [VS (repr (Variable repr))] -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  stringListLists :: [VS (repr (Variable repr))] -> VS (repr (Value repr)) ->
    MS (repr (Statement repr))

  break :: MS (repr (Statement repr))
  continue :: MS (repr (Statement repr))

  returnState :: VS (repr (Value repr)) -> MS (repr (Statement repr))
  multiReturn :: [VS (repr (Value repr))] -> MS (repr (Statement repr))

  valState :: VS (repr (Value repr)) -> MS (repr (Statement repr))

  comment :: Label -> MS (repr (Statement repr))

  free :: VS (repr (Variable repr)) -> MS (repr (Statement repr))

  throw :: Label -> MS (repr (Statement repr))

  initState   :: Label -> Label -> MS (repr (Statement repr))
  changeState :: Label -> Label -> MS (repr (Statement repr))

  initObserverList :: VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
    MS (repr (Statement repr))
  addObserver      :: VS (repr (Value repr)) -> MS (repr (Statement repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [VS (repr (Value repr))] -> [VS (repr (Variable repr))] 
    -> [VS (repr (Variable repr))] -> MS (repr (Statement repr))
  selfInOutCall :: Label -> [VS (repr (Value repr))] -> 
    [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
    MS (repr (Statement repr))
  extInOutCall :: Library -> Label -> [VS (repr (Value repr))] ->
    [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
    MS (repr (Statement repr))

  multi     :: [MS (repr (Statement repr))] -> MS (repr (Statement repr))

class (BodySym repr) => ControlStatementSym repr where
  ifCond     :: [(VS (repr (Value repr)), MS (repr (Body repr)))] -> 
    MS (repr (Body repr)) -> MS (repr (Statement repr))
  ifNoElse   :: [(VS (repr (Value repr)), MS (repr (Body repr)))] -> 
    MS (repr (Statement repr))
  switch     :: VS (repr (Value repr)) -> [(VS (repr (Value repr)), 
    MS (repr (Body repr)))] -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr)) -- is there value in separating Literals into their own type?
  switchAsIf :: VS (repr (Value repr)) -> [(VS (repr (Value repr)), 
    MS (repr (Body repr)))] -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr))

  ifExists :: VS (repr (Value repr)) -> MS (repr (Body repr)) -> 
    MS (repr (Body repr)) -> MS (repr (Statement repr))

  for      :: MS (repr (Statement repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr)) -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr))
  forRange :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr)) -> MS (repr (Body repr)) 
    -> MS (repr (Statement repr))
  forEach  :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Body repr)) -> MS (repr (Statement repr))
  while    :: VS (repr (Value repr)) -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr)) 

  tryCatch :: MS (repr (Body repr)) -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr))

  checkState      :: Label -> [(VS (repr (Value repr)), MS (repr (Body repr)))] 
    -> MS (repr (Body repr)) -> MS (repr (Statement repr))
  notifyObservers :: VS (repr (Function repr)) -> VS (repr (Type repr)) -> 
    MS (repr (Statement repr))

  getFileInputAll  :: VS (repr (Value repr)) -> VS (repr (Variable repr)) -> 
    MS (repr (Statement repr))

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class InternalScope repr where
  scopeDoc :: repr (Scope repr) -> Doc

class (TypeSym repr) => MethodTypeSym repr where
  type MethodType repr
  mType    :: VS (repr (Type repr)) -> MS (repr (MethodType repr))
  construct :: Label -> MS (repr (MethodType repr))

class ParameterSym repr where
  type Parameter repr
  param :: VS (repr (Variable repr)) -> MS (repr (Parameter repr))
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: VS (repr (Variable repr)) -> MS (repr (Parameter repr))

class InternalParam repr where
  parameterName :: repr (Parameter repr) -> Label
  parameterType :: repr (Parameter repr) -> repr (Type repr)
  parameterDoc  :: repr (Parameter repr) -> Doc
  paramFromData :: repr (Variable repr) -> Doc -> repr (Parameter repr)

class (StateVarSym repr, ParameterSym repr, ControlBlockSym repr) => 
  MethodSym repr where
  type Method repr
  method      :: Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> VS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  getMethod   :: VS (repr (Variable repr)) -> MS (repr (Method repr))
  setMethod   :: VS (repr (Variable repr)) -> MS (repr (Method repr)) 
  privMethod  :: Label -> VS (repr (Type repr)) -> [MS (repr (Parameter repr))] 
    -> MS (repr (Body repr)) -> MS (repr (Method repr))
  pubMethod   :: Label -> VS (repr (Type repr)) -> [MS (repr (Parameter repr))] 
    -> MS (repr (Body repr)) -> MS (repr (Method repr))
  constructor :: [MS (repr (Parameter repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  destructor :: [CS (repr (StateVar repr))] -> MS (repr (Method repr))

  docMain :: MS (repr (Body repr)) -> MS (repr (Method repr))

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    VS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  mainFunction  :: MS (repr (Body repr)) -> MS (repr (Method repr))
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> 
    MS (repr (Method repr)) -> MS (repr (Method repr))

  inOutMethod :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
    [VS (repr (Variable repr))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr))
  docInOutMethod :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, VS (repr (Variable repr)))] -> 
    [(String, VS (repr (Variable repr)))] -> 
    [(String, VS (repr (Variable repr)))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
    [VS (repr (Variable repr))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr))
  -- Parameters are: function name, scope, permanence, brief description, input descriptions and variables, output descriptions and variables, descriptions and variables for parameters that are both input and output, function body
  docInOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, VS (repr (Variable repr)))] -> [(String, 
    VS (repr (Variable repr)))] -> [(String, VS (repr (Variable repr)))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))

initializer :: (MethodSym repr) => [MS (repr (Parameter repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  MS (repr (Method repr))
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (MethodSym repr) => [MS (repr (Parameter repr))] -> 
  MS (repr (Body repr)) -> MS (repr (Method repr))
nonInitConstructor ps = constructor ps []

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
    VS (repr (Variable repr)) -> CS (repr (StateVar repr))
  stateVarDef :: Label -> repr (Scope repr) -> repr (Permanence repr) ->
    VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    CS (repr (StateVar repr))
  constVar :: Label -> repr (Scope repr) ->  VS (repr (Variable repr)) -> 
    VS (repr (Value repr)) -> CS (repr (StateVar repr))
  privMVar :: VS (repr (Variable repr)) -> CS (repr (StateVar repr))
  pubMVar  :: VS (repr (Variable repr)) -> CS (repr (StateVar repr))
  pubGVar  :: VS (repr (Variable repr)) -> CS (repr (StateVar repr))

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
  indepVar :: VS (repr (Variable repr)),
  depVar :: VS (repr (Variable repr)),
  otherVars :: [VS (repr (Variable repr))],
  tInit :: VS (repr (Value repr)),
  tFinal :: VS (repr (Value repr)),
  initVal :: VS (repr (Value repr)),
  ode :: VS (repr (Value repr))
}

odeInfo :: VS (repr (Variable repr)) -> VS (repr (Variable repr)) -> 
  [VS (repr (Variable repr))] -> VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
  ODEInfo repr
odeInfo = ODEInfo

data ODEOptions repr = ODEOptions {
  solveMethod :: ODEMethod,
  absTol :: VS (repr (Value repr)),
  relTol :: VS (repr (Value repr)),
  stepSize :: VS (repr (Value repr))
}

odeOptions :: ODEMethod -> VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> ODEOptions repr
odeOptions = ODEOptions

data ODEMethod = RK45 | BDF | Adams