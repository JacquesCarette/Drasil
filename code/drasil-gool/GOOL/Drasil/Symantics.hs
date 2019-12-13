{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.Symantics (
  -- Types
  Label, Library,
  -- Typeclasses
  ProgramSym(..), RenderSym(..), InternalFile(..),  KeywordSym(..), 
  PermanenceSym(..), InternalPerm(..), BodySym(..), ControlBlockSym(..), 
  BlockSym(..), InternalBlock(..), TypeSym(..), InternalType(..), 
  UnaryOpSym(..), BinaryOpSym(..), InternalOp(..), VariableSym(..), 
  InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), 
  Selector(..), FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
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

class (RenderSym repr) => ProgramSym repr where
  type Program repr
  prog :: Label -> [FS (repr (RenderFile repr))] -> 
    GS (repr (Program repr))

class (ModuleSym repr, InternalFile repr) => 
  RenderSym repr where 
  type RenderFile repr
  fileDoc :: FS (repr (Module repr)) -> 
    FS (repr (RenderFile repr))

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

  include :: Label -> repr (Keyword repr)
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

class (InternalPerm repr) => PermanenceSym repr where
  type Permanence repr
  static_  :: repr (Permanence repr)
  dynamic_ :: repr (Permanence repr)

class InternalPerm repr where
  permDoc :: repr (Permanence repr) -> Doc
  binding :: repr (Permanence repr) -> Binding

class (BlockSym repr) => BodySym repr where
  type Body repr
  body           :: [GS (repr (Block repr))] -> GS (repr (Body repr))
  bodyStatements :: [GS (repr (Statement repr))] -> GS (repr (Body repr))
  oneLiner       :: GS (repr (Statement repr)) -> GS (repr (Body repr))

  addComments :: Label -> GS (repr (Body repr)) -> GS (repr (Body repr))

  bodyDoc :: repr (Body repr) -> Doc

class (StatementSym repr, InternalBlock repr) => BlockSym repr where
  type Block repr
  block   :: [GS (repr (Statement repr))] -> GS (repr (Block repr))

class InternalBlock repr where
  blockDoc :: repr (Block repr) -> Doc
  docBlock :: GS Doc -> GS (repr (Block repr))

class (PermanenceSym repr, InternalType repr) => TypeSym repr where
  type Type repr
  bool          :: GS (repr (Type repr))
  int           :: GS (repr (Type repr))
  float         :: GS (repr (Type repr))
  char          :: GS (repr (Type repr))
  string        :: GS (repr (Type repr))
  infile        :: GS (repr (Type repr))
  outfile       :: GS (repr (Type repr))
  listType      :: repr (Permanence repr) -> GS (repr (Type repr)) -> 
    GS (repr (Type repr))
  listInnerType :: GS (repr (Type repr)) -> GS (repr (Type repr))
  obj           :: Label -> GS (repr (Type repr))
  enumType      :: Label -> GS (repr (Type repr))
  iterator      :: GS (repr (Type repr)) -> GS (repr (Type repr))
  void          :: GS (repr (Type repr))

  getType :: repr (Type repr) -> CodeType
  getTypeString :: repr (Type repr) -> String
  getTypeDoc :: repr (Type repr) -> Doc

class InternalType repr where
  typeFromData :: CodeType -> String -> Doc -> repr (Type repr)

class (ControlStatementSym repr) => ControlBlockSym repr where
  runStrategy     :: Label -> [(Label, GS (repr (Body repr)))] -> 
    Maybe (GS (repr (Value repr))) -> Maybe (GS (repr (Variable repr))) -> 
    GS (repr (Block repr))

  listSlice        :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    Maybe (GS (repr (Value repr))) -> Maybe (GS (repr (Value repr))) ->
    Maybe (GS (repr (Value repr))) -> GS (repr (Block repr))

class (InternalOp repr) => UnaryOpSym repr where
  type UnaryOp repr
  notOp    :: repr (UnaryOp repr)
  negateOp :: repr (UnaryOp repr)
  sqrtOp   :: repr (UnaryOp repr)
  absOp    :: repr (UnaryOp repr)
  logOp    :: repr (UnaryOp repr)
  lnOp     :: repr (UnaryOp repr)
  expOp    :: repr (UnaryOp repr)
  sinOp    :: repr (UnaryOp repr)
  cosOp    :: repr (UnaryOp repr)
  tanOp    :: repr (UnaryOp repr)
  asinOp   :: repr (UnaryOp repr)
  acosOp   :: repr (UnaryOp repr)
  atanOp   :: repr (UnaryOp repr)
  floorOp  :: repr (UnaryOp repr)
  ceilOp   :: repr (UnaryOp repr)

class (InternalOp repr) => BinaryOpSym repr where
  type BinaryOp repr
  equalOp        :: repr (BinaryOp repr)
  notEqualOp     :: repr (BinaryOp repr)
  greaterOp      :: repr (BinaryOp repr)
  greaterEqualOp :: repr (BinaryOp repr)
  lessOp         :: repr (BinaryOp repr)
  lessEqualOp    :: repr (BinaryOp repr)
  plusOp         :: repr (BinaryOp repr)
  minusOp        :: repr (BinaryOp repr)
  multOp         :: repr (BinaryOp repr)
  divideOp       :: repr (BinaryOp repr)
  powerOp        :: repr (BinaryOp repr)
  moduloOp       :: repr (BinaryOp repr)
  andOp          :: repr (BinaryOp repr)
  orOp           :: repr (BinaryOp repr)

class InternalOp repr where
  uOpDoc :: repr (UnaryOp repr) -> Doc
  bOpDoc :: repr (BinaryOp repr) -> Doc
  uOpPrec :: repr (UnaryOp repr) -> Int
  bOpPrec :: repr (BinaryOp repr) -> Int

class (TypeSym repr, InternalVariable repr) => VariableSym repr where
  type Variable repr
  var          :: Label -> GS (repr (Type repr)) -> GS (repr (Variable repr))
  staticVar    :: Label -> GS (repr (Type repr)) -> GS (repr (Variable repr))
  const        :: Label -> GS (repr (Type repr)) -> GS (repr (Variable repr))
  extVar       :: Library -> Label -> GS (repr (Type repr)) -> 
    GS (repr (Variable repr))
  self         :: Label -> GS (repr (Variable repr))
  classVar     :: GS (repr (Type repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Variable repr))
  extClassVar  :: GS (repr (Type repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Variable repr))
  objVar       :: GS (repr (Variable repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Variable repr))
  objVarSelf   :: Label -> GS (repr (Variable repr)) -> GS (repr (Variable repr))
  enumVar      :: Label -> Label -> GS (repr (Variable repr))
  listVar      :: Label -> repr (Permanence repr) -> GS (repr (Type repr)) -> 
    GS (repr (Variable repr))
  listOf       :: Label -> GS (repr (Type repr)) -> GS (repr (Variable repr))
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> GS (repr (Type repr)) -> GS (repr (Variable repr))

  ($->) :: GS (repr (Variable repr)) -> GS (repr (Variable repr)) -> GS (repr (Variable repr))
  infixl 9 $->

  variableBind :: repr (Variable repr) -> Binding
  variableName :: repr (Variable repr) -> String
  variableType :: repr (Variable repr) -> repr (Type repr)
  variableDoc  :: repr (Variable repr) -> Doc

class InternalVariable repr where
  varFromData :: Binding -> String -> repr (Type repr) -> Doc -> 
    repr (Variable repr)

class (VariableSym repr, InternalValue repr) => ValueSym repr where
  type Value repr
  litTrue   :: GS (repr (Value repr))
  litFalse  :: GS (repr (Value repr))
  litChar   :: Char -> GS (repr (Value repr))
  litFloat  :: Double -> GS (repr (Value repr))
  litInt    :: Integer -> GS (repr (Value repr))
  litString :: String -> GS (repr (Value repr))

  pi :: GS (repr (Value repr))

  --other operators ($)
  ($:)  :: Label -> Label -> GS (repr (Value repr))
  infixl 9 $:

  valueOf       :: GS (repr (Variable repr)) -> GS (repr (Value repr))
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> GS (repr (Value repr))
  enumElement  :: Label -> Label -> GS (repr (Value repr))

  argsList  :: GS (repr (Value repr))

  valueType :: repr (Value repr) -> repr (Type repr)
  valueDoc :: repr (Value repr) -> Doc

class (ValueSym repr, UnaryOpSym repr, BinaryOpSym repr) => 
  NumericExpression repr where
  (#~)  :: GS (repr (Value repr)) -> GS (repr (Value repr))
  infixl 8 #~
  (#/^) :: GS (repr (Value repr)) -> GS (repr (Value repr))
  infixl 7 #/^
  (#|)  :: GS (repr (Value repr)) -> GS (repr (Value repr))
  infixl 7 #|
  (#+)  :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 5 #+
  (#-)  :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 5 #-
  (#*)  :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 6 #*
  (#/)  :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 6 #/
  (#%)  :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 6 #%
  (#^)  :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 7 #^

  log    :: GS (repr (Value repr)) -> GS (repr (Value repr))
  ln     :: GS (repr (Value repr)) -> GS (repr (Value repr))
  exp    :: GS (repr (Value repr)) -> GS (repr (Value repr))
  sin    :: GS (repr (Value repr)) -> GS (repr (Value repr))
  cos    :: GS (repr (Value repr)) -> GS (repr (Value repr))
  tan    :: GS (repr (Value repr)) -> GS (repr (Value repr))
  csc    :: GS (repr (Value repr)) -> GS (repr (Value repr))
  sec    :: GS (repr (Value repr)) -> GS (repr (Value repr))
  cot    :: GS (repr (Value repr)) -> GS (repr (Value repr))
  arcsin :: GS (repr (Value repr)) -> GS (repr (Value repr))
  arccos :: GS (repr (Value repr)) -> GS (repr (Value repr))
  arctan :: GS (repr (Value repr)) -> GS (repr (Value repr))
  floor  :: GS (repr (Value repr)) -> GS (repr (Value repr))
  ceil   :: GS (repr (Value repr)) -> GS (repr (Value repr))

-- I considered having two separate classes, BooleanExpressions and BooleanComparisons,
-- but this would require cyclic constraints, since it is feasible to have
-- BooleanComparisons of BooleanExpressions and also BooleanExpressions of BooleanComparisons.
-- This has the drawback of requiring a NumericExpression constraint for the first
-- 3 functions here, even though they don't really need it.
class (ValueSym repr, NumericExpression repr) => 
  BooleanExpression repr where
  (?!)  :: GS (repr (Value repr)) -> GS (repr (Value repr))
  infixr 6 ?!
  (?&&) :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 2 ?&&
  (?||) :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 1 ?||

  (?<)  :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 4 ?<
  (?<=) :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 4 ?<=
  (?>)  :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 4 ?>
  (?>=) :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 4 ?>=
  (?==) :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 3 ?==
  (?!=) :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  infixl 3 ?!=

class (ValueSym repr, BooleanExpression repr) => 
  ValueExpression repr where -- for values that can include expressions
  inlineIf     :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Value repr))
  funcApp      :: Label -> GS (repr (Type repr)) -> [GS (repr (Value repr))] -> 
    GS (repr (Value repr))
  selfFuncApp  :: Label -> Label -> GS (repr (Type repr)) -> 
    [GS (repr (Value repr))] -> GS (repr (Value repr))
  extFuncApp   :: Library -> Label -> GS (repr (Type repr)) -> 
    [GS (repr (Value repr))] -> GS (repr (Value repr))
  newObj     :: GS (repr (Type repr)) -> [GS (repr (Value repr))] -> 
    GS (repr (Value repr))
  extNewObj  :: Library -> GS (repr (Type repr)) -> [GS (repr (Value repr))] -> 
    GS (repr (Value repr))

  exists  :: GS (repr (Value repr)) -> GS (repr (Value repr))
  notNull :: GS (repr (Value repr)) -> GS (repr (Value repr))

class InternalValue repr where
  inputFunc       :: GS (repr (Value repr))
  printFunc       :: GS (repr (Value repr))
  printLnFunc     :: GS (repr (Value repr))
  printFileFunc   :: GS (repr (Value repr)) -> GS (repr (Value repr))
  printFileLnFunc :: GS (repr (Value repr)) -> GS (repr (Value repr))

  cast :: GS (repr (Type repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))

  valuePrec :: repr (Value repr) -> Maybe Int
  valFromData :: Maybe Int -> repr (Type repr) -> Doc -> repr (Value repr)

-- The cyclic constraints issue arises here too. I've constrained this by ValueExpression,
-- but really one might want one of these values as part of an expression, so the
-- constraint would have to go both ways. I'm not sure what the solution is for
-- these sorts of problems, other than removing the constraints altogether, but 
-- then what is the purpose of splitting the typeclasses into smaller typeclasses?
-- I'm leaving it as is for now, even though I suspect this will change in the future.
class (FunctionSym repr) => Selector repr where
  objAccess :: GS (repr (Value repr)) -> GS (repr (Function repr)) -> 
    GS (repr (Value repr))
  ($.)      :: GS (repr (Value repr)) -> GS (repr (Function repr)) -> 
    GS (repr (Value repr))
  infixl 9 $.

  objMethodCall     :: GS (repr (Type repr)) -> GS (repr (Value repr)) -> Label 
    -> [GS (repr (Value repr))] -> GS (repr (Value repr))
  objMethodCallNoParams :: GS (repr (Type repr)) -> GS (repr (Value repr)) -> 
    Label -> GS (repr (Value repr))

  selfAccess :: Label -> GS (repr (Function repr)) -> GS (repr (Value repr))

  listIndexExists :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  argExists       :: Integer -> GS (repr (Value repr))

  indexOf :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))

class (ValueExpression repr, InternalFunction repr) => FunctionSym repr where
  type Function repr
  func :: Label -> GS (repr (Type repr)) -> [GS (repr (Value repr))] -> 
    GS (repr (Function repr))

  get :: GS (repr (Value repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Value repr))
  set :: GS (repr (Value repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Value repr))

  listSize   :: GS (repr (Value repr)) -> GS (repr (Value repr))
  listAdd    :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Value repr))
  listAppend :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))

  iterBegin :: GS (repr (Value repr)) -> GS (repr (Value repr))
  iterEnd   :: GS (repr (Value repr)) -> GS (repr (Value repr))

class (Selector repr) => SelectorFunction repr where
  listAccess :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))
  listSet    :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Value repr))
  at         :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr))

class InternalFunction repr where
  getFunc        :: GS (repr (Variable repr)) -> GS (repr (Function repr))
  setFunc        :: GS (repr (Type repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Function repr))

  listSizeFunc       :: GS (repr (Function repr))
  listAddFunc        :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Function repr))
  listAppendFunc         :: GS (repr (Value repr)) -> GS (repr (Function repr))

  iterBeginFunc :: GS (repr (Type repr)) -> GS (repr (Function repr))
  iterEndFunc   :: GS (repr (Type repr)) -> GS (repr (Function repr))

  listAccessFunc :: GS (repr (Type repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Function repr))
  listSetFunc    :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Function repr))

  functionType :: repr (Function repr) -> repr (Type repr)
  functionDoc :: repr (Function repr) -> Doc

  funcFromData :: GS (repr (Type repr)) -> Doc -> GS (repr (Function repr))

class InternalStatement repr where
  -- newLn, printFunc, value to print, maybe a file to print to 
  printSt :: Bool -> GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    Maybe (GS (repr (Value repr))) -> GS (repr (Statement repr))

  state     :: GS (repr (Statement repr)) -> GS (repr (Statement repr))
  loopState :: GS (repr (Statement repr)) -> GS (repr (Statement repr))

  emptyState   :: GS (repr (Statement repr))
  statementDoc :: repr (Statement repr) -> Doc
  statementTerm :: repr (Statement repr) -> Terminator

  stateFromData :: Doc -> Terminator -> repr (Statement repr)

class (KeywordSym repr, SelectorFunction repr, InternalStatement repr) => 
  StatementSym repr where
  type Statement repr
  (&=)   :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  infixr 1 &=
  (&-=)  :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  infixl 1 &-=
  (&+=)  :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  infixl 1 &+=
  (&++)  :: GS (repr (Variable repr)) -> GS (repr (Statement repr))
  infixl 8 &++
  (&~-)  :: GS (repr (Variable repr)) -> GS (repr (Statement repr))
  infixl 8 &~-

  assign            :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  assignToListIndex :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Statement repr))
  multiAssign       :: [GS (repr (Variable repr))] -> [GS (repr (Value repr))] ->
    GS (repr (Statement repr)) 

  varDec           :: GS (repr (Variable repr)) -> GS (repr (Statement repr))
  varDecDef        :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  listDec          :: Integer -> GS (repr (Variable repr)) -> 
    GS (repr (Statement repr))
  listDecDef       :: GS (repr (Variable repr)) -> [GS (repr (Value repr))] -> 
    GS (repr (Statement repr))
  objDecDef        :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  objDecNew        :: GS (repr (Variable repr)) -> [GS (repr (Value repr))] -> 
    GS (repr (Statement repr))
  extObjDecNew     :: Library -> GS (repr (Variable repr)) -> 
    [GS (repr (Value repr))] -> GS (repr (Statement repr))
  objDecNewNoParams    :: GS (repr (Variable repr)) -> GS (repr (Statement repr))
  extObjDecNewNoParams :: Library -> GS (repr (Variable repr)) -> 
    GS (repr (Statement repr))
  constDecDef      :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))

  print      :: GS (repr (Value repr)) -> GS (repr (Statement repr))
  printLn    :: GS (repr (Value repr)) -> GS (repr (Statement repr))
  printStr   :: String -> GS (repr (Statement repr))
  printStrLn :: String -> GS (repr (Statement repr))

  printFile      :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  printFileLn    :: GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  printFileStr   :: GS (repr (Value repr)) -> String -> 
    GS (repr (Statement repr))
  printFileStrLn :: GS (repr (Value repr)) -> String -> 
    GS (repr (Statement repr))

  getInput         :: GS (repr (Variable repr)) -> GS (repr (Statement repr))
  discardInput     :: GS (repr (Statement repr))
  getFileInput     :: GS (repr (Value repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Statement repr))
  discardFileInput :: GS (repr (Value repr)) -> GS (repr (Statement repr))

  openFileR :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  openFileW :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  openFileA :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  closeFile :: GS (repr (Value repr)) -> GS (repr (Statement repr))

  getFileInputLine :: GS (repr (Value repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Statement repr))
  discardFileLine  :: GS (repr (Value repr)) -> GS (repr (Statement repr))
  stringSplit      :: Char -> GS (repr (Variable repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Statement repr))

  stringListVals :: [GS (repr (Variable repr))] -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr))
  stringListLists :: [GS (repr (Variable repr))] -> GS (repr (Value repr)) ->
    GS (repr (Statement repr))

  break :: GS (repr (Statement repr))
  continue :: GS (repr (Statement repr))

  returnState :: GS (repr (Value repr)) -> GS (repr (Statement repr))
  multiReturn :: [GS (repr (Value repr))] -> GS (repr (Statement repr))

  valState :: GS (repr (Value repr)) -> GS (repr (Statement repr))

  comment :: Label -> GS (repr (Statement repr))

  free :: GS (repr (Variable repr)) -> GS (repr (Statement repr))

  throw :: Label -> GS (repr (Statement repr))

  initState   :: Label -> Label -> GS (repr (Statement repr))
  changeState :: Label -> Label -> GS (repr (Statement repr))

  initObserverList :: GS (repr (Type repr)) -> [GS (repr (Value repr))] -> 
    GS (repr (Statement repr))
  addObserver      :: GS (repr (Value repr)) -> GS (repr (Statement repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [GS (repr (Value repr))] -> [GS (repr (Variable repr))] 
    -> [GS (repr (Variable repr))] -> GS (repr (Statement repr))
  selfInOutCall :: Label -> Label -> [GS (repr (Value repr))] -> 
    [GS (repr (Variable repr))] -> [GS (repr (Variable repr))] -> 
    GS (repr (Statement repr))
  extInOutCall :: Library -> Label -> [GS (repr (Value repr))] ->
    [GS (repr (Variable repr))] -> [GS (repr (Variable repr))] -> 
    GS (repr (Statement repr))

  multi     :: [GS (repr (Statement repr))] -> GS (repr (Statement repr))

class (BodySym repr) => ControlStatementSym repr where
  ifCond     :: [(GS (repr (Value repr)), GS (repr (Body repr)))] -> 
    GS (repr (Body repr)) -> GS (repr (Statement repr))
  ifNoElse   :: [(GS (repr (Value repr)), GS (repr (Body repr)))] -> 
    GS (repr (Statement repr))
  switch     :: GS (repr (Value repr)) -> [(GS (repr (Value repr)), 
    GS (repr (Body repr)))] -> GS (repr (Body repr)) -> 
    GS (repr (Statement repr)) -- is there value in separating Literals into their own type?
  switchAsIf :: GS (repr (Value repr)) -> [(GS (repr (Value repr)), 
    GS (repr (Body repr)))] -> GS (repr (Body repr)) -> 
    GS (repr (Statement repr))

  ifExists :: GS (repr (Value repr)) -> GS (repr (Body repr)) -> 
    GS (repr (Body repr)) -> GS (repr (Statement repr))

  for      :: GS (repr (Statement repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Statement repr)) -> GS (repr (Body repr)) -> 
    GS (repr (Statement repr))
  forRange :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Value repr)) -> GS (repr (Value repr)) -> GS (repr (Body repr)) 
    -> GS (repr (Statement repr))
  forEach  :: GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (Body repr)) -> GS (repr (Statement repr))
  while    :: GS (repr (Value repr)) -> GS (repr (Body repr)) -> 
    GS (repr (Statement repr)) 

  tryCatch :: GS (repr (Body repr)) -> GS (repr (Body repr)) -> 
    GS (repr (Statement repr))

  checkState      :: Label -> [(GS (repr (Value repr)), GS (repr (Body repr)))] 
    -> GS (repr (Body repr)) -> GS (repr (Statement repr))
  notifyObservers :: GS (repr (Function repr)) -> GS (repr (Type repr)) -> 
    GS (repr (Statement repr))

  getFileInputAll  :: GS (repr (Value repr)) -> GS (repr (Variable repr)) -> 
    GS (repr (Statement repr))

class (InternalScope repr) => ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class InternalScope repr where
  scopeDoc :: repr (Scope repr) -> Doc

class (TypeSym repr) => MethodTypeSym repr where
  type MethodType repr
  mType    :: GS (repr (Type repr)) -> GS (repr (MethodType repr))
  construct :: Label -> GS (repr (MethodType repr))

class (InternalParam repr) => ParameterSym repr where
  type Parameter repr
  param :: GS (repr (Variable repr)) -> MS (repr (Parameter repr))
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: GS (repr (Variable repr)) -> MS (repr (Parameter repr))

class InternalParam repr where
  parameterName :: repr (Parameter repr) -> Label
  parameterType :: repr (Parameter repr) -> repr (Type repr)
  parameterDoc  :: repr (Parameter repr) -> Doc
  paramFromData :: repr (Variable repr) -> Doc -> repr (Parameter repr)

class (StateVarSym repr, ParameterSym repr, ControlBlockSym repr, 
  InternalMethod repr) => MethodSym repr where
  type Method repr
  -- Second label is class name
  method      :: Label -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> GS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    GS (repr (Body repr)) -> MS (repr (Method repr))
  getMethod   :: Label -> GS (repr (Variable repr)) -> MS (repr (Method repr))
  setMethod   :: Label -> GS (repr (Variable repr)) -> MS (repr (Method repr)) 
  privMethod  :: Label -> Label -> GS (repr (Type repr)) -> 
    [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
    MS (repr (Method repr))
  pubMethod   :: Label -> Label -> GS (repr (Type repr)) -> 
    [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
    MS (repr (Method repr))
  constructor :: Label -> [MS (repr (Parameter repr))] -> GS (repr (Body repr)) 
    -> MS (repr (Method repr))
  destructor :: Label -> [GS (repr (StateVar repr))] -> MS (repr (Method repr))

  docMain :: GS (repr (Body repr)) -> MS (repr (Method repr))

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    GS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    GS (repr (Body repr)) -> MS (repr (Method repr))
  mainFunction  :: GS (repr (Body repr)) -> MS (repr (Method repr))
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> 
    MS (repr (Method repr)) -> MS (repr (Method repr))

  -- Second label is class name, rest is same as inOutFunc
  inOutMethod :: Label -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> [GS (repr (Variable repr))] -> [GS (repr (Variable repr))] -> 
    [GS (repr (Variable repr))] -> GS (repr (Body repr)) -> 
    MS (repr (Method repr))
  -- Second label is class name, rest is same as docInOutFunc
  docInOutMethod :: Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> String -> [(String, GS (repr (Variable repr)))] 
    -> [(String, GS (repr (Variable repr)))] -> 
    [(String, GS (repr (Variable repr)))] -> GS (repr (Body repr)) -> 
    MS (repr (Method repr))

  -- The three lists are inputs, outputs, and both, respectively
  inOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [GS (repr (Variable repr))] -> [GS (repr (Variable repr))] -> 
    [GS (repr (Variable repr))] -> GS (repr (Body repr)) -> 
    MS (repr (Method repr))
  -- Parameters are: function name, scope, permanence, brief description, input descriptions and variables, output descriptions and variables, descriptions and variables for parameters that are both input and output, function body
  docInOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, GS (repr (Variable repr)))] -> [(String, GS (repr 
    (Variable repr)))] -> [(String, GS (repr (Variable repr)))] -> 
    GS (repr (Body repr)) -> MS (repr (Method repr))

class (MethodTypeSym repr, BlockCommentSym repr) => 
  InternalMethod repr where
  intMethod     :: Bool -> Label -> Label -> repr (Scope repr) -> 
    repr (Permanence repr) -> GS (repr (MethodType repr)) -> 
    [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
    MS (repr (Method repr))
  intFunc       :: Bool -> Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> GS (repr (MethodType repr)) -> [MS (repr (Parameter repr))] -> 
    GS (repr (Body repr)) -> MS (repr (Method repr))
  commentedFunc :: MS (repr (BlockComment repr)) -> MS (repr (Method repr)) -> 
    MS (repr (Method repr))

  methodDoc :: repr (Method repr) -> Doc
  methodFromData :: ScopeTag -> Doc -> repr (Method repr)

class (ScopeSym repr, PermanenceSym repr, TypeSym repr, StatementSym repr,
  InternalStateVar repr) => StateVarSym repr where
  type StateVar repr
  stateVar :: repr (Scope repr) -> repr (Permanence repr) ->
    GS (repr (Variable repr)) -> GS (repr (StateVar repr))
  stateVarDef :: Label -> repr (Scope repr) -> repr (Permanence repr) ->
    GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
    GS (repr (StateVar repr))
  constVar :: Label -> repr (Scope repr) ->  GS (repr (Variable repr)) -> 
    GS (repr (Value repr)) -> GS (repr (StateVar repr))
  privMVar :: GS (repr (Variable repr)) -> GS (repr (StateVar repr))
  pubMVar  :: GS (repr (Variable repr)) -> GS (repr (StateVar repr))
  pubGVar  :: GS (repr (Variable repr)) -> GS (repr (StateVar repr))

class InternalStateVar repr where
  stateVarDoc :: repr (StateVar repr) -> Doc
  stateVarFromData :: GS Doc -> GS (repr (StateVar repr))

class (MethodSym repr, InternalClass repr) => ClassSym repr 
  where
  type Class repr
  buildClass :: Label -> Maybe Label -> repr (Scope repr) -> 
    [GS (repr (StateVar repr))] -> 
    [MS (repr (Method repr))] -> 
    FS (repr (Class repr))
  enum :: Label -> [Label] -> repr (Scope repr) -> 
    FS (repr (Class repr))
  privClass :: Label -> Maybe Label -> [GS (repr (StateVar repr))] 
    -> [MS (repr (Method repr))] -> 
    FS (repr (Class repr))
  pubClass :: Label -> Maybe Label -> [GS (repr (StateVar repr))] 
    -> [MS (repr (Method repr))] -> 
    FS (repr (Class repr))

  docClass :: String -> FS (repr (Class repr)) ->
    FS (repr (Class repr))

  commentedClass :: FS (repr (BlockComment repr)) -> 
    FS (repr (Class repr)) -> FS (repr (Class repr))

class InternalClass repr where
  classDoc :: repr (Class repr) -> Doc
  classFromData :: FS Doc -> FS (repr (Class repr))

class (ClassSym repr, InternalMod repr) => ModuleSym repr where
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
