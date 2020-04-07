{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.ClassInterface (
  -- Types
  Label, Library,
  -- Typeclasses
  ProgramSym(..), FileSym(..), PermanenceSym(..), BodySym(..), bodyStatements, 
  oneLiner, BlockSym(..), TypeSym(..), ControlBlock(..), 
  InternalControlBlock(..), listSlice, VariableSym(..), ($->), listOf, 
  ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), funcApp, funcAppNamedArgs, selfFuncApp, extFuncApp, 
  libFuncApp, newObj, extNewObj, libNewObj, exists, Selector(..), ($.), 
  selfAccess, InternalValueExp(..), objMethodCall, objMethodCallMixedArgs, 
  objMethodCallNoParams, FunctionSym(..), listIndexExists, SelectorFunction(..),
  at, StatementSym(..), (&=), assignToListIndex, initState, changeState, 
  observerListName, initObserverList, addObserver, ControlStatement(..), 
  ifNoElse, switchAsIf, ScopeSym(..), ParameterSym(..), MethodSym(..), 
  privMethod, pubMethod, initializer, nonInitConstructor, StateVarSym(..), 
  privMVar, pubMVar, pubGVar, ClassSym(..), ModuleSym(..), ODEInfo(..), 
  odeInfo, ODEOptions(..), odeOptions, ODEMethod(..), convType
) where

import GOOL.Drasil.CodeType (CodeType(..), ClassName)
import GOOL.Drasil.Helpers (onStateValue)
import GOOL.Drasil.State (GS, FS, CS, MS, VS)

import Data.Bifunctor (first)

type Label = String
type Library = String

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

class PermanenceSym repr where
  type Permanence repr
  static  :: repr (Permanence repr)
  dynamic :: repr (Permanence repr)

class (BlockSym repr) => BodySym repr where
  type Body repr
  body           :: [MS (repr (Block repr))] -> MS (repr (Body repr))

  addComments :: Label -> MS (repr (Body repr)) -> MS (repr (Body repr))

bodyStatements :: (BodySym repr) => [MS (repr (Statement repr))] -> 
  MS (repr (Body repr))
bodyStatements sts = body [block sts]

oneLiner :: (BodySym repr) => MS (repr (Statement repr)) -> 
  MS (repr (Body repr))
oneLiner s = bodyStatements [s]

class (StatementSym repr) => BlockSym repr where
  type Block repr
  block   :: [MS (repr (Statement repr))] -> MS (repr (Block repr))

class (PermanenceSym repr) => TypeSym repr where
  type Type repr
  bool          :: VS (repr (Type repr))
  int           :: VS (repr (Type repr)) -- This is 32-bit signed ints except
                                         -- in Python, which has unlimited 
                                         -- precision ints
  float         :: VS (repr (Type repr))
  double        :: VS (repr (Type repr))
  char          :: VS (repr (Type repr))
  string        :: VS (repr (Type repr))
  infile        :: VS (repr (Type repr))
  outfile       :: VS (repr (Type repr))
  listType      :: VS (repr (Type repr)) -> VS (repr (Type repr))
  arrayType     :: VS (repr (Type repr)) -> VS (repr (Type repr))
  listInnerType :: VS (repr (Type repr)) -> VS (repr (Type repr))
  obj           :: ClassName -> VS (repr (Type repr))
  -- enumType      :: Label -> VS (repr (Type repr))
  funcType      :: [VS (repr (Type repr))] -> VS (repr (Type repr)) -> 
    VS (repr (Type repr))
  iterator      :: VS (repr (Type repr)) -> VS (repr (Type repr))
  void          :: VS (repr (Type repr))

  getType :: repr (Type repr) -> CodeType
  getTypeString :: repr (Type repr) -> String

class (ControlStatement repr) => ControlBlock repr where
  runStrategy     :: Label -> [(Label, MS (repr (Body repr)))] -> 
    Maybe (VS (repr (Value repr))) -> Maybe (VS (repr (Variable repr))) -> 
    MS (repr (Block repr))

  solveODE :: ODEInfo repr -> ODEOptions repr -> MS (repr (Block repr))

class (ControlStatement repr) => InternalControlBlock repr where
  listSlice'      :: Maybe (VS (repr (Value repr))) -> 
    Maybe (VS (repr (Value repr))) -> Maybe (VS (repr (Value repr))) ->
    VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Block repr))
  
listSlice :: (InternalControlBlock repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> Maybe (VS (repr (Value repr))) -> 
  Maybe (VS (repr (Value repr))) -> Maybe (VS (repr (Value repr))) -> 
  MS (repr (Block repr))
listSlice vnew vold b e s = listSlice' b e s vnew vold

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
  -- enumVar      :: Label -> Label -> VS (repr (Variable repr))
  listVar      :: Label -> VS (repr (Type repr)) -> VS (repr (Variable repr))
  arrayElem    :: Integer -> VS (repr (Variable repr)) -> 
    VS (repr (Variable repr))
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> VS (repr (Type repr)) -> VS (repr (Variable repr))
  
  variableName :: repr (Variable repr) -> String
  variableType :: repr (Variable repr) -> repr (Type repr)

($->) :: (VariableSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Variable repr)) -> VS (repr (Variable repr))
infixl 9 $->
($->) = objVar

listOf :: (VariableSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Variable repr))
listOf = listVar

class (VariableSym repr) => ValueSym repr where
  type Value repr
  litTrue   :: VS (repr (Value repr))
  litFalse  :: VS (repr (Value repr))
  litChar   :: Char -> VS (repr (Value repr))
  litDouble :: Double -> VS (repr (Value repr))
  litFloat  :: Float -> VS (repr (Value repr))
  litInt    :: Integer -> VS (repr (Value repr))
  litString :: String -> VS (repr (Value repr))
  litArray  :: VS (repr (Type repr)) -> [VS (repr (Value repr))] ->
    VS (repr (Value repr))
  litList   :: VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
    VS (repr (Value repr))

  pi :: VS (repr (Value repr))

  --other operators ($)
  -- ($:)  :: Label -> Label -> VS (repr (Value repr))
  -- infixl 9 $:

  valueOf       :: VS (repr (Variable repr)) -> VS (repr (Value repr))
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> VS (repr (Value repr))
  -- enumElement  :: Label -> Label -> VS (repr (Value repr))

  argsList  :: VS (repr (Value repr))

  valueType :: repr (Value repr) -> repr (Type repr)

class (ValueSym repr) => NumericExpression repr where
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
class (NumericExpression repr) => BooleanExpression repr where
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

-- for values that can include expressions
class (BooleanExpression repr) => ValueExpression repr where
  inlineIf     :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr))
  
  funcAppMixedArgs :: Label -> VS (repr (Type repr)) -> [VS (repr (Value repr))]
    -> [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))
  selfFuncAppMixedArgs :: Label -> VS (repr (Type repr)) -> 
    [VS (repr (Value repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))
  extFuncAppMixedArgs :: Library -> Label -> VS (repr (Type repr)) -> 
    [VS (repr (Value repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))
  libFuncAppMixedArgs :: Library -> Label -> VS (repr (Type repr)) -> 
    [VS (repr (Value repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))
  newObjMixedArgs ::  VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))
  extNewObjMixedArgs :: Library -> VS (repr (Type repr)) -> 
    [VS (repr (Value repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))
  libNewObjMixedArgs :: Library -> VS (repr (Type repr)) -> 
    [VS (repr (Value repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))

  lambda :: [VS (repr (Variable repr))] -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))

  notNull :: VS (repr (Value repr)) -> VS (repr (Value repr))

funcApp :: (ValueExpression repr) => Label -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> VS (repr (Value repr))
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs :: (ValueExpression repr) => Label -> VS (repr (Type repr)) -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
funcAppNamedArgs n t = funcAppMixedArgs n t []

selfFuncApp :: (ValueExpression repr) => Label -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> VS (repr (Value repr))
selfFuncApp n t vs = selfFuncAppMixedArgs n t vs []

extFuncApp :: (ValueExpression repr) => Library -> Label -> 
  VS (repr (Type repr)) -> [VS (repr (Value repr))] -> VS (repr (Value repr))
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp :: (ValueExpression repr) => Library -> Label -> 
  VS (repr (Type repr)) -> [VS (repr (Value repr))] -> VS (repr (Value repr))
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

newObj :: (ValueExpression repr) => VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> VS (repr (Value repr))
newObj t vs = newObjMixedArgs t vs []

extNewObj  :: (ValueExpression repr) => Library -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> VS (repr (Value repr))
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj :: (ValueExpression repr) => Library -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> VS (repr (Value repr))
libNewObj l t vs = libNewObjMixedArgs l t vs []

exists :: (ValueExpression repr) => VS (repr (Value repr)) -> 
  VS (repr (Value repr))
exists = notNull

class (FunctionSym repr) => Selector repr where
  objAccess :: VS (repr (Value repr)) -> VS (repr (Function repr)) -> 
    VS (repr (Value repr))

  argExists :: Integer -> VS (repr (Value repr))

  indexOf :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))

($.) :: (Selector repr) => VS (repr (Value repr)) -> VS (repr (Function repr)) 
  -> VS (repr (Value repr))
infixl 9 $.
($.) = objAccess

selfAccess :: (Selector repr) => VS (repr (Function repr)) ->
  VS (repr (Value repr))
selfAccess = objAccess (valueOf self)

class (FunctionSym repr) => InternalValueExp repr where
  objMethodCallMixedArgs' :: Label -> VS (repr (Type repr)) -> 
    VS (repr (Value repr)) -> [VS (repr (Value repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    VS (repr (Value repr))
  objMethodCallNoParams' :: Label -> VS (repr (Type repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr))

objMethodCall :: (InternalValueExp repr) => VS (repr (Type repr)) -> 
  VS (repr (Value repr)) -> Label -> [VS (repr (Value repr))] -> 
  VS (repr (Value repr))
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

objMethodCallMixedArgs :: (InternalValueExp repr) => VS (repr (Type repr)) -> 
  VS (repr (Value repr)) -> Label -> [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

objMethodCallNoParams :: (InternalValueExp repr) => VS (repr (Type repr)) -> 
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

listIndexExists :: (FunctionSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr))
listIndexExists lst index = listSize lst ?> index

class (Selector repr, InternalValueExp repr) => SelectorFunction repr where
  listAccess :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr))
  listSet    :: VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
    VS (repr (Value repr)) -> VS (repr (Value repr))

at :: (SelectorFunction repr) => VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr))
at = listAccess

class (SelectorFunction repr) => StatementSym repr where
  type Statement repr
  (&-=)  :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  infixl 1 &-=
  (&+=)  :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  infixl 1 &+=
  (&++)  :: VS (repr (Variable repr)) -> MS (repr (Statement repr))
  infixl 8 &++
  (&--)  :: VS (repr (Variable repr)) -> MS (repr (Statement repr))
  infixl 8 &--

  assign            :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))

  varDec           :: VS (repr (Variable repr)) -> MS (repr (Statement repr))
  varDecDef        :: VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    MS (repr (Statement repr))
  listDec          :: Integer -> VS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  listDecDef       :: VS (repr (Variable repr)) -> [VS (repr (Value repr))] -> 
    MS (repr (Statement repr))
  arrayDec         :: Integer -> VS (repr (Variable repr)) -> 
    MS (repr (Statement repr))
  arrayDecDef      :: VS (repr (Variable repr)) -> [VS (repr (Value repr))] -> 
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

  valState :: VS (repr (Value repr)) -> MS (repr (Statement repr))

  comment :: Label -> MS (repr (Statement repr))

  free :: VS (repr (Variable repr)) -> MS (repr (Statement repr))

  throw :: Label -> MS (repr (Statement repr))

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

(&=) :: (StatementSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
infixr 1 &=
(&=) = assign

assignToListIndex :: (StatementSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> MS (repr (Statement repr))
assignToListIndex lst index v = valState $ listSet (valueOf lst) index v

initState :: (StatementSym repr) => Label -> Label -> MS (repr (Statement repr))
initState fsmName initialState = varDecDef (var fsmName string) 
  (litString initialState)

changeState :: (StatementSym repr) => Label -> Label -> 
  MS (repr (Statement repr))
changeState fsmName toState = var fsmName string &= litString toState

observerListName :: Label
observerListName = "observerList"

initObserverList :: (StatementSym repr) => VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> MS (repr (Statement repr))
initObserverList t = listDecDef (var observerListName (listType t))

addObserver :: (StatementSym repr) => VS (repr (Value repr)) -> 
  MS (repr (Statement repr))
addObserver o = valState $ listAdd obsList lastelem o
  where obsList = valueOf $ observerListName `listOf` onStateValue 
          valueType o
        lastelem = listSize obsList

class (BodySym repr) => ControlStatement repr where
  ifCond     :: [(VS (repr (Value repr)), MS (repr (Body repr)))] -> 
    MS (repr (Body repr)) -> MS (repr (Statement repr))
  switch     :: VS (repr (Value repr)) -> [(VS (repr (Value repr)), 
    MS (repr (Body repr)))] -> MS (repr (Body repr)) -> 
    MS (repr (Statement repr)) -- is there value in separating Literals into their own type?

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

ifNoElse :: (ControlStatement repr) => 
  [(VS (repr (Value repr)), MS (repr (Body repr)))] 
  -> MS (repr (Statement repr))
ifNoElse bs = ifCond bs $ body []

switchAsIf :: (ControlStatement repr) => 
  VS (repr (Value repr)) -> [(VS (repr (Value repr)), MS (repr (Body repr)))] 
  -> MS (repr (Body repr)) -> MS (repr (Statement repr))
switchAsIf v = ifCond . map (first (v ?==))

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

class ParameterSym repr where
  type Parameter repr
  param :: VS (repr (Variable repr)) -> MS (repr (Parameter repr))
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: VS (repr (Variable repr)) -> MS (repr (Parameter repr))

class (StateVarSym repr, ParameterSym repr, ControlBlock repr, 
  InternalControlBlock repr) => MethodSym repr where
  type Method repr
  method      :: Label -> repr (Scope repr) -> repr (Permanence repr) 
    -> VS (repr (Type repr)) -> [MS (repr (Parameter repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))
  getMethod   :: VS (repr (Variable repr)) -> MS (repr (Method repr))
  setMethod   :: VS (repr (Variable repr)) -> MS (repr (Method repr)) 
  constructor :: [MS (repr (Parameter repr))] -> 
    [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))

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

privMethod :: (MethodSym repr) => Label -> VS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))
privMethod n = method n private dynamic

pubMethod :: (MethodSym repr) => Label -> VS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))
pubMethod n = method n public dynamic

initializer :: (MethodSym repr) => [MS (repr (Parameter repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  MS (repr (Method repr))
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (MethodSym repr) => [MS (repr (Parameter repr))] -> 
  MS (repr (Body repr)) -> MS (repr (Method repr))
nonInitConstructor ps = constructor ps []

class (ScopeSym repr, StatementSym repr) => StateVarSym repr where
  type StateVar repr
  stateVar :: repr (Scope repr) -> repr (Permanence repr) ->
    VS (repr (Variable repr)) -> CS (repr (StateVar repr))
  stateVarDef :: Label -> repr (Scope repr) -> repr (Permanence repr) ->
    VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
    CS (repr (StateVar repr))
  constVar :: Label -> repr (Scope repr) ->  VS (repr (Variable repr)) -> 
    VS (repr (Value repr)) -> CS (repr (StateVar repr))

privMVar :: (StateVarSym repr) => VS (repr (Variable repr)) -> 
  CS (repr (StateVar repr))
privMVar = stateVar private dynamic

pubMVar :: (StateVarSym repr) => VS (repr (Variable repr)) -> 
  CS (repr (StateVar repr))
pubMVar = stateVar public dynamic

pubGVar :: (StateVarSym repr) => VS (repr (Variable repr)) -> 
  CS (repr (StateVar repr))
pubGVar = stateVar public static

class (MethodSym repr) => ClassSym repr where
  type Class repr
  buildClass :: Label -> Maybe Label -> [CS (repr (StateVar repr))] 
    -> [MS (repr (Method repr))] -> CS (repr (Class repr))
  -- enum :: Label -> [Label] -> repr (Scope repr) -> CS (repr (Class repr))
  extraClass :: Label -> Maybe Label -> [CS (repr (StateVar repr))] 
    -> [MS (repr (Method repr))] -> CS (repr (Class repr))
  implementingClass :: Label -> [Label] -> [CS (repr (StateVar repr))] -> 
    [MS (repr (Method repr))] -> CS (repr (Class repr))

  docClass :: String -> CS (repr (Class repr)) -> CS (repr (Class repr))

class (ClassSym repr) => ModuleSym repr where
  type Module repr
  buildModule :: Label -> [Label] -> [MS (repr (Method repr))] -> 
    [CS (repr (Class repr))] -> FS (repr (Module repr))

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

-- Utility

convType :: (TypeSym repr) => CodeType -> VS (repr (Type repr))
convType Boolean = bool
convType Integer = int
convType Float = float
convType Double = double
convType Char = char
convType String = string
convType (List t) = listType (convType t)
convType (Array t) = arrayType (convType t)
convType (Iterator t) = iterator $ convType t
convType (Object n) = obj n
-- convType (Enum n) = enumType n
convType (Func ps r) = funcType (map convType ps) (convType r)
convType Void = void
convType File = error "convType: File ?"