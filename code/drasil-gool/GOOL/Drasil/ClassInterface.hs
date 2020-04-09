{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.ClassInterface (
  -- Types
  Label, Library, GSProgram, SFile, MSBody, MSBlock, VSType, SVariable, SValue, 
  VSFunction, MSStatement, MSParameter, SMethod, CSStateVar, SClass, FSModule,
  -- Typeclasses
  ProgramSym(..), FileSym(..), PermanenceSym(..), BodySym(..), bodyStatements, 
  oneLiner, BlockSym(..), TypeSym(..), ControlBlock(..), 
  InternalControlBlock(..), listSlice, VariableSym(..), ($->), listOf, 
  ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), funcApp, funcAppNamedArgs, selfFuncApp, extFuncApp, 
  libFuncApp, newObj, extNewObj, libNewObj, exists, Selector(..), ($.), 
  selfAccess, InternalValueExp(..), objMethodCall, objMethodCallMixedArgs, 
  objMethodCallNoParams, FunctionSym(..), listIndexExists, SelectorFunction(..),
  at, StatementSym(..), AssignStatement(..), (&=), assignToListIndex,
  DeclStatement(..), IOStatement(..), FuncAppStatement(..), MiscStatement(..), 
  initState, changeState, observerListName, initObserverList, addObserver, 
  ControlStatement(..), ifNoElse, switchAsIf, ScopeSym(..), ParameterSym(..), 
  MethodSym(..), privMethod, pubMethod, initializer, nonInitConstructor, 
  StateVarSym(..), privDVar, pubDVar, pubSVar, ClassSym(..), ModuleSym(..), 
  ODEInfo(..), odeInfo, ODEOptions(..), odeOptions, ODEMethod(..), convType
) where

import GOOL.Drasil.CodeType (CodeType(..), ClassName)
import GOOL.Drasil.Helpers (onStateValue)
import GOOL.Drasil.State (GS, FS, CS, MS, VS)

import Data.Bifunctor (first)

type Label = String
type Library = String

type GSProgram a = GS (a (Program a))

class (FileSym repr) => ProgramSym repr where
  type Program repr
  prog :: Label -> [SFile repr] -> GSProgram repr

type SFile a = FS (a (RenderFile a))

class (ModuleSym repr) => FileSym repr where 
  type RenderFile repr
  fileDoc :: FSModule repr -> SFile repr

  -- Module description, list of author names, date as a String, file to comment
  docMod :: String -> [String] -> String -> SFile repr -> SFile repr

class PermanenceSym repr where
  type Permanence repr
  static  :: repr (Permanence repr)
  dynamic :: repr (Permanence repr)

type MSBody a = MS (a (Body a))

class (BlockSym repr) => BodySym repr where
  type Body repr
  body           :: [MSBlock repr] -> MSBody repr

  addComments :: Label -> MSBody repr -> MSBody repr

bodyStatements :: (BodySym repr) => [MSStatement repr] -> MSBody repr
bodyStatements sts = body [block sts]

oneLiner :: (BodySym repr) => MSStatement repr -> MSBody repr
oneLiner s = bodyStatements [s]

type MSBlock a = MS (a (Block a))

class (AssignStatement repr, DeclStatement repr, IOStatement repr, 
  FuncAppStatement repr, MiscStatement repr) => BlockSym repr where
  type Block repr
  block   :: [MSStatement repr] -> MSBlock repr

type VSType a = VS (a (Type a))

class TypeSym repr where
  type Type repr
  bool          :: VSType repr
  int           :: VSType repr -- This is 32-bit signed ints except
                                         -- in Python, which has unlimited 
                                         -- precision ints
  float         :: VSType repr
  double        :: VSType repr
  char          :: VSType repr
  string        :: VSType repr
  infile        :: VSType repr
  outfile       :: VSType repr
  listType      :: VSType repr -> VSType repr
  arrayType     :: VSType repr -> VSType repr
  listInnerType :: VSType repr -> VSType repr
  obj           :: ClassName -> VSType repr
  -- enumType      :: Label -> VSType repr
  funcType      :: [VSType repr] -> VSType repr -> VSType repr
  iterator      :: VSType repr -> VSType repr
  void          :: VSType repr

  getType :: repr (Type repr) -> CodeType
  getTypeString :: repr (Type repr) -> String

class (ControlStatement repr) => ControlBlock repr where
  runStrategy     :: Label -> [(Label, MSBody repr)] -> Maybe (SValue repr) -> 
    Maybe (SVariable repr) -> MSBlock repr

  solveODE :: ODEInfo repr -> ODEOptions repr -> MSBlock repr

class (ControlStatement repr) => InternalControlBlock repr where
  listSlice'      :: Maybe (SValue repr) -> Maybe (SValue repr) -> 
    Maybe (SValue repr) -> SVariable repr -> SValue repr -> MSBlock repr
  
listSlice :: (InternalControlBlock repr) => SVariable repr -> SValue repr -> 
  Maybe (SValue repr) -> Maybe (SValue repr) -> Maybe (SValue repr) -> 
  MSBlock repr
listSlice vnew vold b e s = listSlice' b e s vnew vold

type SVariable a = VS (a (Variable a))

class (TypeSym repr) => VariableSym repr where
  type Variable repr
  var          :: Label -> VSType repr -> SVariable repr
  staticVar    :: Label -> VSType repr -> SVariable repr
  const        :: Label -> VSType repr -> SVariable repr
  extVar       :: Library -> Label -> VSType repr -> SVariable repr
  self         :: SVariable repr
  classVar     :: VSType repr -> SVariable repr -> SVariable repr
  extClassVar  :: VSType repr -> SVariable repr -> SVariable repr
  objVar       :: SVariable repr -> SVariable repr -> SVariable repr
  objVarSelf   :: SVariable repr -> SVariable repr
  -- enumVar      :: Label -> Label -> SVariable repr
  listVar      :: Label -> VSType repr -> SVariable repr
  arrayElem    :: Integer -> SVariable repr -> SVariable repr
  -- Use for iterator variables, i.e. in a forEach loop.
  iterVar      :: Label -> VSType repr -> SVariable repr
  
  variableName :: repr (Variable repr) -> String
  variableType :: repr (Variable repr) -> repr (Type repr)

($->) :: (VariableSym repr) => SVariable repr -> SVariable repr -> 
  SVariable repr
infixl 9 $->
($->) = objVar

listOf :: (VariableSym repr) => Label -> VSType repr -> SVariable repr
listOf = listVar

type SValue a = VS (a (Value a))

class (VariableSym repr) => ValueSym repr where
  type Value repr
  litTrue   :: SValue repr
  litFalse  :: SValue repr
  litChar   :: Char -> SValue repr
  litDouble :: Double -> SValue repr
  litFloat  :: Float -> SValue repr
  litInt    :: Integer -> SValue repr
  litString :: String -> SValue repr
  litArray  :: VSType repr -> [SValue repr] -> SValue repr
  litList   :: VSType repr -> [SValue repr] -> SValue repr

  pi :: SValue repr

  --other operators ($)
  -- ($:)  :: Label -> Label -> SValue repr
  -- infixl 9 $:

  valueOf       :: SVariable repr -> SValue repr
--  global       :: Label -> repr (Value repr)         -- not sure how this one works, but in GOOL it was hardcoded to give an error so I'm leaving it out for now
  arg          :: Integer -> SValue repr
  -- enumElement  :: Label -> Label -> SValue repr

  argsList  :: SValue repr

  valueType :: repr (Value repr) -> repr (Type repr)

class (ValueSym repr) => NumericExpression repr where
  (#~)  :: SValue repr -> SValue repr
  infixl 8 #~
  (#/^) :: SValue repr -> SValue repr
  infixl 7 #/^
  (#|)  :: SValue repr -> SValue repr
  infixl 7 #|
  (#+)  :: SValue repr -> SValue repr -> SValue repr
  infixl 5 #+
  (#-)  :: SValue repr -> SValue repr -> SValue repr
  infixl 5 #-
  (#*)  :: SValue repr -> SValue repr -> SValue repr
  infixl 6 #*
  (#/)  :: SValue repr -> SValue repr -> SValue repr
  infixl 6 #/
  (#%)  :: SValue repr -> SValue repr -> SValue repr
  infixl 6 #%
  (#^)  :: SValue repr -> SValue repr -> SValue repr
  infixl 7 #^

  log    :: SValue repr -> SValue repr
  ln     :: SValue repr -> SValue repr
  exp    :: SValue repr -> SValue repr
  sin    :: SValue repr -> SValue repr
  cos    :: SValue repr -> SValue repr
  tan    :: SValue repr -> SValue repr
  csc    :: SValue repr -> SValue repr
  sec    :: SValue repr -> SValue repr
  cot    :: SValue repr -> SValue repr
  arcsin :: SValue repr -> SValue repr
  arccos :: SValue repr -> SValue repr
  arctan :: SValue repr -> SValue repr
  floor  :: SValue repr -> SValue repr
  ceil   :: SValue repr -> SValue repr

-- I considered having two separate classes, BooleanExpressions and BooleanComparisons,
-- but this would require cyclic constraints, since it is feasible to have
-- BooleanComparisons of BooleanExpressions and also BooleanExpressions of BooleanComparisons.
-- This has the drawback of requiring a NumericExpression constraint for the first
-- 3 functions here, even though they don't really need it.
class (NumericExpression repr) => BooleanExpression repr where
  (?!)  :: SValue repr -> SValue repr
  infixr 6 ?!
  (?&&) :: SValue repr -> SValue repr -> SValue repr
  infixl 2 ?&&
  (?||) :: SValue repr -> SValue repr -> SValue repr
  infixl 1 ?||

  (?<)  :: SValue repr -> SValue repr -> SValue repr
  infixl 4 ?<
  (?<=) :: SValue repr -> SValue repr -> SValue repr
  infixl 4 ?<=
  (?>)  :: SValue repr -> SValue repr -> SValue repr
  infixl 4 ?>
  (?>=) :: SValue repr -> SValue repr -> SValue repr
  infixl 4 ?>=
  (?==) :: SValue repr -> SValue repr -> SValue repr
  infixl 3 ?==
  (?!=) :: SValue repr -> SValue repr -> SValue repr
  infixl 3 ?!=

-- for values that can include expressions
class (BooleanExpression repr) => ValueExpression repr where
  inlineIf     :: SValue repr -> SValue repr -> SValue repr -> SValue repr
  
  funcAppMixedArgs :: Label -> VSType repr -> [SValue repr] -> 
    [(SVariable repr, SValue repr)] -> SValue repr
  selfFuncAppMixedArgs :: Label -> VSType repr -> [SValue repr] -> 
    [(SVariable repr, SValue repr)] -> SValue repr
  extFuncAppMixedArgs :: Library -> Label -> VSType repr -> [SValue repr] -> 
    [(SVariable repr, SValue repr)] -> SValue repr
  libFuncAppMixedArgs :: Library -> Label -> VSType repr -> [SValue repr] -> 
    [(SVariable repr, SValue repr)] -> SValue repr
  newObjMixedArgs ::  VSType repr -> [SValue repr] -> 
    [(SVariable repr, SValue repr)] -> SValue repr
  extNewObjMixedArgs :: Library -> VSType repr -> [SValue repr] -> 
    [(SVariable repr, SValue repr)] -> SValue repr
  libNewObjMixedArgs :: Library -> VSType repr -> [SValue repr] -> 
    [(SVariable repr, SValue repr)] -> SValue repr

  lambda :: [SVariable repr] -> SValue repr -> SValue repr

  notNull :: SValue repr -> SValue repr

funcApp :: (ValueExpression repr) => Label -> VSType repr -> [SValue repr] -> 
  SValue repr
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs :: (ValueExpression repr) => Label -> VSType repr -> 
  [(SVariable repr, SValue repr)] -> SValue repr
funcAppNamedArgs n t = funcAppMixedArgs n t []

selfFuncApp :: (ValueExpression repr) => Label -> VSType repr -> [SValue repr] 
  -> SValue repr
selfFuncApp n t vs = selfFuncAppMixedArgs n t vs []

extFuncApp :: (ValueExpression repr) => Library -> Label -> VSType repr -> 
  [SValue repr] -> SValue repr
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp :: (ValueExpression repr) => Library -> Label -> VSType repr -> 
  [SValue repr] -> SValue repr
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

newObj :: (ValueExpression repr) => VSType repr -> [SValue repr] -> SValue repr
newObj t vs = newObjMixedArgs t vs []

extNewObj  :: (ValueExpression repr) => Library -> VSType repr -> [SValue repr] 
  -> SValue repr
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj :: (ValueExpression repr) => Library -> VSType repr -> [SValue repr] 
  -> SValue repr
libNewObj l t vs = libNewObjMixedArgs l t vs []

exists :: (ValueExpression repr) => SValue repr -> SValue repr
exists = notNull

class (FunctionSym repr) => Selector repr where
  objAccess :: SValue repr -> VSFunction repr -> SValue repr

  argExists :: Integer -> SValue repr

  indexOf :: SValue repr -> SValue repr -> SValue repr

($.) :: (Selector repr) => SValue repr -> VSFunction repr -> SValue repr
infixl 9 $.
($.) = objAccess

selfAccess :: (Selector repr) => VSFunction repr -> SValue repr
selfAccess = objAccess (valueOf self)

class (FunctionSym repr) => InternalValueExp repr where
  objMethodCallMixedArgs' :: Label -> VSType repr -> SValue repr -> 
    [SValue repr] -> [(SVariable repr, SValue repr)] -> SValue repr
  objMethodCallNoParams' :: Label -> VSType repr -> SValue repr -> SValue repr

objMethodCall :: (InternalValueExp repr) => VSType repr -> SValue repr -> Label 
  -> [SValue repr] -> SValue repr
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

objMethodCallMixedArgs :: (InternalValueExp repr) => VSType repr -> SValue repr 
  -> Label -> [SValue repr] -> [(SVariable repr, SValue repr)] -> SValue repr
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

objMethodCallNoParams :: (InternalValueExp repr) => VSType repr -> SValue repr 
  -> Label -> SValue repr
objMethodCallNoParams t o f = objMethodCallNoParams' f t o

type VSFunction a = VS (a (Function a))

class (ValueExpression repr) => FunctionSym repr where
  type Function repr
  func :: Label -> VSType repr -> [SValue repr] -> VSFunction repr

  get :: SValue repr -> SVariable repr -> SValue repr
  set :: SValue repr -> SVariable repr -> SValue repr -> SValue repr

  listSize   :: SValue repr -> SValue repr
  listAdd    :: SValue repr -> SValue repr -> SValue repr -> SValue repr
  listAppend :: SValue repr -> SValue repr -> SValue repr

  iterBegin :: SValue repr -> SValue repr
  iterEnd   :: SValue repr -> SValue repr

listIndexExists :: (FunctionSym repr) => SValue repr -> SValue repr -> 
  SValue repr
listIndexExists lst index = listSize lst ?> index

class (Selector repr, InternalValueExp repr) => SelectorFunction repr where
  listAccess :: SValue repr -> SValue repr -> SValue repr
  listSet    :: SValue repr -> SValue repr -> SValue repr -> SValue repr

at :: (SelectorFunction repr) => SValue repr -> SValue repr -> SValue repr
at = listAccess

type MSStatement a = MS (a (Statement a))

class (SelectorFunction repr) => StatementSym repr where
  type Statement repr

class (StatementSym repr) => AssignStatement repr where
  (&-=)  :: SVariable repr -> SValue repr -> MSStatement repr
  infixl 1 &-=
  (&+=)  :: SVariable repr -> SValue repr -> MSStatement repr
  infixl 1 &+=
  (&++)  :: SVariable repr -> MSStatement repr
  infixl 8 &++
  (&--)  :: SVariable repr -> MSStatement repr
  infixl 8 &--

  assign            :: SVariable repr -> SValue repr -> MSStatement repr

(&=) :: (AssignStatement repr) => SVariable repr -> SValue repr -> 
  MSStatement repr
infixr 1 &=
(&=) = assign

assignToListIndex :: (MiscStatement repr) => SVariable repr -> SValue repr -> 
  SValue repr -> MSStatement repr
assignToListIndex lst index v = valState $ listSet (valueOf lst) index v

class (StatementSym repr) => DeclStatement repr where
  varDec           :: SVariable repr -> MSStatement repr
  varDecDef        :: SVariable repr -> SValue repr -> MSStatement repr
  listDec          :: Integer -> SVariable repr -> MSStatement repr
  listDecDef       :: SVariable repr -> [SValue repr] -> MSStatement repr
  arrayDec         :: Integer -> SVariable repr -> MSStatement repr
  arrayDecDef      :: SVariable repr -> [SValue repr] -> MSStatement repr
  objDecDef        :: SVariable repr -> SValue repr -> MSStatement repr
  objDecNew        :: SVariable repr -> [SValue repr] -> MSStatement repr
  extObjDecNew     :: Library -> SVariable repr -> [SValue repr] -> 
    MSStatement repr
  objDecNewNoParams    :: SVariable repr -> MSStatement repr
  extObjDecNewNoParams :: Library -> SVariable repr -> MSStatement repr
  constDecDef      :: SVariable repr -> SValue repr -> MSStatement repr
  funcDecDef       :: SVariable repr -> [SVariable repr] -> SValue repr -> 
    MSStatement repr

class (StatementSym repr) => IOStatement repr where
  print      :: SValue repr -> MSStatement repr
  printLn    :: SValue repr -> MSStatement repr
  printStr   :: String -> MSStatement repr
  printStrLn :: String -> MSStatement repr

  printFile      :: SValue repr -> SValue repr -> MSStatement repr
  printFileLn    :: SValue repr -> SValue repr -> MSStatement repr
  printFileStr   :: SValue repr -> String -> MSStatement repr
  printFileStrLn :: SValue repr -> String -> MSStatement repr

  getInput         :: SVariable repr -> MSStatement repr
  discardInput     :: MSStatement repr
  getFileInput     :: SValue repr -> SVariable repr -> MSStatement repr
  discardFileInput :: SValue repr -> MSStatement repr

  openFileR :: SVariable repr -> SValue repr -> MSStatement repr
  openFileW :: SVariable repr -> SValue repr -> MSStatement repr
  openFileA :: SVariable repr -> SValue repr -> MSStatement repr
  closeFile :: SValue repr -> MSStatement repr

  getFileInputLine :: SValue repr -> SVariable repr -> MSStatement repr
  discardFileLine  :: SValue repr -> MSStatement repr
  stringSplit      :: Char -> SVariable repr -> SValue repr -> MSStatement repr

  stringListVals :: [SVariable repr] -> SValue repr -> MSStatement repr
  stringListLists :: [SVariable repr] -> SValue repr -> MSStatement repr

class (StatementSym repr) => FuncAppStatement repr where
  -- The three lists are inputs, outputs, and both, respectively
  inOutCall :: Label -> [SValue repr] -> [SVariable repr] -> [SVariable repr] 
    -> MSStatement repr
  selfInOutCall :: Label -> [SValue repr] -> [SVariable repr] -> 
    [SVariable repr] -> MSStatement repr
  extInOutCall :: Library -> Label -> [SValue repr] -> [SVariable repr] -> 
    [SVariable repr] -> MSStatement repr
  
class (StatementSym repr) => MiscStatement repr where
  valState :: SValue repr -> MSStatement repr
  comment :: Label -> MSStatement repr
  multi     :: [MSStatement repr] -> MSStatement repr

initState :: (DeclStatement repr) => Label -> Label -> MSStatement repr
initState fsmName initialState = varDecDef (var fsmName string) 
  (litString initialState)

changeState :: (AssignStatement repr) => Label -> Label -> MSStatement repr
changeState fsmName toState = var fsmName string &= litString toState

observerListName :: Label
observerListName = "observerList"

initObserverList :: (DeclStatement repr) => VSType repr -> [SValue repr] -> 
  MSStatement repr
initObserverList t = listDecDef (var observerListName (listType t))

addObserver :: (MiscStatement repr) => SValue repr -> MSStatement repr
addObserver o = valState $ listAdd obsList lastelem o
  where obsList = valueOf $ observerListName `listOf` onStateValue valueType o
        lastelem = listSize obsList

class (BodySym repr) => ControlStatement repr where
  break :: MSStatement repr
  continue :: MSStatement repr

  returnState :: SValue repr -> MSStatement repr

  throw :: Label -> MSStatement repr

  ifCond     :: [(SValue repr, MSBody repr)] -> MSBody repr -> MSStatement repr
  switch     :: SValue repr -> [(SValue repr, MSBody repr)] -> MSBody repr -> 
    MSStatement repr -- is there value in separating Literals into their own type?

  ifExists :: SValue repr -> MSBody repr -> MSBody repr -> MSStatement repr

  for      :: MSStatement repr -> SValue repr -> MSStatement repr -> 
    MSBody repr -> MSStatement repr
  forRange :: SVariable repr -> SValue repr -> SValue repr -> SValue repr ->
    MSBody repr -> MSStatement repr
  forEach  :: SVariable repr -> SValue repr -> MSBody repr -> MSStatement repr
  while    :: SValue repr -> MSBody repr -> MSStatement repr 

  tryCatch :: MSBody repr -> MSBody repr -> MSStatement repr

  checkState      :: Label -> [(SValue repr, MSBody repr)] -> MSBody repr -> 
    MSStatement repr
  notifyObservers :: VSFunction repr -> VSType repr -> MSStatement repr

  getFileInputAll  :: SValue repr -> SVariable repr -> MSStatement repr

ifNoElse :: (ControlStatement repr) => [(SValue repr, MSBody repr)] 
  -> MSStatement repr
ifNoElse bs = ifCond bs $ body []

switchAsIf :: (ControlStatement repr) => SValue repr -> 
  [(SValue repr, MSBody repr)] -> MSBody repr -> MSStatement repr
switchAsIf v = ifCond . map (first (v ?==))

class ScopeSym repr where
  type Scope repr
  private :: repr (Scope repr)
  public  :: repr (Scope repr)

type MSParameter a = MS (a (Parameter a))

class ParameterSym repr where
  type Parameter repr
  param :: SVariable repr -> MSParameter repr
  -- funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) -- not implemented in GOOL
  pointerParam :: SVariable repr -> MSParameter repr

type SMethod a = MS (a (Method a))

class (ParameterSym repr, ControlBlock repr, InternalControlBlock repr,
  ScopeSym repr, PermanenceSym repr) => MethodSym repr where
  type Method repr
  method      :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    VSType repr -> [MSParameter repr] -> MSBody repr -> SMethod repr
  getMethod   :: SVariable repr -> SMethod repr
  setMethod   :: SVariable repr -> SMethod repr 
  constructor :: [MSParameter repr] -> [(SVariable repr, SValue repr)] -> 
    MSBody repr -> SMethod repr

  docMain :: MSBody repr -> SMethod repr

  function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    VSType repr -> [MSParameter repr] -> MSBody repr -> SMethod repr
  mainFunction  :: MSBody repr -> SMethod repr
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> SMethod repr -> SMethod repr

  inOutMethod :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [SVariable repr] -> [SVariable repr] -> [SVariable repr] -> MSBody repr -> 
    SMethod repr
  docInOutMethod :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, SVariable repr)] -> [(String, SVariable repr)] -> 
    [(String, SVariable repr)] -> MSBody repr -> SMethod repr

  -- The three lists are inputs, outputs, and both, respectively
  inOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    [SVariable repr] -> [SVariable repr] -> [SVariable repr] -> MSBody repr -> 
    SMethod repr
  -- Parameters are: function name, scope, permanence, brief description, input descriptions and variables, output descriptions and variables, descriptions and variables for parameters that are both input and output, function body
  docInOutFunc :: Label -> repr (Scope repr) -> repr (Permanence repr) -> 
    String -> [(String, SVariable repr)] -> [(String, SVariable repr)] -> 
    [(String, SVariable repr)] -> MSBody repr -> SMethod repr

privMethod :: (MethodSym repr) => Label -> VSType repr -> [MSParameter repr] -> 
  MSBody repr -> SMethod repr
privMethod n = method n private dynamic

pubMethod :: (MethodSym repr) => Label -> VSType repr -> [MSParameter repr] -> 
  MSBody repr -> SMethod repr
pubMethod n = method n public dynamic

initializer :: (MethodSym repr) => [MSParameter repr] -> 
  [(SVariable repr, SValue repr)] -> SMethod repr
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (MethodSym repr) => [MSParameter repr] -> MSBody repr -> 
  SMethod repr
nonInitConstructor ps = constructor ps []

type CSStateVar a = CS (a (StateVar a))

class (ScopeSym repr, PermanenceSym repr) => StateVarSym repr where
  type StateVar repr
  stateVar :: repr (Scope repr) -> repr (Permanence repr) -> SVariable repr -> 
    CSStateVar repr
  stateVarDef :: Label -> repr (Scope repr) -> repr (Permanence repr) ->
    SVariable repr -> SValue repr -> CSStateVar repr
  constVar :: Label -> repr (Scope repr) ->  SVariable repr -> SValue repr -> 
    CSStateVar repr

privDVar :: (StateVarSym repr) => SVariable repr -> CSStateVar repr
privDVar = stateVar private dynamic

pubDVar :: (StateVarSym repr) => SVariable repr -> CSStateVar repr
pubDVar = stateVar public dynamic

pubSVar :: (StateVarSym repr) => SVariable repr -> CSStateVar repr
pubSVar = stateVar public static

type SClass a = CS (a (Class a))

class (MethodSym repr, StateVarSym repr) => ClassSym repr where
  type Class repr
  buildClass :: Label -> Maybe Label -> [CSStateVar repr] -> [SMethod repr] -> 
    SClass repr
  -- enum :: Label -> [Label] -> repr (Scope repr) -> SClass repr
  extraClass :: Label -> Maybe Label -> [CSStateVar repr] -> [SMethod repr] -> 
    SClass repr
  implementingClass :: Label -> [Label] -> [CSStateVar repr] -> [SMethod repr] 
    -> SClass repr

  docClass :: String -> SClass repr -> SClass repr

type FSModule a = FS (a (Module a))

class (ClassSym repr) => ModuleSym repr where
  type Module repr
  buildModule :: Label -> [Label] -> [SMethod repr] -> [SClass repr] -> 
    FSModule repr

-- Data

data ODEInfo repr = ODEInfo {
  indepVar :: SVariable repr,
  depVar :: SVariable repr,
  otherVars :: [SVariable repr],
  tInit :: SValue repr,
  tFinal :: SValue repr,
  initVal :: SValue repr,
  ode :: SValue repr
}

odeInfo :: SVariable repr -> SVariable repr -> 
  [SVariable repr] -> SValue repr -> 
  SValue repr -> SValue repr -> SValue repr -> 
  ODEInfo repr
odeInfo = ODEInfo

data ODEOptions repr = ODEOptions {
  solveMethod :: ODEMethod,
  absTol :: SValue repr,
  relTol :: SValue repr,
  stepSize :: SValue repr
}

odeOptions :: ODEMethod -> SValue repr -> SValue repr -> 
  SValue repr -> ODEOptions repr
odeOptions = ODEOptions

data ODEMethod = RK45 | BDF | Adams

-- Utility

convType :: (TypeSym repr) => CodeType -> VSType repr
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